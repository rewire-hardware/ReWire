{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module ReWire.Crust.Desugar (desugar, addMainModuleHead) where

import ReWire.Annotation (noAnn, Annote (..))
import ReWire.Error
import ReWire.Crust.Rename hiding (Module)
import ReWire.SYB

import Control.Monad.Catch (MonadCatch)
import Control.Monad (replicateM, (>=>), void, when, msum, unless)
import Control.Monad.State (runStateT, StateT, MonadState (..), modify)
import Data.Foldable (foldl', foldrM)
import Data.Functor.Identity (Identity (..))
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (pack)

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified Data.Map.Strict              as Map

-- | Adds the "module Main where" if no module given (but not the "main"
--   export).
addMainModuleHead :: Module a -> Module a
addMainModuleHead = \ case
      Module l Nothing ps imps ds -> Module l (Just $ ModuleHead l (ModuleName l "Main") Nothing $ Just $ ExportSpecList l []) ps imps ds
      m                           -> m

-- | Desugar into lambdas then normalize the lambdas.
desugar :: (MonadError AstError m, MonadCatch m) => Renamer -> Module Annote -> m (Module Annote)
desugar rn = fmap fst . flip runStateT 0 .
      ( pure
      >=> runT  -- Each "runT" is a separate pass over the AST.
            ( desugarNegs
           <> desugarDos
           <> desugarInfix
           <> desugarFuns
           <> desugarRecords rn
            )
      >=> runT flattenLambdas
      >=> runT
            ( depatLambdas
           <> lambdasToCases
            )
      >=> runT flattenAlts
      >=> runT desugarGuards
      >=> runT
            ( desugarIfs
           <> wheresToLets
            )
      >=> runT
            ( desugarLets
           <> desugarNegLitPats
           <> desugarTuples
           <> normIds
           <> deparenify
           <> normTyContext
           <> desugarTyFuns
            )
      >=> runT flattenAlts -- again
      >=> runT
            ( desugarAsPats
           <> liftDiscriminator
            )
      )

type FreshT = StateT Int

fresh :: Monad m => Annote -> FreshT m (Name Annote)
fresh l = do
      x <- get
      modify (+ 1)
      pure $ Ident l $ "$" <> show x

-- | record ctor name |-> (field, field place, ctor arity)
type FieldInfo = (Name Annote, (Name Annote, Type Annote, Int, Int))

-- | Desugar record type defs, field accessors, record patterns, record
--   construction expressions, and record update syntax.
-- > R {f = a}
-- becomes
-- > R _ a _
-- and
-- > r { f = a }
-- becomes
-- > case r of R x0 _ x1 -> R x0 a x1
desugarRecords :: (MonadCatch m, MonadError AstError m) => Renamer -> Transform (FreshT m)
desugarRecords rn = (\ (Module (l :: Annote) h p imps decls) -> do
            let fs = concatMap fieldInfo $ Map.assocs $ filterRecords $ getLocalCtorSigs rn
            ds <- concatMap tupList <$> mapM fieldDecl fs
            pure $ Module l h p imps (decls ++ ds))
      ||> (\ (PRec (l :: Annote) c fpats) -> do
            let sig  = lookupCtorSig rn (rename Value rn c :: FQName)
            let flds = mapMaybe fst sig
            when (fpats /= [] && not (isRecordSig sig)) $ failAt l "Record pattern found, but not a record"
            unless (all (inSig flds pfToField) fpats)   $ failAt l "Field pattern for nonexistent field"
            pure $ PApp l c $ map (toPat l fpats) flds)
      ||> (\ (RecDecl (l :: Annote) n fs) ->
            pure $ ConDecl l n $ map (\ (FieldDecl _ _ t) -> t) $ concatMap flatten fs)
      ||> (\ x -> case x :: Exp Annote of
            -- R { b = e } becomes R undefined e undefined
            RecConstr l c fups -> do
                  let sig  = lookupCtorSig rn (rename Value rn c :: FQName)
                  let flds = mapMaybe fst sig
                  when (fups /= [] && not (isRecordSig sig)) $ failAt l "Record constructor found, but not a record"
                  unless (all (inSig flds fupToField) fups)  $ failAt l "Field initializer for nonexistent field"
                  pure $ foldl' (App l) (Con l c) $ map (toExp l fups) flds
            RecUpdate l _ [] -> failAt l "Empty record update"
            -- r { b = e } becomes case r of { R x1 _ x2 -> R x1 e x2 }
            RecUpdate l e fups -> do
                  (ctor, sig) <- case msum $ map fupToField fups of
                        Nothing -> failAt l "You cannot use `..' in a record update"
                        Just f  -> maybe (failAt l "Field update for nonexistent field") pure
                                 $ findCtorSigFromField rn (rename Value rn f :: FQName)
                  let flds = mapMaybe fst sig
                  pats <- mapM (toRUpPat l fups) flds
                  exps <- mapM (toRUpExp l fups) $ zip pats flds
                  pure $ Case l e
                        [ Alt l
                              (PApp l (qnamish ctor) pats)
                              (UnGuardedRhs l (foldl' (App l) (Con l $ qnamish ctor) exps))
                              Nothing
                        ]
            )
      ||> TId

      where fieldDecl :: Monad m => FieldInfo -> FreshT m (Decl Annote, Decl Annote)
            fieldDecl (ctor, (f, t, i, arr)) = do
                  let an s = MsgAnnote $ "Generated record field accessor for " <> pack (prettyPrint f) <> " at: " <> s
                  x  <- fresh $ an "x"
                  x' <- fresh $ an "x'"
                  pure $ (,)
                         ( TypeSig (an "TypeSig") [an "TypeSig Name" <$ f] (an "TypeSig Type" <$ t) )
                         $ PatBind (an "PatBind") (PVar (an "PVar") (an "PVar" <$ f))
                               ( UnGuardedRhs (an "UnGuardedRhs")
                                     ( Lambda (an "Lambda") [PVar (an "PVar") x]
                                           ( Case (an "Case") (Var (an "Var") (UnQual (an "Var") x))
                                                 [ Alt (an "Alt") (PApp (an "PApp") (UnQual (an "PApp") ctor) (argPats x' i arr))
                                                       (UnGuardedRhs (an "UnGuardedRhs") (Var (an "Var") (UnQual (an "Var") x')))
                                                       Nothing
                                                 ]
                                           )
                                     )
                               ) Nothing

            tupList :: (a, a) -> [a]
            tupList (x, y) = [x, y]

            flatten :: FieldDecl Annote -> [FieldDecl Annote]
            flatten (FieldDecl l xs t) = map (\ x -> FieldDecl l [x] t) xs

            -- TODO(chathhorn): remove this fieldinfo thing.
            fieldInfo :: (Name (), [(Name (), Type ())]) -> [FieldInfo]
            fieldInfo (ctor, fs) = map (fieldInfo' ctor $ length fs) $ zip [0..] fs

            fieldInfo' :: Name () -> Int -> (Int, (Name (), Type ())) -> FieldInfo
            fieldInfo' ctor arr (i, (f, t)) = (noAnn <$ ctor, (noAnn <$ f, noAnn <$ t, i, arr))

            filterRecords :: CtorSigs -> Map.Map (Name ()) [(Name (), Type ())]
            filterRecords = Map.mapMaybe $ \ a -> if isRecordSig a then Just $ toRecordSig a else Nothing

            isRecordSig :: [(Maybe a, t)] -> Bool
            isRecordSig [] = False
            isRecordSig s = not $ any (isNothing . fst) s

            toRecordSig :: [(Maybe a, t)] -> [(a, t)]
            toRecordSig = concatMap (\ (n, t) -> maybe [] (pure . (, t)) n)

            argPats :: Name Annote -> Int -> Int -> [Pat Annote]
            argPats x i tot = let a = ann x in
                  replicate i (PWildCard a) ++ [PVar a x] ++ replicate (tot - i - 1) (PWildCard a)

            inSig :: [FQName] -> (a -> Maybe FQName) -> a -> Bool
            inSig sig proj = maybe True (flip any sig . (==)) . proj

            pfToField :: PatField Annote -> Maybe FQName
            pfToField = \ case
                  PFieldPat _ f' _ -> Just $ rename Value rn f'
                  PFieldPun _ f'   -> Just $ rename Value rn f'
                  _                -> Nothing

            pfToPat :: FQName -> PatField Annote -> Pat Annote
            pfToPat f = \ case
                  PFieldPat _ _ p  -> p
                  PFieldPun l _    -> PVar l $ l <$ name f
                  PFieldWildcard l -> PVar l $ l <$ name f

            fupToField :: FieldUpdate Annote -> Maybe FQName
            fupToField = \ case
                  FieldUpdate _ f' _ -> Just $ rename Value rn f'
                  FieldPun _ f'      -> Just $ rename Value rn f'
                  _                  -> Nothing

            fupToExp :: FQName -> FieldUpdate Annote -> Exp Annote
            fupToExp f = \ case
                  FieldUpdate _ _ e  -> e
                  FieldPun l _       -> Var l $ UnQual l $ l <$ name f
                  FieldWildcard l    -> Var l $ UnQual l $ l <$ name f

            convField :: FQName -> b -> (a -> Maybe FQName) -> (a -> b) -> [a] -> b
            convField f d proj conv (a:as) = case proj a of
                  Just x | x == f        -> conv a
                  Just _                 -> convField f d proj conv as
                  Nothing {- wildcard -} -> convField f (conv a) proj conv as
            convField _ d _ _ []           = d

            toPat :: Annote -> [PatField Annote] -> FQName -> Pat Annote
            toPat l fpats f = convField f (PWildCard l) pfToField (pfToPat f) fpats

            toExp :: Annote -> [FieldUpdate Annote] -> FQName -> Exp Annote
            toExp l fups f = convField f (err l "uninitialized record field") fupToField (fupToExp f) fups

            toRUpPat :: Monad m => Annote -> [FieldUpdate Annote] -> FQName -> FreshT m (Pat Annote)
            toRUpPat l fups f = do
                  x <- fresh l
                  pure $ convField f (PVar l x) fupToField (PWildCard . ann) fups

            toRUpExp :: MonadError AstError m => Annote -> [FieldUpdate Annote] -> (Pat Annote, FQName) -> m (Exp Annote)
            toRUpExp l _    (PVar _ x, _) = pure $ Var l $ qnamish x
            toRUpExp l fups (_, f)        = convField f (failAt l "Something went wrong while desugaring a record update")
                                                fupToField (pure . fupToExp f) fups

-- | Turns Specials into normal identifiers.
normIds :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
normIds = (\ x -> case x :: QName Annote of
            Special l (UnitCon _)      -> pure $ UnQual l $ Ident l "()"
            Special l (ListCon _)      -> pure $ UnQual l $ Ident l "List"
            Special l (FunCon _)       -> pure $ UnQual l $ Ident l "->"
            -- I think this is only for the prefix constructor.
            Special l (TupleCon _ _ i) -> pure $ UnQual l $ mkTuple l i
            Special l (Cons _)         -> pure $ UnQual l $ Ident l "Cons")
      ||> TId

mkTuple :: Annote -> Int -> Name Annote
mkTuple l n = Ident l $ "(" ++ replicate (n - 1) ',' ++ ")"

-- | Removes parens in types, expressions, and patterns so they don't confuddle
--   everything.
deparenify :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
deparenify = (\ (Paren   (_ :: Annote) n) -> pure n)
         ||> (\ (PParen  (_ :: Annote) n) -> pure n)
         ||> (\ (TyParen (_ :: Annote) n) -> pure n)
         ||> (\ (DHParen (_ :: Annote) n) -> pure n)
         ||> TId

-- | Turns sections and infix ops into regular applications and lambdas.
desugarInfix :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
desugarInfix = (\ case
                 LeftSection l e (QVarOp l' op)  -> pure $ App l (Var l' op) e
                 LeftSection l e (QConOp l' op)  -> pure $ App l (Con l' op) e
                 RightSection l (QVarOp l' op) e -> do
                       x <- fresh l
                       pure $ Lambda l [PVar l x] $ App l (App l (Var l' op) $ Var l $ UnQual l x) e
                 RightSection l (QConOp l' op) e -> do
                       x <- fresh l
                       pure $ Lambda l [PVar l x] $ App l (App l (Con l' op) $ Var l $ UnQual l x) e
                 InfixApp l e1 (QVarOp l' op) e2 -> pure $ App l (App l (Var l' op) e1) e2
                 InfixApp l e1 (QConOp l' op) e2 -> pure $ App l (App l (Con l' op) e1) e2)
           ||> (\ (InfixConDecl (l :: Annote) a n b) -> pure $ ConDecl l n [a, b])
           ||> (\ (InfixMatch (l :: Annote) p1 n p2 rhs bs) -> pure $ Match l n (p1:p2) rhs bs)
           ||> (\ (DHInfix (l :: Annote) bind n) -> pure $ DHApp l (DHead l n) bind)
           ||> TId

-- | TODO Apparently this should actually desugar to guards:
-- > f (-k) = v
-- is actually sugar for
-- > f z | z == negate (fromInteger k) = v
desugarNegLitPats :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
desugarNegLitPats = transform $
      \ (PLit (l :: Annote) (Negative l') lit) -> pure $ PLit l (Signless l') $ neg lit

neg :: Literal Annote -> Literal Annote
neg = \ case
      Int        l n s -> Int        l (-n) s
      Frac       l n s -> Frac       l (-n) s
      PrimInt    l n s -> PrimInt    l (-n) s
      PrimWord   l n s -> PrimWord   l (-n) s
      PrimFloat  l n s -> PrimFloat  l (-n) s
      PrimDouble l n s -> PrimDouble l (-n) s
      n                -> n

-- AFTER: desugarFuns
-- | Turns tuples into applications of a TupleN constructor (also in types and pats):
-- > (x, y, z)
-- becomes
-- > (Tuple3 x y z)
desugarTuples :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
desugarTuples = (\ (Tuple l _ es)   -> pure $ foldl' (App l) (Con l $ UnQual l $ mkTuple l $ length es) es)
            ||> (\ (TyTuple l _ ts) -> pure $ foldl' (TyApp l) (TyCon l $ UnQual l $ mkTuple l $ length ts) ts)
            ||> (\ (PTuple l _ ps)  -> pure $ PApp l (UnQual l $ mkTuple l $ length ps) ps)
            ||> TId

-- | Turns piece-wise function definitions into a single PatBind with a lambda
--   and case expression on the RHS. E.g.:
-- > f p1 p2 = rhs1
-- > f q1 q2 = rhs2
-- becomes
-- > f = \ $1 $2 -> case ($1, $2) of { (p1, p2) -> rhs1; (q1, q2) -> rhs2 }
desugarFuns :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
desugarFuns = transform $ \ case
      FunBind l ms@(Match l' name pats _ _:_) -> do
            e <- buildLambda l ms $ length pats
            pure $ PatBind l (PVar l' name) (UnGuardedRhs l e) Nothing
      -- Turn guards on PatBind into guards on case (of unit) alts.
      PatBind l p rhs@(GuardedRhss l' _) binds ->
            pure $ PatBind l p (UnGuardedRhs l' $ Case l' (Con l' $ Special l' $ UnitCon l') [Alt l' (PWildCard l') rhs binds]) Nothing

      where buildLambda :: (MonadCatch m, MonadError AstError m) => Annote -> [Match Annote] -> Int -> FreshT m (Exp Annote)
            buildLambda l ms 1 = do
                  alts <- mapM toAlt ms
                  x <- fresh l
                  pure $ Lambda l [PVar l x] $ Case l (Var l $ UnQual l x) alts
            buildLambda l ms arrity = do
                  alts <- mapM toAlt ms
                  xs <- replicateM arrity (fresh l)
                  pure $ Lambda l (map (PVar l) xs) $ Case l (Tuple l Boxed (map (Var l . UnQual l) xs)) alts

            toAlt :: MonadError AstError m => Match Annote -> FreshT m (Alt Annote)
            toAlt (Match l' _ [p] rhs binds) = pure $ Alt l' p rhs binds
            toAlt (Match l' _ ps  rhs binds) = pure $ Alt l' (PTuple l' Boxed ps) rhs binds
            toAlt m                          = failAt (ann m) $ "Unsupported decl syntax: " <> pack (show (() <$ m))

-- | Turns
-- > case e of {...}
-- into
-- > (\ x -> case x of {...}) e
liftDiscriminator :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
liftDiscriminator = transform $ \ (Case l e alts) -> do
      x <- fresh l
      pure $ App l (Lambda l [PVar l x] $ Case l (Var l $ UnQual l x) alts) e

-- | Turn cases with multiple alts into cases with two alts: an alt with a
-- pattern and another with a default, wildcard branch.
-- > case x of
-- >   p1 -> e1
-- >   p2 -> e2
-- >   p3 -> e3
-- becomes
-- > case x of
-- >   p1 -> e1
-- >   _  -> case x of
-- >           p2 -> e2
-- >            _ -> case x of
-- >                   p3 -> e3
-- >                    _ -> undefined
flattenAlts :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
flattenAlts = transform $ \ (Case l e alts) -> pure $ Case l e $ flatten l e alts
      where flatten :: Annote -> Exp Annote -> [Alt Annote] -> [Alt Annote]
            flatten _ _ [alt'@Alt {}] = [ alt' ]
            flatten _ _ alts'@[Alt {}, Alt _ (PWildCard _) _ _] = alts'
            flatten l e (Alt l' p' rhs' binds' : alts')
                  = [Alt l' p' rhs' binds', Alt l (PWildCard l) (UnGuardedRhs l $ Case l e $ flatten l e alts') Nothing]

-- | Should run after function desugarage. From the Haskell 98 report:
-- > case v of
-- >   p | g1 -> e1
-- >     | gn -> en where { decls }
-- >   _    -> e'
-- >
-- becomes
-- > case e' of
-- >   y -> case v of
-- >     p -> let { decls } in
-- >       if g1 then e1 else if gn then en else y
-- >     _ -> y
desugarGuards :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
desugarGuards = transform $ \ case
            Case l1 v [Alt l2 p (GuardedRhss l3 rhs) binds, Alt l4 (PWildCard l5) (UnGuardedRhs l6 e') Nothing] -> do
                  y <- fresh l1
                  pure $ Case l1 e'
                        [ Alt l2 (PVar l2 y)
                              ( UnGuardedRhs l3 $ Case l3 v
                                    [ Alt l3 p (UnGuardedRhs l3 $ toLet l3 (Var l6 $ UnQual l6 y) binds rhs) Nothing
                                    , Alt l4 (PWildCard l5) (UnGuardedRhs l6 $ Var l6 $ UnQual l6 y) Nothing
                                    ]
                              )
                              Nothing
                        ]
            Case l1 v [Alt l2 p (GuardedRhss l3 rhs) binds] ->
                  pure $ Case l1 v [ Alt l2 p (UnGuardedRhs l3 $ toLet l3 (err l1 "pattern match failure") binds rhs) Nothing ]
      where toIfs :: GuardedRhs Annote -> Exp Annote -> Exp Annote
            toIfs (GuardedRhs l [Qualifier _ g1] e1) = If l g1 e1

            toLet :: Annote -> Exp Annote -> Maybe (Binds Annote) -> [GuardedRhs Annote] -> Exp Annote
            toLet l y (Just binds) rhs =  Let l binds $ foldr toIfs y rhs
            toLet _ y Nothing rhs      =  foldr toIfs y rhs

err :: Annote -> String -> Exp Annote
err l s = App l (Var l $ UnQual l $ Ident l "error") $ Lit l $ String l s ""


-- | Turns where clauses into lets. Only valid after guard desugarage. E.g.:
-- > f x = a where a = b
-- becomes
-- > f x = let a = b in a
wheresToLets :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
wheresToLets = (\ case
                  Match l name ps (UnGuardedRhs l' e) (Just binds) -> pure $ Match l name ps (UnGuardedRhs l' $ Let l' binds e) Nothing
                  Match l _ _ (GuardedRhss _ _) _                  -> failAt (l :: Annote) "Guards are not supported")
           ||> (\ case
                  PatBind l p (UnGuardedRhs l' e) (Just binds) -> pure $ PatBind l p (UnGuardedRhs l' $ Let l' binds e) Nothing
                  PatBind l _ (GuardedRhss _ _) _              -> failAt (l :: Annote) "Guards are not supported")
           ||> (\ case
                  Alt l p (UnGuardedRhs l' e) (Just binds) -> pure $ Alt l p (UnGuardedRhs l' $ Let l' binds e) Nothing
                  a@(Alt l _ (GuardedRhss _ _) _)          -> failAt (l :: Annote) $ "Guards are not supported: " <> (pack $ show $ void a))
           ||> TId

-- | Turns do-notation into a series of >>= \ x ->. Turns LetStmts into Lets.
--   E.g.:
-- > do p1 <- m
-- >    let p2 = e
-- >    return e
-- becomes
-- > m >>= (\ p1 -> (let p2 = e in return e))
desugarDos :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
desugarDos = transform $ \ (Do l stmts) -> transDo l stmts
      where transDo :: (MonadCatch m, MonadError AstError m) => Annote -> [Stmt Annote] -> FreshT m (Exp Annote)
            transDo l = \ case
                  Generator l' p e : stmts -> App l' (App l' (Var l' $ UnQual l' $ Symbol l' ">>=") e) . Lambda l' [p] <$> transDo l stmts
                  [Qualifier _ e]          -> pure e
                  Qualifier l' e : stmts   -> App l' (App l' (Var l' $ UnQual l' $ Symbol l' ">>=") e) . Lambda l' [PWildCard l'] <$> transDo l stmts
                  LetStmt l' binds : stmts -> Let l' binds <$> transDo l stmts
                  s : _                    -> failAt (ann s) $ "Unsupported syntax in do-block: " <> (pack $ show (() <$ s))
                  []                       -> failAt l "Ill-formed do-block"

normTyContext :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
normTyContext = transform $ \ x -> case x :: Type Annote of
      TyForall l tvs Nothing t               -> pure $ TyForall l tvs (Just $ CxTuple l []) t
      TyForall l tvs (Just (CxEmpty _)) t    -> pure $ TyForall l tvs (Just $ CxTuple l []) t
      TyForall l tvs (Just (CxSingle _ a)) t -> pure $ TyForall l tvs (Just $ CxTuple l [a]) t

-- | Turns the type a -> b into (->) a b.
desugarTyFuns :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
desugarTyFuns = transform $
      \ (TyFun (l :: Annote) a b) -> pure $ TyApp l (TyApp l (TyCon l (UnQual l (Ident l "->"))) a) b

-- TODO(chathhorn): recursive bindings?
-- | Turns Lets into Cases. Assumes functions in Lets are already desugared.
--   E.g.:
-- > let p = e1
-- >     q = e2
-- > in e3
-- becomes
-- > case e1 of { p -> (case e2 of { q -> e3 } }
desugarLets :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
desugarLets = transform $ \ case
      Let _ (BDecls _ ds) e -> foldrM transLet e $ filter isPatBind ds
      n@Let{}               -> failAt (ann n) "Unsupported let syntax"

      where transLet :: MonadError AstError m => Decl Annote -> Exp Annote -> FreshT m (Exp Annote)
            transLet (PatBind l p (UnGuardedRhs l' e1) Nothing) inner = pure $ Case l e1 [Alt l p (UnGuardedRhs l' inner) Nothing]
            transLet n _                                              = failAt (ann n) "Unsupported syntax in a let binding"

            isPatBind :: Decl Annote -> Bool
            isPatBind PatBind {} = True
            isPatBind _          = False

-- | Turns ifs into cases.
-- > if e1 then e2 else e3
-- becomes
-- > case e1 of { True -> e2; False -> e3 }
desugarIfs :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
desugarIfs = transform $
      \ (If (l :: Annote) e1 e2 e3) -> pure $ Case l e1
            [ Alt l (PApp l (UnQual l $ Ident l "True")  []) (UnGuardedRhs l e2) Nothing
            , Alt l (PApp l (UnQual l $ Ident l "False") []) (UnGuardedRhs l e3) Nothing
            ]

desugarNegs :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
desugarNegs = transform $
      \ (NegApp (l :: Annote) e) -> pure $ App l (App l (Var l $ UnQual l $ Ident l "-") $ Lit l $ Int l 0 "0") e

-- | Turns Lambdas with several bindings into several lambdas with single
--   bindings. E.g.:
-- > \ p1 p2 -> e
-- becomes
-- > \ p1 -> \ p2 -> e
flattenLambdas :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
flattenLambdas = transform $
      \ (Lambda (l :: Annote) ps e) -> pure $ foldr (Lambda l . pure) e ps

-- | Replaces non-var patterns in lambdas with a fresh var and a case. E.g.:
-- > \ (a, b) -> e
-- becomes
-- > \ $x -> case $x of { (a, b) -> e }
depatLambdas :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
depatLambdas = transform $ \ case
      n@(Lambda _ [PVar _ _] _) -> pure n
      Lambda l [p] e          -> do
            x <- fresh l
            pure $ Lambda l [PVar l x] (Case l (Var l $ UnQual l x) [Alt l p (UnGuardedRhs l e) Nothing])

-- | Desugars as-patterns in (and only in) cases into more cases. Should run
--   after depatLambdas. E.g.:
-- > case e1 of
-- >   x@(C y@p) -> e2
-- becomes
-- > case e1 of { C p -> (\ x -> ((\ y -> e2) p)) (C p) }
desugarAsPats :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
desugarAsPats = transform $ \ (Alt l p (UnGuardedRhs l' e) Nothing) -> do
            p'  <- deWild p
            app <- foldrM (mkApp l) e $ getAses p'
            pure $ Alt l (deAs p') (UnGuardedRhs l' app) Nothing

      where mkApp :: (Functor m, MonadError AstError m) => Annote -> (Pat Annote, Pat Annote) -> Exp Annote -> FreshT m (Exp Annote)
            mkApp l (p, p') e = App l (Lambda l [p] e) <$> patToExp p'

            getAses :: Pat Annote -> [(Pat Annote, Pat Annote)]
            getAses = runQ $ query $ \ case
                  PAsPat (l :: Annote) n p -> [(PVar l n, p)]
                  _                        -> []

            deAs :: Pat Annote -> Pat Annote
            deAs = runIdentity . runPureT (transform $ \ case
                  PAsPat (_ :: Annote) _ p -> pure p
                  n                        -> pure n)

            deWild :: MonadCatch m => Pat Annote -> FreshT m (Pat Annote)
            deWild = runT $ transform $
                  \ (PWildCard l) -> PVar l <$> fresh l

            patToExp :: (Functor m, MonadError AstError m) => Pat Annote -> FreshT m (Exp Annote)
            patToExp = \ case
                  PVar l n                -> pure $ Var l $ UnQual l n
                  PLit l (Signless _) n   -> pure $ Lit l n
                  -- PNPlusK _name _int ->
                  PApp l n ps             -> foldl' (App l) (Con l n) <$> mapM patToExp ps
                  PList l ps              -> List l <$> mapM patToExp ps
                  -- PRec _qname _patfields ->
                  PAsPat _ _ p            -> patToExp p
                  PIrrPat _ p             -> patToExp p
                  PatTypeSig _ p _        -> patToExp p
                  -- PViewPat _exp _pat ->
                  PBangPat _ p            -> patToExp p
                  p                       -> failAt (ann p) $ "Unsupported pattern: " <> (pack $ show (() <$ p))

-- | Turns beta-redexes into cases. E.g.:
-- > (\ x -> e2) e1
-- becomes
-- > case e1 of { x -> e2 }
lambdasToCases :: (MonadCatch m, MonadError AstError m) => Transform (FreshT m)
lambdasToCases = transform $
      \ (App (l :: Annote) (Lambda _ [p] e2) e1) -> pure $ Case l e1 [Alt l p (UnGuardedRhs l e2) Nothing]
