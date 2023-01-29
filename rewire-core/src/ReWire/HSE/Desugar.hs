{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.HSE.Desugar (desugar, addMainModuleHead) where

import ReWire.Annotation (noAnn, Annote (..))
import ReWire.Error (MonadError, AstError, failAt)
import ReWire.HSE.Rename (Renamer, FQName, CtorSigs, qnamish, name, Namespace (Value), rename, getLocalCtorSigs, lookupCtorSig, findCtorSigFromField)
import ReWire.SYB (Tr (TId, TM, T), transformTr, transform, transformM, query)

import Control.Monad (replicateM, (>=>), void, when, msum, unless)
import Control.Monad.State (evalStateT, MonadState (..), modify)
import Data.Foldable (foldl', foldrM)
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (pack)

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified Data.Map.Strict as Map

data Desugar m = Desugar
      { dsModule   :: Tr m (Module Annote)
      , dsPat      :: Tr m (Pat Annote)
      , dsConDecl  :: Tr m (ConDecl Annote)
      , dsExp      :: Tr m (Exp Annote)
      , dsType     :: Tr m (Type Annote)
      , dsDeclHead :: Tr m (DeclHead Annote)
      , dsBinds    :: Tr m (Binds Annote)
      , dsMatch    :: Tr m (Match Annote)
      , dsAlt      :: Tr m (Alt Annote)
      , dsQName    :: Tr m (QName Annote)
      , dsDecl     :: Tr m (Decl Annote)
      }

instance Monad m => Semigroup (Desugar m) where
      (<>) (Desugar f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11)
           (Desugar g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 g11)
            = Desugar (f1  <> g1)
                      (f2  <> g2)
                      (f3  <> g3)
                      (f4  <> g4)
                      (f5  <> g5)
                      (f6  <> g6)
                      (f7  <> g7)
                      (f8  <> g8)
                      (f9  <> g9)
                      (f10 <> g10)
                      (f11 <> g11)

instance Monad m => Monoid (Desugar m) where
      mempty = Desugar TId TId TId TId TId TId TId TId TId TId TId

pass :: Monad m => Desugar m -> Module Annote -> m (Module Annote)
pass (Desugar f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11)
      =   transformTr f11
      >=> transformTr f10
      >=> transformTr f9
      >=> transformTr f8
      >=> transformTr f7
      >=> transformTr f6
      >=> transformTr f5
      >=> transformTr f4
      >=> transformTr f3
      >=> transformTr f2
      >=> transformTr f1

-- | Adds the "module Main where" if no module given (but not the "main"
--   export).
addMainModuleHead :: Module a -> Module a
addMainModuleHead = \ case
      Module l Nothing ps imps ds -> Module l (Just $ ModuleHead l (ModuleName l "Main") Nothing $ Just $ ExportSpecList l []) ps imps ds
      m                           -> m

-- | Desugar into lambdas then normalize the lambdas.
desugar :: MonadError AstError m => Renamer -> Module Annote -> m (Module Annote)
desugar rn = flip evalStateT 0 .
      ( pure
      >=> pass desugarInfix
      >=> pass
            ( desugarNegs
           <> desugarDos
           <> desugarInfix
           <> desugarFuns
           <> desugarRecords rn
            )
      >=> pass flattenLambdas
      >=> pass
            ( depatLambdas
           <> lambdasToCases
            )
      >=> pass flattenAlts
      >=> pass desugarGuards
      >=> pass
            ( desugarIfs
           <> wheresToLets
            )
      >=> pass
            ( desugarLets
           <> desugarNegLitPats
           <> desugarTuples
           <> normIds
           <> deparenify
           <> normTyContext
           <> desugarTyFuns
            )
      >=> pass flattenAlts -- again
      >=> pass
            ( desugarAsPats
           <> liftDiscriminator
            )
      )

type Fresh = Int

fresh :: (MonadState Fresh m, Monad m) => Annote -> m (Name Annote)
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
desugarRecords :: (MonadState Fresh m, MonadError AstError m) => Renamer -> Desugar m
desugarRecords rn = mempty
      { dsModule = TM $ \ case
            Module l h p imps decls -> do
                  let fs = concatMap fieldInfo $ Map.assocs $ filterRecords $ getLocalCtorSigs rn
                  ds <- concatMap tupList <$> mapM fieldDecl fs
                  pure $ Module l h p imps (decls <> ds)
            e                       -> pure e
      , dsPat = TM $ \ case
            PRec l c fpats -> do
                  let sig  = lookupCtorSig rn (rename Value rn c :: FQName)
                  let flds = mapMaybe fst sig
                  when (fpats /= [] && not (isRecordSig sig)) $ failAt l "Record pattern found, but not a record"
                  unless (all (inSig flds pfToField) fpats)   $ failAt l "Field pattern for nonexistent field"
                  pure $ PApp l c $ map (toPat l fpats) flds
            e              -> pure e

      , dsConDecl = T $ \ case
            RecDecl l n fs -> ConDecl l n $ map (\ (FieldDecl _ _ t) -> t) $ concatMap flatten fs
            e              -> e
      , dsExp = TM $ \ case
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
            e -> pure e
      }

      where fieldDecl :: MonadState Fresh m => FieldInfo -> m (Decl Annote, Decl Annote)
            fieldDecl (ctor, (f, t, i, arr)) = do
                  let an s = MsgAnnote $ "Generated record field accessor for " <> pack (prettyPrint f) <> " at: " <> s
                  x  <- fresh $ an "x"
                  x' <- fresh $ an "x'"
                  pure   ( TypeSig (an "TypeSig") [an "TypeSig Name" <$ f] (an "TypeSig Type" <$ t)
                         , PatBind (an "PatBind") (PVar (an "PVar") (an "PVar" <$ f))
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
                        )

            tupList :: (a, a) -> [a]
            tupList (x, y) = [x, y]

            flatten :: FieldDecl Annote -> [FieldDecl Annote]
            flatten (FieldDecl l xs t) = map (\ x -> FieldDecl l [x] t) xs

            -- TODO(chathhorn): remove this fieldinfo thing.
            fieldInfo :: (Name (), [(Name (), Type ())]) -> [FieldInfo]
            fieldInfo (ctor, fs) = zipWith (curry $ fieldInfo' ctor $ length fs) [0..] fs

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
                  replicate i (PWildCard a) <> [PVar a x] <> replicate (tot - i - 1) (PWildCard a)

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

            toRUpPat :: MonadState Fresh m => Annote -> [FieldUpdate Annote] -> FQName -> m (Pat Annote)
            toRUpPat l fups f = do
                  x <- fresh l
                  pure $ convField f (PVar l x) fupToField (PWildCard . ann) fups

            toRUpExp :: MonadError AstError m => Annote -> [FieldUpdate Annote] -> (Pat Annote, FQName) -> m (Exp Annote)
            toRUpExp l _    (PVar _ x, _) = pure $ Var l $ qnamish x
            toRUpExp l fups (_, f)        = convField f (failAt l "Something went wrong while desugaring a record update")
                                                fupToField (pure . fupToExp f) fups

-- | Turns Specials into normal identifiers.
normIds :: Monad m => Desugar m
normIds = mempty { dsQName = T $ \ case
      Special l (UnitCon _)      -> UnQual l $ Ident l "()"
      Special l (ListCon _)      -> UnQual l $ Ident l "[_]"
      Special l (FunCon _)       -> UnQual l $ Ident l "->"
      -- I think this is only for the prefix constructor.
      Special l (TupleCon _ _ i) -> UnQual l $ mkTuple l i
      Special l (Cons _)         -> UnQual l $ Ident l "(:)"
      e                          -> e }

mkTuple :: Annote -> Int -> Name Annote
mkTuple l n = Ident l $ "(" <> replicate (n - 1) ',' <> ")"

-- | Removes parens in types, expressions, and patterns so they don't confuddle
--   everything.
deparenify :: Monad m => Desugar m
deparenify = mempty
      { dsExp = T $ \ case
            Paren   _ n -> n
            e           -> e
      , dsPat = T $ \ case
            PParen  _ n -> n
            e           -> e
      , dsType = T $ \ case
            TyParen _ n -> n
            e           -> e
      , dsDeclHead = T $ \ case
            DHParen _ n -> n
            e           -> e
      }

-- BEFORE: desugarFuns
-- | Turns sections and infix ops into regular applications and lambdas.
desugarInfix :: MonadState Fresh m => Desugar m
desugarInfix = mempty
      { dsExp = TM $ \ case
            LeftSection l e (QVarOp l' op)  -> pure $ App l (Var l' op) e
            LeftSection l e (QConOp l' op)  -> pure $ App l (Con l' op) e
            RightSection l (QVarOp l' op) e -> do
                  x <- fresh l
                  pure $ Lambda l [PVar l x] $ App l (App l (Var l' op) $ Var l $ UnQual l x) e
            RightSection l (QConOp l' op) e -> do
                  x <- fresh l
                  pure $ Lambda l [PVar l x] $ App l (App l (Con l' op) $ Var l $ UnQual l x) e
            InfixApp l e1 (QVarOp l' op) e2 -> pure $ App l (App l (Var l' op) e1) e2
            InfixApp l e1 (QConOp l' op) e2 -> pure $ App l (App l (Con l' op) e1) e2
            e                               -> pure e
      , dsConDecl = T $ \ case
            InfixConDecl l a n b -> ConDecl l n [a, b]
            e                    -> e
      , dsMatch = T $ \ case
            InfixMatch l p1 n p2 rhs bs -> Match l n (p1:p2) rhs bs
            e                           -> e
      , dsDeclHead = T $ \ case
            DHInfix l bind n -> DHApp l (DHead l n) bind
            e                -> e
      }

-- | TODO Apparently this should actually desugar to guards:
-- > f (-k) = v
-- is actually sugar for
-- > f z | z == negate (fromInteger k) = v
desugarNegLitPats :: Monad m => Desugar m
desugarNegLitPats = mempty {dsPat = T $ \ case
      PLit l (Negative l') lit -> PLit l (Signless l') $ neg lit
      p                        -> p}

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
desugarTuples :: Monad m => Desugar m
desugarTuples = mempty
      { dsExp = T $ \ case
            Tuple l _ es   -> foldl' (App l) (Con l $ UnQual l $ mkTuple l $ length es) es
            e              -> e
      , dsType = T $ \ case
            TyTuple l _ ts -> foldl' (TyApp l) (TyCon l $ UnQual l $ mkTuple l $ length ts) ts
            t              -> t
      , dsPat = T $ \ case
            PTuple l _ ps  -> PApp l (UnQual l $ mkTuple l $ length ps) ps
            p              -> p
      }

-- BEFORE: desugarTyFuns
-- AFTER: desugarInfix
-- | Turns piece-wise function definitions into a single PatBind with a lambda
--   and case expression on the RHS. E.g.:
-- > f p1 p2 = rhs1
-- > f q1 q2 = rhs2
-- becomes
-- > f = \ $1 $2 -> case ($1, $2) of { (p1, p2) -> rhs1; (q1, q2) -> rhs2 }
desugarFuns :: (MonadState Fresh m, MonadError AstError m) => Desugar m
desugarFuns = mempty
      { dsModule = TM $ \ case
            Module man hd prags imps ds -> Module man hd prags imps <$> mapM (desugarFun $ tySigMap ds) ds
            m                           -> pure m
      , dsBinds = TM $ \ case
            BDecls ban ds               -> BDecls ban <$> mapM (desugarFun $ tySigMap ds) ds
            b                           -> pure b
      }
      where desugarFun :: (MonadState Fresh m, MonadError AstError m) => [(Name Annote, Type Annote)] -> Decl Annote -> m (Decl Annote)
            desugarFun ts = \ case
                  FunBind l ms@(Match l' name pats _ _:_)  -> do
                        let tps = paramTys <$> lookup name ts
                        alts <- mapM (toAlt ts tps) ms
                        e    <- buildLambda l tps alts $ length pats
                        pure $ PatBind l (PVar l' name) (UnGuardedRhs l e) Nothing
                  -- Turn guards on PatBind into guards on case (of unit) alts.
                  PatBind l p rhs@(GuardedRhss l' _) binds -> pure $ PatBind l p (UnGuardedRhs l' $ Case l' (Con l' $ Special l' $ UnitCon l') [Alt l' (PWildCard l') rhs binds]) Nothing
                  d                                        -> pure d

            buildLambda :: (MonadState Fresh m, MonadError AstError m) => Annote -> Maybe [Type Annote] -> [Alt Annote] -> Int -> m (Exp Annote)
            buildLambda l tps alts 1 = do
                  x <- fresh l
                  pure $ Lambda l (annotateParams l [PVar l x] tps) $ Case l (Var l $ UnQual l x) alts
            buildLambda l tps alts arity = do
                  xs <- replicateM arity (fresh l)
                  pure $ Lambda l (annotateParams l (PVar l <$> xs) tps) $ Case l (Tuple l Boxed (map (Var l . UnQual l) xs)) alts

            toAlt :: (MonadState Fresh m, MonadError AstError m) => [(Name Annote, Type Annote)] -> Maybe [Type Annote] -> Match Annote -> m (Alt Annote)
            toAlt ts tps = \ case
                  Match l' _ [p] rhs binds -> pure $ Alt l' (annotatePVars ts $ annotateParam l' p tps) rhs binds
                  Match l' _ ps  rhs binds -> pure $ Alt l' (PTuple l' Boxed $ map (annotatePVars ts) $ annotateParams l' ps tps) rhs binds
                  m                        -> failAt (ann m) $ "Unsupported decl syntax: " <> pack (show $ void m)

            annotateParams :: Annote -> [Pat Annote] -> Maybe [Type Annote] -> [Pat Annote]
            annotateParams an (p : ps')  pts@(Just (_ : pts')) = annotateParam an p pts : annotateParams an ps' (Just pts')
            annotateParams _  ps _                             = ps

            annotateParam :: Annote -> Pat Annote -> Maybe [Type Annote] -> Pat Annote
            annotateParam an p (Just (t : _)) = PatTypeSig an p t
            annotateParam _  p _              = p

            paramTys :: Type Annote -> [Type Annote]
            paramTys = paramTys' . rmTyContext

            paramTys' :: Type Annote -> [Type Annote]
            paramTys' = \ case
                  TyFun _ t1 t2 -> t1 : paramTys t2
                  TyParen _ t   -> paramTys' t
                  t             -> [t]

            rmTyContext :: Type Annote -> Type Annote
            rmTyContext = \ case
                  TyParen _ t      -> rmTyContext t
                  TyForall _ _ _ t -> t
                  t                -> t

-- | Turns
-- > case e of {...}
-- into
-- > (\ x -> case x of {...}) e
liftDiscriminator :: MonadState Fresh m => Desugar m
liftDiscriminator = mempty {dsExp = TM $ \ case
      Case l e alts -> do
            x <- fresh l
            pure $ App l (Lambda l [PVar l x] $ Case l (Var l $ UnQual l x) alts) e
      e             -> pure e}

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
flattenAlts :: Monad m => Desugar m
flattenAlts = mempty {dsExp = T $ \ case
      Case l e alts -> Case l e $ flatten l e alts
      e             -> e}
      where flatten :: Annote -> Exp Annote -> [Alt Annote] -> [Alt Annote]
            flatten l e = \ case
                  [a@Alt {}]                           -> [ a ]
                  as@[Alt {}, Alt _ (PWildCard _) _ _] -> as
                  (Alt l' p' rhs' binds' : as)         -> [Alt l' p' rhs' binds', Alt l (PWildCard l) (UnGuardedRhs l $ Case l e $ flatten l e as) Nothing]
                  as                                   -> as

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
desugarGuards :: MonadState Fresh m => Desugar m
desugarGuards = mempty { dsExp = TM $ \ case
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
            e -> pure e}
      where toLet :: Annote -> Exp Annote -> Maybe (Binds Annote) -> [GuardedRhs Annote] -> Exp Annote
            toLet l y binds rhs = maybe id (Let l) binds $ foldr toIfs y rhs

            toIfs :: GuardedRhs Annote -> Exp Annote -> Exp Annote
            toIfs = \ case
                  GuardedRhs l [Qualifier _ g1] e1 -> If l g1 e1
                  _                                -> id

err :: Annote -> String -> Exp Annote
err l s = App l (Var l $ UnQual l $ Ident l "error") $ Lit l $ String l s ""

-- | Turns where clauses into lets. Only valid after guard desugarage. E.g.:
-- > f x = a where a = b
-- becomes
-- > f x = let a = b in a
wheresToLets :: MonadError AstError m => Desugar m
wheresToLets = mempty
      { dsMatch = TM $ \ case
            Match l name ps (UnGuardedRhs l' e) (Just binds) -> pure $ Match l name ps (UnGuardedRhs l' $ Let l' binds e) Nothing
            Match l _ _ (GuardedRhss _ _) _                  -> failAt (l :: Annote) "Guards are not supported"
            m                                                -> pure m
      , dsDecl = TM $ \ case
            PatBind l p (UnGuardedRhs l' e) (Just binds) -> pure $ PatBind l p (UnGuardedRhs l' $ Let l' binds e) Nothing
            PatBind l _ (GuardedRhss _ _) _              -> failAt (l :: Annote) "Guards are not supported"
            p                                            -> pure p
      , dsAlt = TM $ \ case
            Alt l p (UnGuardedRhs l' e) (Just binds) -> pure $ Alt l p (UnGuardedRhs l' $ Let l' binds e) Nothing
            a@(Alt l _ (GuardedRhss _ _) _)          -> failAt (l :: Annote) $ "Guards are not supported: " <> pack (show $ void a)
            a                                        -> pure a
      }

-- | Turns do-notation into a series of >>= \ x ->. Turns LetStmts into Lets.
--   E.g.:
-- > do p1 <- m
-- >    let p2 = e
-- >    return e
-- becomes
-- > m >>= (\ p1 -> (let p2 = e in return e))
desugarDos :: (MonadState Fresh m, MonadError AstError m) => Desugar m
desugarDos = mempty { dsExp = TM $ \ case
      Do l stmts -> transDo l stmts
      e          -> pure e}
      where transDo :: (MonadState Fresh m, MonadError AstError m) => Annote -> [Stmt Annote] -> m (Exp Annote)
            transDo l = \ case
                  Generator l' p e : stmts -> App l' (App l' (Var l' $ UnQual l' $ Symbol l' ">>=") e) . Lambda l' [p] <$> transDo l stmts
                  [Qualifier _ e]          -> pure e
                  Qualifier l' e : stmts   -> App l' (App l' (Var l' $ UnQual l' $ Symbol l' ">>=") e) . Lambda l' [PWildCard l'] <$> transDo l stmts
                  LetStmt l' binds : stmts -> Let l' binds <$> transDo l stmts
                  s : _                    -> failAt (ann s) $ "Unsupported syntax in do-block: " <> pack (show $ void s)
                  []                       -> failAt l "Ill-formed do-block"

normTyContext :: Monad m => Desugar m
normTyContext = mempty { dsType = T $ \ case
      TyForall l tvs Nothing t               -> TyForall l tvs (Just $ CxTuple l []) t
      TyForall l tvs (Just (CxEmpty _)) t    -> TyForall l tvs (Just $ CxTuple l []) t
      TyForall l tvs (Just (CxSingle _ a)) t -> TyForall l tvs (Just $ CxTuple l [a]) t
      t                                      -> t}

-- | Turns the type a -> b into (->) a b.
desugarTyFuns :: Monad m => Desugar m
desugarTyFuns = mempty { dsType = T $ \ case
      TyFun (l :: Annote) a b -> TyApp l (TyApp l (TyCon l (UnQual l (Ident l "->"))) a) b
      t                       -> t}

-- TODO(chathhorn): recursive bindings?
-- | Turns Lets into Cases. Assumes functions in Lets are already desugared.
--   E.g.:
-- > let p = e1
-- >     q = e2
-- > in e3
-- becomes
-- > case e1 of { p -> (case e2 of { q -> e3 } }
desugarLets :: (MonadState Fresh m, MonadError AstError m) => Desugar m
desugarLets = mempty { dsExp = TM $ \ case
      Let _ (BDecls _ ds) e -> foldrM (transLet $ tySigMap ds) e $ filter isPatBind ds
      n@Let{}               -> failAt (ann n) "Unsupported let syntax"
      e                     -> pure e}
      where transLet :: (MonadState Fresh m, MonadError AstError m) => [(Name Annote, Type Annote)] -> Decl Annote -> Exp Annote -> m (Exp Annote)
            transLet ts (PatBind l p (UnGuardedRhs l' e1) Nothing) inner = pure $ Case l e1 [Alt l (annotatePVars ts p) (UnGuardedRhs l' inner) Nothing]
            transLet _ n _                                               = failAt (ann n) "Unsupported syntax in a let binding"

            isPatBind :: Decl Annote -> Bool
            isPatBind PatBind {} = True
            isPatBind _          = False

tySigMap :: [Decl Annote] -> [(Name Annote, Type Annote)]
tySigMap = concatMap tySig
      where tySig :: Decl Annote -> [(Name Annote, Type Annote)]
            tySig = \ case
                  TypeSig _ ns t -> (,t) <$> ns
                  _              -> []

annotatePVars :: [(Name Annote, Type Annote)] -> Pat Annote -> Pat Annote
annotatePVars ts = transform $ \ case
      PVar an n | Just t <- lookup n ts -> PatTypeSig an (PVar an n) t
      n                                 -> n

-- | Turns ifs into cases.
-- > if e1 then e2 else e3
-- becomes
-- > case e1 of { True -> e2; False -> e3 }
desugarIfs :: Monad m => Desugar m
desugarIfs = mempty { dsExp = T $ \ case
      If l e1 e2 e3 -> Case l e1
            [ Alt l (PApp l (UnQual l $ Ident l "True")  []) (UnGuardedRhs l e2) Nothing
            , Alt l (PApp l (UnQual l $ Ident l "False") []) (UnGuardedRhs l e3) Nothing
            ]
      e             -> e}

desugarNegs :: Monad m => Desugar m
desugarNegs = mempty { dsExp = T $ \ case
      NegApp l e -> App l (App l (Var l $ UnQual l $ Ident l "-") $ Lit l $ Int l 0 "0") e
      e          -> e}

-- | Turns Lambdas with several bindings into several lambdas with single
--   bindings. E.g.:
-- > \ p1 p2 -> e
-- becomes
-- > \ p1 -> \ p2 -> e
flattenLambdas :: Monad m => Desugar m
flattenLambdas = mempty { dsExp = T $ \ case
      Lambda l ps e -> foldr (Lambda l . pure) e ps
      e             -> e}

-- | Replaces non-var patterns in lambdas with a fresh var and a case. E.g.:
-- > \ (a, b) -> e
-- becomes
-- > \ $x -> case $x of { (a, b) -> e }
depatLambdas :: MonadState Fresh m => Desugar m
depatLambdas = mempty { dsExp = TM $ \ case
      n@(Lambda _ [PVar _ _] _) -> pure n
      Lambda l [p] e            -> do
            x <- fresh l
            pure $ Lambda l [PVar l x] (Case l (Var l $ UnQual l x) [Alt l p (UnGuardedRhs l e) Nothing])
      e                         -> pure e}

-- | Desugars as-patterns in (and only in) cases into more cases. Should run
--   after depatLambdas. E.g.:
-- > case e1 of
-- >   x@(C y@p) -> e2
-- becomes
-- > case e1 of { C p -> (\ x -> ((\ y -> e2) p)) (C p) }
desugarAsPats :: (MonadState Fresh m, MonadError AstError m) => Desugar m
desugarAsPats = mempty { dsAlt = TM $ \ case
      Alt l p (UnGuardedRhs l' e) Nothing -> do
            p'  <- deWild p
            app <- foldrM (mkApp l) e $ getAses p'
            pure $ Alt l (deAs p') (UnGuardedRhs l' app) Nothing
      e                                   -> pure e}

      where mkApp :: (MonadState Fresh m, MonadError AstError m) => Annote -> (Pat Annote, Pat Annote) -> Exp Annote -> m (Exp Annote)
            mkApp l (p, p') e = App l (Lambda l [p] e) <$> patToExp p'

            getAses :: Pat Annote -> [(Pat Annote, Pat Annote)]
            getAses p = [(PVar l n, p) | PAsPat (l :: Annote) n p <- query p]

            deAs :: Pat Annote -> Pat Annote
            deAs = transform $ \ case
                  PAsPat (_ :: Annote) _ p -> p
                  n                        -> n

            deWild :: MonadState Fresh m => Pat Annote -> m (Pat Annote)
            deWild = transformM $ \ case
                  PWildCard l -> PVar l <$> fresh l
                  p           -> pure p

            patToExp :: (MonadState Fresh m, MonadError AstError m) => Pat Annote -> m (Exp Annote)
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
                  p                       -> failAt (ann p) $ "Unsupported pattern: " <> pack (show $ void p)

-- | Turns beta-redexes into cases. E.g.:
-- > (\ x -> e2) e1
-- becomes
-- > case e1 of { x -> e2 }
lambdasToCases :: Monad m => Desugar m
lambdasToCases = mempty { dsExp = T $ \ case
      App l (Lambda _ [p] e2) e1 -> Case l e1 [Alt l p (UnGuardedRhs l e2) Nothing]
      e                          -> e}
