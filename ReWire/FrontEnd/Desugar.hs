{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module ReWire.FrontEnd.Desugar (desugar) where

import ReWire.Error
import ReWire.FrontEnd.Annotate
import ReWire.SYB

import Control.Monad.Catch (MonadCatch)
import Control.Monad (liftM, replicateM, (>=>), void)
import Control.Monad.State (runStateT, StateT, MonadState (..), modify)
import Data.Foldable (foldl', foldrM)
import Data.Functor.Identity (Identity (..))
import Data.Monoid ((<>))

import Language.Haskell.Exts.Annotated.Syntax

-- TODO(chathhorn): record syntax should be fairly easy to desugar.

-- | Desugar into lambdas then normalize the lambdas.
desugar :: (SyntaxError m, MonadCatch m) => Module Annote -> m (Module Annote)
desugar = liftM fst . flip runStateT 0 .
      ( runT  -- Each "runT" is a separate pass over the AST.
            ( desugarNegs
           <> desugarDos
           <> desugarInfix
           <> desugarFuns
            )
      >=> runT flattenLambdas
      >=> runT
            ( depatLambdas
           <> lambdasToCases
            )
      >=> runT liftDiscriminator
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
           <> addMainModuleHead
            )
      >=> runT
            ( flattenAlts -- again
            )
      >=> runT
            (
            desugarWildCards
           <> desugarAsPats
            )
      )

type FreshT = StateT Int

fresh :: Monad m => Annote -> FreshT m (Name Annote)
fresh l = do
      x <- get
      modify $ const $ x + 1
      return $ Ident l $ "$" ++ show x

-- | Turns Specials into normal identifiers.
normIds :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
normIds = (\ x -> case x :: QName Annote of
            Special l (UnitCon _)      -> return $ UnQual l $ Ident l "()"
            Special l (ListCon _)      -> return $ UnQual l $ Ident l "List"
            Special l (FunCon _)       -> return $ UnQual l $ Ident l "->"
            -- I think this is only for the prefix constructor.
            Special l (TupleCon _ _ i) -> return $ UnQual l $ mkTuple l i
            Special l (Cons _)         -> return $ UnQual l $ Ident l "Cons")
      ||> TId

mkTuple :: Annote -> Int -> Name Annote
mkTuple l n = Ident l $ "(" ++ replicate (n - 1) ',' ++ ")"

-- | Removes parens in types, expressions, and patterns so they don't confuddle
--   everything.
deparenify :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
deparenify = (\ (Paren   (_ :: Annote) n) -> return n)
         ||> (\ (PParen  (_ :: Annote) n) -> return n)
         ||> (\ (TyParen (_ :: Annote) n) -> return n)
         ||> (\ (DHParen (_ :: Annote) n) -> return n)
         ||> TId

-- | Turns sections and infix ops into regular applications and lambdas.
desugarInfix :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
desugarInfix = (\ case
                 LeftSection l e (QVarOp l' op)  -> return $ App l (Var l' op) e
                 LeftSection l e (QConOp l' op)  -> return $ App l (Con l' op) e
                 RightSection l (QVarOp l' op) e -> do
                       x <- fresh l
                       return $ Lambda l [PVar l x] $ App l (App l (Var l' op) $ Var l $ UnQual l x) e
                 RightSection l (QConOp l' op) e -> do
                       x <- fresh l
                       return $ Lambda l [PVar l x] $ App l (App l (Con l' op) $ Var l $ UnQual l x) e
                 InfixApp l e1 (QVarOp l' op) e2 -> return $ App l (App l (Var l' op) e1) e2
                 InfixApp l e1 (QConOp l' op) e2 -> return $ App l (App l (Con l' op) e1) e2)
           ||> (\ (InfixConDecl (l :: Annote) a n b) -> return $ ConDecl l n [a, b])
           ||> (\ (InfixMatch (l :: Annote) p1 n p2 rhs bs) -> return $ Match l n (p1:p2) rhs bs)
           ||> (\ (DHInfix (l :: Annote) bind n) -> return $ DHApp l (DHead l n) bind)
           ||> TId

-- | TODO Apparently this should actually desugar to guards:
-- > f (-k) = v
-- is actually sugar for
-- > f z | z == negate (fromInteger k) = v
desugarNegLitPats :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
desugarNegLitPats = transform $
      \ (PLit (l :: Annote) (Negative l') lit) -> return $ PLit l (Signless l') $ neg lit

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
desugarTuples :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
desugarTuples = (\ (Tuple l _ es)   -> return $ foldl' (App l) (Con l $ UnQual l $ mkTuple l $ length es) es)
            ||> (\ (TyTuple l _ ts) -> return $ foldl' (TyApp l) (TyCon l $ UnQual l $ mkTuple l $ length ts) ts)
            ||> (\ (PTuple l _ ps)  -> return $ PApp l (UnQual l $ mkTuple l $ length ps) ps)
            ||> TId

-- | Turns piece-wise function definitions into a single PatBind with a lambda
--   and case expression on the RHS. E.g.:
-- > f p1 p2 = rhs1
-- > f q1 q2 = rhs2
-- becomes
-- > f = \ $1 $2 -> case ($1, $2) of { (p1, p2) -> rhs1; (q1, q2) -> rhs2 }
desugarFuns :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
desugarFuns = transform $ \ case
      FunBind l ms@(Match l' name pats _ _:_) -> do
            e <- buildLambda l ms $ length pats
            return $ PatBind l (PVar l' name) (UnGuardedRhs l e) Nothing
      -- Turn guards on PatBind into guards on case (of unit) alts.
      PatBind l p rhs@(GuardedRhss l' _) binds ->
            return $ PatBind l p (UnGuardedRhs l' $ Case l' (Con l' $ Special l' $ UnitCon l') [Alt l' (PWildCard l') rhs binds]) Nothing

      where buildLambda :: (MonadCatch m, SyntaxError m) => Annote -> [Match Annote] -> Int -> FreshT m (Exp Annote)
            buildLambda l ms 1 = do
                  alts <- mapM toAlt ms
                  x <- fresh l
                  return $ Lambda l [PVar l x] $ Case l (Var l $ UnQual l x) alts
            buildLambda l ms arrity = do
                  alts <- mapM toAlt ms
                  xs <- replicateM arrity (fresh l)
                  return $ Lambda l (map (PVar l) xs) $ Case l (Tuple l Boxed (map (Var l . UnQual l) xs)) alts

            toAlt :: SyntaxError m => Match Annote -> FreshT m (Alt Annote)
            toAlt (Match l' _ [p] rhs binds) = return $ Alt l' p rhs binds
            toAlt (Match l' _ ps  rhs binds) = return $ Alt l' (PTuple l' Boxed ps) rhs binds
            toAlt m                          = failAt (ann m) "Unsupported decl syntax"

liftDiscriminator :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
liftDiscriminator = transform $ \ (Case l e alts) -> do
      x <- fresh l
      return $ App l (Lambda l [PVar l x] $ Case l (Var l $ UnQual l x) alts) e

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
flattenAlts :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
flattenAlts = transform $ \ (Case l e alts) -> return $ Case l e $ flatten l e alts
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
desugarGuards :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
desugarGuards = transform $ \ case
            Case l1 v [Alt l2 p (GuardedRhss l3 rhs) binds, Alt l4 (PWildCard l5) (UnGuardedRhs l6 e') Nothing] -> do
                  y <- fresh l1
                  return $ Case l1 e'
                        [ Alt l2 (PVar l2 y)
                              ( UnGuardedRhs l3 $ Case l3 v
                                    [ Alt l3 p (UnGuardedRhs l3 $ toLet l3 (Var l6 $ UnQual l6 y) binds rhs) Nothing
                                    , Alt l4 (PWildCard l5) (UnGuardedRhs l6 $ Var l6 $ UnQual l6 y) Nothing
                                    ]
                              )
                              Nothing
                        ]
            Case l1 v [Alt l2 p (GuardedRhss l3 rhs) binds] ->
                  return $ Case l1 v [ Alt l2 p (UnGuardedRhs l3 $ toLet l3 (err l1) binds rhs) Nothing ]
      where toIfs :: GuardedRhs Annote -> Exp Annote -> Exp Annote
            toIfs (GuardedRhs l [Qualifier _ g1] e1) = If l g1 e1

            toLet :: Annote -> Exp Annote -> Maybe (Binds Annote) -> [GuardedRhs Annote] -> Exp Annote
            toLet l y (Just binds) rhs =  Let l binds $ foldr toIfs y rhs
            toLet _ y Nothing rhs      =  foldr toIfs y rhs

            err :: Annote -> Exp Annote
            err l = App l (Var l $ UnQual l $ Ident l "primError") $ Lit l $ String l "pattern match failure" ""


-- | Turns where clauses into lets. Only valid after guard desugarage. E.g.:
-- > f x = a where a = b
-- becomes
-- > f x = let a = b in a
wheresToLets :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
wheresToLets = (\ case
                  Match l name ps (UnGuardedRhs l' e) (Just binds) -> return $ Match l name ps (UnGuardedRhs l' $ Let l' binds e) Nothing
                  Match l _ _ (GuardedRhss _ _) _                  -> failAt (l :: Annote) "Guards are not supported")
           ||> (\ case
                  PatBind l p (UnGuardedRhs l' e) (Just binds) -> return $ PatBind l p (UnGuardedRhs l' $ Let l' binds e) Nothing
                  PatBind l _ (GuardedRhss _ _) _              -> failAt (l :: Annote) "Guards are not supported")
           ||> (\ case
                  Alt l p (UnGuardedRhs l' e) (Just binds) -> return $ Alt l p (UnGuardedRhs l' $ Let l' binds e) Nothing
                  a@(Alt l _ (GuardedRhss _ _) _)          -> failAt (l :: Annote) $ "Guards are not supported: " ++ show (void a))
           ||> TId

-- | Adds the "module Main where" if no module given (but not the "main"
--   export).
addMainModuleHead :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
addMainModuleHead = transform $
      \ (Module (l :: Annote) Nothing ps imps ds) -> return $ Module l (Just $ ModuleHead l (ModuleName l "Main") Nothing $ Just $ ExportSpecList l []) ps imps ds

-- | Turns do-notation into a series of >>= \ x ->. Turns LetStmts into Lets.
--   E.g.:
-- > do p1 <- m
-- >    let p2 = e
-- >    return e
-- becomes
-- > m >>= (\ p1 -> (let p2 = e in return e))
desugarDos :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
desugarDos = transform $ \ (Do l stmts) -> transDo l stmts
      where transDo :: (MonadCatch m, SyntaxError m) => Annote -> [Stmt Annote] -> FreshT m (Exp Annote)
            transDo l = \ case
                  Generator l' p e : stmts -> App l' (App l' (Var l' $ UnQual l' $ Ident l' ">>=") e) . Lambda l' [p] <$> transDo l stmts
                  [Qualifier _ e]          -> return e
                  Qualifier l' e : stmts   -> App l' (App l' (Var l' $ UnQual l' $ Ident l' ">>=") e) . Lambda l' [PWildCard l'] <$> transDo l stmts
                  LetStmt l' binds : stmts -> Let l' binds <$> transDo l stmts
                  s : _                    -> failAt (ann s) "Unsupported syntax in do-block"
                  []                       -> failAt l "Ill-formed do-block"

normTyContext :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
normTyContext = transform $ \ x -> case x :: Type Annote of
      TyForall l tvs Nothing t               -> return $ TyForall l tvs (Just $ CxTuple l []) t
      TyForall l tvs (Just (CxEmpty _)) t    -> return $ TyForall l tvs (Just $ CxTuple l []) t
      TyForall l tvs (Just (CxSingle _ a)) t -> return $ TyForall l tvs (Just $ CxTuple l [a]) t

-- | Turns the type a -> b into (->) a b.
desugarTyFuns :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
desugarTyFuns = transform $
      \ (TyFun (l :: Annote) a b) -> return $ TyApp l (TyApp l (TyCon l (UnQual l (Ident l "->"))) a) b

-- | Turns Lets into Cases. Assumes functions in Lets are already desugared.
--   E.g.:
-- > let p = e1
-- >     q = e2
-- > in e3
-- becomes
-- > case e1 of { p -> (case e2 of { q -> e3 } }
desugarLets :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
desugarLets = transform $ \ case
      Let _ (BDecls _ ds) e -> foldrM transLet e $ filter isPatBind ds
      n@Let{}               -> failAt (ann n) "Unsupported let syntax"

      where transLet :: SyntaxError m => Decl Annote -> Exp Annote -> FreshT m (Exp Annote)
            transLet (PatBind l p (UnGuardedRhs l' e1) Nothing) inner = return $ Case l e1 [Alt l p (UnGuardedRhs l' inner) Nothing]
            transLet n _                                              = failAt (ann n) "Unsupported syntax in a let binding"

            isPatBind :: Decl Annote -> Bool
            isPatBind PatBind {} = True
            isPatBind _          = False

-- | Turns ifs into cases and unary minus.
-- > if e1 then e2 else e3
-- becomes
-- > case e1 of { True -> e2; False -> e3 }
desugarIfs :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
desugarIfs = transform $
      \ (If (l :: Annote) e1 e2 e3) -> return $ Case l e1
            [ Alt l (PApp l (UnQual l $ Ident l "True")  []) (UnGuardedRhs l e2) Nothing
            , Alt l (PApp l (UnQual l $ Ident l "False") []) (UnGuardedRhs l e3) Nothing
            ]

desugarNegs :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
desugarNegs = transform $
      \ (NegApp (l :: Annote) e) -> return $ App l (App l (Var l $ UnQual l $ Ident l "-") $ Lit l $ Int l 0 "0") e

-- | Turns wildcard patterns into variable patterns.
desugarWildCards :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
desugarWildCards = transform $
      \ (PWildCard (l :: Annote)) -> PVar l <$> fresh l

-- | Turns Lambdas with several bindings into several lambdas with single
--   bindings. E.g.:
-- > \ p1 p2 -> e
-- becomes
-- > \ p1 -> \ p2 -> e
flattenLambdas :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
flattenLambdas = transform $
      \ (Lambda (l :: Annote) ps e) -> return $ foldr (Lambda l . return) e ps

-- | Replaces non-var patterns in lambdas with a fresh var and a case. E.g.:
-- > \ (a, b) -> e
-- becomes
-- > \ $x -> case $x of { (a, b) -> e }
depatLambdas :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
depatLambdas = transform $ \ case
      n@(Lambda _ [PVar _ _] _) -> return n
      Lambda l [p] e          -> do
            x <- fresh l
            return $ Lambda l [PVar l x] (Case l (Var l $ UnQual l x) [Alt l p (UnGuardedRhs l e) Nothing])

-- | Desugars as-patterns in (and only in) cases into more cases. Should run
--   after depatLambdas. E.g.:
-- > case e1 of
-- >   x@(C y@p) -> e2
-- becomes
-- > case e1 of { C p -> (\ x -> ((\ y -> e2) p)) (C p) }
desugarAsPats :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
desugarAsPats = transform $ \ (Alt l p (UnGuardedRhs l' e) Nothing) -> do
            app <- foldrM (mkApp l) e $ getAses p
            return $ Alt l (deAs p) (UnGuardedRhs l' app) Nothing

      where mkApp :: (Functor m, SyntaxError m) => Annote -> (Pat Annote, Pat Annote) -> Exp Annote -> FreshT m (Exp Annote)
            mkApp l (p, p') e = App l (Lambda l [p] e) <$> patToExp p'

            getAses :: Pat Annote -> [(Pat Annote, Pat Annote)]
            getAses = runQ $ query $ \ case
                  PAsPat (l :: Annote) n p -> [(PVar l n, p)]
                  _                        -> []

            deAs :: Pat Annote -> Pat Annote
            deAs = runIdentity . runPureT (transform $ \ case
                  PAsPat (_ :: Annote) _ p -> return p
                  n                        -> return n)

            patToExp :: (Functor m, SyntaxError m) => Pat Annote -> FreshT m (Exp Annote)
            patToExp = \ case
                  PVar l n                -> return $ Var l $ UnQual l n
                  PWildCard l             -> Var l <$> (UnQual l <$> fresh l)
                  PLit l (Signless _) n   -> return $ Lit l n
                  -- PNPlusK _name _int ->
                  PApp l n ps             -> foldl' (App l) (Con l n) <$> mapM patToExp ps
                  PList l ps              -> List l <$> mapM patToExp ps
                  -- PRec _qname _patfields ->
                  PAsPat _ _ p            -> patToExp p
                  PIrrPat _ p             -> patToExp p
                  PatTypeSig _ p _        -> patToExp p
                  -- PViewPat _exp _pat ->
                  PBangPat _ p            -> patToExp p
                  p                       -> failAt (ann p) "Unsupported pattern"

-- | Turns beta-redexes into cases. E.g.:
-- > (\ x -> e2) e1
-- becomes
-- > case e1 of { x -> e2 }
lambdasToCases :: (MonadCatch m, SyntaxError m) => Transform (FreshT m)
lambdasToCases = transform $
      \ (App (l :: Annote) (Lambda _ [p] e2) e1) -> return $ Case l e1 [Alt l p (UnGuardedRhs l e2) Nothing]
