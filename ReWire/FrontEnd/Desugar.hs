{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module ReWire.FrontEnd.Desugar (desugar) where

import ReWire.FrontEnd.Error (ParseError, pFailAt)
import ReWire.SYB (Transform, runT, runQ, match, match', query')

import Control.Monad (liftM, replicateM, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (runStateT, StateT, get, modify)
import Data.Foldable (foldl', foldrM)
import Data.Functor ((<$>))
import Data.Functor.Identity
import Data.Monoid ((<>))

import Language.Haskell.Exts (prettyPrint)
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts.Syntax

-- TODO(chathhorn): record syntax should be fairly easy to desugar.

-- | Desugar into lambdas then normalize the lambdas.
desugar :: Module -> ParseError IO Module
desugar = liftM fst . flip runStateT 0 .
      ( runT
            ( normIds
           <> deparenify
           <> desugarInfix
           <> wheresToLets
            )
      >=> runT desugarFuns
      >=> runT
            ( desugarTuples
           <> desugarDos
            )
      >=> runT desugarLets
      >=> runT
            ( desugarIfs
           <> desugarNegs
           <> desugarWildCards
           <> desugarNegLitPats
            )
      -- *** Normalize lambdas:
      >=> runT flattenLambdas
      >=> runT depatLambdas
      >=> runT desugarAsPats
      >=> runT lambdasToCases
      )

type Fresh = StateT Int (ParseError IO)

fresh :: Fresh Name
fresh = do
      x <- get
      modify $ const $ x + 1
      return $ Ident $ "$" ++ show x

-- | Turns Symbols and Specials into normal identifiers and adds a prefix to
--   all names in the type namespace.
normIds :: Transform Fresh
normIds = match (\case
            Symbol n               -> return $ Ident n)
       <> match (\case
            Special UnitCon        -> return $ UnQual $ Ident "()"
            Special ListCon        -> return $ UnQual $ Ident "List"
            Special FunCon         -> return $ UnQual $ Ident "->"
            -- I think this is only for the prefix constructor.
            Special (TupleCon _ i) -> return $ UnQual $ mkTuple i
            Special Cons           -> return $ UnQual $ Ident "Cons")

mkTuple :: Int -> Name
mkTuple n = Ident $ "(" ++ replicate (n-1) ',' ++ ")"

-- | Removes parens in types, expressions, and patterns so they don't confuddle
--   everything.
deparenify :: Transform Fresh
deparenify = match (\(Paren n)   -> return n)
          <> match (\(PParen n)  -> return n)
          <> match (\(TyParen n) -> return n)

-- | Turns sections and infix ops into regular applications and lambdas.
desugarInfix :: Transform Fresh
desugarInfix = match (\case
                  LeftSection e (QVarOp op)  -> return $ App (Var op) e
                  LeftSection e (QConOp op)  -> return $ App (Con op) e
                  RightSection (QVarOp op) e -> do
                        x <- fresh
                        return $ Lambda noLoc [PVar x] $ App (App (Var op) $ Var $ UnQual x) e
                  RightSection (QConOp op) e -> do
                        x <- fresh
                        return $ Lambda noLoc [PVar x] $ App (App (Con op) $ Var $ UnQual x) e
                  InfixApp e1 (QVarOp op) e2 -> return $ App (App (Var op) e1) e2
                  InfixApp e1 (QConOp op) e2 -> return $ App (App (Con op) e1) e2)
            <> match (\case
                  InfixConDecl a n b -> return $ ConDecl n [a, b])

-- | Turns wildcard patterns into variable patterns.
desugarWildCards :: Transform Fresh
desugarWildCards = match $ \PWildCard -> return $ PVar $ Ident "$_"

-- | TODO Apparently we'd actually need guards to properly support negated
-- literal patterns:
-- > f (-k) = v
-- is actually sugar for
-- > f z | z == negate (fromInteger k) = v
desugarNegLitPats :: Transform Fresh
desugarNegLitPats = match $ \(PLit Negative lit) -> return $ PLit Signless $ neg lit

neg :: Literal -> Literal
neg = \case
      Int        l -> Int (-l)
      Frac       l -> Frac (-l)
      PrimInt    l -> PrimInt (-l)
      PrimWord   l -> PrimWord (-l)
      PrimFloat  l -> PrimFloat (-l)
      PrimDouble l -> PrimDouble (-l)
      l            -> l

-- | Turns piece-wise function definitions into a single PatBind with a lambda
--   and case expression on the RHS. E.g.:
-- > f p1 p2 = rhs1
-- > f q1 q2 = rhs2
-- becomes
-- > f = \$1 $2 -> case ($1, $2) of { (p1, p2) -> rhs1; (q1, q2) -> rhs2 }
desugarFuns :: Transform Fresh
desugarFuns = match $ \case
      FunBind ms@(Match loc name pats _ _ Nothing:_) -> do
            e <- buildLambda loc ms $ length pats
            return $ PatBind loc (PVar name) (UnGuardedRhs e) Nothing
      n@(FunBind (Match loc _ _ _ _ _:_))            -> lift $ pFailAt loc $ "unsupported decl syntax: " ++ prettyPrint n
      where buildLambda :: SrcLoc -> [Match] -> Int -> Fresh Exp
            buildLambda loc ms 1 = do
                  alts <- mapM toAlt ms
                  x <- fresh
                  return $ Lambda loc [PVar x] $ Case (Var $ UnQual x) alts
            buildLambda loc ms arrity = do
                  alts <- mapM toAlt ms
                  xs <- replicateM arrity fresh
                  return $ Lambda loc (map PVar xs) $ Case (Tuple Boxed (map (Var . UnQual) xs)) alts
            toAlt :: Match -> Fresh Alt
            toAlt (Match loc' _ [p] Nothing rhs binds) = return $ Alt loc' p rhs binds
            toAlt (Match loc' _ ps Nothing rhs binds)  = return $ Alt loc' (PTuple Boxed ps) rhs binds
            toAlt m@(Match loc' _ _ _ _ _)             = lift $ pFailAt loc' $ "unsupported decl syntax: " ++ prettyPrint m

-- | Turns tuples into applications of a TupleN constructor (also in types and pats):
-- > (x, y, z)
-- becomes
-- > (Tuple3 x y z)
desugarTuples :: Transform Fresh
desugarTuples = match (\(Tuple _ es)   -> return $ foldl' App (Con $ UnQual $ mkTuple $ length es) es)
             <> match (\(TyTuple _ ts) -> return $ foldl' TyApp (TyCon $ UnQual $ mkTuple $ length ts) ts)
             <> match (\(PTuple _ ps)  -> return $ PApp (UnQual $ mkTuple $ length ps) ps)

-- | Turns do-notation into a series of >>= \x ->. Turns LetStmts into Lets.
--   Should run before Let and Lambda desugarage. E.g.:
-- > do p1 <- m
-- >    let p2 = e
-- >    return e
-- becomes
-- > m >>= (\p1 -> (let p2 = e in return e))
desugarDos :: Transform Fresh
desugarDos = match $ \(Do stmts) -> transDo stmts
      where transDo :: [Stmt] -> Fresh Exp
            transDo = \case
                  Generator loc p e : stmts -> App (App (Var $ UnQual $ Ident ">>=") e) . Lambda loc [p] <$> transDo stmts
                  [Qualifier e]             -> return e
                  Qualifier e : stmts       -> App (App (Var $ UnQual $ Ident ">>=") e) . Lambda noLoc [PWildCard] <$> transDo stmts
                  LetStmt binds : stmts     -> Let binds <$> transDo stmts
                  s : _                     -> lift $ pFailAt noLoc $ "unsupported syntax in do-block: " ++ prettyPrint s
                  _                         -> lift $ pFailAt noLoc  "something went wrong while translating a do-block."

-- | Turns where clauses into lets. Only valid because we're disallowing
--   guards, so this pass also raises an error if it encounters a guard.
wheresToLets :: Transform Fresh
wheresToLets = match (\case
                  Match loc name ps t (UnGuardedRhs e) (Just binds) -> return $ Match loc name ps t (UnGuardedRhs $ Let binds e) Nothing
                  n@(Match loc _ _ _ (GuardedRhss _) _)             -> lift $ pFailAt loc $ "guards are not supported: " ++ prettyPrint n)
            <> match (\case
                  PatBind loc p (UnGuardedRhs e) (Just binds) -> return $ PatBind loc p (UnGuardedRhs $ Let binds e) Nothing
                  n@(PatBind loc _ (GuardedRhss _) _)         -> lift $ pFailAt loc $ "guards are not supported: " ++ prettyPrint n)
            <> match (\case
                  Alt loc p (UnGuardedRhs e) (Just binds) -> return $ Alt loc p (UnGuardedRhs $ Let binds e) Nothing
                  n@(Alt loc _ (GuardedRhss _) _)         -> lift $ pFailAt loc $ "guards are not supported: " ++ prettyPrint n)

-- | Turns Lets into Cases. Assumes functions in Lets are already desugared.
--   E.g.:
-- > let p = e1
-- >     q = e2
-- > in e3
-- becomes
-- > case e1 of { p -> (case e2 of { q -> e3 } }
desugarLets :: Transform Fresh
desugarLets = match $ \case
      Let (BDecls ds) e -> foldrM transLet e ds
      n@(Let _ _)       -> lift $ pFailAt noLoc $ "unsupported let syntax: " ++ prettyPrint n
      where transLet :: Decl -> Exp -> Fresh Exp
            transLet (PatBind loc p (UnGuardedRhs e1) Nothing) inner = return $ Case e1 [Alt loc p (UnGuardedRhs inner) Nothing]
            transLet n@(PatBind loc _ _ _)                     _     = lift $ pFailAt loc $ "unsupported let syntax: " ++ prettyPrint n
            transLet n                                         _     = lift $ pFailAt noLoc $ "unsupported syntax: " ++ prettyPrint n

-- | Turns ifs into cases and unary minus.
-- > if e1 then e2 else e3
-- becomes
-- > case e1 of { True -> e2; False -> e3 }
desugarIfs :: Transform Fresh
desugarIfs = match $
      \(If e1 e2 e3) -> return $ Case e1
            [ Alt noLoc (PApp (UnQual $ Ident "True")  []) (UnGuardedRhs e2) Nothing
            , Alt noLoc (PApp (UnQual $ Ident "False") []) (UnGuardedRhs e3) Nothing
            ]

desugarNegs :: Transform Fresh
desugarNegs = match $
      \(NegApp e) -> return $ App (App (Var $ UnQual $ Ident "-") $ Lit $ Int 0) e

-- | Turns Lambdas with several bindings into several lambdas with single
--   bindings. E.g.:
-- > \p1 p2 -> e
-- becomes
-- > \p1 -> \p2 -> e
flattenLambdas :: Transform Fresh
flattenLambdas = match $
      \(Lambda loc ps e) -> return $ foldr (Lambda loc . return) e ps

-- | Replaces non-var patterns in lambdas with a fresh var and a case. E.g.:
-- > \(a,b) -> e
-- becomes
-- > \$x -> case $x of { (a,b) -> e }
depatLambdas :: Transform Fresh
depatLambdas = match $ \case
      n@(Lambda _ [PVar _] _) -> return n
      Lambda loc [p] e        -> do
            x <- fresh
            return $ Lambda loc [PVar x] (Case (Var $ UnQual x) [Alt loc p (UnGuardedRhs e) Nothing])

-- | Desugars as-patterns in (and only in) cases into more cases. Should run
--   after depatLambdas. E.g.:
-- > case e1 of
-- >   x@(C y@p) -> e2
-- becomes
-- > case e1 of { C p -> (\x -> ((\y -> e2) p)) (C p) }
desugarAsPats :: Transform Fresh
desugarAsPats = match $
      \(Alt loc p (UnGuardedRhs e) Nothing) -> do
            app <- foldrM (mkApp loc) e $ getAses p
            return $ Alt loc (deAs p) (UnGuardedRhs app) Nothing
      where mkApp :: SrcLoc -> (Pat, Pat) -> Exp -> Fresh Exp
            mkApp loc (p, p') e = App (Lambda loc [p] e) <$> patToExp p'
            getAses :: Pat -> [(Pat, Pat)]
            getAses = runQ $ query' $ \case
                  PAsPat n p -> [(PVar n, p)]
                  _          -> []
            deAs :: Pat -> Pat
            deAs = runIdentity . runT (match' $
                  \case PAsPat _ p -> return p
                        n          -> return n)
            patToExp :: Pat -> Fresh Exp
            patToExp = \case
                  PVar n            -> return $ Var $ UnQual n
                  PLit Signless n   -> return $ Lit n
                  -- PNPlusK _name _int ->
                  PApp n ps         -> foldl' App (Con n) <$> mapM patToExp ps
                  PList ps          -> List <$> mapM patToExp ps
                  -- PRec _qname _patfields ->
                  PAsPat _ p        -> patToExp p
                  PIrrPat p         -> patToExp p
                  PatTypeSig _ p _  -> patToExp p
                  -- PViewPat _exp _pat ->
                  PBangPat p        -> patToExp p
                  p                 -> lift $ pFailAt noLoc $ "unsupported pattern: " ++ prettyPrint p

-- | Turns beta-redexes into cases. E.g.:
-- > (\x -> e2) e1
-- becomes
-- > case e1 of { x -> e2 }
lambdasToCases :: Transform Fresh
lambdasToCases = match $
      \(App (Lambda loc [p] e2) e1) -> return $ Case e1 [Alt loc p (UnGuardedRhs e2) Nothing]

