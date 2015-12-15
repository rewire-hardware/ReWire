{-# LANGUAGE ViewPatterns, RankNTypes, LambdaCase #-}
module ReWire.Core.FrontEnd (parseFile, ParseResult(..), SrcLoc(..), prettyPrint) where

import ReWire.Scoping (mkId, Id, fv)
import ReWire.Core.Syntax
import Data.List (nub)
import Data.Functor ((<$>))
import Data.Functor.Identity
import Control.Applicative ((<*>))
import Control.Monad (foldM, replicateM, (>=>), msum, mplus)
import Control.Monad.Trans.State (runStateT, StateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (foldl', foldrM)
import Data.Data (Data, Typeable, gmapM, gmapQr, cast)
import Data.Maybe (fromJust)

import qualified Language.Haskell.Exts as Haskell (parseFile)
import           Language.Haskell.Exts hiding (parseFile, loc, name, binds, op)

-- | Parse a ReWire source file.
parseFile :: FilePath -> IO (ParseResult RWCProg)
parseFile = Haskell.parseFile
      -- *** Desugar into lambdas:
      !=> deparenify
      >=> normIds
      >=> desugarInfix
      >=> desugarFuns
      >=> desugarTuples
      >=> desugarDos
      >=> wheresToLets
      >=> desugarLets
      >=> desugarIfsNegs
      >=> desugarWildCards
      -- *** Normalize lambdas:
      >=> flattenLambdas
      >=> depatLambdas
      >=> desugarAsPats
      >=> lambdasToCases
      -- *** Translate into the core abstract syntax:
      >=> trans
      where (!=>) :: (FilePath -> IO (ParseResult Module)) -> (Module -> Trans a) -> FilePath -> IO (ParseResult a)
            f !=> g = f ^=> (>>= runTrans . g)
            infix 0 !=>
            runTrans :: Trans a -> ParseResult a
            runTrans = flip runStateT 0 >=> return . fst

(^=>) :: (Monad m1, Monad m2) => (a -> m1 b) -> (b -> m2 c) -> a -> m1 (m2 c)
f ^=> g = f >=> return . g
infix 0 ^=>

everywhere :: (Data a, Monad m) => (forall d. Data d => (d -> m d)) -> a -> m a
everywhere f = gmapM (everywhere f) >=> f

cases :: (Monad m, Typeable a, Typeable b) => [a -> Maybe (m b)] -> a -> m b
cases cs n = fromJust $ msum (cs <*> [n]) `mplus` return (tr n)

everywhereQ :: Data a => (forall d. Data d => d -> [b]) -> a -> [b]
everywhereQ f n = f n ++ gmapQr (++) [] (everywhereQ f) n

unknownLoc :: SrcLoc
unknownLoc = SrcLoc "" 0 0

tr :: (Monad m, Typeable a, Typeable b) => a -> m b
tr = return . fromJust . cast

type Trans = StateT Int ParseResult

pFail :: SrcLoc -> String -> Trans a
pFail loc msg = lift $ ParseFailed loc msg

fresh :: StateT Int ParseResult String
fresh = do
      x <- get
      put $ x + 1
      return $ "$" ++ show x

-- | Removes parens in types, expressions, and patterns so they don't confuddle
--   everything.
deparenify :: Module -> Trans Module
deparenify = everywhere $ cases
      [ cast ^=> \case
            Paren n -> tr n
            n       -> tr n
      , cast ^=> \case
            PParen n -> tr n
            n        -> tr n
      , cast ^=> \case
            TyParen n -> tr n
            n         -> tr n
      ]

mkTuple :: Int -> String
mkTuple n = "Tuple" ++ show n

-- | Turns Symbols and Specials into normal identifiers.
normIds :: Module -> Trans Module
normIds = everywhere $ cases
      [ cast ^=> \case
            Symbol n -> tr $ Ident n
            n        -> tr n
      , cast ^=> \case
            Qual _ n               -> tr $ UnQual n
            Special UnitCon        -> tr $ UnQual $ Ident "Unit"
            Special ListCon        -> tr $ UnQual $ Ident "List"
            Special FunCon         -> tr $ UnQual $ Ident "->"
            -- I think this is only for the prefix constructor.
            Special (TupleCon _ i) -> tr $ UnQual $ Ident $ mkTuple i
            Special Cons           -> tr $ UnQual $ Ident "Cons"
            n                      -> tr n
      ]

-- | Turns sections and infix ops into regular applications and lambdas.
desugarInfix :: Module -> Trans Module
desugarInfix = everywhere $ cases
      [ cast ^=> \case
            LeftSection e (QVarOp op)  -> tr $ App (Var op) e
            LeftSection e (QConOp op)  -> tr $ App (Con op) e
            RightSection (QVarOp op) e -> do
                  x <- fresh
                  tr $ Lambda unknownLoc [PVar $ Ident x] $ App (App (Var op) (Var $ UnQual $ Ident x)) e
            RightSection (QConOp op) e -> do
                  x <- fresh
                  tr $ Lambda unknownLoc [PVar $ Ident x] $ App (App (Con op) (Var $ UnQual $ Ident x)) e
            InfixApp e1 (QVarOp op) e2 -> tr $ App (App (Var op) e1) e2
            InfixApp e1 (QConOp op) e2 -> tr $ App (App (Con op) e1) e2
            n                          -> tr n
      ]

-- | Turns wildcard patterns into variable patterns.
desugarWildCards :: Module -> Trans Module
desugarWildCards = everywhere $ cases
      [ cast ^=> \case
            PWildCard -> tr $ PVar $ Ident "$_"
            n         -> tr n
      ]

-- | Turns piece-wise function definitions into a single PatBind with a lambda
--   and case expression on the RHS. E.g.:
-- > f p1 p2 = rhs1
-- > f q1 q2 = rhs2
-- becomes
-- > f = \$1 $2 -> case ($1, $2) of { (p1, p2) -> rhs1; (q1, q2) -> rhs2 }
desugarFuns :: Module -> Trans Module
desugarFuns = everywhere $ cases
      [ cast ^=> \case
            FunBind ms@(Match loc name pats _ _ Nothing:_) -> do
                  e <- buildLambda loc ms $ length pats
                  tr $ PatBind loc (PVar name) (UnGuardedRhs e) Nothing
            n@(FunBind _)                                    -> pFail unknownLoc $ "unsupported syntax: " ++ prettyPrint n
            n                                                -> tr n
      ]
      where buildLambda :: SrcLoc -> [Match] -> Int -> Trans Exp
            buildLambda loc ms 1 = do
                  alts <- mapM toAlt ms
                  x <- fresh
                  return $ Lambda loc [PVar $ Ident x] $ Case (Var $ UnQual $ Ident x) alts
            buildLambda loc ms arrity = do
                  alts <- mapM toAlt ms
                  xs <- replicateM arrity fresh
                  return $ Lambda loc (map (PVar . Ident) xs) $ Case (Tuple Boxed (map (Var . UnQual . Ident) xs)) alts
            toAlt :: Match -> Trans Alt
            toAlt (Match loc' _ [p] Nothing rhs binds) = return $ Alt loc' p rhs binds
            toAlt (Match loc' _ ps Nothing rhs binds)  = return $ Alt loc' (PTuple Boxed ps) rhs binds
            toAlt m@(Match loc' _ _ _ _ _)             = pFail loc' $ "unsupported syntax: " ++ prettyPrint m

-- | Turns tuples into applications of a TupleN constructor (also in types and pats):
-- > (x, y, z)
-- becomes
-- > (Tuple3 x y z)
desugarTuples :: Module -> Trans Module
desugarTuples = everywhere $ cases
      [ cast ^=> \case
            Tuple _ es -> tr $ foldl' App (Con $ UnQual $ Ident $ mkTuple $ length es) es
            n          -> tr n
      , cast ^=> \case
            TyTuple _ ts -> tr $ foldl' TyApp (TyCon $ UnQual $ Ident $ mkTuple $ length ts) ts
            n            -> tr n
      , cast ^=> \case
            PTuple _ ps -> tr $ PApp (UnQual $ Ident $ mkTuple $ length ps) ps
            n           -> tr n
      ]

-- | Turns do-notation into a series of >>= \x ->. Turns LetStmts into Lets.
--   Should run before Let and Lambda desugarage. E.g.:
-- > do p1 <- m
-- >    let p2 = e
-- >    return e
-- becomes
-- > m >>= (\p1 -> (let p2 = e in return e))
desugarDos :: Module -> Trans Module
desugarDos =  everywhere $ cases
      [ cast ^=> \case
            Do stmts -> transDo stmts >>= tr
            n        -> tr n
      ]
      where transDo :: [Stmt] -> Trans Exp
            transDo = \case 
                  Generator loc p e : stmts -> App (App (Var $ UnQual $ Ident ">>=") e) . Lambda loc [p] <$> transDo stmts
                  [Qualifier e]             -> return e
                  Qualifier e : stmts       -> App (App (Var $ UnQual $ Ident ">>=") e) . Lambda unknownLoc [PWildCard] <$> transDo stmts
                  LetStmt binds : stmts     -> Let binds <$> transDo stmts
                  s : _                     -> pFail unknownLoc $ "unsupported syntax: " ++ prettyPrint s
                  _                         -> pFail unknownLoc "something went wrong while translating a do-block."

-- | Turns where clauses into lets. Only valid because we're disallowing
--   guards, so this pass also raises an error if it encounters a guard.
wheresToLets :: Module -> Trans Module
wheresToLets = everywhere $ cases
      [ cast ^=> \case
            Match loc name ps t (UnGuardedRhs e) (Just binds) -> tr $ Match loc name ps t (UnGuardedRhs $ Let binds e) Nothing
            n@(Match loc _ _ _ (GuardedRhss _) _)             -> pFail loc $ "guards are not supported: " ++ prettyPrint n
            n                                                 -> tr n
      , cast ^=> \case
            PatBind loc p (UnGuardedRhs e) (Just binds) -> tr $ PatBind loc p (UnGuardedRhs $ Let binds e) Nothing
            n@(PatBind loc _ (GuardedRhss _) _)         -> pFail loc $ "guards are not supported: " ++ prettyPrint n
            n                                           -> tr n
      , cast ^=> \case
            Alt loc p (UnGuardedRhs e) (Just binds) -> tr $ Alt loc p (UnGuardedRhs $ Let binds e) Nothing
            n@(Alt loc _ (GuardedRhss _) _)         -> pFail loc $ "guards are not supported: " ++ prettyPrint n
            n                                       -> tr n
      ]

-- | Turns Lets into Cases. Assumes functions in Lets are already desugared.
--   E.g.:
-- > let p = e1
-- >     q = e2
-- > in e3
-- becomes
-- > case e1 of { p -> (case e2 of { q -> e3 } }
desugarLets :: Module -> Trans Module
desugarLets = everywhere $ cases
      [ cast ^=> \case
            Let (BDecls ds) e -> foldrM transLet e ds >>= tr
            n@(Let _ _)       -> pFail unknownLoc $ "unsupported syntax: " ++ prettyPrint n
            n                 -> tr n
      ]
      where transLet :: Decl -> Exp -> Trans Exp
            transLet (PatBind loc p (UnGuardedRhs e1) Nothing) inner = return $ Case e1 [Alt loc p (UnGuardedRhs inner) Nothing]
            transLet n@(PatBind loc _ _ _)                     _     = pFail loc $ "unsupported syntax: " ++ prettyPrint n
            transLet n                                         _     = pFail unknownLoc $ "unsupported syntax: " ++ prettyPrint n

-- | Turns ifs into cases and unary minus.
-- > if e1 then e2 else e3
-- becomes
-- > case e1 of { True -> e2; False -> e3 }
desugarIfsNegs :: Module -> Trans Module
desugarIfsNegs = everywhere $ cases
      [ cast ^=> \case
            If e1 e2 e3 -> tr $ Case e1
                  [ Alt unknownLoc (PApp (UnQual $ Ident "True")  []) (UnGuardedRhs e2) Nothing
                  , Alt unknownLoc (PApp (UnQual $ Ident "False") []) (UnGuardedRhs e3) Nothing
                  ]
            NegApp e    -> tr $ App (App (Var $ UnQual $ Ident "-") $ Lit $ Int 0) e
            n             -> tr n
      ]

-- | Turns Lambdas with several bindings into several lambdas with single
--   bindings. E.g.:
-- > \p1 p2 -> e
-- becomes
-- > \p1 -> \p2 -> e
flattenLambdas :: Module -> Trans Module
flattenLambdas = everywhere $ cases
      [ cast ^=> \case
            Lambda loc ps e -> tr $ foldr (Lambda loc . return) e ps
            n               -> tr n
      ]

-- | Replaces non-var patterns in lambdas with a fresh var and a case. E.g.:
-- > \(a,b) -> e
-- becomes
-- > \$x -> case $x of { (a,b) -> e }
depatLambdas :: Module -> Trans Module
depatLambdas = everywhere $ cases
      [ cast ^=> \case
            n@(Lambda _ [PVar _] _) -> tr n
            Lambda loc [p] e        -> do
                  x <- fresh
                  tr $ Lambda loc [PVar $ Ident x] (Case (Var $ UnQual $ Ident x) [Alt loc p (UnGuardedRhs e) Nothing])
            n                       -> tr n
      ]

-- | Desugars as-patterns in (and only in) cases into more cases. Should run
--   after depatLambdas. E.g.:
-- > case e1 of
-- >   x@(C y@p) -> e2
-- becomes
-- > case e1 of { C p -> (\x -> ((\y -> e2) p)) (C p) }
desugarAsPats :: Module -> Trans Module
desugarAsPats = everywhere $ cases
      [ cast ^=> \case
            Alt loc p (UnGuardedRhs e) Nothing -> do
                  app <- foldrM (mkApp loc) e $ getAses p
                  tr $ Alt loc (deAs p) (UnGuardedRhs app) Nothing
            n                                  -> tr n
      ]
      where mkApp :: SrcLoc -> (Pat, Pat) -> Exp -> Trans Exp
            mkApp loc (p, p') e = App (Lambda loc [p] e) <$> patToExp p'
            getAses :: Pat -> [(Pat, Pat)]
            getAses = everywhereQ $ \n -> case cast n of
                  Just (PAsPat n p) -> [(PVar n, p)]
                  _                 -> []
            deAs :: Pat -> Pat
            deAs = runIdentity . everywhere (cases
                  [ cast ^=> \case
                        PAsPat _ p -> tr p
                        n          -> tr n
                  ])
            patToExp :: Pat -> Trans Exp
            patToExp = \case
                  PVar n            -> return $ Var $ UnQual n
                  PLit s n          -> return $ Lit $ deSign s n
                  -- PNPlusK _name _int ->
                  PApp n ps         -> foldl' App (Con n) <$> mapM patToExp ps
                  PList ps          -> List <$> mapM patToExp ps
                  -- PRec _qname _patfields ->
                  PAsPat _ p        -> patToExp p
                  PIrrPat p         -> patToExp p
                  PatTypeSig _ p _  -> patToExp p
                  -- PViewPat _exp _pat ->
                  PBangPat p        -> patToExp p
                  p                 -> pFail unknownLoc $ "unsupported pattern: " ++ prettyPrint p

-- | Turns beta-redexes into cases. E.g.:
-- > (\x -> e2) e1
-- becomes
-- > case e1 of { x -> e2 }
lambdasToCases :: Module -> Trans Module
lambdasToCases = everywhere $ cases
      [ cast ^=> \case
            App (Lambda loc [p] e2) e1 -> tr $ Case e1 [Alt loc p (UnGuardedRhs e2) Nothing]
            n                          -> tr n
      ]

-- | Lifts lambdas. Not used.
liftLambdas :: Module -> Trans Module
liftLambdas m = runStateT (everywhere (liftLambdas' $ concatMap getGVar (getDecls m) ++ builtins) m) [] >>= addDecls
      where liftLambdas' :: Data d => [String] -> d -> StateT [Decl] Trans d
            liftLambdas' bvs = cases
                  [ cast ^=> \case
                        Lambda loc ps e -> do
                              x <- lift fresh
                              ds <- get
                              let fvs = fv (bvs ++ concatMap getVars ps) e
                              put $ PatBind loc (PVar $ Ident x) (UnGuardedRhs $ foldr (\x -> Lambda loc [PVar $ Ident x]) e $ fvs ++ concatMap getVars ps) Nothing : ds
                              tr $ foldl' (\e -> App e . Var . UnQual . Ident) (Var $ UnQual $ Ident x) fvs
                        n               -> tr n
                  ]
            getDecls :: Module -> [Decl]
            getDecls (Module _ _ _ _ _ _ ds) = ds
            getGVar :: Decl -> [String]
            getGVar (PatBind _ (PVar (Ident x)) _ _)  = [x]
            getGVar _                                 = []
            addDecls :: (Module, [Decl]) -> Trans Module
            addDecls (Module loc n ps w es is ds, ds') = return $ Module loc n ps w es is $ ds ++ ds'
            fvAlt :: [String] -> Alt -> [String]
            fvAlt bvs (Alt _ p (UnGuardedRhs e) _) = fv (bvs ++ getVars p) e
            fv :: [String] -> Exp -> [String]
            fv bvs = \case
                  App e1 e2              -> fv bvs e1 ++ fv bvs e2
                  Lambda _ ps e          -> fv (bvs ++ concatMap getVars ps) e
                  Var (UnQual (Ident x)) -> [x | x `notElem` bvs]
                  Case e alts            -> fv bvs e ++ concatMap (fvAlt bvs) alts
                  _                      -> []
            getVars :: Pat -> [String]
            getVars = everywhereQ $ \n -> case cast n of
                  Just (PVar (Ident x)) -> [x]
                  _                     -> []
            builtins :: [String]
            builtins = [ "nativeVhdl"
                       , ">>="
                       , "return"
                       , "get"
                       , "put"
                       , "signal"
                       , "lift"
                       , "extrude"
                       ]

-- | Translate a Haskell module into the ReWire abstract syntax.
trans :: Module -> Trans RWCProg
trans (Module _loc _name _pragmas _ _exports _imports (reverse -> ds)) = do
      datas <- foldM transData [] ds
      sigs  <- foldM transTySig [] ds
      inls  <- foldM transInlineSig [] ds
      defs  <- foldM (transDef sigs inls) [] ds
      return $ RWCProg datas defs

transData :: [RWCData] -> Decl -> Trans [RWCData]
transData datas (DataDecl loc _ _ (Ident x) tyVars cons _deriving) = do
      tyVars' <- mapM (transTyVar loc) tyVars
      cons' <- mapM transCon cons
      return $ RWCData (TyConId x) tyVars' cons' : datas
transData datas _                                                  = return datas

transTySig :: [(String, RWCTy)] -> Decl -> Trans [(String, RWCTy)]
transTySig sigs (TypeSig loc names t) = do
      t' <- transTy loc [] t
      return $ zip (map (\(Ident x) -> x) names) (repeat t') ++ sigs
transTySig sigs _                     = return sigs

-- I guess this doesn't need to be in the monad, really, but whatever...  --adam
-- Not sure what the boolean field means here, so we ignore it!  --adam
transInlineSig :: [String] -> Decl -> Trans [String]
transInlineSig inls (InlineSig _ _ AlwaysActive (UnQual (Ident x))) = return (x:inls)
transInlineSig inls _                                               = return inls

transDef :: [(String, RWCTy)] -> [String] -> [RWCDefn] -> Decl -> Trans [RWCDefn]
transDef tys inls defs (PatBind loc (PVar (Ident x)) (UnGuardedRhs e) Nothing) = case lookup x tys of
      Just t -> (:defs) . RWCDefn (mkId x) (nub (fv t) :-> t) (x `elem` inls) <$> transExp loc e
      _      -> pFail loc $ "no type signature for " ++ x
transDef _   _    defs _                                                       = return defs

transTyVar :: SrcLoc -> TyVarBind -> Trans (Id RWCTy)
transTyVar loc = \case
      UnkindedVar (Ident x) -> return $ mkId x
      tv                    -> pFail loc $ "unsupported syntax: " ++ prettyPrint tv

transCon :: QualConDecl -> Trans RWCDataCon
transCon = \case
      QualConDecl loc [] _ (ConDecl (Ident x) tys) -> (RWCDataCon $ DataConId x) <$> mapM (transTy loc []) tys
      d@(QualConDecl loc _ _ _)                    -> pFail loc $ "unsupported syntax: " ++ prettyPrint d

transTy :: SrcLoc -> [String] -> Type -> Trans RWCTy
transTy loc ms = \case
      TyForall Nothing cs t    -> do
           ms' <- mapM (getNad loc) cs
           transTy loc (ms ++ ms') t
      TyFun a b                -> mkArrow <$> transTy loc ms a <*> transTy loc ms b
      TyApp a b | isMonad ms a -> RWCTyComp <$> transTy loc ms a <*> transTy loc ms b
                | otherwise    -> RWCTyApp <$> transTy loc ms a <*> transTy loc ms b
      TyCon (UnQual (Ident x)) -> return $ RWCTyCon (TyConId x)
      TyVar (Ident x)          -> return $ RWCTyVar (mkId x)
      t                        -> pFail loc $ "unsupported syntax: " ++ prettyPrint t

getNad :: SrcLoc -> Asst -> Trans String
getNad _   (ClassA (UnQual (Ident "Monad")) [TyVar (Ident x)]) = return x
getNad loc a                                                   = pFail loc $ "unsupported typeclass constraint: " ++ prettyPrint a

isMonad :: [String] -> Type -> Bool
isMonad ms = \case
      TyApp (TyApp (TyApp (TyCon (UnQual (Ident "ReT"))) _) _) t -> isMonad ms t
      TyApp (TyApp (TyCon (UnQual (Ident "StT"))) _) t           -> isMonad ms t
      TyCon (UnQual (Ident "I"))                                 -> True
      TyVar (Ident x)                                            -> x `elem` ms
      _                                                          -> False

tblank :: RWCTy
tblank = RWCTyCon (TyConId "_")

transExp :: SrcLoc -> Exp -> Trans RWCExp
transExp loc = \case
      App (App (Var (UnQual (Ident "nativeVhdl")))  (Lit (String f))) e
                                    -> RWCNativeVHDL f <$> transExp loc e
      App e1 e2                     -> RWCApp <$> transExp loc e1 <*> transExp loc e2
      Lambda loc [PVar (Ident x)] e -> RWCLam (mkId x) tblank <$> transExp loc e
      Var (UnQual (Ident x))        -> return $ RWCVar (mkId x) tblank
      Con (UnQual (Ident x))        -> return $ RWCCon (DataConId x) tblank
      Lit lit                       -> RWCLiteral <$> transLit loc lit
      Case e alts                   -> RWCCase <$> transExp loc e <*> mapM transAlt alts
      e                             -> pFail loc $ "unsupported syntax: " ++ prettyPrint e

-- Not entirely sure this is right...
deSign :: Sign -> Literal -> Literal
deSign Negative = \case
      Int        l -> Int (-l)
      Frac       l -> Frac (-l)
      PrimInt    l -> PrimInt (-l)
      PrimWord   l -> PrimWord (-l)
      PrimFloat  l -> PrimFloat (-l)
      PrimDouble l -> PrimDouble (-l)
deSign _ = id

transLit :: SrcLoc -> Literal -> Trans RWCLit
transLit loc = \case 
      Int i  -> return $ RWCLitInteger i
      Frac d -> return $ RWCLitFloat (fromRational d)
      Char c -> return $ RWCLitChar c
      lit    -> pFail loc $ "unsupported syntax: " ++ prettyPrint lit

transAlt :: Alt -> Trans RWCAlt
transAlt = \case
      Alt loc p (UnGuardedRhs e) Nothing -> RWCAlt <$> transPat loc p <*> transExp loc e
      a@(Alt loc _ _ _)                  -> pFail loc $ "unsupported syntax: " ++ prettyPrint a

transPat :: SrcLoc -> Pat -> Trans RWCPat
transPat loc = \case
      PApp (UnQual (Ident x)) ps -> RWCPatCon (DataConId x) <$> mapM (transPat loc) ps
      PLit s lit                 -> RWCPatLiteral <$> transLit loc (deSign s lit)
      PVar (Ident x)             -> return $ RWCPatVar (mkId x) tblank
      p                          -> pFail loc $ "unsupported syntax: " ++ prettyPrint p
