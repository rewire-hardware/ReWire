{-# LANGUAGE ViewPatterns, LambdaCase #-}
module ReWire.Core.FrontEnd
      ( parseFile
      , ParseResult(..)
      , SrcLoc(..)
      , prettyPrint
      ) where

import ReWire.Core.Kinds
import ReWire.Core.Syntax
import ReWire.Scoping (mkId, Id, fv)
import ReWire.SYB

import Control.Applicative ((<*>))
import Control.Monad (foldM, replicateM, (>=>), mzero)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (runStateT, StateT, get, put)
import Data.Data (Data, cast)
import Data.Foldable (foldl', foldrM)
import Data.Functor ((<$>))
import Data.Functor.Identity
import Data.List (nub)
import Data.Monoid ((<>))

import qualified Language.Haskell.Exts as Haskell (parseFile)
import           Language.Haskell.Exts hiding (parseFile, loc, name, binds, op, Kind)

-- | Parse a ReWire source file.
parseFile :: FilePath -> IO (ParseResult RWCModule)
parseFile = Haskell.parseFile
      -- *** Desugar into lambdas:
      !=> runT (normIds <> deparenify <> desugarInfix <> wheresToLets)
      >=> runT desugarFuns
      >=> runT (desugarTuples <> desugarDos)
      >=> runT desugarLets
      >=> runT (desugarIfs <> desugarNegs <> desugarWildCards)
      -- *** Normalize lambdas:
      >=> runT flattenLambdas
      >=> runT depatLambdas
      >=> runT desugarAsPats
      >=> runT lambdasToCases
      -- *** Translate into the core abstract syntax:
      >=> trans
      where (!=>) :: (FilePath -> IO (ParseResult Module)) -> (Module -> Trans a) -> FilePath -> IO (ParseResult a)
            f !=> g = f >=> runTrans . (pr2Trans >=> g)
            infix 0 !=>
            runTrans :: Trans a -> IO (ParseResult a)
            runTrans = except2PR . (flip runStateT 0 >=> return . fst)
            pr2Trans :: ParseResult a -> Trans a
            pr2Trans = \case
                  ParseOk p           -> return p
                  ParseFailed loc msg -> pFail loc msg
            except2PR :: ExceptT (SrcLoc, String) IO a -> IO (ParseResult a)
            except2PR (runExceptT -> e) = \case
                  Left (loc, msg) -> ParseFailed loc msg
                  Right a         -> return a
                  <$> e

unknownLoc :: SrcLoc
unknownLoc = SrcLoc "" 0 0

type Trans = StateT Int (ExceptT (SrcLoc, String) IO)

pFail :: SrcLoc -> String -> Trans a
pFail loc msg = lift $ throwE (loc, msg)

fresh :: Trans String
fresh = do
      x <- get
      put $ x + 1
      return $ "$" ++ show x

-- | Removes parens in types, expressions, and patterns so they don't confuddle
--   everything.
deparenify :: Transform Trans
deparenify = match (\(Paren n)   -> return n)
          <> match (\(PParen n)  -> return n)
          <> match (\(TyParen n) -> return n)

mkTuple :: Int -> String
mkTuple n = "(" ++ replicate (n-1) ',' ++ ")"

-- | Turns Symbols and Specials into normal identifiers.
normIds :: Transform Trans
normIds = match (\case
            Symbol n               -> return $ Ident n)
       <> match (\case
            Qual _ n               -> return $ UnQual n
            Special UnitCon        -> return $ UnQual $ Ident "()"
            Special ListCon        -> return $ UnQual $ Ident "List"
            Special FunCon         -> return $ UnQual $ Ident "->"
            -- I think this is only for the prefix constructor.
            Special (TupleCon _ i) -> return $ UnQual $ Ident $ mkTuple i
            Special Cons           -> return $ UnQual $ Ident "Cons")

-- | Turns sections and infix ops into regular applications and lambdas.
desugarInfix :: Transform Trans
desugarInfix = match $ \case
      LeftSection e (QVarOp op)  -> return $ App (Var op) e
      LeftSection e (QConOp op)  -> return $ App (Con op) e
      RightSection (QVarOp op) e -> do
            x <- fresh
            return $ Lambda unknownLoc [PVar $ Ident x] $ App (App (Var op) $ Var $ UnQual $ Ident x) e
      RightSection (QConOp op) e -> do
            x <- fresh
            return $ Lambda unknownLoc [PVar $ Ident x] $ App (App (Con op) $ Var $ UnQual $ Ident x) e
      InfixApp e1 (QVarOp op) e2 -> return $ App (App (Var op) e1) e2
      InfixApp e1 (QConOp op) e2 -> return $ App (App (Con op) e1) e2

-- | Turns wildcard patterns into variable patterns.
desugarWildCards :: Transform Trans
desugarWildCards = match $ \PWildCard -> return $ PVar $ Ident "$_"

-- | Turns piece-wise function definitions into a single PatBind with a lambda
--   and case expression on the RHS. E.g.:
-- > f p1 p2 = rhs1
-- > f q1 q2 = rhs2
-- becomes
-- > f = \$1 $2 -> case ($1, $2) of { (p1, p2) -> rhs1; (q1, q2) -> rhs2 }
desugarFuns :: Transform Trans
desugarFuns = match $ \case
      FunBind ms@(Match loc name pats _ _ Nothing:_) -> do
            e <- buildLambda loc ms $ length pats
            return $ PatBind loc (PVar name) (UnGuardedRhs e) Nothing
      n@(FunBind _)                                  -> pFail unknownLoc $ "unsupported syntax: " ++ prettyPrint n
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
desugarTuples :: Transform Trans
desugarTuples = match (\(Tuple _ es)   -> return $ foldl' App (Con $ UnQual $ Ident $ mkTuple $ length es) es)
             <> match (\(TyTuple _ ts) -> return $ foldl' TyApp (TyCon $ UnQual $ Ident $ mkTuple $ length ts) ts)
             <> match (\(PTuple _ ps)  -> return $ PApp (UnQual $ Ident $ mkTuple $ length ps) ps)

-- | Turns do-notation into a series of >>= \x ->. Turns LetStmts into Lets.
--   Should run before Let and Lambda desugarage. E.g.:
-- > do p1 <- m
-- >    let p2 = e
-- >    return e
-- becomes
-- > m >>= (\p1 -> (let p2 = e in return e))
desugarDos :: Transform Trans
desugarDos = match $ \(Do stmts) -> transDo stmts
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
wheresToLets :: Transform Trans
wheresToLets = match (\case
                  Match loc name ps t (UnGuardedRhs e) (Just binds) -> return $ Match loc name ps t (UnGuardedRhs $ Let binds e) Nothing
                  n@(Match loc _ _ _ (GuardedRhss _) _)             -> pFail loc $ "guards are not supported: " ++ prettyPrint n)
            <> match (\case
                  PatBind loc p (UnGuardedRhs e) (Just binds) -> return $ PatBind loc p (UnGuardedRhs $ Let binds e) Nothing
                  n@(PatBind loc _ (GuardedRhss _) _)         -> pFail loc $ "guards are not supported: " ++ prettyPrint n)
            <> match (\case
                  Alt loc p (UnGuardedRhs e) (Just binds) -> return $ Alt loc p (UnGuardedRhs $ Let binds e) Nothing
                  n@(Alt loc _ (GuardedRhss _) _)         -> pFail loc $ "guards are not supported: " ++ prettyPrint n)

-- | Turns Lets into Cases. Assumes functions in Lets are already desugared.
--   E.g.:
-- > let p = e1
-- >     q = e2
-- > in e3
-- becomes
-- > case e1 of { p -> (case e2 of { q -> e3 } }
desugarLets :: Transform Trans
desugarLets = match $ \case
      Let (BDecls ds) e -> foldrM transLet e ds
      n@(Let _ _)       -> pFail unknownLoc $ "unsupported syntax: " ++ prettyPrint n
      where transLet :: Decl -> Exp -> Trans Exp
            transLet (PatBind loc p (UnGuardedRhs e1) Nothing) inner = return $ Case e1 [Alt loc p (UnGuardedRhs inner) Nothing]
            transLet n@(PatBind loc _ _ _)                     _     = pFail loc $ "unsupported syntax: " ++ prettyPrint n
            transLet n                                         _     = pFail unknownLoc $ "unsupported syntax: " ++ prettyPrint n

-- | Turns ifs into cases and unary minus.
-- > if e1 then e2 else e3
-- becomes
-- > case e1 of { True -> e2; False -> e3 }
desugarIfs :: Transform Trans
desugarIfs = match $
      \(If e1 e2 e3) -> return $ Case e1
            [ Alt unknownLoc (PApp (UnQual $ Ident "True")  []) (UnGuardedRhs e2) Nothing
            , Alt unknownLoc (PApp (UnQual $ Ident "False") []) (UnGuardedRhs e3) Nothing
            ]

desugarNegs :: Transform Trans
desugarNegs = match $
      \(NegApp e) -> return $ App (App (Var $ UnQual $ Ident "-") $ Lit $ Int 0) e

-- | Turns Lambdas with several bindings into several lambdas with single
--   bindings. E.g.:
-- > \p1 p2 -> e
-- becomes
-- > \p1 -> \p2 -> e
flattenLambdas :: Transform Trans
flattenLambdas = match $
      \(Lambda loc ps e) -> return $ foldr (Lambda loc . return) e ps

-- | Replaces non-var patterns in lambdas with a fresh var and a case. E.g.:
-- > \(a,b) -> e
-- becomes
-- > \$x -> case $x of { (a,b) -> e }
depatLambdas :: Transform Trans
depatLambdas = match $ \case
      n@(Lambda _ [PVar _] _) -> return n
      Lambda loc [p] e        -> do
            x <- fresh
            return $ Lambda loc [PVar $ Ident x] (Case (Var $ UnQual $ Ident x) [Alt loc p (UnGuardedRhs e) Nothing])

-- | Desugars as-patterns in (and only in) cases into more cases. Should run
--   after depatLambdas. E.g.:
-- > case e1 of
-- >   x@(C y@p) -> e2
-- becomes
-- > case e1 of { C p -> (\x -> ((\y -> e2) p)) (C p) }
desugarAsPats :: Transform Trans
desugarAsPats = match $
      \(Alt loc p (UnGuardedRhs e) Nothing) -> do
            app <- foldrM (mkApp loc) e $ getAses p
            return $ Alt loc (deAs p) (UnGuardedRhs app) Nothing
      where mkApp :: SrcLoc -> (Pat, Pat) -> Exp -> Trans Exp
            mkApp loc (p, p') e = App (Lambda loc [p] e) <$> patToExp p'
            getAses :: Pat -> [(Pat, Pat)]
            getAses = runQ $ query' $ \case
                  PAsPat n p -> [(PVar n, p)]
                  _          -> []
            deAs :: Pat -> Pat
            deAs = runIdentity . runT (match' $
                  \case PAsPat _ p -> return p
                        n          -> return n)
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
lambdasToCases :: Transform Trans
lambdasToCases = match $
      \(App (Lambda loc [p] e2) e1) -> return $ Case e1 [Alt loc p (UnGuardedRhs e2) Nothing]

-- | Translate a Haskell module into the ReWire abstract syntax.
trans :: Module -> Trans RWCModule
trans (Module _loc (ModuleName n) _pragmas _ _exports _imports (reverse -> ds)) = do
      datas <- foldM transData [] ds
      sigs  <- foldM transTySig [] ds
      inls  <- foldM transInlineSig [] ds
      defs  <- foldM (transDef sigs inls) [] ds
      -- FIXME: have fun  --adam
      let imps = []
      return $ RWCModule (ModuleId n) imps datas defs

transData :: [RWCData] -> Decl -> Trans [RWCData]
transData datas (DataDecl loc _ _ (Ident x) tyVars cons _deriving) = do
      tyVars' <- mapM (transTyVar loc) tyVars
      cons' <- mapM transCon cons
      return $ RWCData (TyConId x) tyVars' kblank cons' : datas
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

kblank :: Kind
kblank = Kstar

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
