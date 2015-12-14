{-# LANGUAGE ViewPatterns, RankNTypes, LambdaCase #-}
module ReWire.Core.Parser (parseFile, ParseResult(..), SrcLoc(..), prettyPrint) where

import ReWire.Scoping (mkId, Id, fv)
import ReWire.Core.Syntax
import Data.List (nub)
import Data.Functor ((<$>))
import Data.Functor.Identity
import Control.Applicative ((<*>))
import Control.Monad (foldM, replicateM, (>=>))
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
            -- General idea:
            --    Haskell
            --    => (desugar into) lambdas
            --    => normalize lambdas
            --    => lambdas to cases (where possible)
            --   [=> lift the remaining lambdas into fun defs?]
            !=> deparenify
            >=> desugarFuns
            >=> desugarDos
            >=> desugarLets
            >=> purgeWildCards
            -- Normalize lambdas (+ desugarAsPats).
            >=> flattenLambdas
            >=> depatLambdas
            >=> desugarAsPats
            >=> lambdasToCases
            -- TODO(chathhorn): lift lambdas?
            >=> trans
      where f !=> g = f >=> return . (>>= runTrans . g)
            infix 0 !=>
            runTrans = flip runStateT 0 >=> return . fst

everywhere :: (Data a, Monad m) => (forall d. Data d => d -> m d) -> a -> m a
everywhere f = gmapM (everywhere f) >=> f

everywhereQ :: (Data a) => (forall d. Data d => d -> [b]) -> a -> [b]
everywhereQ f n = f n ++ gmapQr (++) [] (everywhereQ f) n

unknownLoc ::SrcLoc
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
deparenify = everywhere $ \n -> case cast n of
      Nothing -> case cast n of
            Nothing -> case cast n of
                  Nothing         -> return n
                  Just (PParen n) -> tr n
                  Just n          -> tr n
            Just (TyParen n) -> tr n
            Just n           -> tr n
      Just (Paren n) -> tr n
      Just n         -> tr n

-- | Turns wildcard patterns into variable patterns.
purgeWildCards :: Module -> Trans Module
purgeWildCards = everywhere $ \n -> case cast n of
      Nothing        -> return n
      Just PWildCard -> tr $ PVar $ Ident "$_"
      Just n         -> tr n

-- | Turns piece-wise function definitions into a single PatBind with a lambda
--   and case expression on the RHS. E.g.:
-- > f p1 p2 = rhs1
-- > f q1 q2 = rhs2
-- becomes
-- > f = \$1 $2 -> case ($1, $2) of { (p1, p2) -> rhs1; (q1, q2) -> rhs2 }
desugarFuns :: Module -> Trans Module
desugarFuns = everywhere $ \n -> case cast n of
      Nothing                                               -> return n
      Just (FunBind ms@(Match loc name pats _ _ Nothing:_)) -> do
            e <- buildLambda loc ms $ length pats
            tr $ PatBind loc (PVar name) (UnGuardedRhs e) Nothing
      Just n@(FunBind _)                                    -> pFail unknownLoc $ "unsupported syntax: " ++ prettyPrint n
      Just n                                                -> tr n
      where buildLambda loc ms arrity = do
                  alts <- mapM toAlt ms
                  xs <- replicateM arrity fresh
                  return $ Lambda loc (map (PVar . Ident) xs) $ Case (Tuple Boxed (map (Var . UnQual . Ident) xs)) alts
            toAlt (Match loc' _ pats Nothing rhs binds) = return $ Alt loc' (PTuple Boxed pats) rhs binds
            toAlt m@(Match loc' _ _ _ _ _)              = pFail loc' $ "unsupported syntax: " ++ prettyPrint m

-- | Turns do-notation into a series of >>= \x ->. Turns LetStmts into Lets.
--   Should run before Let and Lambda desugarage. E.g.:
-- > do p1 <- m
-- >    let p2 = e
-- >    return e
-- becomes
-- > m >>= (\p1 -> (let p2 = e in return e))
desugarDos :: Module -> Trans Module
desugarDos =  everywhere $ \n -> case cast n of
      Nothing         -> return n
      Just (Do stmts) -> transDo stmts >>= tr
      Just n          -> tr n
      where transDo (Generator loc p e:stmts) = App (App (Var (UnQual (Symbol ">>="))) e) <$> (Lambda loc [p] <$> transDo stmts)
            transDo [Qualifier e]             = return e
            transDo (Qualifier e:stmts)       = App (App (Var (UnQual (Symbol ">>="))) e) <$> (Lambda unknownLoc [PWildCard] <$> transDo stmts)
            transDo (LetStmt binds:stmts)     = Let binds <$> transDo stmts
            transDo (stmt:_)                  = pFail unknownLoc $ "unsupported syntax: " ++ prettyPrint stmt
            transDo _                         = pFail unknownLoc "something went wrong while translating a do-block."

-- | Turns Lets into Cases. Assumes functions in Lets are already desugared.
--   E.g.:
-- > let p = e1
-- >     q = e2
-- > in e3
-- becomes
-- > case e1 of { p -> (case e2 of { q -> e3 } }
desugarLets :: Module -> Trans Module
desugarLets = everywhere $ \n -> case cast n of
      Nothing                  -> return n
      Just (Let (BDecls ds) e) -> foldrM transLet e ds >>= tr
      Just n@(Let _ _)         -> pFail unknownLoc $ "unsupported syntax: " ++ prettyPrint n
      Just n                   -> tr n
      where transLet (PatBind loc p (UnGuardedRhs e1) Nothing) inner = return $ Case e1 [Alt loc p (UnGuardedRhs inner) Nothing]
            transLet n@(PatBind loc _ _ _) _ = pFail loc $ "unsupported syntax: " ++ prettyPrint n
            transLet n                     _ = pFail unknownLoc $ "unsupported syntax: " ++ prettyPrint n

-- | Turns Lambdas with several bindings into several lambdas with single
--   bindings. E.g.:
-- > \p1 p2 -> e
-- becomes
-- > \p1 -> \p2 -> e
flattenLambdas :: Module -> Trans Module
flattenLambdas = everywhere $ \n -> case cast n of
      Nothing                -> return n
      Just (Lambda loc ps e) -> tr $ foldr (Lambda loc . return) e ps
      Just n                 -> tr n

-- | Replaces non-var patterns in lambdas with a fresh var and a case. E.g.:
-- > \(a,b) -> e
-- becomes
-- > \$x -> case $x of { (a,b) -> e }
depatLambdas :: Module -> Trans Module
depatLambdas = everywhere $ \n -> case cast n of
      Nothing                      -> return n
      Just n@(Lambda _ [PVar _] _) -> tr n
      Just (Lambda loc [p] e)      -> do
            x <- fresh
            tr $ Lambda loc [PVar (Ident x)] (Case (Var (UnQual (Ident x))) [Alt loc p (UnGuardedRhs e) Nothing])
      Just n -> tr n

-- | Desugars as-patterns in (and only in) cases into more cases. Should run
--   after depatLambdas. E.g.:
-- > case e1 of
-- >   x@(C y@p) -> e2
-- becomes
-- > case e1 of { C p -> (\x -> ((\y -> e2) p)) (C p) }
desugarAsPats :: Module -> Trans Module
desugarAsPats = everywhere $ \n -> case cast n of
      Nothing                                   -> return n
      Just (Alt loc p (UnGuardedRhs e) Nothing) -> tr $ Alt loc (deAs p) (UnGuardedRhs $ foldr (mkApp loc) e $ getAses p) Nothing
      Just n                                    -> tr n
      where mkApp loc (p, e') e = App (Lambda loc [p] e) e'
            getAses :: Pat -> [(Pat, Exp)]
            getAses = everywhereQ $ \n -> case cast n of
                  Just (PAsPat n p) -> [(PVar n, toExp p)]
                  _                 -> []
            deAs :: Pat -> Pat
            deAs = runIdentity . everywhere (\n -> case cast n of
                  Nothing           -> return n
                  Just (PAsPat _ p) -> tr p
                  Just n            -> tr n)
            toExp :: Pat -> Exp
            toExp = \case
                  PVar n            -> Var $ UnQual n
                  PLit s n          -> Lit $ deSign s n
                  -- PNPlusK _name _int -> 
                  PInfixApp p1 n p2 -> InfixApp (toExp p1) (QConOp n) (toExp p2)
                  PApp n ps         -> foldl' App (Con n) (map toExp ps)
                  PTuple b ps       -> Tuple b $ map toExp ps
                  PList ps          -> List $ map toExp ps
                  -- PRec _qname _patfields ->
                  PAsPat _ p        -> toExp p
                  PIrrPat p         -> toExp p
                  PatTypeSig _ p _  -> toExp p
                  -- PViewPat _exp _pat ->
                  PBangPat p        -> toExp p
                  _                 -> error "unsupported pattern" -- TODO(chathhorn)

-- | Turns beta-redexes into cases. E.g.:
-- > (\x -> e2) e1
-- becomes
-- > case e1 of { x -> e2 }
lambdasToCases :: Module -> Trans Module
lambdasToCases = everywhere $ \n -> case cast n of
      Nothing                           -> return n
      Just (App (Lambda loc [p] e2) e1) -> tr $ Case e1 [Alt loc p (UnGuardedRhs e2) Nothing]
      Just n                            -> tr n

-- | Translate a Haskell module into the ReWire abstract syntax.
trans :: Module -> Trans RWCProg
trans (Module _loc _name _pragmas _ _exports _imports (reverse -> ds)) = do
      -- TODO(chathhorn): it'd probably be nicer to do this in a single pass.
      datas <- foldM transData [] ds
      sigs  <- foldM transTySigs [] ds
      defs  <- foldM (transDef sigs) [] ds
      return $ RWCProg datas defs

transData :: [RWCData] -> Decl -> Trans [RWCData]
transData datas (DataDecl loc _ _ (Ident x) tyVars cons _deriving) = do
      tyVars' <- mapM (transTyVar loc) tyVars
      cons' <- mapM transCon cons
      return $ RWCData (TyConId x) tyVars' cons' : datas
transData datas _                                                  = return datas

transTySigs :: [(String, RWCTy)] -> Decl -> Trans [(String, RWCTy)]
transTySigs sigs (TypeSig loc names t) = do
      t' <- transTy loc [] t
      return $ zip (map (\(Ident x) -> x) names) (repeat t') ++ sigs
transTySigs sigs _                     = return sigs

transDef :: [(String, RWCTy)] -> [RWCDefn] -> Decl -> Trans [RWCDefn]
transDef tys defs (PatBind loc (PVar (Ident x)) (UnGuardedRhs e) Nothing) = case lookup x tys of
      Just t -> ((:defs) . RWCDefn (mkId x) (nub (fv t) :-> t)) <$> transExp loc e
      _      -> pFail loc $ "no type signature for " ++ x
transDef _   defs _                                                       = return defs
-- TODO(chathhorn): guards, where clauses, type annotations.

transTyVar :: SrcLoc -> TyVarBind -> Trans (Id RWCTy)
transTyVar _   (UnkindedVar (Ident x)) = return $ mkId x
transTyVar loc tv                      = pFail loc $ "unsupported syntax: " ++ prettyPrint tv

transCon :: QualConDecl -> Trans RWCDataCon
transCon (QualConDecl loc [] _ (ConDecl (Ident x) tys)) = (RWCDataCon $ DataConId x) <$> mapM (transTy loc []) tys
transCon d@(QualConDecl loc _ _ _)                      = pFail loc $ "unsupported syntax: " ++ prettyPrint d

transTy :: SrcLoc -> [String] -> Type -> Trans RWCTy
transTy loc ms (TyForall Nothing cs t)    = do
      ms' <- mapM (getNad loc) cs
      transTy loc (ms ++ ms') t
transTy loc ms (TyFun a b)                = mkArrow <$> transTy loc ms a <*> transTy loc ms b
transTy loc ms (TyApp a b) | isMonad ms a = RWCTyComp <$> transTy loc ms a <*> transTy loc ms b
                           | otherwise    = RWCTyApp <$> transTy loc ms a <*> transTy loc ms b
transTy _   _  (TyCon (UnQual (Ident x))) = return $ RWCTyCon (TyConId x)
transTy _   _  (TyCon (Special UnitCon))  = return $ RWCTyCon (TyConId "Unit")
transTy loc ms (TyTuple _ ts)             = foldl' RWCTyApp (RWCTyCon (TyConId $ mkTuple $ length ts)) <$> mapM (transTy loc ms) ts
transTy _   _  (TyVar (Ident x))          = return $ RWCTyVar (mkId x)
transTy loc _  t                          = pFail loc $ "unsupported syntax: " ++ prettyPrint t

getNad :: SrcLoc -> Asst -> Trans String
getNad _   (ClassA (UnQual (Ident "Monad")) [TyVar (Ident x)]) = return x
getNad loc a                                                   = pFail loc $ "unsupported typeclass constraint: " ++ prettyPrint a

isMonad :: [String] -> Type -> Bool
isMonad ms (TyApp (TyApp (TyApp (TyCon (UnQual (Ident "ReT"))) _) _) t) = isMonad ms t
isMonad ms (TyApp (TyApp (TyCon (UnQual (Ident "StT"))) _) t)           = isMonad ms t
isMonad _  (TyCon (UnQual (Ident "I")))                                 = True
isMonad ms (TyVar (Ident x))                                            = x `elem` ms
isMonad _  _                                                            = False

tblank :: RWCTy
tblank = RWCTyCon (TyConId "_")

mkTuple :: Int -> String
mkTuple n = "Tuple" ++ show n

transExp :: SrcLoc -> Exp -> Trans RWCExp
transExp loc (App (App (Var (UnQual (Ident "nativeVhdl")))  (Lit (String f))) e)
                                             = RWCNativeVHDL f <$> transExp loc e
transExp loc (App e1 e2)                     = RWCApp <$> transExp loc e1 <*> transExp loc e2
transExp loc (InfixApp e1 (QVarOp op) e2)    = transExp loc (App (App (Var op) e1) e2)
transExp loc (InfixApp e1 (QConOp op) e2)    = transExp loc (App (App (Con op) e1) e2)
transExp _   (Lambda loc [PVar (Ident x)] e) = RWCLam (mkId x) tblank <$> transExp loc e
transExp _   (Var (UnQual (Ident x)))        = return $ RWCVar (mkId x) tblank
transExp _   (Var (UnQual (Symbol x)))       = return $ RWCVar (mkId x) tblank
transExp _   (Con (UnQual (Ident x)))        = return $ RWCCon (DataConId x) tblank
transExp _   (Con (UnQual (Symbol x)))       = return $ RWCCon (DataConId x) tblank
transExp _   (Con (Special UnitCon))         = return $ RWCCon (DataConId "Unit") tblank
transExp loc (Lit lit)                       = RWCLiteral <$> transLit loc lit
transExp loc (Case e alts)                   = RWCCase <$> transExp loc e <*> mapM transAlt alts
transExp loc (Tuple _ [e])                   = transExp loc e -- silly mono-tuples introduced during fun desugaring.
transExp loc (Tuple _ es)                    = foldl' RWCApp (RWCCon (DataConId $ mkTuple $ length es) tblank) <$> mapM (transExp loc) es
transExp loc e                               = pFail loc $ "unsupported syntax: " ++ prettyPrint e
-- TODO(chathhorn): all those other expressions...

-- Not entirely sure this is right...
deSign :: Sign -> Literal -> Literal
deSign Negative (Int        l) = Int (-l)
deSign Negative (Frac       l) = Frac (-l)
deSign Negative (PrimInt    l) = PrimInt (-l)
deSign Negative (PrimWord   l) = PrimWord (-l)
deSign Negative (PrimFloat  l) = PrimFloat (-l)
deSign Negative (PrimDouble l) = PrimDouble (-l)
deSign _ l = l

transLit :: SrcLoc -> Literal -> Trans RWCLit
transLit _ (Int i)  = return $ RWCLitInteger i
transLit _ (Frac d) = return $ RWCLitFloat (fromRational d)
transLit _ (Char c) = return $ RWCLitChar c
transLit loc lit           = pFail loc $ "unsupported syntax: " ++ prettyPrint lit

transAlt :: Alt -> Trans RWCAlt
transAlt (Alt loc p (UnGuardedRhs e) Nothing) = RWCAlt <$> transPat loc p <*> transExp loc e
transAlt a@(Alt loc _ _ _)                    = pFail loc $ "unsupported syntax: " ++ prettyPrint a

transPat :: SrcLoc -> Pat -> Trans RWCPat
transPat loc (PApp (UnQual (Ident x)) ps) = RWCPatCon (DataConId x) <$> mapM (transPat loc) ps
transPat _   (PApp (Special UnitCon) [])  = return $ RWCPatCon (DataConId "Unit") []
transPat loc (PLit s lit)                 = RWCPatLiteral <$> transLit loc (deSign s lit)
transPat _   (PVar (Ident x))             = return $ RWCPatVar (mkId x) tblank
transPat loc (PTuple _ [p])               = transPat loc p
transPat loc (PTuple _ ps)                = (RWCPatCon $ DataConId $ mkTuple $ length ps) <$> mapM (transPat loc) ps
transPat loc p                            = pFail loc $ "unsupported syntax: " ++ show p
