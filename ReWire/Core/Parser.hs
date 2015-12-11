{-# LANGUAGE ViewPatterns, RankNTypes #-}
module ReWire.Core.Parser (parseFile, ParseResult(..), SrcLoc(..), prettyPrint) where

import ReWire.Scoping
import ReWire.Core.Syntax
import Data.List (nub)
import Control.Monad (liftM, liftM2, foldM, (>=>))
import Data.Foldable (foldl')
import Data.Data
import Data.Maybe (fromJust)

import qualified Language.Haskell.Exts as Haskell (parseFile)
import           Language.Haskell.Exts hiding (parseFile, loc, name, binds, op)

-- | Parse a ReWire source file.
parseFile :: FilePath -> IO (ParseResult RWCProg)
parseFile = Haskell.parseFile >=> io (deparenify >=> desugarDo >=> desugarFuns >=> transMod)
      where io f = return . (>>= f)

everywhere :: Data a => (forall b. Data b => b -> b) -> a -> a
everywhere f = f . gmapT (everywhere f)

unknownLoc ::SrcLoc
unknownLoc = SrcLoc "" 0 0

-- | Turns do-notation into a series of >>= \x ->.
desugarDo :: Module -> ParseResult Module
desugarDo =  return . everywhere desugarDo'
      where desugarDo' n = case cast n of
                  Nothing       -> n
                  Just n@(Do _) -> fromJust $ cast $ transDo n
                  Just n        -> fromJust $ cast n
            transDo (Do (Generator loc p e:stmts)) = App (App (Var (UnQual (Symbol ">>="))) e) (Lambda loc [p] $ transDo $ Do stmts)
            transDo (Do [Qualifier e])             = e
            transDo (Do (Qualifier e:stmts))       = App (App (Var (UnQual (Symbol ">>="))) e) (Lambda unknownLoc [PVar (Ident "$_")] $ transDo $ Do stmts)
            transDo (Do (LetStmt binds:stmts))     = Let binds $ transDo $ Do stmts

-- | Removes parens in types, expressions, and patterns so they don't confuddle
--   everything.
deparenify :: Module -> ParseResult Module
deparenify = return . everywhere deparenify'
      where deparenify' n = case cast n of
                  Nothing -> case cast n of
                        Nothing -> case cast n of
                              Nothing         -> n
                              Just (PParen n) -> fromJust $ cast n
                              Just n          -> fromJust $ cast n
                        Just (TyParen n) -> fromJust $ cast n
                        Just n           -> fromJust $ cast n
                  Just (Paren n) -> fromJust $ cast n
                  Just n         -> fromJust $ cast n

-- | Turns piece-wise function definitions into a single PatBind with a series
--   of nested lambdas and a case expression on the RHS. E.g.:
-- > f p1 p2 = rhs1
-- > f q1 q2 = rhs2
-- becomes
-- > f = \$1 -> \$2 -> case ($1, $2) of { (p1, p2) -> rhs1; (q1, q2) -> rhs2 }
desugarFuns :: Module -> ParseResult Module
desugarFuns (Module loc name prags warns exps imps ds) =
      (liftM $ Module loc name prags warns exps imps) (mapM desugarFun ds)

desugarFun :: Decl -> ParseResult Decl
desugarFun (FunBind ms@(Match loc name pats _mt _rhs Nothing:_)) = do
      e <- buildLambda loc ms $ length pats
      return $ PatBind loc (PVar name) (UnGuardedRhs e) Nothing
desugarFun d = return d

buildLambda :: SrcLoc -> [Match] -> Int -> ParseResult Exp
buildLambda loc ms arrity = do
      alts <- mapM toAlt ms
      return $ foldr (\x -> Lambda loc [PVar (Ident x)]) (Case (Tuple Boxed (map (Var . UnQual . Ident) xs)) alts) xs
      where xs = map (("$"++).show) [1..arrity]
            toAlt (Match loc' _ pats Nothing rhs binds) = return $ Alt loc' (PTuple Boxed pats) rhs binds
            toAlt m@(Match loc' _ _ _ _ _)              = ParseFailed loc' $ "unsupported syntax: " ++ prettyPrint m

-- | Translate a Haskell module into the ReWire abstract syntax.
transMod :: Module -> ParseResult RWCProg
transMod (Module _loc _name _pragmas _ _exports _imports (reverse -> ds)) = do
      -- TODO(chathhorn): it'd probably be nicer to do this in a single pass.
      datas <- foldM transData [] ds
      sigs  <- foldM transTySigs [] ds
      defs  <- foldM (transDef sigs) [] ds
      return $ RWCProg datas defs

transData :: [RWCData] -> Decl -> ParseResult [RWCData]
transData datas (DataDecl loc _ _ (Ident x) tyVars cons _deriving) = do
      tyVars' <- mapM (transTyVar loc) tyVars
      cons' <- mapM transCon cons
      return $ RWCData (TyConId x) tyVars' cons' : datas
transData datas _                                                  = return datas

transTySigs :: [(String, RWCTy)] -> Decl -> ParseResult [(String, RWCTy)]
transTySigs sigs (TypeSig loc names t) = do
      t' <- transTy loc [] t
      return $ zip (map (\(Ident x) -> x) names) (repeat t') ++ sigs
transTySigs sigs _                     = return sigs

transDef :: [(String, RWCTy)] -> [RWCDefn] -> Decl -> ParseResult [RWCDefn]
transDef tys defs (PatBind loc (PVar (Ident x)) (UnGuardedRhs e) Nothing) = case lookup x tys of
      Just t -> liftM ((:defs) . RWCDefn (mkId x) (nub (fv t) :-> t)) $ transExp loc e
      _      -> ParseFailed loc $ "no type signature for " ++ x
transDef _   defs _                                                       = return defs
-- TODO(chathhorn): guards, where clauses, type annotations.

transTyVar :: SrcLoc -> TyVarBind -> ParseResult (Id RWCTy)
transTyVar _   (UnkindedVar (Ident x)) = return $ mkId x
transTyVar loc tv                      = ParseFailed loc $ "unsupported syntax: " ++ prettyPrint tv

transCon :: QualConDecl -> ParseResult RWCDataCon
transCon (QualConDecl loc [] _ (ConDecl (Ident x) tys)) = liftM (RWCDataCon $ DataConId x) $ mapM (transTy loc []) tys
transCon d@(QualConDecl loc _ _ _)                      = ParseFailed loc $ "unsupported syntax: " ++ prettyPrint d

transTy :: SrcLoc -> [String] -> Type -> ParseResult RWCTy
transTy loc ms (TyForall Nothing cs t)    = do
      ms' <- mapM (getNad loc) cs
      transTy loc (ms ++ ms') t
transTy loc ms (TyFun a b)                = liftM2 mkArrow (transTy loc ms a) (transTy loc ms b)
transTy loc ms (TyApp a b) | isMonad ms a = liftM2 RWCTyComp (transTy loc ms a) (transTy loc ms b)
                           | otherwise    = liftM2 RWCTyApp (transTy loc ms a) (transTy loc ms b)
transTy _   _  (TyCon (UnQual (Ident x))) = return $ RWCTyCon (TyConId x)
transTy _   _  (TyCon (Special UnitCon))  = return $ RWCTyCon (TyConId "Unit")
transTy loc ms (TyTuple _ ts)             = liftM (foldl' RWCTyApp (RWCTyCon (TyConId $ mkTuple $ length ts))) $ mapM (transTy loc ms) ts
transTy _   _  (TyVar (Ident x))          = return $ RWCTyVar (mkId x)
transTy loc _  t                          = ParseFailed loc $ "unsupported syntax: " ++ prettyPrint t

getNad :: SrcLoc -> Asst -> ParseResult String
getNad _   (ClassA (UnQual (Ident "Monad")) [TyVar (Ident x)]) = return x
getNad loc a                                                   = ParseFailed loc $ "unsupported typeclass constraint: " ++ prettyPrint a

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

transExp :: SrcLoc -> Exp -> ParseResult RWCExp
transExp loc (App (App (Var (UnQual (Ident "nativeVhdl")))  (Lit (String f))) e)
                                             = liftM (RWCNativeVHDL f) (transExp loc e)
transExp loc (App e1 e2)                     = liftM2 RWCApp (transExp loc e1) (transExp loc e2)
transExp loc (InfixApp e1 (QVarOp op) e2)    = transExp loc (App (App (Var op) e1) e2)
transExp loc (InfixApp e1 (QConOp op) e2)    = transExp loc (App (App (Con op) e1) e2)
-- TODO(chathhorn): (irrefutable) patterns in lambdas, lets, wheres.
transExp _   (Lambda loc [PVar (Ident x)] e) = liftM (RWCLam (mkId x) tblank) (transExp loc e)
-- TODO(chathhorn): full-power lets...
transExp _   (Let (BDecls [PatBind loc (PVar (Ident x)) (UnGuardedRhs e1) Nothing]) e2)
                                             = liftM2 (RWCLet $ mkId x) (transExp loc e1) (transExp loc e2)
transExp _   (Var (UnQual (Ident x)))        = return $ RWCVar (mkId x) tblank
transExp _   (Var (UnQual (Symbol x)))       = return $ RWCVar (mkId x) tblank
transExp _   (Con (UnQual (Ident x)))        = return $ RWCCon (DataConId x) tblank
transExp _   (Con (UnQual (Symbol x)))       = return $ RWCCon (DataConId x) tblank
transExp _   (Con (Special UnitCon))         = return $ RWCCon (DataConId "Unit") tblank
transExp loc (Lit lit)                       = liftM RWCLiteral (transLit loc Signless lit)
transExp loc (Case e alts)                   = liftM2 RWCCase (transExp loc e) (mapM transAlt alts)
transExp loc (Tuple _ [e])                   = transExp loc e -- silly mono-tuples introduced during fun desugaring.
transExp loc (Tuple _ es)                    = liftM (foldl' RWCApp (RWCCon (DataConId $ mkTuple $ length es) tblank)) $ mapM (transExp loc) es
transExp loc e                               = ParseFailed loc $ "unsupported syntax: " ++ prettyPrint e
-- TODO(chathhorn): all those other expressions...

transLit :: SrcLoc -> Sign -> Literal -> ParseResult RWCLit
transLit _ Signless (Int i)  = return $ RWCLitInteger i
transLit _ Negative (Int i)  = return $ RWCLitInteger (-i)
transLit _ Signless (Frac d) = return $ RWCLitFloat (fromRational d)
transLit _ Negative (Frac d) = return $ RWCLitFloat (fromRational (-d))
transLit _ Signless (Char c) = return $ RWCLitChar c
transLit loc _ lit           = ParseFailed loc $ "unsupported syntax: " ++ prettyPrint lit

transAlt :: Alt -> ParseResult RWCAlt
transAlt (Alt loc p (UnGuardedRhs e) Nothing) = liftM2 RWCAlt (transPat loc p) (transExp loc e)
transAlt a@(Alt loc _ _ _)                    = ParseFailed loc $ "unsupported syntax: " ++ prettyPrint a

transPat :: SrcLoc -> Pat -> ParseResult RWCPat
transPat loc (PApp (UnQual (Ident x)) ps) = liftM (RWCPatCon (DataConId x)) $ mapM (transPat loc) ps
transPat _   (PApp (Special UnitCon) [])  = return $ RWCPatCon (DataConId "Unit") []
transPat loc (PLit s lit)                 = liftM RWCPatLiteral (transLit loc s lit)
transPat _   (PVar (Ident x))             = return $ RWCPatVar (mkId x) tblank
transPat _   PWildCard                    = return RWCPatWild
transPat loc (PTuple _ [p])               = transPat loc p
transPat loc (PTuple _ ps)                = liftM (RWCPatCon $ DataConId $ mkTuple $ length ps) $ mapM (transPat loc) ps
transPat loc p                            = ParseFailed loc $ "unsupported syntax: " ++ prettyPrint p
