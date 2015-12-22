{-# LANGUAGE LambdaCase, ViewPatterns #-}
module ReWire.FrontEnd.Translate (translate, Export(..), deSigil) where

import ReWire.Core.Kinds
import ReWire.Core.Syntax
import ReWire.FrontEnd.Error
import ReWire.FrontEnd.Renamer (Renamer, FQName(..), rename, extend, exclude, finger)
import ReWire.Scoping (mkId, Id, fv)
import ReWire.SYB (runQ, query')

import Control.Applicative ((<*>))
import Control.Monad (foldM)
import Data.Foldable (foldl')
import Data.Functor ((<$>))
import Data.List (nub)

import Language.Haskell.Exts hiding (loc, name, binds, op, Kind)

-- | Translate a Haskell module into the ReWire abstract syntax.
translate :: (Functor m, Monad m) => Renamer -> Module -> ParseError m (RWCProgram, [ModuleName], [Export])
translate rn (Module loc m _pragmas _ exps imps (reverse -> ds)) = do
      let rn' = extend (zip (getGlobs ds) $ map (deSigil . FQName m) $ getGlobs ds) rn
      tyDefs <- foldM (transData rn') [] ds
      tySigs <- foldM (transTySig rn') [] ds
      inls   <- foldM transInlineSig [] ds
      fnDefs <- foldM (transDef rn' tySigs inls) [] ds
      exps'  <- maybe (return $ map (Export . FQName m) $ getGlobs ds) (foldM (transExport loc ds rn') []) exps
      return (RWCProgram tyDefs fnDefs, map importModule imps, exps')
      where getGlobs :: [Decl] -> [Name]
            getGlobs = foldl' (flip getGlobs') []
                  where getGlobs' :: Decl -> [Name] -> [Name]
                        getGlobs' = \case
                              DataDecl _ _ _ n _ cons _     -> ((n : foldr getCtors [] cons) ++)
                              PatBind _ (PVar n) _ _        -> (n :)
                              _                             -> id
                        getCtors :: QualConDecl -> [Name] -> [Name]
                        getCtors = \case
                              QualConDecl _ _ _ (ConDecl n _) -> (n :)
                              _                               -> id

data Export = Export FQName | ExportWith FQName [FQName] | ExportAll FQName | ExportMod ModuleName

transExport :: Monad m => SrcLoc -> [Decl] -> Renamer -> [Export] -> ExportSpec -> ParseError m [Export]
transExport loc ds rn exps = \case
      EVar x@(UnQual (Ident "main")) -> if finger rn x
            then return $ Export (rename rn x) : exps
            else return exps
      EVar x                         -> if finger rn x
            then return $ Export (rename rn x) : exps
            else pFailAt loc $ "unknown name in export list: " ++ prettyPrint x
      EAbs _ x                       -> if finger rn x
            then return $ Export (rename rn x) : exps
            else pFailAt loc $ "unknown name in export list: " ++ prettyPrint x
      EThingAll x                    -> if finger rn x
            then return $ lookupCtors rn (rename rn x) ds : exps
            else pFailAt loc $ "unknown name in export list: " ++ prettyPrint x
      EThingWith x cs                -> if and $ finger rn x : map (finger rn . unwrap) cs
            then return $ ExportWith (rename rn x) (map (rename rn . unwrap) cs) : exps
            else pFailAt loc $ "unknown name in export list: " ++ prettyPrint x
      EModuleContents m              -> return $ ExportMod m : exps
      where unwrap :: CName -> Name
            unwrap (VarName x) = x
            unwrap (ConName x) = x

            lookupCtors :: Renamer -> FQName -> [Decl] -> Export
            lookupCtors rn x = foldl' lookupCtors' (ExportAll x) . map (toExport rn) . filter isDataDecl

            isDataDecl :: Decl -> Bool
            isDataDecl DataDecl{} = True
            isDataDecl _          = False

            toExport :: Renamer -> Decl -> Export
            toExport rn (DataDecl _ _ _ x _ cs _) = ExportWith (rename rn x) (map (toExport' rn) cs)
            toExport _ _                          = error "should be impossible"

            toExport' :: Renamer -> QualConDecl -> FQName
            toExport' rn (QualConDecl _ _ _ (ConDecl x _)) = rename rn x
            toExport' _  _                                 = error "unsupported syntax (shouldn't ever make it here)"

            lookupCtors' :: Export -> Export -> Export
            lookupCtors' (ExportWith x cs) _ = ExportWith x cs
            lookupCtors' (ExportAll x) (ExportWith x' cs)
                  | x == x'                  = ExportWith x' cs
                  | otherwise                = ExportAll x
            lookupCtors' e _                 = e

transData :: (Functor m, Monad m) => Renamer -> [RWCData] -> Decl -> ParseError m [RWCData]
transData rn datas = \case
      DataDecl loc _ _ x tyVars cons _deriving -> do
            tyVars' <- mapM (transTyVar loc) tyVars
            cons' <- mapM (transCon rn) cons
            return $ RWCData (TyConId $ rename rn x) tyVars' kblank cons' : datas
      _                                        -> return datas

transTySig :: (Functor m, Monad m) => Renamer -> [(Name, RWCTy)] -> Decl -> ParseError m [(Name, RWCTy)]
transTySig rn sigs = \case
      TypeSig loc names t -> do
            t' <- transTy loc rn [] t
            return $ zip names (repeat t') ++ sigs
      _                   -> return sigs

-- I guess this doesn't need to be in the monad, really, but whatever...  --adam
-- Not sure what the boolean field means here, so we ignore it!  --adam
transInlineSig :: Monad m => [Name] -> Decl -> ParseError m [Name]
transInlineSig inls = \case
      InlineSig _ _ AlwaysActive (Qual _ x) -> return $ x : inls
      InlineSig _ _ AlwaysActive (UnQual x) -> return $ x : inls
      _                                     -> return inls

transDef :: (Functor m, Monad m) => Renamer -> [(Name, RWCTy)] -> [Name] -> [RWCDefn] -> Decl -> ParseError m [RWCDefn]
transDef rn tys inls defs = \case
      PatBind loc (PVar x) (UnGuardedRhs e) Nothing -> case lookup x tys of
            Just t -> (:defs) . RWCDefn (mkId $ rename rn x) (nub (fv t) :-> t) (x `elem` inls) <$> transExp loc rn e
            _      -> pFailAt loc $ "no type signature for " ++ prettyPrint x
      _                                               -> return defs

transTyVar :: Monad m => SrcLoc -> TyVarBind -> ParseError m (Id RWCTy)
transTyVar loc = \case
      UnkindedVar (Ident x) -> return $ mkId x
      tv                    -> pFailAt loc $ "unsupported type syntax: " ++ prettyPrint tv

transCon :: (Functor m, Monad m) => Renamer -> QualConDecl -> ParseError m RWCDataCon
transCon rn = \case
      QualConDecl loc [] _ (ConDecl x tys) -> (RWCDataCon $ DataConId $ rename rn x) <$> mapM (transTy loc rn []) tys
      d@(QualConDecl loc _ _ _)            -> pFailAt loc $ "unsupported ctor syntax: " ++ prettyPrint d

transTy :: (Functor m, Monad m) => SrcLoc -> Renamer -> [Name] -> Type -> ParseError m RWCTy
transTy loc rn ms = \case
      TyForall Nothing cs t    -> do
           ms' <- mapM (getNad loc) cs
           transTy loc rn (ms ++ ms') t
      TyFun a b                -> mkArrow <$> transTy loc rn ms a <*> transTy loc rn ms b
      TyApp a b | isMonad ms a -> RWCTyComp <$> transTy loc rn ms a <*> transTy loc rn ms b
                | otherwise    -> RWCTyApp <$> transTy loc rn ms a <*> transTy loc rn ms b
      TyCon x                  -> return $ RWCTyCon (TyConId $ rename rn x)
      TyVar x                  -> return $ RWCTyVar (mkId $ prettyPrint x)
      t                        -> pFailAt loc $ "unsupported type syntax: " ++ prettyPrint t

getNad :: Monad m => SrcLoc -> Asst -> ParseError m Name
getNad loc = \case
      ClassA (UnQual (Ident "Monad")) [TyVar x] -> return x
      a                                         -> pFailAt loc $ "unsupported typeclass constraint: " ++ prettyPrint a

isMonad :: [Name] -> Type -> Bool
isMonad ms = \case
      TyApp (TyApp (TyApp (TyCon (UnQual (Ident "ReT"))) _) _) t -> isMonad ms t
      TyApp (TyApp (TyCon (UnQual (Ident "StT"))) _) t           -> isMonad ms t
      TyCon (UnQual (Ident "I"))                                 -> True
      TyVar x                                                    -> x `elem` ms
      _                                                          -> False

kblank :: Kind
kblank = Kstar

tblank :: RWCTy
tblank = RWCTyCon (TyConId "_")

transExp :: (Functor m, Monad m) => SrcLoc -> Renamer -> Exp -> ParseError m RWCExp
transExp loc rn = \case
      App (App (Var (UnQual (Ident "nativeVhdl"))) (Lit (String f))) e
                             -> RWCNativeVHDL f <$> transExp loc rn e
      App e1 e2              -> RWCApp <$> transExp loc rn e1 <*> transExp loc rn e2
      Lambda loc' [PVar x] e -> RWCLam (mkId $ prettyPrint x) tblank <$> transExp loc' (exclude [x] rn) e
      Var x                  -> return $ RWCVar (mkId $ rename rn x) tblank
      Con x                  -> return $ RWCCon (DataConId $ rename rn x) tblank
      Lit lit                -> RWCLiteral <$> transLit loc lit
      Case e alts            -> RWCCase <$> transExp loc rn e <*> mapM (transAlt rn) alts
      e                      -> pFailAt loc $ "unsupported expression syntax: " ++ prettyPrint e

transLit :: Monad m => SrcLoc -> Literal -> ParseError m RWCLit
transLit loc = \case
      Int i  -> return $ RWCLitInteger i
      Frac d -> return $ RWCLitFloat (fromRational d)
      Char c -> return $ RWCLitChar c
      lit    -> pFailAt loc $ "unsupported syntax for a literal: " ++ prettyPrint lit

transAlt :: (Functor m, Monad m) => Renamer -> Alt -> ParseError m RWCAlt
transAlt rn = \case
      Alt loc p (UnGuardedRhs e) Nothing -> RWCAlt <$> transPat loc rn p <*> transExp loc (exclude (getVars p) rn) e
      a@(Alt loc _ _ _)                  -> pFailAt loc $ "unsupported syntax: " ++ prettyPrint a
      where getVars :: Pat -> [Name]
            getVars = runQ $ query' $ \case
                  PVar x -> [x]
                  _      -> []

transPat :: (Functor m, Monad m) => SrcLoc -> Renamer -> Pat -> ParseError m RWCPat
transPat loc rn = \case
      PApp x ps          -> RWCPatCon (DataConId $ rename rn x) <$> mapM (transPat loc rn) ps
      PLit Signless lit  -> RWCPatLiteral <$> transLit loc lit
      PVar x             -> return $ RWCPatVar (mkId $ prettyPrint x) tblank
      p                  -> pFailAt loc $ "unsupported syntax in a pattern: " ++ prettyPrint p

deSigil :: FQName -> FQName
deSigil (FQName m (Ident ('#':cs))) = FQName m (Ident cs)
deSigil x                           = x
