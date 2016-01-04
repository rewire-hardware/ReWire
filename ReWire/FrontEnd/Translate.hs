{-# LANGUAGE LambdaCase, ViewPatterns #-}
module ReWire.FrontEnd.Translate
      ( translate
      , translateExps
      , Export(..)
      ) where

import ReWire.Core.Kinds
import ReWire.Core.Syntax
import ReWire.FrontEnd.Error
import ReWire.FrontEnd.Renamer (Renamer, FQName(..), Namespace(..), rename, extend, exclude, finger)
import ReWire.Scoping (mkId, Id, fv)
import ReWire.SYB (runQ, query')

--import Control.Applicative ((<*>))
import Control.Monad (foldM)
import Data.Foldable (foldl')
--import Data.Functor ((<$>))
import Data.List (nub)

import Language.Haskell.Exts hiding (loc, name, binds, op, Kind)

-- | Translate a Haskell module into the ReWire abstract syntax.
translate :: (Functor m, Monad m) => Renamer -> Module -> ParseError m RWCProgram
translate rn (Module loc m _ _ _ _ (reverse -> ds)) = do
      let rn' = extendWithGlobs m ds rn
      setLoc loc
      tyDefs <- foldM (transData rn') [] ds
      tySigs <- foldM (transTySig rn') [] ds
      inls   <- foldM transInlineSig [] ds
      fnDefs <- foldM (transDef rn' tySigs inls) [] ds
      return $ RWCProgram tyDefs fnDefs

data Export = Export FQName | ExportWith FQName [FQName] | ExportAll FQName | ExportMod ModuleName
      deriving Show

translateExps :: Monad m => Renamer -> Module -> ParseError m [Export]
translateExps rn (Module loc m _ _ exps _ ds) = do
      let rn' = extendWithGlobs m ds rn
      setLoc loc
      maybe (return $ getGlobExps rn' ds) (foldM (transExport rn' ds) []) exps
      where getGlobExps :: Renamer -> [Decl] -> [Export]
            getGlobExps rn = foldl' (flip getGlobExps') []
                  where getGlobExps' :: Decl -> [Export] -> [Export]
                        getGlobExps' = \case
                              DataDecl _ _ _ n _ cons _     -> (:) $ ExportWith (rename TypeNS rn n) $ map (rename ValueNS rn . getCtor) cons
                              PatBind _ (PVar n) _ _        -> (:) $ Export $ rename ValueNS rn n
                              _                             -> id


transExport :: Monad m => Renamer -> [Decl] -> [Export] -> ExportSpec -> ParseError m [Export]
transExport rn ds exps = \case
      EVar x@(UnQual (Ident "main")) -> if finger ValueNS rn x
            then return $ Export (rename ValueNS rn x) : exps
            else return exps
      EVar x                         -> if finger ValueNS rn x
            then return $ Export (rename ValueNS rn x) : exps
            else pFail $ "unknown name in export list: " ++ prettyPrint x
      EAbs _ x                       -> if finger TypeNS rn x
            then return $ Export (rename TypeNS rn x) : exps
            else pFail $ "unknown name in export list: " ++ prettyPrint x
      EThingAll x                    -> if finger TypeNS rn x
            then return $ lookupCtors rn x ds : exps
            else pFail $ "unknown name in export list: " ++ prettyPrint x
      EThingWith x cs                -> if and $ finger TypeNS rn x : map (finger ValueNS rn . unwrap) cs
            then return $ ExportWith (rename TypeNS rn x) (map (rename ValueNS rn . unwrap) cs) : exps
            else pFail $ "unknown name in export list: " ++ prettyPrint x
      EModuleContents m              -> return $ ExportMod m : exps
      where unwrap :: CName -> Name
            unwrap (VarName x) = x
            unwrap (ConName x) = x

            lookupCtors :: Renamer -> QName -> [Decl] -> Export
            lookupCtors rn x = foldl' lookupCtors' (ExportAll $ rename TypeNS rn x) . map (toExport rn) . filter isDataDecl

            isDataDecl :: Decl -> Bool
            isDataDecl DataDecl{} = True
            isDataDecl _          = False

            toExport :: Renamer -> Decl -> Export
            toExport rn (DataDecl _ _ _ x _ cs _) = ExportWith (rename TypeNS rn x) $ map (toExport' rn) cs
            toExport _ _                          = undefined

            toExport' :: Renamer -> QualConDecl -> FQName
            toExport' rn (QualConDecl _ _ _ (ConDecl x _)) = rename ValueNS rn x
            toExport' _  _                                 = undefined

            lookupCtors' :: Export -> Export -> Export
            lookupCtors' (ExportWith x cs) _ = ExportWith x cs
            lookupCtors' (ExportAll x) (ExportWith x' cs)
                  | x == x'                  = ExportWith x' cs
                  | otherwise                = ExportAll x
            lookupCtors' e _                 = e


extendWithGlobs :: ModuleName -> [Decl] -> Renamer -> Renamer
extendWithGlobs m ds rn = extend ValueNS (zip (getGlobValDefs ds) $ map (FQName m) $ getGlobValDefs ds)
                        $ extend TypeNS  (zip (getGlobTyDefs ds)  $ map (FQName m) $ getGlobTyDefs ds) rn
      where getGlobValDefs :: [Decl] -> [Name]
            getGlobValDefs = foldl' (flip getGlobs') []
                  where getGlobs' :: Decl -> [Name] -> [Name]
                        getGlobs' = \case
                              DataDecl _ _ _ _ _ cons _     -> (++) $ map getCtor cons
                              PatBind _ (PVar n) _ _        -> (:) n
                              _                             -> id
            getGlobTyDefs :: [Decl] -> [Name]
            getGlobTyDefs = foldl' (flip getGlobs') []
                  where getGlobs' :: Decl -> [Name] -> [Name]
                        getGlobs' = \case
                              DataDecl _ _ _ n _ _ _     -> (:) n
                              _                             -> id
getCtor :: QualConDecl -> Name
getCtor = \case
      QualConDecl _ _ _ (ConDecl n _)        -> n
      QualConDecl _ _ _ (RecDecl n _)        -> n
      QualConDecl _ _ _ (InfixConDecl _ n _) -> n

transData :: (Functor m, Monad m) => Renamer -> [RWCData] -> Decl -> ParseError m [RWCData]
transData rn datas = \case
      DataDecl loc _ _ x tyVars cons _deriving -> do
            setLoc loc
            tyVars' <- mapM transTyVar tyVars
            cons' <- mapM (transCon rn) cons
            return $ RWCData (TyConId $ rename TypeNS rn x) tyVars' kblank cons' : datas
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
      PatBind loc (PVar x) (UnGuardedRhs e) Nothing -> setLoc loc >> case lookup x tys of
            Just t -> (:defs) . RWCDefn (mkId $ rename ValueNS rn x) (nub (fv t) :-> t) (x `elem` inls) <$> transExp rn e
            _      -> pFail $ "no type signature for " ++ prettyPrint x
      _                                             -> return defs

transTyVar :: Monad m => TyVarBind -> ParseError m (Id RWCTy)
transTyVar = \case
      UnkindedVar (Ident x) -> return $ mkId x
      tv                    -> pFail $ "unsupported type syntax: " ++ prettyPrint tv

transCon :: (Functor m, Monad m) => Renamer -> QualConDecl -> ParseError m RWCDataCon
transCon rn = \case
      QualConDecl loc [] _ (ConDecl x tys) -> (RWCDataCon $ DataConId $ rename ValueNS rn x) <$> mapM (transTy loc rn []) tys
      d@(QualConDecl loc _ _ _)            -> pFailAt loc $ "unsupported ctor syntax: " ++ prettyPrint d

transTy :: (Functor m, Monad m) => SrcLoc -> Renamer -> [Name] -> Type -> ParseError m RWCTy
transTy loc rn ms = \case
      TyForall Nothing cs t    -> do
           ms' <- mapM (getNad loc) cs
           transTy loc rn (ms ++ ms') t
      TyFun a b                -> mkArrow <$> transTy loc rn ms a <*> transTy loc rn ms b
      TyApp a b | isMonad ms a -> RWCTyComp <$> transTy loc rn ms a <*> transTy loc rn ms b
                | otherwise    -> RWCTyApp <$> transTy loc rn ms a <*> transTy loc rn ms b
      TyCon x                  -> return $ RWCTyCon (TyConId $ rename TypeNS rn x)
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

transExp :: (Functor m, Monad m) => Renamer -> Exp -> ParseError m RWCExp
transExp rn = \case
      App (App (Var (UnQual (Ident "nativeVhdl"))) (Lit (String f))) e
                             -> RWCNativeVHDL f <$> transExp rn e
      App e1 e2              -> RWCApp <$> transExp rn e1 <*> transExp rn e2
      Lambda loc [PVar x] e  -> setLoc loc >> RWCLam (mkId $ prettyPrint x) tblank <$> transExp (exclude ValueNS [x] rn) e
      Var x                  -> return $ RWCVar (mkId $ rename ValueNS rn x) tblank
      Con x                  -> return $ RWCCon (DataConId $ rename ValueNS rn x) tblank
      Lit lit                -> RWCLiteral <$> transLit lit
      Case e alts            -> RWCCase <$> transExp rn e <*> mapM (transAlt rn) alts
      e                      -> pFail $ "unsupported expression syntax: " ++ prettyPrint e

transLit :: Monad m => Literal -> ParseError m RWCLit
transLit = \case
      Int i  -> return $ RWCLitInteger i
      Frac d -> return $ RWCLitFloat (fromRational d)
      Char c -> return $ RWCLitChar c
      lit    -> pFail $ "unsupported syntax for a literal: " ++ prettyPrint lit

transAlt :: (Functor m, Monad m) => Renamer -> Alt -> ParseError m RWCAlt
transAlt rn = \case
      Alt loc p (UnGuardedRhs e) Nothing -> setLoc loc >> RWCAlt <$> transPat rn p <*> transExp (exclude ValueNS (getVars p) rn) e
      a@(Alt loc _ _ _)                  -> pFailAt loc $ "unsupported syntax: " ++ prettyPrint a
      where getVars :: Pat -> [Name]
            getVars = runQ $ query' $ \case
                  PVar x -> [x]
                  _      -> []

transPat :: (Functor m, Monad m) => Renamer -> Pat -> ParseError m RWCPat
transPat rn = \case
      PApp x ps          -> RWCPatCon (DataConId $ rename ValueNS rn x) <$> mapM (transPat rn) ps
      PLit Signless lit  -> RWCPatLiteral <$> transLit lit
      PVar x             -> return $ RWCPatVar (mkId $ prettyPrint x) tblank
      p                  -> pFail $ "unsupported syntax in a pattern: " ++ prettyPrint p
