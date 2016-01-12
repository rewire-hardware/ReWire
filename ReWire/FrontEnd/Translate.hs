{-# LANGUAGE LambdaCase, ViewPatterns #-}
module ReWire.FrontEnd.Translate
      ( translate
      , translateExps
      , Export(..)
      ) where

import ReWire.Core.Kinds
import ReWire.Core.Syntax
import ReWire.FrontEnd.Error
import ReWire.FrontEnd.Rename
import ReWire.Scoping
import ReWire.SYB

import Control.Applicative ((<*>))
import Control.Monad (foldM)
import Data.Foldable (foldl')
import Data.Functor ((<$>))
import Data.List (nub)
import Language.Haskell.Exts.Annotated.Simplify (sName, sDeclHead, sQName, sModuleName)
import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified Language.Haskell.Exts.Syntax as S

import Language.Haskell.Exts.Annotated.Syntax hiding (Kind)

-- | Translate a Haskell module into the ReWire abstract syntax.
translate :: (Functor m, Monad m) => Renamer -> Module Annote -> ParseError m RWCProgram
translate rn (Module _ (Just (ModuleHead _ m _ _)) _ _ (reverse -> ds)) = do
      let rn' = extendWithGlobs (sModuleName m) ds rn
      tyDefs <- foldM (transData rn') [] ds
      tySigs <- foldM (transTySig rn') [] ds
      inls   <- foldM transInlineSig [] ds
      fnDefs <- foldM (transDef rn' tySigs inls) [] ds
      return $ RWCProgram tyDefs fnDefs
translate _ m = pFailAt (ann m) "unsupported module syntax"

data Export = Export FQName | ExportWith FQName [FQName] | ExportAll FQName | ExportMod S.ModuleName
      deriving Show

translateExps :: Monad m => Renamer -> Module Annote -> ParseError m [Export]
translateExps rn (Module _ (Just (ModuleHead _ m _ exps)) _ _ ds) = do
      let rn' = extendWithGlobs (sModuleName m) ds rn
      maybe (return $ getGlobExps rn' ds) (\ (ExportSpecList _ exps') -> foldM (transExport rn' ds) [] exps') exps
      where getGlobExps :: Renamer -> [Decl Annote] -> [Export]
            getGlobExps rn = foldl' (flip $ getGlobExps' rn) []
            getGlobExps' :: Renamer -> Decl Annote -> [Export] -> [Export]
            getGlobExps' rn = \ case
                  DataDecl _ _ _ hd cs _   -> (:) $ ExportWith (rename TypeNS rn $ fst $ sDeclHead hd) $ map (rename ValueNS rn . getCtor) cs
                  PatBind _ (PVar _ n) _ _ -> (:) $ Export $ rename ValueNS rn (sName n)
                  _                        -> id
translateExps _ m = pFailAt (ann m) "unsupported module syntax"

transExport :: Monad m => Renamer -> [Decl Annote] -> [Export] -> ExportSpec Annote -> ParseError m [Export]
transExport rn ds exps = \ case
      EVar l (sQName -> x)                 -> if finger ValueNS rn x
            then return $ Export (rename ValueNS rn x) : exps
            else pFailAt l "unknown name in export list"
      EAbs l _ (sQName -> x)               -> if finger TypeNS rn x
            then return $ Export (rename TypeNS rn x) : exps
            else pFailAt l "unknown name in export list"
      EThingAll l (sQName -> x)            -> if finger TypeNS rn x
            then return $ lookupCtors rn x ds : exps
            else pFailAt l "unknown name in export list"
      EThingWith l (sQName -> x) cs        -> if and $ finger TypeNS rn x : map (finger ValueNS rn . unwrap) cs
            then return $ ExportWith (rename TypeNS rn x) (map (rename ValueNS rn . unwrap) cs) : exps
            else pFailAt l "unknown name in export list"
      EModuleContents _ (sModuleName -> m) -> return $ ExportMod m : exps
      where unwrap :: CName Annote -> S.Name
            unwrap (VarName _ x) = sName x
            unwrap (ConName _ x) = sName x

            lookupCtors :: Renamer -> S.QName -> [Decl Annote] -> Export
            lookupCtors rn x = foldl' lookupCtors' (ExportAll $ rename TypeNS rn x) . map (toExport rn) . filter isDataDecl

            isDataDecl :: Decl Annote -> Bool
            isDataDecl DataDecl{} = True
            isDataDecl _          = False

            toExport :: Renamer -> Decl Annote -> Export
            toExport rn (DataDecl _ _ _ hd cs _) = ExportWith (rename TypeNS rn $ fst $ sDeclHead hd) $ map (toExport' rn) cs
            toExport _ _                         = undefined

            toExport' :: Renamer -> QualConDecl Annote -> FQName
            toExport' rn (QualConDecl _ _ _ (ConDecl _ (sName -> x) _)) = rename ValueNS rn x
            toExport' _  _                                   = undefined

            lookupCtors' :: Export -> Export -> Export
            lookupCtors' (ExportWith x cs) _ = ExportWith x cs
            lookupCtors' (ExportAll x) (ExportWith x' cs)
                  | x == x'                  = ExportWith x' cs
                  | otherwise                = ExportAll x
            lookupCtors' e _                 = e

extendWithGlobs :: S.ModuleName -> [Decl Annote] -> Renamer -> Renamer
extendWithGlobs m ds rn = extend ValueNS (zip (getGlobValDefs ds) $ map (FQName m) $ getGlobValDefs ds)
                        $ extend TypeNS  (zip (getGlobTyDefs ds)  $ map (FQName m) $ getGlobTyDefs ds) rn
      where getGlobValDefs :: [Decl Annote] -> [S.Name]
            getGlobValDefs = foldl' (flip getGlobs') []
                  where getGlobs' :: Decl Annote -> [S.Name] -> [S.Name]
                        getGlobs' = \ case
                              DataDecl _ _ _ _ cons _  -> (++) $ map getCtor cons
                              PatBind _ (PVar _ n) _ _ -> (:) $ sName n
                              _                        -> id
            getGlobTyDefs :: [Decl Annote] -> [S.Name]
            getGlobTyDefs = foldl' (flip getGlobs') []
                  where getGlobs' :: Decl Annote -> [S.Name] -> [S.Name]
                        getGlobs' = \ case
                              DataDecl _ _ _ hd _ _ -> (:) $ fst $ sDeclHead hd
                              _                     -> id
getCtor :: QualConDecl Annote -> S.Name
getCtor = \ case
      QualConDecl _ _ _ (ConDecl _ n _) -> sName n
      QualConDecl _ _ _ (RecDecl _ n _) -> sName n
      _                                 -> error "impossible"

transData :: (Functor m, Monad m) => Renamer -> [RWCData] -> Decl Annote -> ParseError m [RWCData]
transData rn datas = \ case
      DataDecl l _ _ (sDeclHead -> hd) cons _ -> do
            tyVars' <- mapM (transTyVar l) $ snd hd
            cons' <- mapM (transCon rn) cons
            return $ RWCData l (TyConId $ rename TypeNS rn $ fst hd) tyVars' kblank cons' : datas
      _                                       -> return datas

transTySig :: (Functor m, Monad m) => Renamer -> [(S.Name, RWCTy)] -> Decl Annote -> ParseError m [(S.Name, RWCTy)]
transTySig rn sigs = \ case
      TypeSig _ names t -> do
            t' <- transTy rn [] t
            return $ zip (map sName names) (repeat t') ++ sigs
      _                 -> return sigs

-- I guess this doesn't need to be in the monad, really, but whatever...  --adam
-- Not sure what the boolean field means here, so we ignore it!  --adam
transInlineSig :: Monad m => [S.Name] -> Decl Annote -> ParseError m [S.Name]
transInlineSig inls = \ case
      InlineSig _ _ Nothing (Qual _ _ x) -> return $ sName x : inls
      InlineSig _ _ Nothing (UnQual _ x) -> return $ sName x : inls
      _                                  -> return inls

transDef :: (Functor m, Monad m) => Renamer -> [(S.Name, RWCTy)] -> [S.Name] -> [RWCDefn] -> Decl Annote -> ParseError m [RWCDefn]
transDef rn tys inls defs = \ case
      PatBind l (PVar _ (sName -> x)) (UnGuardedRhs _ e) Nothing -> case lookup x tys of
            Just t -> (:defs) . RWCDefn l (mkId $ rename ValueNS rn x) (nub (fv t) :-> t) (x `elem` inls) <$> transExp rn e
            _      -> pFailAt l "no type signature for"
      _                                             -> return defs

transTyVar :: Monad m => Annote -> S.TyVarBind -> ParseError m (Id RWCTy)
transTyVar l = \ case
      S.UnkindedVar (S.Ident x) -> return $ mkId x
      _                         -> pFailAt l "unsupported type syntax"

transCon :: (Functor m, Monad m) => Renamer -> QualConDecl Annote -> ParseError m RWCDataCon
transCon rn = \ case
      QualConDecl l Nothing _ (ConDecl _ x tys) -> RWCDataCon l (DataConId $ rename ValueNS rn $ sName x) <$> mapM (transTy rn []) tys
      d                                         -> pFailAt (ann d) "unsupported ctor syntax"

transTy :: (Functor m, Monad m) => Renamer -> [S.Name] -> Type Annote -> ParseError m RWCTy
transTy rn ms = \ case
      TyForall _ Nothing (Just (CxTuple _ cs)) t   -> do
           ms' <- mapM getNad cs
           transTy rn (ms ++ ms') t
      TyApp l a b | isMonad ms a -> RWCTyComp l <$> transTy rn ms a <*> transTy rn ms b
                  | otherwise    -> RWCTyApp l <$> transTy rn ms a <*> transTy rn ms b
      TyCon l x                  -> return $ RWCTyCon l (TyConId $ rename TypeNS rn $ sQName x)
      TyVar l x                  -> return $ RWCTyVar l (mkId $ prettyPrint x)
      t                          -> pFailAt (ann t) "unsupported type syntax"

getNad :: Monad m => Asst Annote -> ParseError m S.Name
getNad = \ case
      ClassA _ (UnQual _ (Ident _ "Monad")) [TyVar _ x] -> return $ sName x
      a                                                 -> pFailAt (ann a) "unsupported typeclass constraint"

isMonad :: [S.Name] -> Type Annote -> Bool
isMonad ms = \ case
      TyApp _ (TyApp _ (TyApp _ (TyCon _ (UnQual _ (Ident _ "ReT"))) _) _) t -> isMonad ms t
      TyApp _ (TyApp _ (TyCon _ (UnQual _ (Ident _ "StT"))) _) t             -> isMonad ms t
      TyCon _ (UnQual _ (Ident _ "I"))                                       -> True
      TyVar _ (sName -> x)                                                   -> x `elem` ms
      _                                                                      -> False

kblank :: Kind
kblank = Kstar

tblank :: RWCTy
tblank = RWCTyCon noAnn (TyConId "_")

transExp :: (Functor m, Monad m) => Renamer -> Exp Annote -> ParseError m RWCExp
transExp rn = \ case
      App l (App _ (Var _ (UnQual _ (Ident _ "nativeVhdl"))) (Lit _ (String _ f _))) e
                            -> RWCNativeVHDL l f <$> transExp rn e
      App l e1 e2           -> RWCApp l <$> transExp rn e1 <*> transExp rn e2
      Lambda l [PVar _ x] e -> RWCLam l (mkId $ prettyPrint x) tblank <$> transExp (exclude ValueNS [sName x] rn) e
      Var l x               -> return $ RWCVar l (mkId $ rename ValueNS rn $ sQName x) tblank
      Con l x               -> return $ RWCCon l (DataConId $ rename ValueNS rn $ sQName x) tblank
      Lit l lit             -> RWCLiteral l <$> transLit lit
      Case l e alts         -> RWCCase l <$> transExp rn e <*> mapM (transAlt rn) alts
      e                     -> pFailAt (ann e) "unsupported expression syntax"

transLit :: Monad m => Literal Annote -> ParseError m RWCLit
transLit = \ case
      Int _ i _  -> return $ RWCLitInteger i
      Frac _ d _ -> return $ RWCLitFloat $ fromRational d
      Char _ c _ -> return $ RWCLitChar c
      lit        -> pFailAt (ann lit) "unsupported syntax for a literal"

transAlt :: (Functor m, Monad m) => Renamer -> Alt Annote -> ParseError m RWCAlt
transAlt rn = \ case
      Alt l p (UnGuardedRhs _ e) Nothing -> RWCAlt l <$> transPat rn p <*> transExp (exclude ValueNS (getVars p) rn) e
      a                                  -> pFailAt (ann a) "unsupported syntax"
      where getVars :: Pat Annote -> [S.Name]
            getVars = runPureQ $ (||? QEmpty) $ \ p -> case p :: Pat Annote of
                  PVar _ x -> [sName x]
                  _        -> []

transPat :: (Functor m, Monad m) => Renamer -> Pat Annote -> ParseError m RWCPat
transPat rn = \ case
      PApp l x ps             -> RWCPatCon l (DataConId $ rename ValueNS rn $ sQName x) <$> mapM (transPat rn) ps
      PLit l (Signless _) lit -> RWCPatLiteral l <$> transLit lit
      PVar l x                -> return $ RWCPatVar l (mkId $ prettyPrint x) tblank
      p                       -> pFailAt (ann p) "unsupported syntax in a pattern"
