{-# LANGUAGE LambdaCase, ViewPatterns #-}
module ReWire.FrontEnd.Translate (translate) where

import ReWire.Core.Kinds
import ReWire.Core.Syntax hiding (ann)
import ReWire.Error
import ReWire.FrontEnd.Fixity
import ReWire.FrontEnd.Rename
import ReWire.Scoping (Id, IdSort, fv, mkId)
import ReWire.SYB

import Control.Applicative (Applicative, (<*>))
import Control.Monad (foldM, void)
import Data.Foldable (foldl')
import Data.Functor ((<$>))
import Data.List (nub)
import Data.Monoid (mempty, (<>))
import Language.Haskell.Exts.Annotated.Fixity (Fixity (..))
import Language.Haskell.Exts.Annotated.Simplify (sName, sDeclHead, sQName, sModuleName)

import qualified Data.Set                     as Set
import qualified Language.Haskell.Exts.Syntax as S

import Language.Haskell.Exts.Annotated.Syntax hiding (Kind)

-- | An intermediate form for exports. TODO(chathhorn): get rid of it.
data Export = Export FQName
            | ExportWith FQName [FQName]
            | ExportAll FQName
            | ExportMod S.ModuleName
            | ExportFixity S.Assoc Int S.Name
      deriving Show

mkUId :: IdSort b => S.Name -> Id b
mkUId (S.Ident n)  = mkId n
mkUId (S.Symbol n) = mkId n

-- | Translate a Haskell module into the ReWire abstract syntax.
translate :: (Applicative m, Functor m, SyntaxError m) => Renamer -> Module Annote -> m (RWCProgram, Exports)
translate rn (Module _ (Just (ModuleHead _ m _ exps)) _ _ (reverse -> ds)) = do
      let rn' = extendWithGlobs (sModuleName m) ds rn
      tyDefs <- foldM (transData rn') [] ds
      tySigs <- foldM (transTySig rn') [] ds
      inls   <- foldM transInlineSig [] ds
      fnDefs <- foldM (transDef rn' tySigs inls) [] ds
      exps'  <- maybe (return $ getGlobExps rn' ds) (\ (ExportSpecList _ exps') -> foldM (transExport rn' ds) [] exps') exps
      return (RWCProgram tyDefs fnDefs, resolveExports rn exps')
      where getGlobExps :: Renamer -> [Decl Annote] -> [Export]
            getGlobExps rn ds = getExportFixities ds ++ foldr (getGlobExps' rn) [] ds

            getGlobExps' :: Renamer -> Decl Annote -> [Export] -> [Export]
            getGlobExps' rn = \ case
                  DataDecl _ _ _ hd cs _   -> (:) $ ExportWith (rename Type rn $ fst $ sDeclHead hd) $ map (rename Value rn . getCtor) cs
                  PatBind _ (PVar _ n) _ _ -> (:) $ Export $ rename Value rn $ sName n
                  _                        -> id
translate _ m = failAt (ann m) "Unsupported module syntax"

resolveExports :: Renamer -> [Export] -> Exports
resolveExports rn = foldr (resolveExport rn) mempty

resolveExport :: Renamer -> Export -> Exports -> Exports
resolveExport rn = \ case
      Export x               -> expValue x
      ExportAll x            -> expType x $ getCtors (qnamish x) $ allExports rn
      ExportWith x cs        -> expType x (Set.fromList cs)
      ExportMod m            -> (<> getExports m rn)
      ExportFixity asc lvl x -> expFixity asc lvl x

getExportFixities :: [Decl Annote] -> [Export]
getExportFixities = map toExportFixity . getFixities
      where toExportFixity :: Fixity -> Export
            toExportFixity (Fixity asc lvl (S.UnQual n)) = ExportFixity asc lvl n
            toExportFixity _                             = undefined

transExport :: SyntaxError m => Renamer -> [Decl Annote] -> [Export] -> ExportSpec Annote -> m [Export]
transExport rn ds exps = \ case
      EVar l x                             ->
            if finger Value rn (sQName x)
            then return $ Export (rename Value rn $ sQName x) : fixities (qnamish x) ++ exps
            else failAt l "Unknown variable name in export list"
      EAbs l _ (sQName -> x)               ->
            if finger Type rn x
            then return $ Export (rename Type rn x) : exps
            else failAt l "Unknown class or type name in export list"
      EThingAll l (sQName -> x)            ->
            if finger Type rn x
            then return $ lookupCtors x : concatMap fixities (getCtors $ lookupCtors x) ++ exps
            else failAt l "Unknown class or type name in export list"
      EThingWith l (sQName -> x) cs        ->
            if and $ finger Type rn x : map (finger Value rn . unwrap) cs
            then return $ ExportWith (rename Type rn x) (map (rename Value rn . unwrap) cs) : concatMap (fixities . unwrap) cs ++ exps
            else failAt l "Unknown class or type name in export list"
      EModuleContents _ (sModuleName -> m) ->
            return $ ExportMod m : exps
      where unwrap :: CName Annote -> S.Name
            unwrap (VarName _ x) = sName x
            unwrap (ConName _ x) = sName x

            lookupCtors :: S.QName -> Export
            lookupCtors x = foldl' lookupCtors' (ExportAll $ rename Type rn x) (map (toExport rn) $ filter isDataDecl ds)

            isDataDecl :: Decl Annote -> Bool
            isDataDecl DataDecl {} = True
            isDataDecl _           = False

            toExport :: Renamer -> Decl Annote -> Export
            toExport rn (DataDecl _ _ _ hd cs _) = ExportWith (rename Type rn $ fst $ sDeclHead hd) $ map (toExport' rn) cs
            toExport _ _                         = undefined

            toExport' :: Renamer -> QualConDecl Annote -> FQName
            toExport' rn (QualConDecl _ _ _ (ConDecl _ (sName -> x) _)) = rename Value rn x
            toExport' _  _                                              = undefined

            lookupCtors' :: Export -> Export -> Export
            lookupCtors' (ExportWith x cs) _ = ExportWith x cs
            lookupCtors' (ExportAll x) (ExportWith x' cs)
                  | x == x'                  = ExportWith x' cs
                  | otherwise                = ExportAll x
            lookupCtors' e _                 = e

            getCtors :: Export -> [S.Name]
            getCtors (ExportWith _ cs) = map name cs
            getCtors _                 = []

            fixities :: S.Name -> [Export]
            fixities n = flip filter (getExportFixities ds) $ \ case
                  ExportFixity _ _ n' -> n == n'
                  _                   -> False

extendWithGlobs :: S.ModuleName -> [Decl Annote] -> Renamer -> Renamer
extendWithGlobs m ds rn = extend Value (zip (getGlobValDefs ds) $ map (qnamish . S.Qual m) $ getGlobValDefs ds)
                        $ extend Type  (zip (getGlobTyDefs ds)  $ map (qnamish . S.Qual m) $ getGlobTyDefs ds) rn
      where getGlobValDefs :: [Decl Annote] -> [S.Name]
            getGlobValDefs = flip foldr [] $ \ case
                  DataDecl _ _ _ _ cons _              -> (++) $ map getCtor cons
                  PatBind _ (PVar _ n) _ _             -> (:) $ sName n
                  _                                    -> id
            getGlobTyDefs :: [Decl Annote] -> [S.Name]
            getGlobTyDefs = flip foldr [] $ \ case
                  DataDecl _ _ _ hd _ _ -> (:) $ fst $ sDeclHead hd
                  _                     -> id
getCtor :: QualConDecl Annote -> S.Name
getCtor = \ case
      QualConDecl _ _ _ (ConDecl _ n _) -> sName n
      QualConDecl _ _ _ (RecDecl _ n _) -> sName n
      _                                 -> undefined

transData :: (Applicative m, Functor m, SyntaxError m) => Renamer -> [RWCData] -> Decl Annote -> m [RWCData]
transData rn datas = \ case
      DataDecl l _ _ (sDeclHead -> hd) cons _ -> do
            tyVars' <- mapM (transTyVar l) $ snd hd
            cons' <- mapM (transCon rn) cons
            return $ RWCData l (TyConId $ rename Type rn $ fst hd) tyVars' kblank cons' : datas
      _                                       -> return datas

transTySig :: (Applicative m, Functor m, SyntaxError m) => Renamer -> [(S.Name, RWCTy)] -> Decl Annote -> m [(S.Name, RWCTy)]
transTySig rn sigs = \ case
      TypeSig _ names t -> do
            t' <- transTy rn [] t
            return $ zip (map sName names) (repeat t') ++ sigs
      _                 -> return sigs

-- I guess this doesn't need to be in the monad, really, but whatever...  --adam
-- Not sure what the boolean field means here, so we ignore it!  --adam
transInlineSig :: SyntaxError m => [S.Name] -> Decl Annote -> m [S.Name]
transInlineSig inls = \ case
      InlineSig _ _ Nothing (Qual _ _ x) -> return $ sName x : inls
      InlineSig _ _ Nothing (UnQual _ x) -> return $ sName x : inls
      _                                  -> return inls

transDef :: (Applicative m, Functor m, SyntaxError m) => Renamer -> [(S.Name, RWCTy)] -> [S.Name] -> [RWCDefn] -> Decl Annote -> m [RWCDefn]
transDef rn tys inls defs = \ case
      PatBind l (PVar _ (sName -> x)) (UnGuardedRhs _ e) Nothing -> case lookup x tys of
            Just t -> (:defs) . RWCDefn l (mkId $ rename Value rn x) (nub (fv t) :-> t) (x `elem` inls) <$> transExp rn e
            _      -> failAt l "No type signature for"
      _                                             -> return defs

transTyVar :: SyntaxError m => Annote -> S.TyVarBind -> m (Id RWCTy)
transTyVar l = \ case
      S.UnkindedVar x -> return $ mkUId x
      _               -> failAt l "Unsupported type syntax"

transCon :: (Applicative m, Functor m, SyntaxError m) => Renamer -> QualConDecl Annote -> m RWCDataCon
transCon rn = \ case
      QualConDecl l Nothing _ (ConDecl _ x tys) -> RWCDataCon l (DataConId $ rename Value rn x) <$> mapM (transTy rn []) tys
      d                                         -> failAt (ann d) "Unsupported ctor syntax"

transTy :: (Applicative m, Functor m, SyntaxError m) => Renamer -> [S.Name] -> Type Annote -> m RWCTy
transTy rn ms = \ case
      TyForall _ Nothing (Just (CxTuple _ cs)) t   -> do
           ms' <- mapM getNad cs
           transTy rn (ms ++ ms') t
      TyApp l a b | isMonad ms a -> RWCTyComp l <$> transTy rn ms a <*> transTy rn ms b
                  | otherwise    -> RWCTyApp l <$> transTy rn ms a <*> transTy rn ms b
      TyCon l x                  -> return $ RWCTyCon l (TyConId $ rename Type rn x)
      TyVar l x                  -> return $ RWCTyVar l $ mkUId $ sName x
      t                          -> failAt (ann t) "Unsupported type syntax"

getNad :: SyntaxError m => Asst Annote -> m S.Name
getNad = \ case
      ClassA _ (UnQual _ (Ident _ "Monad")) [TyVar _ x] -> return $ sName x
      a                                                 -> failAt (ann a) "Unsupported typeclass constraint"

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

transExp :: (Applicative m, Functor m, SyntaxError m) => Renamer -> Exp Annote -> m RWCExp
transExp rn = \ case
      App l (App _ (Var _ (UnQual _ (Ident _ "nativeVhdl"))) (Lit _ (String _ f _))) e
                            -> RWCNativeVHDL l f <$> transExp rn e
      App l (Var _ (UnQual _ (Ident _ "primError"))) (Lit _ (String _ m _))
                            -> return $ RWCError l m tblank
      App l e1 e2           -> RWCApp l <$> transExp rn e1 <*> transExp rn e2
      Lambda l [PVar _ x] e -> RWCLam l (mkUId $ sName x) tblank <$> transExp (exclude Value [sName x] rn) e
      Var l x               -> return $ RWCVar l (mkId $ rename Value rn x) tblank
      Con l x               -> return $ RWCCon l (DataConId $ rename Value rn x) tblank
      Lit l lit             -> RWCLiteral l <$> transLit lit
      Case l e [Alt _ p (UnGuardedRhs _ e1) _, Alt _ _ (UnGuardedRhs _ e2) _]
                            -> RWCCase l
                                    <$> transExp rn e
                                    <*> transPat rn p
                                    <*> transExp (exclude Value (getVars p) rn) e1
                                    <*> transExp rn e2
      e                     -> failAt (ann e) $ "Unsupported expression syntax: " ++ show (void e)
      where getVars :: Pat Annote -> [S.Name]
            getVars = runPureQ $ query $ \ p -> case p :: Pat Annote of
                  PVar _ x -> [sName x]
                  _        -> []

transLit :: SyntaxError m => Literal Annote -> m RWCLit
transLit = \ case
      Int _ i _  -> return $ RWCLitInteger i
      Frac _ d _ -> return $ RWCLitFloat $ fromRational d
      Char _ c _ -> return $ RWCLitChar c
      lit        -> failAt (ann lit) "Unsupported syntax for a literal"

transPat :: (Functor m, SyntaxError m) => Renamer -> Pat Annote -> m RWCPat
transPat rn = \ case
      PApp l x ps             -> RWCPatCon l (DataConId $ rename Value rn x) <$> mapM (transPat rn) ps
      PLit l (Signless _) lit -> RWCPatLiteral l <$> transLit lit
      PVar l x                -> return $ RWCPatVar l (mkUId $ sName x) tblank
      p                       -> failAt (ann p) $ "Unsupported syntax in a pattern: " ++ (show $ void p)
