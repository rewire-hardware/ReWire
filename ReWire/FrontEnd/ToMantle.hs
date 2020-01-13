{-# LANGUAGE LambdaCase, ViewPatterns, ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE Safe #-}
module ReWire.FrontEnd.ToMantle (toMantle) where

import ReWire.Annotation hiding (ann)
import ReWire.Error
import ReWire.FrontEnd.Fixity
import ReWire.FrontEnd.Rename hiding (Module)
import ReWire.FrontEnd.Syntax ((|->))
import ReWire.FrontEnd.Unbound
      ( string2Name, name2String
      , fresh, Fresh, Name, Embed (..), bind
      )
import ReWire.SYB

import Control.Monad ((>=>),foldM, replicateM, void)
import Data.Foldable (foldl')
import Language.Haskell.Exts.Fixity (Fixity (..))
import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified Data.Set                     as Set
import qualified Language.Haskell.Exts.Syntax as S
import qualified ReWire.FrontEnd.Rename       as M
import qualified ReWire.FrontEnd.Syntax       as M

import Language.Haskell.Exts.Syntax hiding (Name, Kind)

-- | An intermediate form for exports. TODO(chathhorn): get rid of it.
data Export = Export FQName
            | ExportWith FQName [FQName]
            | ExportAll FQName
            | ExportMod (S.ModuleName ())
            | ExportFixity (S.Assoc ()) Int (S.Name ())
      deriving Show

sDeclHead :: S.DeclHead a -> (S.Name (), [S.TyVarBind ()])
sDeclHead = \ case
      DHead _ n                          -> (void n, [])
      DHInfix _ tv n                     -> (void n, [void tv])
      DHParen _ dh                       -> sDeclHead dh
      DHApp _ (sDeclHead -> (n, tvs)) tv -> (n, tvs ++ [void tv])

mkId :: String -> Name b
mkId = string2Name

mkUId :: S.Name a -> Name b
mkUId (Ident _ n)  = mkId n
mkUId (Symbol _ n) = mkId n

-- | Translate a Haskell module into the ReWire abstract syntax.
toMantle :: (Fresh m, MonadError AstError m) => Renamer -> Module Annote -> m (M.Module, Exports)
toMantle rn (Module _ (Just (ModuleHead _ m _ exps)) _ _ (reverse -> ds)) = do
      let rn' = extendWithGlobs (void m) ds rn
      tyDefs <- foldM (transData rn') [] ds
      tySigs <- foldM (transTySig rn') [] ds
      inls   <- foldM transInlineSig [] ds
      fnDefs <- foldM (transDef rn' tySigs inls) [] ds
      exps'  <- maybe (pure $ getGlobExps rn' ds) (\ (ExportSpecList _ exps') -> foldM (transExport rn' ds) [] exps') exps
      pure (M.Module tyDefs fnDefs, resolveExports rn exps')
      where getGlobExps :: Renamer -> [Decl Annote] -> [Export]
            getGlobExps rn ds = getExportFixities ds ++ foldr (getGlobExps' rn) [] ds

            getGlobExps' :: Renamer -> Decl Annote -> [Export] -> [Export]
            getGlobExps' rn = \ case
                  DataDecl _ _ _ hd cs _   -> (:) $ ExportWith (rename Type rn $ fst $ sDeclHead hd) $ map (rename Value rn . getCtor) cs
                  PatBind _ (PVar _ n) _ _ -> (:) $ Export $ rename Value rn $ void n
                  _                        -> id
toMantle _ m = failAt (ann m) "Unsupported module syntax"

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
            toExportFixity (Fixity asc lvl (S.UnQual () n)) = ExportFixity asc lvl n
            toExportFixity _                                = undefined

transExport :: MonadError AstError m => Renamer -> [Decl Annote] -> [Export] -> ExportSpec Annote -> m [Export]
transExport rn ds exps = \ case
      EVar l (void -> x)                 ->
            if finger Value rn x
            then pure $ Export (rename Value rn x) : fixities (qnamish x) ++ exps
            else failAt l "Unknown variable name in export list"
      EAbs l _ (void -> x)               ->
            if finger Type rn x
            then pure $ Export (rename Type rn x) : exps
            else failAt l "Unknown class or type name in export list"
      EThingWith l (NoWildcard _) (void -> x) cs        ->
            if and $ finger Type rn x : map (finger Value rn . qnamish . unwrap) cs
            then pure $ ExportWith (rename Type rn x) (map (rename Value rn . unwrap) cs) : concatMap (fixities . unwrap) cs ++ exps
            else failAt l "Unknown class or type name in export list"
      -- TODO(chathhorn): I don't know what it means for a wildcard to appear in the middle of an export list.
      EThingWith l (EWildcard _ _) (void -> x) _       ->
            if finger Type rn x
            then pure $ lookupCtors x : concatMap fixities (getCtors $ lookupCtors x) ++ exps
            else failAt l "Unknown class or type name in export list"
      EModuleContents _ (void -> m) ->
            pure $ ExportMod m : exps
      where unwrap :: CName Annote -> S.Name ()
            unwrap (VarName _ x) = void x
            unwrap (ConName _ x) = void x

            lookupCtors :: S.QName () -> Export
            lookupCtors x = foldl' lookupCtors' (ExportAll $ rename Type rn x) (map (toExport rn) $ filter isDataDecl ds)

            isDataDecl :: Decl Annote -> Bool
            isDataDecl DataDecl {} = True
            isDataDecl _           = False

            toExport :: Renamer -> Decl Annote -> Export
            toExport rn (DataDecl _ _ _ hd cs _) = ExportWith (rename Type rn $ fst $ sDeclHead hd) $ map (toExport' rn) cs
            toExport _ _                         = undefined

            toExport' :: Renamer -> QualConDecl Annote -> FQName
            toExport' rn (QualConDecl _ _ _ (ConDecl _ (void -> x) _)) = rename Value rn x
            toExport' _  _                                              = undefined

            lookupCtors' :: Export -> Export -> Export
            lookupCtors' (ExportWith x cs) _ = ExportWith x cs
            lookupCtors' (ExportAll x) (ExportWith x' cs)
                  | x == x'                  = ExportWith x' cs
                  | otherwise                = ExportAll x
            lookupCtors' e _                 = e

            getCtors :: Export -> [S.Name ()]
            getCtors (ExportWith _ cs) = map name cs
            getCtors _                 = []

            fixities :: S.Name () -> [Export]
            fixities n = flip filter (getExportFixities ds) $ \ case
                  ExportFixity _ _ n' -> n == n'
                  _                   -> False

extendWithGlobs :: S.ModuleName () -> [Decl Annote] -> Renamer -> Renamer
extendWithGlobs m ds rn = extend Value (zip (getGlobValDefs ds) $ map (qnamish . S.Qual () m) $ getGlobValDefs ds)
                        $ extend Type  (zip (getGlobTyDefs ds)  $ map (qnamish . S.Qual () m) $ getGlobTyDefs ds) rn
      where getGlobValDefs :: [Decl Annote] -> [S.Name ()]
            getGlobValDefs = flip foldr [] $ \ case
                  DataDecl _ _ _ _ cons _              -> (++) $ map getCtor cons
                  PatBind _ (PVar _ n) _ _             -> (:) $ void n
                  _                                    -> id
            getGlobTyDefs :: [Decl Annote] -> [S.Name ()]
            getGlobTyDefs = flip foldr [] $ \ case
                  DataDecl _ _ _ hd _ _ -> (:) $ fst $ sDeclHead hd
                  _                     -> id
getCtor :: QualConDecl Annote -> S.Name ()
getCtor = \ case
      QualConDecl _ _ _ (ConDecl _ n _) -> void n
      QualConDecl _ _ _ (RecDecl _ n _) -> void n
      _                                 -> undefined

transData :: (MonadError AstError m, Fresh m) => Renamer -> [M.DataDefn] -> Decl Annote -> m [M.DataDefn]
transData rn datas = \ case
      DataDecl l _ _ (sDeclHead -> hd) cs _ -> do
            let n = string2Name $ rename Type rn $ fst hd
            tvs' <- mapM (transTyVar l) $ snd hd
            ks   <- replicateM (length tvs') $ freshKVar $ name2String n
            cs'  <- mapM (transCon rn ks tvs' n) cs
            pure $ M.DataDefn l n (foldr M.KFun M.KStar ks) cs' : datas
      _                                       -> pure datas

transTySig :: (Fresh m, MonadError AstError m) => Renamer -> [(S.Name (), M.Ty)] -> Decl Annote -> m [(S.Name (), M.Ty)]
transTySig rn sigs = \ case
      TypeSig _ names t -> do
            t' <- transTy rn [] t
            pure $ zip (map void names) (repeat t') ++ sigs
      _                 -> pure sigs

-- I guess this doesn't need to be in the monad, really, but whatever...  --adam
-- Not sure what the boolean field means here, so we ignore it!  --adam
transInlineSig :: MonadError AstError m => [S.Name ()] -> Decl Annote -> m [S.Name ()]
transInlineSig inls = \ case
      InlineSig _ _ Nothing (Qual _ _ x) -> pure $ void x : inls
      InlineSig _ _ Nothing (UnQual _ x) -> pure $ void x : inls
      _                                  -> pure inls

transDef :: MonadError AstError m => Renamer -> [(S.Name (), M.Ty)] -> [S.Name ()] -> [M.Defn] -> Decl Annote -> m [M.Defn]
transDef rn tys inls defs = \ case
      PatBind l (PVar _ (void -> x)) (UnGuardedRhs _ e) Nothing -> case lookup x tys of
            Just t -> do
                  e' <- transExp rn e
                  pure $ M.Defn l (mkId $ rename Value rn x) (M.fv t |-> t) (x `elem` inls) (Embed (bind [] e')) : defs
            _      -> failAt l "No type signature for"
      _                                             -> pure defs

transTyVar :: MonadError AstError m => Annote -> S.TyVarBind () -> m (Name M.Ty)
transTyVar l = \ case
      S.UnkindedVar _ x -> pure $ mkUId x
      _                 -> failAt l "Unsupported type syntax"

transCon :: (Fresh m, MonadError AstError m) => Renamer -> [M.Kind] -> [Name M.Ty] -> Name M.TyConId -> QualConDecl Annote -> m M.DataCon
transCon rn ks tvs tc = \ case
      QualConDecl l Nothing _ (ConDecl _ x tys) -> do
            let tvs' = zipWith (M.TyVar l) ks tvs
            t <- foldr M.arr0 (foldl' (M.TyApp l) (M.TyCon l tc) tvs') <$> mapM (transTy rn []) tys
            return $ M.DataCon l (string2Name $ rename Value rn x) (tvs |-> t)
      -- The following is kind of hacky, sorry. Bill.
      QualConDecl l Nothing _ (RecDecl _ x fs)  -> do
            let tys  = map (\ (FieldDecl _ _ t) -> t) fs
            let flds = map (\ (FieldDecl _ xs _) -> (map (string2Name . rename Value rn) xs)) fs
            let tvs' = zipWith (M.TyVar l) ks tvs
            t <- foldr M.arr0 (foldl' (M.TyApp l) (M.TyCon l tc) tvs') <$> mapM (transTy rn []) tys
            ts <- mapM (transTy rn [] >=> (return . (tvs |->))) tys
            return $ M.RecCon l (string2Name $ rename Value rn x) (tvs |-> t) (zip flds ts)
      d                                         -> failAt (ann d) "Unsupported Ctor syntax"

transTy :: (Fresh m, MonadError AstError m) => Renamer -> [S.Name ()] -> Type Annote -> m M.Ty
transTy rn ms = \ case
      TyForall _ Nothing (Just (CxTuple _ cs)) t   -> do
           ms' <- mapM getNad cs
           transTy rn (ms ++ ms') t
      TyApp l a b | isMonad ms a -> M.TyComp l <$> transTy rn ms a <*> transTy rn ms b
                  | otherwise    -> M.TyApp l <$> transTy rn ms a <*> transTy rn ms b
      TyCon l x                  -> pure $ M.TyCon l (string2Name $ rename Type rn x)
      TyVar l x                  -> M.TyVar l <$> freshKVar (prettyPrint x) <*> (pure $ mkUId $ void x)
      t                          -> failAt (ann t) "Unsupported type syntax"

freshKVar :: Fresh m => String -> m M.Kind
freshKVar n = M.KVar <$> fresh (string2Name $ "?K_" ++ n)

getNad :: MonadError AstError m => Asst Annote -> m (S.Name ())
getNad = \ case
      ClassA _ (UnQual _ (Ident _ "Monad")) [TyVar _ x] -> pure $ void x
      a                                                   -> failAt (ann a) "Unsupported typeclass constraint"

isMonad :: [S.Name ()] -> Type Annote -> Bool
isMonad ms = \ case
      TyApp _ (TyApp _ (TyApp _ (TyCon _ (UnQual _ (Ident _ "ReT"))) _) _) t -> isMonad ms t
      TyApp _ (TyApp _ (TyCon _ (UnQual _ (Ident _ "StT"))) _) t             -> isMonad ms t
      TyCon _ (UnQual _ (Ident _ "I"))                                       -> True
      TyVar _ (void -> x)                                                   -> x `elem` ms
      _                                                                      -> False


transExp :: MonadError AstError m => Renamer -> Exp Annote -> m M.Exp
transExp rn = \ case
      App l (App _ (Var _ (UnQual _ (Ident _ "nativeVhdl"))) (Lit _ (String _ f _))) e
                            -> M.NativeVHDL l f <$> transExp rn e
      App l (Var _ (UnQual _ (Ident _ "primError"))) (Lit _ (String _ m _))
                            -> pure $ M.Error l M.tblank m
      App l e1 e2           -> M.App l <$> transExp rn e1 <*> transExp rn e2
      Lambda l [PVar _ x] e -> do
            e' <- transExp (exclude Value [void x] rn) e
            pure $ M.Lam l M.tblank $ bind (mkUId $ void x) e'
      Var l x               -> pure $ M.Var l M.tblank (mkId $ rename Value rn x)
      Con l x               -> pure $ M.Con l M.tblank (string2Name $ rename Value rn x)
      RecUpdate l e fus -> do
            e' <- transExp rn e
            es' <- mapM (transExp rn) (map (\ (FieldUpdate _ _ e) -> e) fus)
            ns' <- mapM (return . mkId . rename Value rn) (map (\ (FieldUpdate _ n _) -> n) fus)
            pure $ M.RecUp l M.tblank e' (zip ns' es')
      RecConstr l n fus -> do
            es' <- mapM (transExp rn) (map (\ (FieldUpdate _ _ e) -> e) fus)
            ns' <- mapM (return . mkId . rename Value rn) (map (\ (FieldUpdate _ n _) -> n) fus)
            pure $ M.RecConApp l M.tblank (string2Name $ rename Value rn n) (zip ns' es')
      Case l e [Alt _ p (UnGuardedRhs _ e1) _, Alt _ _ (UnGuardedRhs _ e2) _] -> do
            e'  <- transExp rn e
            p'  <- transPat rn p
            e1' <- transExp (exclude Value (getVars p) rn) e1
            e2' <- transExp rn e2
            pure $ M.Case l M.tblank e' (bind p' e1') (Just e2')
      Case l e [Alt _ p (UnGuardedRhs _ e1) _] -> do
            e'  <- transExp rn e
            p'  <- transPat rn p
            e1' <- transExp (exclude Value (getVars p) rn) e1
            pure $ M.Case l M.tblank e' (bind p' e1') Nothing
      e                     -> failAt (ann e) $ "Unsupported expression syntax: " ++ show (void e)
      where getVars :: Pat Annote -> [S.Name ()]
            getVars = runQ $ query $ \ case
                  PVar (_::Annote) x -> [void x]
                  _                  -> []

transPat :: MonadError AstError m => Renamer -> Pat Annote -> m M.Pat
transPat rn = \ case
      PApp l x ps             -> M.PatCon l (Embed M.tblank) (Embed $ string2Name $ rename Value rn x) <$> mapM (transPat rn) ps
      PVar l x                -> pure $ M.PatVar l (Embed M.tblank) (mkUId $ void x)
      p                       -> failAt (ann p) $ "Unsupported syntax in a pattern: " ++ (show $ void p)
