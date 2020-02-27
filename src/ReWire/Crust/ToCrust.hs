{-# LANGUAGE LambdaCase, ViewPatterns, ScopedTypeVariables, FlexibleContexts, TupleSections #-}
{-# LANGUAGE Safe #-}
module ReWire.Crust.ToCrust (toCrust, extendWithGlobs, getImps) where

import ReWire.Annotation hiding (ann)
import ReWire.Error
import ReWire.Crust.Fixity
import ReWire.Crust.Rename hiding (Module)
import ReWire.Crust.Syntax ((|->))
import ReWire.Unbound (s2n, n2s, fresh, Fresh, Name, Embed (..), bind)
import ReWire.SYB

import Control.Arrow ((&&&), second)
import Control.Monad (foldM, replicateM, void)
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)
import Language.Haskell.Exts.Fixity (Fixity (..))
import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified Data.Set                     as Set
import qualified Data.Map.Strict              as Map
import qualified Language.Haskell.Exts.Syntax as S
import qualified ReWire.Crust.Rename          as M
import qualified ReWire.Crust.Syntax          as M

import Language.Haskell.Exts.Syntax hiding (Annotation, Name, Kind)

-- | An intermediate form for exports. TODO(chathhorn): get rid of it.
data Export = Export FQName
            -- ExportWith: Type name, ctors
            | ExportWith FQName (Set.Set FQName) FQCtorSigs
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

mkUId :: S.Name a -> Name b
mkUId = \ case
      Ident _ n  -> s2n n
      Symbol _ n -> s2n n

-- | Translate a Haskell module into the ReWire abstract syntax.
toCrust :: (Fresh m, MonadError AstError m) => Renamer -> Module Annote -> m (M.Module, Exports)
toCrust rn = \ case
      Module _ (Just (ModuleHead _ _ _ exps)) _ _ (reverse -> ds) -> do
            tyDefs <- foldM (transData rn) [] ds
            tySigs <- foldM (transTySig rn) [] ds
            inls   <- foldM transInlineSig [] ds
            fnDefs <- foldM (transDef rn tySigs inls) [] ds
            exps'  <- maybe (pure $ getGlobExps rn ds) (\ (ExportSpecList _ exps') -> foldM (transExport rn ds) [] exps') exps
            pure (M.Module tyDefs fnDefs, resolveExports rn exps')
            where getGlobExps :: Renamer -> [Decl Annote] -> [Export]
                  getGlobExps rn ds = getTypeExports rn
                        ++ getExportFixities ds
                        ++ concatMap (getFunExports rn) ds

                  getFunExports :: Renamer -> Decl Annote -> [Export]
                  getFunExports rn = \ case
                        PatBind _ (PVar _ n) _ _ -> [Export $ rename Value rn $ void n]
                        _                        -> []

                  getTypeExports :: Renamer -> [Export]
                  getTypeExports rn = map (exportAll rn) (Set.toList $ getLocalTypes rn)

                  resolveExports :: Renamer -> [Export] -> Exports
                  resolveExports rn = foldr (resolveExport rn) mempty

                  resolveExport :: Renamer -> Export -> Exports -> Exports
                  resolveExport rn = \ case
                        Export x               -> expValue x
                        ExportAll x            -> expType x (lookupCtors rn x) (lookupCtorSigsForType rn x)
                        ExportWith x cs fs     -> expType x cs fs
                        ExportMod m            -> (<> getExports m rn)
                        ExportFixity asc lvl x -> expFixity asc lvl x
      m                                                           -> failAt (ann m) "Unsupported module syntax"

exportAll :: QNamish a => Renamer -> a -> Export
exportAll rn x = let x' = rename Type rn x in
      ExportWith x' (lookupCtors rn x') (lookupCtorSigsForType rn x')

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
      EThingWith l (NoWildcard _) (void -> x) cs       -> let cs' = Set.fromList $ map (rename Value rn . unwrap) cs in
            if and $ finger Type rn x : map (finger Value rn . name) (Set.toList cs')
            then pure $ ExportWith (rename Type rn x) cs' (Map.fromSet (lookupCtorSig rn) cs')
                  : concatMap (fixities . unwrap) cs ++ exps
            else failAt l "Unknown class or type name in export list"
      -- TODO(chathhorn): I don't know what it means for a wildcard to appear in the middle of an export list.
      EThingWith l (EWildcard _ _) (void -> x) _       ->
            if finger Type rn x
            then pure $ exportAll rn x : concatMap (fixities . name) (Set.toList $ lookupCtors rn x) ++ exps
            else failAt l "Unknown class or type name in export list"
      EModuleContents _ (void -> m) ->
            pure $ ExportMod m : exps
      where unwrap :: CName Annote -> S.Name ()
            unwrap (VarName _ x) = void x
            unwrap (ConName _ x) = void x

            fixities :: S.Name () -> [Export]
            fixities n = flip filter (getExportFixities ds) $ \ case
                  ExportFixity _ _ n' -> n == n'
                  _                   -> False

transData :: (MonadError AstError m, Fresh m) => Renamer -> [M.DataDefn] -> Decl Annote -> m [M.DataDefn]
transData rn datas = \ case
      DataDecl l _ _ (sDeclHead -> hd) cs _ -> do
            let n = s2n $ rename Type rn $ fst hd
            tvs' <- mapM (transTyVar l) $ snd hd
            ks   <- replicateM (length tvs') $ freshKVar $ n2s n
            cs'  <- mapM (transCon rn ks tvs' n) cs
            pure $ M.DataDefn l n (foldr M.KFun M.KStar ks) cs' : datas
      _                                       -> pure datas

transTySig :: (Fresh m, MonadError AstError m) => Renamer -> [(S.Name (), M.Ty)] -> Decl Annote -> m [(S.Name (), M.Ty)]
transTySig rn sigs = \ case
      TypeSig _ names t -> do
            t' <- transTy rn t
            pure $ zip (map void names) (repeat t') ++ sigs
      _                 -> pure sigs

-- I guess this doesn't need to be in the monad, really, but whatever...  --adam
-- Not sure what the boolean field means here, so we ignore it!  --adam
transInlineSig :: MonadError AstError m => [S.Name ()] -> Decl Annote -> m [S.Name ()]
transInlineSig inls = \ case
      InlineSig _ _ Nothing (Qual _ _ x) -> pure $ void x : inls
      InlineSig _ _ Nothing (UnQual _ x) -> pure $ void x : inls
      _                                  -> pure inls

-- TODO(chathhorn): should be a map, not a fold
transDef :: MonadError AstError m => Renamer -> [(S.Name (), M.Ty)] -> [S.Name ()] -> [M.Defn] -> Decl Annote -> m [M.Defn]
transDef rn tys inls defs = \ case
      PatBind l (PVar _ (void -> x)) (UnGuardedRhs _ e) Nothing -> do
            let x' = s2n $ rename Value rn x
            case lookup x tys of
                  -- TODO(chathhorn): hackily elide definition of primitives. Allows providing alternate defs for GHC compat.
                  Just t -> do
                        e' <- if M.isPrim x' && x `notElem` inls then pure $ M.Error (ann e) t $ "Prim: " ++ n2s x' else transExp rn e
                        pure $ M.Defn l x' (M.fv t |-> t) (x `elem` inls) (Embed (bind [] e')) : defs
                  Nothing -> do
                        e' <- if M.isPrim x' then pure $ M.Error (ann e) (M.TyBlank $ ann e) $ "Prim: " ++ n2s x' else transExp rn e
                        -- TODO(chathhorn): would like to raise a warning here about lack of top-level type sig
                        -- (as opposed to an error).
                        pure $ M.Defn l x' ([] |-> M.TyBlank l) (x `elem` inls) (Embed (bind [] e')) : defs
      DataDecl {}                                               -> pure defs -- TODO(chathhorn): elide
      InlineSig {}                                              -> pure defs -- TODO(chathhorn): elide
      TypeSig {}                                                -> pure defs -- TODO(chathhorn): elide
      InfixDecl {}                                              -> pure defs -- TODO(chathhorn): elide
      TypeDecl {}                                               -> pure defs -- TODO(chathhorn): elide
      d                                                         -> failAt (ann d) $ "Unsupported definition syntax: " ++ show (void d)

transTyVar :: MonadError AstError m => Annote -> S.TyVarBind () -> m (Name M.Ty)
transTyVar l = \ case
      S.UnkindedVar _ x -> pure $ mkUId x
      _                 -> failAt l "Unsupported type syntax"

transCon :: (Fresh m, MonadError AstError m) => Renamer -> [M.Kind] -> [Name M.Ty] -> Name M.TyConId -> QualConDecl Annote -> m M.DataCon
transCon rn ks tvs tc = \ case
      QualConDecl l Nothing _ (ConDecl _ x tys) -> do
            let tvs' = zipWith (M.TyVar l) ks tvs
            t <- foldr M.arr (foldl' (M.TyApp l) (M.TyCon l tc) tvs') <$> mapM (transTy rn) tys
            pure $ M.DataCon l (s2n $ rename Value rn x) (tvs |-> t)
      d                                         -> failAt (ann d) "Unsupported Ctor syntax"

transTy :: (Fresh m, MonadError AstError m) => Renamer -> Type Annote -> m M.Ty
transTy rn = \ case
      TyForall _ _ _ t -> transTy rn t
      TyApp l a b      -> M.TyApp l <$> transTy rn a <*> transTy rn b
      TyCon l x        -> pure $ M.TyCon l (s2n $ rename Type rn x)
      TyVar l x        -> M.TyVar l <$> freshKVar (prettyPrint x) <*> pure (mkUId $ void x)
      t                -> failAt (ann t) "Unsupported type syntax"

freshKVar :: Fresh m => String -> m M.Kind
freshKVar n = M.KVar <$> fresh (s2n $ "?K_" ++ n)

transExp :: MonadError AstError m => Renamer -> Exp Annote -> m M.Exp
transExp rn = \ case
      App l (App _ (Var _ n) (Lit _ (String _ f _))) e
            | isNativeVhdl n -> M.NativeVHDL l f <$> transExp rn e
      App l (Var _ n) (Lit _ (String _ m _))
            | isError n      -> pure $ M.Error l (M.TyBlank l) m
      App l e1 e2            -> M.App l <$> transExp rn e1 <*> transExp rn e2
      Lambda l [PVar _ x] e  -> do
            e' <- transExp (exclude Value [void x] rn) e
            pure $ M.Lam l (M.TyBlank l) $ bind (mkUId $ void x) e'
      Var l x                -> pure $ M.Var l (M.TyBlank l) (s2n $ rename Value rn x)
      Con l x                -> pure $ M.Con l (M.TyBlank l) (s2n $ rename Value rn x)
      Case l e [Alt _ p (UnGuardedRhs _ e1) _, Alt _ _ (UnGuardedRhs _ e2) _] -> do
            e'  <- transExp rn e
            p'  <- transPat rn p
            e1' <- transExp (exclude Value (getVars p) rn) e1
            e2' <- transExp rn e2
            pure $ M.Case l (M.TyBlank l) e' (bind p' e1') (Just e2')
      Case l e [Alt _ p (UnGuardedRhs _ e1) _] -> do
            e'  <- transExp rn e
            p'  <- transPat rn p
            e1' <- transExp (exclude Value (getVars p) rn) e1
            pure $ M.Case l (M.TyBlank l) e' (bind p' e1') Nothing
      e                      -> failAt (ann e) $ "Unsupported expression syntax: " ++ show (void e)
      where getVars :: Pat Annote -> [S.Name ()]
            getVars = runQ $ query $ \ case
                  PVar (_::Annote) x -> [void x]
                  _                  -> []

            isError :: QName Annote -> Bool
            isError n = prettyPrint (name $ rename Value rn n) == "error"

            isNativeVhdl :: QName Annote -> Bool
            isNativeVhdl n = prettyPrint (name $ rename Value rn n) == "nativeVhdl"

transPat :: MonadError AstError m => Renamer -> Pat Annote -> m M.Pat
transPat rn = \ case
      PApp l x ps             -> M.PatCon l (Embed (M.TyBlank l)) (Embed $ s2n $ rename Value rn x) <$> mapM (transPat rn) ps
      PVar l x                -> pure $ M.PatVar l (Embed (M.TyBlank l)) (mkUId $ void x)
      p                       -> failAt (ann p) $ "Unsupported syntax in a pattern: " ++ show (void p)

-- Note: runs before desugaring.
-- TODO(chathhorn): GADT style decls?
extendWithGlobs :: Annotation a => S.Module a -> Renamer -> Renamer
extendWithGlobs = \ case
      Module l (Just (ModuleHead _ (ModuleName _ "ReWire") _ _)) _ _ ds -> extendWith ds $ ModuleName l ""
      Module _ (Just (ModuleHead _ m _ _)) _ _ ds                       -> extendWith ds m
      _                                                                 -> id
      where extendWith :: Annotation a => [Decl a] -> ModuleName a -> Renamer -> Renamer
            extendWith ds m = setCtors (getModCtors ds) (getModCtorSigs ds) . extendWith' ds (void m)

            extendWith' :: Annotation a => [Decl a] -> ModuleName () -> Renamer -> Renamer
            extendWith' ds m = extend Value (zip (getGlobValDefs ds) $ map (qnamish . S.Qual () m) $ getGlobValDefs ds)
                             . extend Type  (zip (getGlobTyDefs ds)  $ map (qnamish . S.Qual () m) $ getGlobTyDefs ds)

            getGlobValDefs :: Annotation a => [Decl a] -> [S.Name ()]
            getGlobValDefs = concatMap $ \ case
                  DataDecl _ _ _ _ cons _               -> Set.toList $ Set.unions $ map getCtor cons
                  PatBind _ (PVar _ n) _ _              -> [void n]
                  FunBind _ (Match _ n _ _ _ : _)       -> [void n]
                  FunBind _ (InfixMatch _ _ n _ _ _: _) -> [void n]
                  _                                     -> []

            getGlobTyDefs :: Annotation a => [Decl a] -> [S.Name ()]
            getGlobTyDefs = concatMap $ \ case
                  DataDecl _ _ _ hd _ _ -> [fst $ sDeclHead hd]
                  _                     -> []

            getModCtors :: Annotation a => [S.Decl a] -> Ctors
            getModCtors ds =  Map.unions $ map getCtors' ds

            getCtors' :: Decl l -> Ctors
            getCtors' = \ case
                  DataDecl _ _ _ dh cons _ -> Map.singleton (fst $ sDeclHead dh) $ Set.unions $ map getCtor cons
                  _                        -> mempty

            getModCtorSigs :: Annotation a => [S.Decl a] -> CtorSigs
            getModCtorSigs ds = Map.unions $ map getCtorSigs' ds

            getCtorSigs' :: Decl l -> CtorSigs
            getCtorSigs' = \ case
                  DataDecl _ _ _ dh cons _ -> getCtorSigs (sDeclHead dh) cons
                  _                        -> mempty

            getCtorSigs :: (S.Name (), [S.TyVarBind ()]) -> [QualConDecl l] -> CtorSigs
            getCtorSigs dsig = Map.fromList . map (getCtorName &&& map (extendCtorSig $ dsigToTyApp dsig) . getFields)

            dsigToTyApp  :: (S.Name (), [S.TyVarBind ()]) -> S.Type ()
            dsigToTyApp (n, vs) = foldl' (S.TyApp ()) (S.TyCon () $ qnamish n) $ map toTyVar vs

            toTyVar :: S.TyVarBind () -> S.Type ()
            toTyVar = \ case
                  KindedVar   _ n _ -> TyVar () n
                  UnkindedVar _ n   -> TyVar () n

            extendCtorSig :: S.Type () -> (n, S.Type ()) -> (n, S.Type ())
            extendCtorSig t = second $ S.TyFun () t

            getCtor :: QualConDecl l -> Set.Set (S.Name ())
            getCtor d = Set.fromList $ getCtorName d : getFieldNames (getFields d)

            getCtorName :: QualConDecl l -> S.Name ()
            getCtorName = \ case
                  QualConDecl _ _ _ (ConDecl _ n _)        -> void n
                  QualConDecl _ _ _ (InfixConDecl _ _ n _) -> void n
                  QualConDecl _ _ _ (RecDecl _ n _)        -> void n

            getFields :: QualConDecl l -> [(Maybe (S.Name ()), S.Type ())]
            getFields = \ case
                  QualConDecl _ _ _ (ConDecl _ _ ts)         -> map ((Nothing, ) . void) ts
                  QualConDecl _ _ _ (InfixConDecl _ t1 _ t2) -> [(Nothing, void t1), (Nothing, void t2)]
                  QualConDecl _ _ _ (RecDecl _ _ fs)         -> concatMap getFields' fs

            getFields' :: FieldDecl l -> [(Maybe (S.Name ()), S.Type ())]
            getFields' (FieldDecl _ ns t) = map ((, void t) . Just . void) ns

            getFieldNames :: [(Maybe (S.Name ()), S.Type ())] -> [S.Name ()]
            getFieldNames = mapMaybe fst

getImps :: Annotation a => S.Module a -> [ImportDecl a]
getImps = \ case
      -- TODO(chathhorn): hacky. The prim/ReWire module really shouldn't depend on Prelude.
      S.Module _ (Just (ModuleHead _ (ModuleName _ "ReWire") _ _)) _ _ _            -> [] -- all imports ignored in the Prim module.
      S.Module l (Just (ModuleHead _ (ModuleName _ "ReWire.Prelude") _ _)) _ imps _ -> addMod l "ReWire" $ filter (not . isMod "Prelude") imps -- TODO(chathhorn) gah!
      S.Module l _ _ imps _                                                         -> addMod l "ReWire.Prelude" $ addMod l "ReWire" $ filter (not . isMod "Prelude") imps
      S.XmlPage {}                                                                  -> []
      S.XmlHybrid l _ _ imps _ _ _ _ _                                              -> addMod l "ReWire.Prelude" $ addMod l "ReWire" $ filter (not . isMod "Prelude") imps
      where addMod :: Annotation a => a -> String -> [ImportDecl a] -> [ImportDecl a]
            addMod l m imps = if any (isMod m) imps then imps
                  else ImportDecl l (ModuleName l m) False False False Nothing Nothing Nothing : imps

            isMod :: Annotation a => String -> ImportDecl a -> Bool
            isMod m ImportDecl { importModule = ModuleName _ n } = n == m
