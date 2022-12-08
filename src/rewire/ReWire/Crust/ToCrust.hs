{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings, MultiWayIf #-}
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
import Data.Char (isUpper)
import Data.Foldable (foldl')
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text, pack, unpack)
import Language.Haskell.Exts.Fixity (Fixity (..))
import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified Data.Set                     as Set
import qualified Data.Map.Strict              as Map
import qualified Language.Haskell.Exts.Syntax as S
import qualified ReWire.Crust.Rename          as M
import qualified ReWire.Crust.Syntax          as M

import Language.Haskell.Exts.Syntax hiding (Annotation, Name, Kind)

isPrimMod :: String -> Bool
isPrimMod = (== "RWC.Primitives")

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
      Ident _ n  -> s2n (pack n)
      Symbol _ n -> s2n (pack n)

-- | Translate a Haskell module into the ReWire abstract syntax.
toCrust :: (Fresh m, MonadError AstError m) => Renamer -> Module Annote -> m (M.Module, Exports)
toCrust rn = \ case
      Module _ (Just (ModuleHead _ (ModuleName _ mname) _ exps)) _ _ (reverse -> ds) -> do
            tyDefs <- foldM (transData rn) [] ds
            tySyns <- if | isPrimMod mname -> pure [] -- TODO(chathhorn): ignore type synonyms in ReWire.hs module.
                         | otherwise       -> foldM (transTyDecl rn) [] ds
            tySigs <- foldM (transTySig rn) [] ds
            fnDefs <- foldM (transDef rn tySigs inls) [] ds
            exps'  <- maybe (pure $ getGlobExps rn ds) (\ (ExportSpecList _ exps') -> foldM (transExport rn ds) [] exps') exps
            pure (M.Module tyDefs tySyns fnDefs, resolveExports rn exps')
            where getGlobExps :: Renamer -> [Decl Annote] -> [Export]
                  getGlobExps rn ds = getTypeExports rn <> getExportFixities ds <> concatMap (getFunExports rn) ds

                  getFunExports :: Renamer -> Decl Annote -> [Export]
                  getFunExports rn = \ case
                        PatBind _ (PVar _ n) _ _ -> [Export $ rename Value rn $ void n]
                        _                        -> []

                  getTypeExports :: Renamer -> [Export]
                  getTypeExports rn = map (exportAll rn) $ Set.toList (getLocalTypes rn) <> tysynNames ds

                  resolveExports :: Renamer -> [Export] -> Exports
                  resolveExports rn = foldr (resolveExport rn) mempty

                  isValueName :: FQName -> Bool
                  isValueName = \ case
                        (name -> Ident _ (c : _)) | isUpper c -> False
                        _                                     -> True

                  resolveExport :: Renamer -> Export -> Exports -> Exports
                  resolveExport rn = \ case
                        Export x              | isValueName x -> expValue x
                        Export x                              -> expType x mempty mempty
                        ExportAll x                           -> expType x (lookupCtors rn x) (lookupCtorSigsForType rn x)
                        ExportWith x cs fs                    -> expType x cs fs
                        ExportMod m                           -> (<> getExports m rn)
                        ExportFixity asc lvl x                -> expFixity asc lvl x

                  inls :: Map (S.Name ()) M.DefnAttr
                  inls = foldr inl' mempty ds
                        where inl' :: Decl Annote -> Map (S.Name ()) M.DefnAttr -> Map (S.Name ()) M.DefnAttr
                              inl' = \ case
                                    InlineSig _ b Nothing (Qual _ _ x) -> Map.insert (void x) $ if b then M.Inline else M.NoInline
                                    InlineSig _ b Nothing (UnQual _ x) -> Map.insert (void x) $ if b then M.Inline else M.NoInline
                                    _                                  -> id
      m                                                                -> failAt (ann m) "Unsupported module syntax"


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
      -- TODO(chathhorn): Ignore type exports.
      -- This is to ease compatibility with GHC -- we can have built-in type
      -- operators while ignoring parts exported from GHC.TypeLits in the
      -- RWC.Primitives module.
      EAbs _ (TypeNamespace _) _         -> pure exps
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
            let n    = s2n $ rename Type rn $ fst hd
                tvs' = map transTyVar $ snd hd
            ks   <- replicateM (length tvs') $ freshKVar $ n2s n
            cs'  <- mapM (transCon rn ks tvs' n) cs
            pure $ M.DataDefn l n (foldr M.KFun M.KStar ks) cs' : datas
      _                                       -> pure datas

tysynNames :: [Decl Annote] -> [S.Name ()]
tysynNames = concatMap tySynName
      where tySynName :: Decl Annote -> [S.Name ()]
            tySynName = \ case
                  TypeDecl _ (sDeclHead -> hd) _ -> [fst hd]
                  _                              -> []

transTyDecl :: (MonadError AstError m, Fresh m) => Renamer -> [M.TypeSynonym] -> Decl Annote -> m [M.TypeSynonym]
transTyDecl rn syns = \ case
      TypeDecl l (sDeclHead -> hd) t -> do
            let n   = s2n $ rename Type rn $ fst hd
                lhs = map transTyVar $ snd hd
            t'  <- transTy rn t
            let rhs :: [Name M.Ty]
                rhs = M.fv t'

            let tvs' = map (renumber rhs) lhs
            pure $ M.TypeSynonym l n (tvs' |-> t') : syns
      _                              -> pure syns

      where renumber :: [Name M.Ty] -> Name M.Ty -> Name M.Ty
            renumber rhs n = fromMaybe n $ find ((== n2s n) . n2s) rhs

transTySig :: (Fresh m, MonadError AstError m) => Renamer -> [(S.Name (), M.Ty)] -> Decl Annote -> m [(S.Name (), M.Ty)]
transTySig rn sigs = \ case
      TypeSig _ names t -> do
            t' <- transTy rn t
            pure $ zip (map void names) (repeat t') <> sigs
      _                 -> pure sigs

-- TODO(chathhorn): should be a map, not a fold
transDef :: (Fresh m, MonadError AstError m) => Renamer -> [(S.Name (), M.Ty)] -> Map (S.Name ()) M.DefnAttr -> [M.Defn] -> Decl Annote -> m [M.Defn]
transDef rn tys inls defs = \ case
      PatBind l (PVar _ (void -> x)) (UnGuardedRhs _ e) Nothing -> do
            k <- freshKVar "def"
            let x' = s2n $ rename Value rn x
                t  = fromMaybe (M.TyVar l k $ s2n "a") $ lookup x tys
            -- Elide definition of primitives. Allows providing alternate defs for GHC compat.
            e' <- if | M.isPrim x' -> pure $ M.mkError (ann e) t $ "Prim: " <> n2s x'
                     | otherwise   -> transExp rn e
            pure $ M.Defn l x' (Embed $ M.poly' t) (Map.lookup x inls) (Embed (bind [] e')) : defs
      DataDecl       {}                                         -> pure defs -- TODO(chathhorn): elide
      InlineSig      {}                                         -> pure defs -- TODO(chathhorn): elide
      TypeSig        {}                                         -> pure defs -- TODO(chathhorn): elide
      InfixDecl      {}                                         -> pure defs -- TODO(chathhorn): elide
      TypeDecl       {}                                         -> pure defs -- TODO(chathhorn): elide
      AnnPragma      {}                                         -> pure defs -- TODO(chathhorn): elide
      MinimalPragma  {}                                         -> pure defs -- TODO(chathhorn): elide
      CompletePragma {}                                         -> pure defs -- TODO(chathhorn): elide
      RulePragmaDecl {}                                         -> pure defs -- TODO(chathhorn): elide
      DeprPragmaDecl {}                                         -> pure defs -- TODO(chathhorn): elide
      WarnPragmaDecl {}                                         -> pure defs -- TODO(chathhorn): elide
      d                                                         -> failAt (ann d) $ "Unsupported definition syntax: " <> pack (show $ void d)

transTyVar :: S.TyVarBind () -> Name M.Ty
transTyVar = \ case
      S.UnkindedVar _ x -> mkUId x
      S.KindedVar _ x _ -> mkUId x

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
      TyVar l x        -> M.TyVar l <$> freshKVar (pack $ prettyPrint x) <*> pure (mkUId $ void x)
      TyList l a       -> M.listTy l <$> transTy rn a
      TyPromoted l (PromotedInteger _ n _)
            | n >= 0   -> pure $ M.TyNat l $ fromInteger n
      TyInfix l a x b -> M.TyApp l <$> (M.TyApp l (M.TyCon l (s2n $ rename Type rn x')) <$> transTy rn a) <*> transTy rn b
            where x' | PromotedName   _ qn <- x = qn
                     | UnpromotedName _ qn <- x = qn
      t                -> failAt (ann t) $ "Unsupported type syntax: " <> pack (show t)

freshKVar :: Fresh m => Text -> m M.Kind
freshKVar n = M.KVar <$> fresh (s2n $ "?K_" <> n)

transExp :: (Fresh m, MonadError AstError m) => Renamer -> Exp Annote -> m M.Exp
transExp rn = \ case
      App l e1 e2            -> M.App l <$> transExp rn e1 <*> transExp rn e2
      Lambda l [PVar _ x] e  -> do
            e' <- transExp (exclude Value [void x] rn) e
            pure $ M.Lam l (M.TyBlank l) $ bind (mkUId $ void x) e'
      Var l x | Just b <- builtin x
                             -> pure $ M.Builtin l (M.TyBlank l) b
      Var l x                -> pure $ M.Var l (M.TyBlank l) $ s2n $ rename Value rn x
      Con l x                -> pure $ M.Con l (M.TyBlank l) $ s2n $ rename Value rn x
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
      Lit l (Int _ n _)      -> pure $ M.LitInt l n
      Lit l (String _ s _)   -> pure $ M.LitStr l $ pack s
      List l es              -> M.LitList l (M.TyBlank l) <$> mapM (transExp rn) es
      ExpTypeSig l e t       -> M.TypeAnn l <$> (M.poly' <$> transTy rn t) <*> transExp rn e
      e                      -> failAt (ann e) $ "Unsupported expression syntax: " <> pack (show $ void e)
      where getVars :: Pat Annote -> [S.Name ()]
            getVars = runQ $ query $ \ case
                  PVar (_::Annote) x -> [void x]
                  _                  -> []

            builtin :: QName Annote -> Maybe M.Builtin
            builtin = M.builtin . pack . prettyPrint . name . rename Value rn

            isError :: QName Annote -> Bool
            isError n = prettyPrint (name $ rename Value rn n) == "error"

            isFromList :: QName Annote -> Bool
            isFromList n = prettyPrint (name $ rename Value rn n) == "fromList"

transPat :: MonadError AstError m => Renamer -> Pat Annote -> m M.Pat
transPat rn = \ case
      PApp l x ps -> M.PatCon l (Embed (M.TyBlank l)) (Embed $ s2n $ rename Value rn x) <$> mapM (transPat rn) ps
      PVar l x    -> pure $ M.PatVar l (Embed (M.TyBlank l)) (mkUId $ void x)
      PWildCard l -> pure $ M.PatWildCard l (Embed (M.TyBlank l))
      p           -> failAt (ann p) $ "Unsupported syntax in a pattern: " <> pack (show $ void p)

-- Note: runs before desugaring.
-- TODO(chathhorn): GADT style decls?
extendWithGlobs :: Annotation a => S.Module a -> Renamer -> Renamer
extendWithGlobs = \ case
      Module l (Just (ModuleHead _ (ModuleName _ m) _ _)) _ _ ds | isPrimMod m -> extendWith ds $ ModuleName l ""
      Module _ (Just (ModuleHead _ m _ _)) _ _ ds                              -> extendWith ds m
      _                                                                        -> id
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
                  TypeDecl _ hd _       -> [fst $ sDeclHead hd]
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
      S.Module _ (Just (ModuleHead _ (ModuleName _ m) _ _)) _ _ _ | isPrimMod m     -> [] -- all imports ignored in the Prim module.
      S.Module l (Just (ModuleHead _ (ModuleName _ "ReWire.Prelude") _ _)) _ imps _ -> addMod l "ReWire" $ filter (not . isMod "Prelude") imps -- TODO(chathhorn) gah!
      S.Module l _ _ imps _                                                         -> addMod l "ReWire.Prelude" $ addMod l "ReWire" $ filter (not . isMod "Prelude") imps
      S.XmlPage {}                                                                  -> []
      S.XmlHybrid l _ _ imps _ _ _ _ _                                              -> addMod l "ReWire.Prelude" $ addMod l "ReWire" $ filter (not . isMod "Prelude") imps
      where addMod :: Annotation a => a -> Text -> [ImportDecl a] -> [ImportDecl a]
            addMod l m imps = if any (isMod m) imps then imps
                  else ImportDecl l (ModuleName l (unpack m)) False False False Nothing Nothing Nothing : imps

            isMod :: Annotation a => Text -> ImportDecl a -> Bool
            isMod m ImportDecl { importModule = ModuleName _ n } = pack n == m
