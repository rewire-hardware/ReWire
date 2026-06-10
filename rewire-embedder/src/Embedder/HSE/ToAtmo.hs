{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Safe #-}
module Embedder.HSE.ToAtmo (toAtmo) where

import ReWire.Annotation hiding (ann)
import ReWire.Error
import ReWire.HSE.Exports (Export (..), sDeclHead, getExportFixities, transExport, getTypeExports, resolveExports, getInlines)
import ReWire.HSE.Rename
import Embedder.Atmo.Types ((|->))
import ReWire.SYB (query)

import Control.Monad (foldM, void, when)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Text as T (Text, pack, replicate, splitOn)
import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified Data.Map.Strict              as Map
import qualified Language.Haskell.Exts.Syntax as S
import qualified Embedder.Builtins              as M
import qualified Embedder.Atmo.Syntax          as M
import qualified Embedder.Atmo.Util            as M
import qualified Embedder.Atmo.Types           as M

import Language.Haskell.Exts.Syntax hiding (Annotation, Name, Kind)
import Embedder.Atmo.Util (isTupleCtor)

-- import Debug.Trace (trace)
-- import Embedder.Builtins (tb2s)


-- Name Handling:
      -- Remove s2n after renames [and in mkUid]
      -- Remove Embed
      -- remove n2s
      -- FOR NOW: remove renumber function
      -- remove binds (pretty straightforward, one for each constructor that used it)

type RecEnv = Map T.Text [T.Text]

isPrimMod :: String -> Bool
isPrimMod = (== "RWC.Primitives")

{- Name Handling
mkUId :: S.Name a -> Name b
mkUId = \ case
      Ident _ n  -> s2n (pack n)
      Symbol _ n -> s2n (pack n)
-}
mkUId :: S.Name a -> T.Text
mkUId = \ case
      Ident _ n  -> T.pack n
      Symbol _ n -> T.pack n

-- | Translate a Haskell module into the ReWire abstract syntax.
-- Name Handling: Fresh m, rename returns an FQName which is then T.packaged into an Export
toAtmo :: (MonadError AstError m) => Renamer -> Module Annote -> m (M.Module, Exports)
toAtmo rn = \ case
      Module _ (Just (ModuleHead _ (ModuleName _ mname) _ exps)) _ _ (reverse -> ds) -> do
            tyDefs <- foldM (transData rn) [] ds
            (recDefs,recEnv) <- transRecs rn ds
            -- (tyDefs, recDefs) <- partitionEithers $ map (transDataOrRec rn) ds
            tySyns <- if | isPrimMod mname -> pure [] -- TODO(chathhorn): ignore type synonyms in Embedder.hs module.
                         | otherwise       -> foldM (transTyDecl rn) [] ds
            tySigs <- foldM (transTySig rn) [] ds
            fnDefs <- foldM (transDef rn recEnv tySigs inls) [] ds
            exps'  <- maybe (pure $ getGlobExps rn ds) (\ (ExportSpecList _ exps') -> foldM (transExport rn ds) [] exps') exps
            pure (M.Module tyDefs recDefs tySyns fnDefs, resolveExports rn exps')
            where getGlobExps :: Renamer -> [Decl Annote] -> [Export]
                  getGlobExps rn ds = getTypeExports rn ds <> getExportFixities ds <> concatMap (getFunExports rn) ds

                  getFunExports :: Renamer -> Decl Annote -> [Export]
                  getFunExports rn = \ case
                        PatBind _ (PVar _ n) _ _ -> [Export $ rename Value rn $ void n]
                        FunBind _ ms -> map (getMatchExport rn) ms
                        _                        -> []

                  getMatchExport :: Renamer -> Match Annote -> Export
                  getMatchExport rn (Match _ n _ _ _) = Export $ rename Value rn $ void n
                  getMatchExport _ _ = error "getMatchExport: not a supported Match"

                  inls :: Map (S.Name ()) M.DefnAttr
                  inls = getInlines (\ b -> if b then M.Inline else M.NoInline) ds
      m                                                                -> failAt (ann m) "Unsupported module syntax"

isRecDecl :: QualConDecl l -> Bool
isRecDecl (QualConDecl _ _ _ (RecDecl {})) = True
isRecDecl _ = False

getRecFields :: [QualConDecl l] -> Maybe [(S.Name (), Type ())]
getRecFields [QualConDecl _ _ _ (RecDecl _ _ fields)]
  | all isRecDecl [QualConDecl undefined undefined undefined (RecDecl undefined undefined fields)] =
      Just $ concatMap flattenField fields
      where
            flattenField :: FieldDecl l -> [(S.Name (), Type ())]
            flattenField (FieldDecl _ ns ty) = [(void n, void ty) | n <- ns]
getRecFields _ = Nothing

getRecConName :: [QualConDecl l] -> Maybe (S.Name l)
getRecConName [QualConDecl _ _ _ (RecDecl _ con _)] = Just con
getRecConName _ = Nothing

-- Name Handling: Fresh m, rename to T.Text
transRec :: (MonadError AstError m) => Renamer -> [M.RecDefn] -> Decl Annote -> m [M.RecDefn]
transRec rn acc = \ case
      DataDecl l _ _ (sDeclHead -> (name,vars)) cons _ 
        | Just flatFields <- getRecFields cons
        , all isRecDecl cons ->
            do
            when (length cons > 1) $
                  failAt l $ "Multi-constructor record type `" <> unqualText name <> "` is not currently supported (try using a single-constructor record instead)"
            let tvs   = map transTyVar $ vars
                n     = rename Type rn $ name
                retTy = M.mkTyApp l (M.TyCon l n) (map (M.TyVar l) tvs)
            tys'  <- mapM (transTy rn . fmap (const l) . snd) flatFields
            let fieldNames = map (unqualText . fst) flatFields
                fields' = zip fieldNames tys'
                poly    = M.poly' (M.sig tys' retTy)
            pure $ M.RecDefn l n tvs poly fields' : acc
      _                      -> pure acc


buildRecEnv :: [Decl Annote] -> RecEnv
buildRecEnv decls = Map.fromList . mapMaybe extract $ decls
  where
    extract (DataDecl _ _ _ (sDeclHead -> (_, _)) cons _)
      | Just flatFields <- getRecFields cons
      , Just con <- getRecConName cons
      , all isRecDecl cons
      , length cons == 1 =
          Just (unqualText con, map (unqualText . fst) flatFields)
    extract _ = Nothing

transRecs :: (MonadError AstError m) => Renamer -> [Decl Annote] -> m ([M.RecDefn], RecEnv)
transRecs rn decls = do
  recDefs <- foldM (transRec rn) [] decls
  let recEnv = buildRecEnv decls
  pure (recDefs, recEnv)

transData :: (MonadError AstError m) => Renamer -> [M.DataDefn] -> Decl Annote -> m [M.DataDefn]
transData rn acc = \ case
      DataDecl l _ _ (sDeclHead -> (name,vars)) cons _ ->
            if any isRecDecl cons
            then pure acc
            else
                  do
                  let n    = rename Type rn $ name
                      tvs' = map transTyVar $ vars
                  cs'  <- mapM (transCon rn tvs' n) cons
                  pure $ M.DataDefn l n tvs' cs' : acc
      _                                       -> pure acc

-- Name Handling: Fresh m, rename returns T.Text
transTyDecl :: (MonadError AstError m) => Renamer -> [M.TypeSynonym] -> Decl Annote -> m [M.TypeSynonym]
transTyDecl rn syns = \ case
      TypeDecl l (sDeclHead -> hd) t -> do
            let n   = rename Type rn $ fst hd
                lhs = map transTyVar $ snd hd
            t'  <- transTy rn t
            -- let rhs :: [Name M.Ty]
            --     rhs = fv t'

            -- let tvs' = map (renumber rhs) lhs
            pure $ M.TypeSynonym l n (lhs |-> t') : syns
      _                              -> pure syns

      -- where renumber :: [Name M.Ty] -> Name M.Ty -> Name M.Ty
      --       renumber rhs n = fromMaybe n $ find ((== n2s n) . n2s) rhs
-- FOR NOW: Let's operate on the assumption that renumber does nothing...
      -- It seems like all the numbering for all variables on either side of this should be zero.
      -- lhs used to be tvs'
-- Name Handling: renumber selects 'n : Name Ty' from 'rhs : [Name Ty]' by
--   taking one one with a matching n2s (I think that's just the fv name part)
--   and if there isn't one, then use 'n' instead
-- this is used to fix an error in transTy, it seems...
--     lhs = transTyVar (fvs from TypeDecl header)
--     rhs = fv of (transTy of (t from TypeDecl))
--     the tvs' that we keep for our TypeSynonym are lhs, renumbered using rhs 


-- Name Handling: Fresh m
transTySig :: (MonadError AstError m) => Renamer -> [(S.Name (), M.Ty)] -> Decl Annote -> m [(S.Name (), M.Ty)]
transTySig rn sigs = \ case
      TypeSig _ names t -> do
            t' <- transTy rn t
            pure $ zip (map void names) (repeat t') <> sigs
      _                 -> pure sigs

-- TODO(mheim): need the simple allUnguarded Match case, and also the non-var PatBind case
-- TODO(chathhorn): should be a map, not a fold
-- Name Handling: Fresh m, rename returns T.Text
transDef :: (MonadError AstError m) => Renamer -> RecEnv -> [(S.Name (), M.Ty)] -> Map (S.Name ()) M.DefnAttr -> [M.Defn] -> Decl Annote -> m [M.Defn]
transDef rn recEnv tys inls defs = \ case
      PatBind l (PVar _ (void -> x)) (UnGuardedRhs _ e) Nothing -> do
            let x' = rename Value rn x
                t  = fromMaybe (M.TyVar l "a") $ lookup x tys
            -- Elide definition of primitives. Allows providing alternate defs for GHC compat.
            e' <- if | M.isPrim x' -> pure $ M.mkError (ann e) (Just t) $ "Prim: " <> x'
                     | otherwise   -> transExp rn recEnv e
            pure $ M.Defn l x' (M.poly' t) (Map.lookup x inls) [M.FunBinding l [] e'] : defs
      FunBind l ms@(Match _l' (void -> name) _ps _rhs _binds : _) -> do
            let name' = rename Value rn name
                t  = fromMaybe (M.TyVar l "a") $ lookup name tys
            ms' <- mapM (transFunMatch rn recEnv) ms
            pure $ M.Defn l name' (M.poly' t) (Map.lookup name inls) ms' : defs
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
      d                                                         -> failAt (ann d) $ "Unsupported definition syntax: " <> T.pack (show $ void d)


-- We no longer desugar simple function bindings during HSE.Desugar
-- That is, we assume that we can have function bindings with zero guards and zero local bindings
transFunMatch :: (MonadError AstError m) => Renamer -> RecEnv -> Match Annote -> m M.FunBinding
transFunMatch rn recEnv (Match _l name ps (UnGuardedRhs l' e) Nothing) = do
      let name' = rename Value rn name
      e' <- if | M.isPrim name' -> pure $ M.mkError (ann e) Nothing $ "Prim: " <> name'
               | otherwise   -> transExp rn recEnv e
      ps' <- mapM (transPat rn) ps
      pure $ M.FunBinding l' ps' e'
transFunMatch _ _ m = failAt (ann m) "transFunMatch: unsupported Match type"

-- Name Handling: used to return Name a b/c of mkUId
transTyVar :: S.TyVarBind () -> T.Text
transTyVar = \ case
      S.UnkindedVar _ x -> mkUId x
      S.KindedVar _ x _ -> mkUId x

-- Name Handling: Fresh m, rename returns T.Text
   -- previously: Renamer -> [Name M.Ty] -> Name M.TyConId ->
transCon :: (MonadError AstError m) => Renamer -> [T.Text] -> T.Text -> QualConDecl Annote -> m M.DataCon
transCon rn tvs tc = \ case
      QualConDecl l Nothing _ (ConDecl _ x tys) -> do
            let tvs' = map (M.TyVar l) tvs
            tys' <- mapM (transTy rn) tys
            let t = M.sig tys' (M.TyApp l (M.TyCon l tc) tvs')
            -- foldr M.arr (foldl' (M.TyApp l) (M.TyCon l tc) tvs') <$> mapM (transTy rn) tys
            pure $ M.DataCon l (rename Value rn x) (tvs |-> t)
      d                                         -> failAt (ann d) "Unsupported Ctor syntax"

{- Kind Handling:
takes ks : [M.Kind] after Renamer
uses them in creating TyVars (let tvs' = zipWith (M.TyVar l) ks tvs)

-}

--Var l x | Just b <- builtin x
--                             -> pure $ M.Builtin l Nothing Nothing b

flattenTyApp :: Type Annote -> [Type Annote]
flattenTyApp = \ case
      TyApp _l a b -> flattenTyApp a <> [b]
      t           -> [t]

-- Name Handling: Fresh m, rename returns T.Text
transTy :: (MonadError AstError m) => Renamer -> Type Annote -> m M.Ty
transTy rn = \ case
      TyForall _ _ _ t -> transTy rn t
      t@(TyApp l _ _)  -> case flattenTyApp t of
            [] -> error "Impossible flattenTyApp return"
            [_t] -> error "transTy: impossible given value of t"
            (t1 : ts) -> M.TyApp l <$> transTy rn t1 <*> mapM (transTy rn) ts
      TyCon l x | Just b <- tybuiltin (rename Type rn x)
                       -> do
                        -- _ <- trace ("ToAtmo.transTy: TyCon: tybuiltin found: " <> T.unpack (tb2s b)) $ pure ()
                        pure $ M.TyBuiltin l b
      TyCon l x -> pure $ M.TyCon l (rename Type rn x)
      TyVar l x        -> (pure (M.TyVar l (mkUId $ void x)))
      TyList l a       -> M.listTy l <$> transTy rn a
      TyTuple l _ ts     -> do
                        ts' <- mapM (transTy rn) ts
                        pure $ M.TyTuple l ts'
      TyPromoted l (PromotedInteger _ n _)
            | n >= 0   -> pure $ M.TyNat l $ fromInteger n
      TyInfix l a x b -> M.TyApp l (M.TyCon l (rename Type rn x')) <$> sequence [transTy rn a , transTy rn b]
            where x' | PromotedName   _ qn <- x = qn
                     | UnpromotedName _ qn <- x = qn
      t                -> failAt (ann t) $ "Unsupported type syntax: " <> T.pack (show t)
      where 
            tybuiltin :: S.Name Annote -> Maybe M.TyBuiltin
            tybuiltin = M.tybuiltin . T.pack . prettyPrint . name . rename Value rn

-- Name Handling: Fresh m, rename returns T.Text, except builtin function where 'name' is called on the return value
transExp :: (MonadError AstError m) => Renamer -> RecEnv -> Exp Annote -> m M.Exp
transExp rn recEnv = \ case
      App l e1 e2            -> do
            let es = flattenApp (App l e1 e2)
            es' <- mapM (transExp rn recEnv) es
            case es' of 
                  (M.Con _ _ _ cname : args)
                    | Just fieldNames <- Map.lookup (stripModPrefix cname) recEnv
                    , length args == length fieldNames ->
                        pure $ M.RecVal l Nothing Nothing (zip fieldNames args)
                  (e' : es'') -> pure $ M.mkApp l e' es''
                  _ -> failAt l "transExp: Application with no arguments"
      -- M.mkApp l <$> transExp rn recEnv e1 <*> (pure <$> transExp rn recEnv e2)
      Lambda l [PVar a x] e  -> do
            (vs',e') <- flattenLam rn (Lambda l [PVar a x] e)
            pure $ M.Lam l Nothing Nothing vs' e'  -- vs' =? map (mkUId . void) vs
      Var l x | Just b <- rwUserDef x
                             -> pure $ M.RWUser l Nothing Nothing b
      Var l x | Just b <- builtins x
                             -> pure $ M.RWUser l Nothing Nothing (M.RWBuiltin b)
      Var l x                -> pure $ M.Var l Nothing Nothing $ rename Value rn x
      Con l x                -> pure $ M.Con l Nothing Nothing $ rename Value rn x
      Case l e alts -> do
            e'  <- transExp rn recEnv e
            alts' <- mapM transAlt alts
            pure $ M.Case l Nothing Nothing e' alts'
      Tuple l _ es           -> do
            es' <- mapM (transExp rn recEnv) es
            pure $ M.Tuple l Nothing Nothing es'
      Lit l (Int _ n _)      -> pure $ M.LitInt l Nothing n
      Lit l (String _ s _)   -> pure $ M.LitStr l Nothing $ T.pack s
      List l es              -> M.LitList l Nothing Nothing <$> mapM (transExp rn recEnv) es
      RecConstr l _ fields -> do 
            fields' <- mapM (transFieldUpdate rn recEnv) fields
            pure $ M.RecVal l Nothing Nothing fields'
      RecUpdate l e fieldUpdates -> do
            e' <- transExp rn recEnv e
            fieldUpdates' <- mapM (transFieldUpdate rn recEnv) fieldUpdates
            pure $ M.RecUpd l Nothing Nothing e' fieldUpdates'
      ExpTypeSig _ e t       -> M.setTyAnn <$> (Just . M.poly' <$> transTy rn t) <*> transExp rn recEnv e
      e                      -> failAt (ann e) $ "Unsupported expression syntax: " <> T.pack (show $ void e)
      where getVars :: Pat Annote -> [S.Name ()]
            getVars p = [void x | PVar (_::Annote) x <- query p]

            transAlt :: (MonadError AstError m) => Alt Annote -> m M.PatBind
            transAlt (Alt _ p (UnGuardedRhs _ e1) _) = do
                  p'  <- transPat rn p
                  e1' <- transExp (exclude Value (getVars p) rn) recEnv e1
                  pure $ M.PatBind p' e1'
            transAlt a = failAt (ann a) $ "Unsupported Alt syntax: " <> T.pack (show $ void a)

            rwUserDef :: QName Annote -> Maybe M.RWUserOp
            rwUserDef = M.qn2rwu . rename Value rn
                  -- M.s2rwu . T.pack . prettyPrint . name . rename Value rn

            builtins :: QName Annote -> Maybe M.Builtin
            builtins =  M.builtin . T.pack . prettyPrint . name . rename Value rn
            
            flattenApp :: Exp Annote -> [Exp Annote]
            flattenApp = \ case
                  App _l e e' -> flattenApp e <> [e']
                  e          -> [e]
            flattenLam :: (MonadError AstError m) => Renamer -> Exp Annote 
                       -> m ([T.Text],M.Exp)
            flattenLam rn = \ case
                  Lambda _ [PVar _ x] e  -> do
                        (vs',e') <- flattenLam rn e -- rn =? exclude Value [void x] rn
                        let x' = rename Value rn x
                        return (x' : vs', e')
                  e -> do
                        e' <- transExp rn recEnv e
                        return ([],e')


-- Name Handling: Fresh m, rename returns T.Text
transPat :: (MonadError AstError m) => Renamer -> Pat Annote -> m M.Pat
transPat rn = \ case
      PApp l x ps | isTupleCtor (rename Value rn x) -> M.PatTuple l Nothing Nothing <$> mapM (transPat rn) ps
      PApp l x ps      -> M.PatCon l Nothing Nothing (rename Value rn x) <$> mapM (transPat rn) ps
      PVar l x         -> pure $ M.PatVar l Nothing Nothing (mkUId $ void x)
      PWildCard l      -> pure $ M.PatWildCard l Nothing Nothing
      PatTypeSig _ p t -> M.setTyAnn <$> (Just . M.poly' <$> transTy rn t) <*> transPat rn p
      PTuple l _b ps   -> M.PatTuple l Nothing Nothing <$> mapM (transPat rn) ps
      PAsPat l n p     -> case p of
            PRec l _ [] -> pure $ M.PatVar l Nothing Nothing (mkUId $ void n)
            _ -> M.PatAs l Nothing Nothing (mkUId $ void n) <$> transPat rn p
      PRec l _ fields -> do
            case fields of
                  [] -> failAt l "Empty record pattern without as-pattern is unsupported or unnecessary"
                  _ -> do
                        fields' <- mapM (transPatField rn) fields
                        pure $ M.PatRec l Nothing Nothing fields' 
      p                -> failAt (ann p) $ "Unsupported syntax in a pattern: " <> T.pack (show $ void p)

transPatField :: MonadError AstError m => Renamer -> PatField Annote -> m (T.Text, M.Pat)
transPatField rn = \case
  PFieldPat _ qname p -> do
    p' <- transPat rn p
    pure (unqualQName qname, p')

  PFieldPun _ qname -> do
    let n = nameFromQName qname
    pure (unqualQName qname, M.PatVar (ann qname) Nothing Nothing (mkUId $ void n))

  PFieldWildcard l ->
    failAt l "Pattern wildcards are not supported in records"




unqualText :: S.Name l -> T.Text
unqualText = \case
  Ident _ s  -> T.pack s
  Symbol _ s -> T.pack s

unqualQName :: QName l -> T.Text
unqualQName = \case
  Qual _ _ name -> unqualText name
  UnQual _ name -> unqualText name
  Special _ s   -> specialToText s  -- optional

specialToText :: SpecialCon l -> T.Text
specialToText = \case
  UnitCon _    -> "()"
  ListCon _    -> "[]"
  FunCon _     -> "->"
  TupleCon _ Boxed n   -> "(" <> T.replicate (n - 1) "," <> ")"
  TupleCon _ Unboxed n -> "(#" <> T.replicate (n - 1) "," <> "#)"
  Cons _       -> ":"
  UnboxedSingleCon _ -> "(# #)"
  _ -> "<SPECIAL>"


nameFromQName :: QName l -> S.Name l
nameFromQName = \case
  Qual _ _ n -> n
  UnQual _ n -> n
  Special _ _ -> error "unexpected special name in field"


transFieldUpdate :: MonadError AstError m => Renamer -> RecEnv -> FieldUpdate Annote -> m (T.Text, M.Exp)
transFieldUpdate rn recEnv = \case
  FieldUpdate _ qname e -> do
    e' <- transExp rn recEnv e
    pure (unqualQName qname, e')

  FieldPun _ qname -> do
    let n = unqualQName qname
    pure (n, M.Var (ann qname) Nothing Nothing (rename Value rn (nameFromQName qname)))

  FieldWildcard l -> failAt l "Field wildcards are not supported"


stripModPrefix :: T.Text -> T.Text
stripModPrefix = last . T.splitOn "."

