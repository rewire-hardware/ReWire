{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | The two module-loader helpers the HSE front end's recursive import
--   loader (ReWire.HSE.Cache) needs: computing a module's global-name
--   renamer extension and its effective import list.
module ReWire.HSE.Globs (extendWithGlobs, getImps) where

import ReWire.Annotation (Annotation)
import ReWire.HSE.Exports (sDeclHead)
import ReWire.HSE.Rename

import Control.Arrow ((&&&), second)
import Control.Monad (void)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Text (Text, pack, unpack)

import qualified Data.Set                     as Set
import qualified Data.Map.Strict              as Map
import qualified Language.Haskell.Exts.Syntax as S

import Language.Haskell.Exts.Syntax hiding (Annotation, Name, Kind)

isPrimMod :: String -> Bool
isPrimMod = (== "RWC.Primitives")

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

            getCtor :: QualConDecl l -> Set (S.Name ())
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
