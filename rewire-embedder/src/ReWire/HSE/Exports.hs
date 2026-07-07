{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | Export-list resolution on the HSE AST, used by the embedder
--   (Embedder.HSE.ToAtmo).
module ReWire.HSE.Exports
      ( Export (..)
      , sDeclHead
      , exportAll
      , getExportFixities
      , transExport
      , tysynNames
      , getTypeExports
      , resolveExports
      , getInlines
      ) where

import ReWire.Annotation (Annote)
import ReWire.Error (failAt, MonadError, AstError)
import ReWire.HSE.Fixity (getFixities)
import ReWire.HSE.Rename

import Control.Monad (void)
import Data.Char (isUpper)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Language.Haskell.Exts.Fixity (Fixity (..))

import qualified Data.Set                     as Set
import qualified Data.Map.Strict              as Map
import qualified Language.Haskell.Exts.Syntax as S

import Language.Haskell.Exts.Syntax hiding (Annotation, Name, Kind)

-- | An intermediate form for exports. TODO(chathhorn): get rid of it.
data Export = Export FQName
            -- ExportWith: Type name, ctors
            | ExportWith FQName (Set FQName) FQCtorSigs
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

tysynNames :: [Decl Annote] -> [S.Name ()]
tysynNames = concatMap tySynName
      where tySynName :: Decl Annote -> [S.Name ()]
            tySynName = \ case
                  TypeDecl _ (sDeclHead -> hd) _ -> [fst hd]
                  _                              -> []

getTypeExports :: Renamer -> [Decl Annote] -> [Export]
getTypeExports rn ds = map (exportAll rn) $ Set.toList (getLocalTypes rn) <> tysynNames ds

resolveExports :: Renamer -> [Export] -> Exports
resolveExports rn = foldr (resolveExport rn) mempty
      where resolveExport :: Renamer -> Export -> Exports -> Exports
            resolveExport rn = \ case
                  Export x              | isValueName x -> expValue x
                  Export x                              -> expType x mempty mempty
                  ExportAll x                           -> expType x (lookupCtors rn x) (lookupCtorSigsForType rn x)
                  ExportWith x cs fs                    -> expType x cs fs
                  ExportMod m                           -> (<> getExports m rn)
                  ExportFixity asc lvl x                -> expFixity asc lvl x

            isValueName :: FQName -> Bool
            isValueName = \ case
                  (name -> Ident _ (c : _)) | isUpper c -> False
                  _                                     -> True

-- | Collect INLINE/NOINLINE pragmas into a map from the pragma'd name to an
--   attribute built from the pragma's polarity (True for INLINE).
getInlines :: (Bool -> a) -> [Decl Annote] -> Map (S.Name ()) a
getInlines attr = foldr inl' mempty
      where inl' = \ case
                  InlineSig _ b Nothing (Qual _ _ x) -> Map.insert (void x) $ attr b
                  InlineSig _ b Nothing (UnQual _ x) -> Map.insert (void x) $ attr b
                  _                                  -> id
