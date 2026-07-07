{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | The IR-agnostic part of the HSE front end: locating, parsing, and
--   caching modules and (recursively) their imports. The per-module
--   pipeline (desugaring and translation out of the HSE AST) is supplied
--   by the caller; the embedder instantiates it for Atmo (see
--   'Embedder.ModCache').
module ReWire.HSE.Cache
      ( Cache
      , LoadPath
      , runCache
      , getModuleWith
      ) where

import ReWire.Annotation (Annotation)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import ReWire.Config (Config, loadPath, pDebug)
import ReWire.Error (failAt, AstError, MonadError, filePath, relocatingTo)
import ReWire.HSE.Desugar (addMainModuleHead)
import ReWire.HSE.Parse (tryParseInDir)
import ReWire.HSE.Rename (Exports, Renamer, fromImps, toFilePath)
import ReWire.HSE.Globs (extendWithGlobs, getImps)
import ReWire.Pretty (showt)

import Control.Arrow ((***))
import Control.Lens ((^.))
import Control.Monad (msum, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (runStateT, StateT, MonadState (..), modify)
import Data.HashMap.Strict (HashMap)
import Data.Text (pack)
import Language.Haskell.Exts.Syntax (ImportDecl (..))
import System.FilePath ((</>), takeDirectory)

import qualified Data.HashMap.Strict          as Map
import qualified Language.Haskell.Exts.Syntax as S (Module (..))

type LoadPath = [FilePath]
type Cache mod m = StateT (HashMap FilePath (mod, Exports)) m

runCache :: Monad m => Cache mod m a -> m a
runCache m = fst <$> runStateT m mempty

-- | Locates a module in the load path, parses it, loads its imports
--   (recursively, with caching), and runs the supplied translation, which
--   takes the module's file path, renamer, loaded imports, and HSE AST to
--   the target IR. The translation's result is cached per file path.
getModuleWith :: forall mod m. (MonadIO m, MonadError AstError m, Monoid mod)
      => (FilePath -> Renamer -> mod -> S.Module SrcSpanInfo -> Cache mod m (mod, Exports))
      -> Config -> FilePath -> FilePath -> Cache mod m (mod, Exports)
getModuleWith translate conf pwd fp = pDebug conf ("Fetching module: " <> pack fp <> " (pwd: " <> pack pwd <> ")") >> Map.lookup fp <$> get >>= \ case
      Just p  -> pure p
      Nothing -> do
            modify $ Map.insert fp mempty

            let lp     = pwd : conf^.loadPath

            mmods      <- mapM (tryParseInDir fp) lp
            (pwd', m)  <- maybe
                              (failAt (filePath fp) $ "File not found in load-path: " <> showt lp)
                              (pure . (elideDot *** addMainModuleHead))
                        $ msum mmods

            rn         <- mkRenamer pwd' m
            imps       <- loadImports pwd' m

            p          <- translate fp rn imps m

            modify $ Map.insert fp p
            pure p

      where mkRenamer :: FilePath -> S.Module SrcSpanInfo -> Cache mod m Renamer
            mkRenamer pwd' m = extendWithGlobs m . mconcat <$> mapM mkRenamer' (getImps m)
                  where mkRenamer' :: ImportDecl SrcSpanInfo -> Cache mod m Renamer
                        mkRenamer' (ImportDecl l (void -> m) quald _ _ _ (fmap void -> as) specs) = do
                              (_, exps) <- relocatingTo l $ getModuleWith translate conf pwd' $ toFilePath m
                              fromImps m quald exps as specs

            loadImports :: Annotation a => FilePath -> S.Module a -> Cache mod m mod
            loadImports pwd' = fmap mconcat . mapM loadImp . getImps
                  where loadImp imp@(ImportDecl l _ _ _ _ _ _ _) = fst <$> relocatingTo l (getModuleWith translate conf pwd' $ toFilePath $ void $ importModule imp)

            elideDot :: FilePath -> FilePath
            elideDot = \ case
                  "." -> takeDirectory fp
                  d   -> d </> takeDirectory fp
