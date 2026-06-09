{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module Embedder.ModCache
      ( runCache
      , LoadPath
      , getModule
      , printInfoHSE
      , printHeader
      ) where

import ReWire.Annotation (Annotation, SrcSpanInfo, unAnn, noAnn)
import Embedder.Config (Config, verbose, dump, loadPath)
import Embedder.Builtins (builtins)
import qualified Embedder.Atmo.Syntax as A (Module (..), Defn (..), FreeProgram (..), FunBinding (..), Ty (..), TyConId (..), DataConId (..), Exp (..), untype)
import qualified Embedder.Atmo.ToIsabelle as AtmoIsabelle
import ReWire.Error (failAt, AstError, MonadError, filePath)
import ReWire.HSE.Annotate (annotate)
import Embedder.HSE.Desugar (desugar, addMainModuleHead)
import ReWire.HSE.Parse (tryParseInDir)
import Embedder.HSE.Rename (Exports, Renamer, fromImps, allExports, toFilePath, fixFixity)
import Embedder.HSE.ToAtmo (extendWithGlobs, toAtmo, getImps)
import ReWire.Pretty (Pretty (..), prettyPrint, prettyPrint', fastPrint, showt)

import Control.Lens ((^.))
import Control.Arrow ((***))
import Control.Monad ((>=>), msum, void, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State.Strict (runStateT, StateT, MonadState (..), modify, lift)
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text, pack)
import Language.Haskell.Exts.Syntax hiding (Annotation, Exp, Module (..), Namespace, Name, Kind)
import Numeric.Natural (Natural)
import System.FilePath ((</>), takeDirectory, takeBaseName)

import qualified Data.HashMap.Strict          as Map
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Language.Haskell.Exts.Pretty as P
import qualified Language.Haskell.Exts.Syntax as S (Module (..))
import qualified Embedder.Config                as C
import Embedder.Atmo.Desugar (desugarAtmo)
import qualified Data.Foldable

type Cache m = StateT ModCache m
type LoadPath = [FilePath]
type ModCache = HashMap FilePath (A.Module, Exports)

runCache :: (MonadIO m, MonadError AstError m) => Cache m a -> m a
runCache m = fst <$> runStateT m mempty

mkRenamer :: (MonadFail m, MonadIO m, MonadError AstError m, MonadState AstError m) => Config -> FilePath -> S.Module SrcSpanInfo -> Cache m Renamer
mkRenamer conf pwd' m = extendWithGlobs m . mconcat <$> mapM mkRenamer' (getImps m)
      where mkRenamer' :: (MonadFail m, MonadIO m, MonadError AstError m, MonadState AstError m) => ImportDecl SrcSpanInfo -> Cache m Renamer
            mkRenamer' (ImportDecl _ (void -> m) quald _ _ _ (fmap void -> as) specs) = do
                  (_, exps) <- getModule conf pwd' $ toFilePath m
                  fromImps m quald exps as specs

-- Pass 1    Parse.
-- Pass 2-4  Fixity fixing (uniquify + fix + deuniquify, because bug in applyFixities).
-- Pass 5    Annotate.
-- Pass 6-14 Desugar.
-- Pass 15   Translate to crust + rename globals.
-- Pass 16   Translate to core

getModule :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m) => Config -> FilePath -> FilePath -> Cache m (A.Module, Exports)
getModule conf pwd fp = pDebug conf ("Fetching module: " <> pack fp <> " (pwd: " <> pack pwd <> ")") >> Map.lookup fp <$> get >>= \ case
      Just p  -> pure p
      Nothing -> do
            modify $ Map.insert fp mempty

            let lp       = pwd : conf^.loadPath
            let filename = takeBaseName fp

            mmods      <- mapM (tryParseInDir fp) lp
            -- FIXME: The directory crawling could be more robust here. (Should
            -- use exception handling.)
            (pwd', m)  <- maybe
                              (failAt (filePath fp) $ "File not found in load-path: "
                                                    <> showt lp)
                              (pure . (elideDot *** addMainModuleHead))
                        $ msum mmods

            rn         <- mkRenamer conf pwd' m
            imps       <- loadImports pwd' m

            -- Phase 1 (haskell-src-exts) transformations.
            (m', exps) <- pure
                      >=> pDebug' "Fixing fixity."
                      >=> lift . fixFixity rn
                      >=> pDebug' "Annotating."
                      >=> pure . annotate
                      >=> pDebug' "[Pass 1] Pre-desugaring."
                      >=> whenDump 1 (printInfoHSE "[Pass 1] Haskell: Pre-desugaring" rn imps)
                      >=> pDebug' "HSE Desugaring."
                      >=> desugar rn
                      >=> pDebug' "[Pass 2] Post-desugaring."
                      >=> whenDump 2 (printInfoHSE "[Pass 2] Haskell: Post-desugaring" rn imps)
                      >=> pDebug' "[Pass 51] Translating to atmo."
                      >=> toAtmo rn
                      >=> whenDump 3 (printInfoAtmo "[Pass 51] Atmo: Pre-embedding" rn imps)
                      >=> embedAtmo (C.getEmbedFile conf filename)

                          
                      $ m

            modify $ Map.insert fp (m' <> imps, exps)
            pure (m' <> imps, exps)

      where loadImports :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m, Annotation a) => FilePath -> S.Module a -> Cache m A.Module
            loadImports pwd' = fmap mconcat . mapM (fmap fst . getModule conf pwd' . toFilePath . void . importModule) . getImps

            whenDump :: Applicative m => Natural -> (Bool -> a -> m a) -> a -> m a
            whenDump n m = if (conf^.dump) n then m $ conf^.verbose else pure

            embedAtmo :: (MonadError AstError m, MonadIO m) => FilePath -> (A.Module, Exports) -> m (A.Module, Exports)
            embedAtmo filename (m@(A.Module ddefs rdefs tsyns defs),es) = do
                  let fout = C.getEmbedFile conf filename
                  a' <- AtmoIsabelle.embedModule fout m
                  Data.Foldable.forM_ a' (embedAST fout)
                  return (m,es)

                        

            embedAST :: (MonadError AstError m, MonadIO m, Pretty a) =>  String -> a -> m ()
            embedAST fout a = do
                  liftIO $ T.writeFile fout (if conf^.C.pretty then prettyPrint a else fastPrint a)

            pDebug' :: MonadIO m => Text -> a -> m a
            pDebug' s a = pDebug conf (pack fp <> ": " <> s) >> pure a

            elideDot :: FilePath -> FilePath
            elideDot = \ case
                  "." -> takeDirectory fp
                  d   -> d </> takeDirectory fp

pDebug :: MonadIO m => Config -> Text -> m ()
pDebug conf s = when (conf^.verbose) $ liftIO $ T.putStrLn $ "Debug: " <> s

printHeader :: MonadIO m => Text -> m ()
printHeader hd = do
      liftIO $ T.putStrLn   "# ======================================="
      liftIO $ T.putStrLn $ "# " <> hd
      liftIO $ T.putStrLn   "# =======================================\n"

printInfoHSE :: MonadIO m => Text -> Renamer -> A.Module -> Bool -> S.Module a -> m (S.Module a)
printInfoHSE hd rn imps verbose hse = do
      printHeader hd
      when verbose $ liftIO $ T.putStrLn "\n## Renamer:\n"
      when verbose $ liftIO $ T.putStrLn $ showt rn
      when verbose $ liftIO $ T.putStrLn "\n## Exports:\n"
      when verbose $ liftIO $ T.putStrLn $ showt $ allExports rn
      when verbose $ liftIO $ T.putStrLn "\n## Show imps:\n"
      when verbose $ liftIO $ T.putStrLn $ showt imps
      when verbose $ liftIO $ T.putStrLn "\n## Show HSE mod:\n"
      when verbose $ liftIO $ T.putStrLn $ showt $ void hse
      when verbose $ liftIO $ T.putStrLn "\n## Pretty HSE mod:\n"
      liftIO $ putStrLn $ P.prettyPrint $ void hse
      pure hse

printInfoAtmo :: MonadIO m => Text -> Renamer -> A.Module -> Bool -> (A.Module, Exports) -> m (A.Module, Exports)
printInfoAtmo hd rn imps verbose hse = do
      printHeader hd
      when verbose $ liftIO $ T.putStrLn "\n## Renamer:\n"
      when verbose $ liftIO $ T.putStrLn $ showt rn
      when verbose $ liftIO $ T.putStrLn "\n## Exports:\n"
      when verbose $ liftIO $ T.putStrLn $ showt $ allExports rn
      when verbose $ liftIO $ T.putStrLn "\n## Show imps:\n"
      when verbose $ liftIO $ T.putStrLn $ showt imps
      when verbose $ liftIO $ T.putStrLn "\n## Show HSE mod:\n"
      when verbose $ liftIO $ T.putStrLn $ showt $ void hse
      when verbose $ liftIO $ T.putStrLn "\n## Pretty HSE mod:\n"
      liftIO $ T.putStrLn $ prettyPrint $ fst hse
      pure hse
