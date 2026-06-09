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

import Embedder.Config (Config, verbose, dump, pDebug, getEmbedFile)
import Embedder.HSE.Desugar (desugar)
import qualified Embedder.Atmo.Syntax as A (Module (..))
import qualified Embedder.Atmo.ToIsabelle as AtmoIsabelle
import Embedder.HSE.ToAtmo (toAtmo)
import ReWire.Annotation (SrcSpanInfo)
import ReWire.Error (AstError, MonadError)
import ReWire.HSE.Annotate (annotate)
import ReWire.HSE.Cache (LoadPath, getModuleWith)
import ReWire.HSE.Rename (Exports, Renamer, allExports, fixFixity)
import ReWire.Pretty (Pretty (..), prettyPrint, fastPrint, showt)

import Control.Lens ((^.))
import Control.Monad ((>=>), void, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State.Strict (MonadState, lift)
import Data.Foldable (forM_)
import Data.Text (Text, pack)
import Numeric.Natural (Natural)
import System.FilePath (takeBaseName)

import qualified Data.Text.IO                 as T
import qualified Language.Haskell.Exts.Pretty as P
import qualified Language.Haskell.Exts.Syntax as S (Module (..))
import qualified ReWire.HSE.Cache             as Cache
import qualified Embedder.Config              as C

type Cache m = Cache.Cache A.Module m

runCache :: (MonadIO m, MonadError AstError m) => Cache m a -> m a
runCache = Cache.runCache

getModule :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m) => Config -> FilePath -> FilePath -> Cache m (A.Module, Exports)
getModule conf = getModuleWith translate conf
      where translate :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m) => FilePath -> Renamer -> A.Module -> S.Module SrcSpanInfo -> Cache m (A.Module, Exports)
            translate fp rn imps m = do
                  let filename = takeBaseName fp

                      pDebug' :: MonadIO m => Text -> a -> m a
                      pDebug' s a = pDebug conf (pack fp <> ": " <> s) >> pure a

                      whenDump :: Applicative m => Natural -> (Bool -> a -> m a) -> a -> m a
                      whenDump n f = if (conf^.dump) n then f $ conf^.verbose else pure

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
                            >=> embedAtmo (getEmbedFile conf filename)
                            $ m

                  pure (m' <> imps, exps)

            embedAtmo :: (MonadError AstError m, MonadIO m) => FilePath -> (A.Module, Exports) -> m (A.Module, Exports)
            embedAtmo fout (m, es) = do
                  a' <- AtmoIsabelle.embedModule fout m
                  forM_ a' (embedAST fout)
                  pure (m, es)

            embedAST :: (MonadIO m, Pretty a) => FilePath -> a -> m ()
            embedAST fout a =
                  liftIO $ T.writeFile fout (if conf^.C.pretty then prettyPrint a else fastPrint a)

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
