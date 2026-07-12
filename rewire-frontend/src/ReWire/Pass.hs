{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
-- | Scaffolding for running, tracing, and dumping pipeline passes, shared
--   between the compiler (ReWire.ModCache, ReWire.FrontEnd) and the embedder
--   (Embedder.ModCache).
module ReWire.Pass
      ( pass
      , printHeader
      , verb'
      ) where

import ReWire.Annotation (unAnn)
import ReWire.Config (Config, dump, outFile, verbose, pDebug)
import ReWire.Pretty (showt)

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Numeric.Natural (Natural)
import System.FilePath ((-<.>), (<.>))
import Text.Pretty.Simple (pShowOpt, defaultOutputOptionsNoColor)

import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Lazy as TL

-- | Run one numbered, named pipeline pass: announce it (verbose only, as
--   "[n] name"), run the transformation, then write the rendering of its
--   output to a file beside the output file when -d n (or --dump-all) is on
--   -- so "-d n" reads "dump the IR after pass n". The dump file is the
--   output file (or the given source file) with its extension replaced by
--   "<n>.<ext>" (e.g., MiniISA.6.eir); ext names the IR's textual format,
--   which the render argument produces. Under -v, the dump also carries the
--   IR's show output (indented by pretty-simple) in a comment, so it stays
--   parseable. Numbers are assigned consecutively at the call sites
--   (ReWire.ModCache and ReWire.FrontEnd for the compiler).
pass :: forall m a b. (MonadIO m, Data b, Show b) => Config -> FilePath -> Natural -> Text -> String -> (b -> Text) -> (a -> m b) -> a -> m b
pass conf fp n name ext render f a = do
      pDebug conf msg
      b <- f a
      when ((conf^.dump) n) $ do
            let fout = fromMaybe fp (conf^.outFile) -<.> (show n <.> ext)
            pDebug conf $ "Dumping the IR after pass " <> showt n <> " to file: " <> pack fout
            liftIO $ T.writeFile fout $ dumpText b
      pure b
      where msg :: Text
            msg = "[" <> showt n <> "] " <> name

            dumpText :: b -> Text
            dumpText b = "-- # " <> msg <> "\n\n" <> render b <> "\n"
                  <> (if conf^.verbose then "\n" <> comment ("## Show:\n\n" <> indented b) else mempty)

            indented :: b -> Text
            indented = TL.toStrict . pShowOpt defaultOutputOptionsNoColor . unAnn

            comment :: Text -> Text
            comment = T.unlines . map ("-- " <>) . T.lines

printHeader :: MonadIO m => Text -> m ()
printHeader hd = do
      liftIO $ T.putStrLn   "-- # ==================================================="
      liftIO $ T.putStrLn $ "-- # " <> hd
      liftIO $ T.putStrLn   "-- # ===================================================\n"

-- | Print a debug message (verbose only), passing the program through.
verb' :: MonadIO m => Config -> Text -> a -> m a
verb' conf s a = pDebug conf s >> pure a
