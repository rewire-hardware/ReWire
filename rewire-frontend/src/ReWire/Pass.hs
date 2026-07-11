{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | Scaffolding for running, tracing, and dumping pipeline passes, shared
--   between the compiler (ReWire.ModCache, ReWire.FrontEnd) and the embedder
--   (Embedder.ModCache).
module ReWire.Pass
      ( pass
      , printHeader
      , verb'
      ) where

import ReWire.Config (Config, dump, pDebug)
import ReWire.Pretty (showt)

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.Text.IO as T

-- | Run one numbered, named pipeline pass: announce it (verbose only, as
--   "[n] name"), run the transformation, then print the rendering of its
--   output under the same bracketed header when -d n is on -- so "-d n"
--   reads "dump the IR after pass n". Numbers are assigned consecutively at
--   the call sites (ReWire.ModCache and ReWire.FrontEnd for the compiler);
--   the render argument chooses the dump format (the IR's textual format).
pass :: MonadIO m => Config -> Natural -> Text -> (b -> Text) -> (a -> m b) -> a -> m b
pass conf n name render f a = do
      pDebug conf msg
      b <- f a
      when ((conf^.dump) n) $ liftIO $ do
            printHeader msg
            T.putStrLn $ render b
      pure b
      where msg :: Text
            msg = "[" <> showt n <> "] " <> name

printHeader :: MonadIO m => Text -> m ()
printHeader hd = do
      liftIO $ T.putStrLn   "-- # ==================================================="
      liftIO $ T.putStrLn $ "-- # " <> hd
      liftIO $ T.putStrLn   "-- # ===================================================\n"

-- | Print a debug message (verbose only), passing the program through.
verb' :: MonadIO m => Config -> Text -> a -> m a
verb' conf s a = pDebug conf s >> pure a
