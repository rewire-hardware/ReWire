{-# LANGUAGE Trustworthy #-}
-- | A thin shim over rewire-core's 'ReWire.Config' adding the embedder's
--   output-file helpers. The embedder ignores the hardware-oriented config
--   fields (clock, reset, signal names, etc.).
module Embedder.Config
      ( module ReWire.Config
      , getEmbedFile, getAtmoFile
      ) where

import ReWire.Config

import Control.Lens ((^.))
import Data.Maybe (fromMaybe)
import System.FilePath ((-<.>))

-- | Output path for the generated Isabelle theory (.thy) file.
getEmbedFile :: Config -> FilePath -> FilePath
getEmbedFile c filename = flip fromMaybe (c^.outFile) $ filename -<.> "thy"

-- | Output path for the generated Atmo IR (.atmo) file.
getAtmoFile :: Config -> FilePath -> FilePath
getAtmoFile c filename = flip fromMaybe (c^.outFile) $ filename -<.> "atmo"
