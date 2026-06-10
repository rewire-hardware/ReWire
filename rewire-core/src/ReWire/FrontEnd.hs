{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.FrontEnd
      ( LoadPath
      , compileFile
      ) where

import ReWire.Annotation (noAnn)
import ReWire.Config (Config, Language (..), getOutFile, target, cycles, inputsFile, source, rtlOpt, pDebug)
import ReWire.Core.Interp (interp, Ins, run)
import ReWire.Core.Parse (parseCore)
import ReWire.Core.Syntax (Device)
import ReWire.Core.Transform (mergeSlices, purgeUnused, partialEval, dedupe)
import ReWire.Error (MonadError, AstError, runSyntaxError, failAt)
import ReWire.Fix (fixPure)
import ReWire.ModCache (runCache, getDevice, LoadPath)
import ReWire.Pretty (Pretty, prettyPrint, fastPrint, showt)

import qualified ReWire.Config         as Config
import qualified ReWire.Core.Check     as Core
import qualified ReWire.Core.Syntax    as Core
import qualified ReWire.Core.ToVHDL    as VHDL
import qualified ReWire.Core.ToVerilog as Verilog

import Control.Arrow ((>>>))
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Numeric.Natural (Natural)
import System.Exit (exitFailure)
import System.IO (stderr)

import qualified Data.HashMap.Strict as Map
import qualified Data.Text.IO        as T
import qualified Data.Yaml           as YAML

-- | Opens and parses a file and, recursively, its imports.
loadDevice :: (MonadFail m, MonadError AstError m, MonadState AstError m, MonadIO m) => Config -> FilePath -> m Device
loadDevice conf fp = runCache $ getDevice conf fp

compileFile :: MonadIO m => Config -> FilePath -> m ()
compileFile conf filename = do
      verb $ "Compiling: " <> pack filename

      runSyntaxError (loadCore >>= Core.check >>= compile)
            >>= either (liftIO . (>> exitFailure) . T.hPutStrLn stderr . prettyPrint) pure

      where loadCore :: (MonadError AstError m, MonadState AstError m, MonadFail m, MonadIO m) => m Core.Device
            loadCore = case conf^.source of
                  Haskell -> loadDevice conf filename 
                  RWCore  -> parseCore filename
                  s       -> failAt noAnn $ "Not a supported source language: " <> pack (show s)

            compile :: (MonadFail m, MonadError AstError m, MonadIO m) => Core.Device -> m ()
            compile a = do
                  verb "Partially evaluating/reducing core IR. If this is taking too long, consider disabling with --rtl-opt=0."
                  let b = fixPure (conf^.rtlOpt) -- TODO: re-work these passes to avoid fix if possible
                              ( mergeSlices
                              >>> partialEval
                              >>> mergeSlices
                              >>> dedupe
                              >>> purgeUnused
                              ) a
                  b' <- Core.check b
                  case conf^.target of
                        VHDL      -> VHDL.compileProgram conf b' >>= writeOutput
                        RWCore    -> writeOutput b'
                        Interpret -> do
                              verb $ "Interpreting core: reading inputs: " <> pack (conf^.inputsFile)
                              ips  <- boundInput (conf^.cycles) . fromRight mempty <$> liftIO (YAML.decodeFileEither $ conf^.inputsFile)
                              verb $ "Interpreting core: running for " <> showt (conf^.cycles) <> " cycles."
                              outs <- run conf (interp conf b') ips
                              let fout = getOutFile conf filename
                              verb $ "Interpreting core: done running; writing YAML output to file: " <> pack fout
                              liftIO $ YAML.encodeFile fout outs
                        Verilog   -> Verilog.compileProgram conf b' >>= writeOutput
                        Haskell   -> failAt noAnn "Haskell is not a supported target language."

            writeOutput :: (MonadError AstError m, MonadIO m, Pretty a) => a -> m ()
            writeOutput a = do
                  let fout = getOutFile conf filename
                  verb $ "Writing to file: " <> pack fout
                  liftIO $ T.writeFile fout $ if conf^.Config.pretty then prettyPrint a else fastPrint a

            verb :: MonadIO m => Text -> m ()
            verb = pDebug conf

-- | Replicates/truncates inputs to fill up exactly ncycles cycles.
boundInput :: Natural -> [Ins] -> [Ins]
boundInput ncycles ips = foldl' (\ ms m -> ms <> [Map.union m (last' ms)]) [] ips'
      where ips' :: [Ins]
            ips' = take (fromIntegral ncycles) $ ips <> repeat (last' ips)

lastMaybe :: [a] -> Maybe a
lastMaybe = \ case
      []       -> Nothing
      [a]      -> Just a
      (_ : as) -> lastMaybe as

last' :: Monoid a => [a] -> a
last' = fromMaybe mempty . lastMaybe
