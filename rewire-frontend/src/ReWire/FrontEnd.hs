{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.FrontEnd
      ( LoadPath
      , compileFile
      ) where

import ReWire.Annotation (noAnn)
import ReWire.Config (Config, Language (..), getOutFile, target, cycles, inputsFile, defaultInputsFile, source, rtlOpt, testbench, pDebug, loadPath)
import ReWire.Error (MonadError, AstError, runSyntaxError, failAt, warnAt, printError, relocatingNoLocTo, filePath)
import ReWire.Hyle.Interp (Ins, run)
import ReWire.Hyle.Parse (parseHyle)
import ReWire.Hyle.Syntax (Program, progDevice)
import ReWire.ModCache (runCache, getDevice, LoadPath)
import ReWire.Pass (pass)
import ReWire.Pretty (Pretty, prettyPrint, fastPrint, showt)

import qualified ReWire.Config           as Config
import qualified ReWire.Hyle.Check     as Hyle
import qualified ReWire.Hyle.Interp    as Hyle
import qualified ReWire.Hyle.ToCryptol as HyleCry
import qualified ReWire.Hyle.ToVHDL    as HyleH
import qualified ReWire.Hyle.ToVerilog as HyleV
import qualified ReWire.Hyle.Transform as Hyle

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import Data.Text (Text, pack)
import Numeric.Natural (Natural)
import System.Exit (exitFailure)
import System.FilePath (dropExtension, takeExtension, (<.>))

import qualified Data.HashMap.Strict as Map
import qualified Data.Text.IO        as T
import qualified Data.Yaml           as YAML

-- | Opens and parses a file and, recursively, its imports.
loadProgram :: (MonadFail m, MonadError AstError m, MonadState AstError m, MonadIO m) => Config -> FilePath -> m Program
loadProgram conf fp = runCache $ getDevice conf fp

compileFile :: MonadIO m => Config -> FilePath -> m ()
compileFile conf filename = do
      verb $ "Compiling: " <> pack filename

      runSyntaxError (relocatingNoLocTo (filePath filename) $ load >>= Hyle.check >>= compile)
            >>= either (\ err -> printError (conf ^. loadPath) err >> liftIO exitFailure) pure

      where load :: (MonadError AstError m, MonadState AstError m, MonadFail m, MonadIO m) => m Program
            load = case conf^.source of
                  Haskell -> loadProgram conf filename
                  RWCore  -> parseHyle filename
                  s       -> failAt noAnn $ "Not a supported source language: " <> pack (show s)

            -- The Hyle-level passes are numbered after ReWire.ModCache's 1-9
            -- (which -d/-v also address, but which only run for Haskell
            -- source), so the -d numbering is uniform across --from-core.
            compile :: (MonadFail m, MonadError AstError m, MonadIO m) => Program -> m ()
            compile a = do
                  when (conf^.testbench && (conf^.target) `notElem` [VHDL, Verilog]) $
                        warnAt conf noAnn "--testbench: no testbench generated (only the Verilog and VHDL targets support testbench generation)."
                  p <- pass conf filename 10 "Partially evaluating/reducing the Hyle IR (if this is slow, consider --rtl-opt=0)." "rwc" prettyPrint
                        (Hyle.check . Hyle.optimize (conf^.rtlOpt)) a
                  let inline = pass conf filename 11 "Inlining Hyle definitions." "rwc" prettyPrint
                        (Hyle.check . Hyle.inline (conf^.Config.flatten))
                  case conf^.target of
                        VHDL      -> do
                              p' <- inline p
                              HyleH.compileProgram conf p' >>= writeOutput
                              writeTestbench $ HyleH.testbench conf $ progDevice p'
                        Verilog   -> do
                              p' <- inline p
                              HyleV.compileProgram conf p' >>= writeOutput
                              writeTestbench $ HyleV.testbench conf $ progDevice p'
                        Cryptol   -> HyleCry.compileProgram conf p >>= writeOutput
                        RWCore    -> writeOutput p
                        Interpret -> do
                              ips  <- loadInputs
                              verb $ "Interpreting hyle: running for " <> showt (length ips) <> " cycles."
                              outs <- run conf (Hyle.interp conf p) ips
                              let fout = getOutFile conf filename
                              verb $ "Interpreting hyle: done running; writing YAML output to file: " <> pack fout
                              liftIO $ YAML.encodeFile fout outs
                        Haskell   -> failAt noAnn "Haskell is not a supported target language."

            -- | Inputs for --interpret and --testbench, padded/truncated to
            --   the cycle count. An unreadable inputs file means all wires
            --   are driven to zero -- warn, unless the file is missing and
            --   the user never named one explicitly (driving a device with
            --   no inputs file is a legitimate workflow).
            loadInputs :: (MonadError AstError m, MonadIO m) => m [Ins]
            loadInputs = do
                  verb $ "Reading inputs: " <> pack (conf^.inputsFile)
                  r <- liftIO $ YAML.decodeFileEither $ conf^.inputsFile
                  case r of
                        Right ips -> pure $ boundInput (effectiveCycles conf ips) ips
                        Left err  -> do
                              exists <- liftIO $ doesFileExist $ conf^.inputsFile
                              when (exists || conf^.inputsFile /= defaultInputsFile) $ warnAt conf noAnn
                                    $ "could not read inputs from " <> pack (conf^.inputsFile)
                                    <> (if exists then " (" <> pack (YAML.prettyPrintParseException err) <> ")" else " (file does not exist)")
                                    <> "; driving all inputs with zeros."
                              pure $ boundInput (effectiveCycles conf mempty) mempty

            writeTestbench :: (MonadError AstError m, MonadIO m, Pretty tb) => ([Ins] -> tb) -> m ()
            writeTestbench gen = when (conf^.testbench) $ do
                  ips <- loadInputs
                  let fout = getOutFile conf filename
                      tbout = dropExtension fout <> "_tb" <.> takeExtension fout
                  verb $ "Writing testbench to file: " <> pack tbout
                  liftIO $ T.writeFile tbout $ (if conf^.Config.pretty then prettyPrint else fastPrint) $ gen ips

            writeOutput :: (MonadError AstError m, MonadIO m, Pretty a) => a -> m ()
            writeOutput a = do
                  let fout = getOutFile conf filename
                  verb $ "Writing to file: " <> pack fout
                  liftIO $ T.writeFile fout $ if conf^.Config.pretty then prettyPrint a else fastPrint a

            verb :: MonadIO m => Text -> m ()
            verb = pDebug conf

-- | The number of cycles to interpret/simulate: the explicit --cycles value if
--   the user gave one, otherwise the larger of 10 or the number of inputs
--   supplied in the inputs file.
effectiveCycles :: Config -> [Ins] -> Natural
effectiveCycles conf ips = fromMaybe (max 10 (fromIntegral (length ips))) (conf^.cycles)

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
