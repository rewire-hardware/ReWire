{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.FrontEnd
      ( LoadPath
      , compileFile
      ) where

import ReWire.Annotation (noAnn, unAnn)
import ReWire.Config (Config, Language (..), getOutFile, target, cycles, inputsFile, defaultInputsFile, source, rtlOpt, testbench, pDebug, loadPath, locators, noLocators)
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

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, findExecutable)
import Data.Text (Text, pack)
import Numeric.Natural (Natural)
import System.Environment (lookupEnv, getExecutablePath)
import System.Exit (exitFailure)
import System.FilePath (dropExtension, takeDirectory, takeExtension, (<.>), (-<.>), (</>))
import System.Process (proc, readCreateProcessWithExitCode)

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
                  when (conf^.Config.certify && conf^.target == Interpret) $
                        warnAt conf noAnn "--certify: nothing to certify (only device targets are certified)."
                  p10 <- pass conf filename 10 "Partially evaluating/reducing the Hyle IR (if this is slow, consider --rtl-opt=0)." "rwc" prettyPrint
                        (Hyle.check . Hyle.optimize (conf^.rtlOpt)) a
                  -- Pass 11 runs for every device target, so every consumer
                  -- -- the HDL and Cryptol backends, the interpreter, the
                  -- .rwc emitted by --core, and the certified artifact --
                  -- reads the same fully lowered program.
                  p <- pass conf filename 11 "Inlining Hyle definitions." "rwc" prettyPrint
                        (Hyle.check . Hyle.inline (conf^.Config.flatten)) p10
                  case conf^.target of
                        VHDL      -> do
                              HyleH.compileProgram conf p >>= writeOutput
                              writeTestbench $ HyleH.testbench conf $ progDevice p
                              certifyOutput p
                        Verilog   -> do
                              HyleV.compileProgram conf p >>= writeOutput
                              writeTestbench $ HyleV.testbench conf $ progDevice p
                              certifyOutput p
                        Cryptol   -> do
                              HyleCry.compileProgram conf p >>= writeOutput
                              certifyOutput p
                        -- The .rwc output only carries source locators
                        -- ('--@' lines) under --locators (and not under
                        -- --no-locators, which wins): spans can embed
                        -- absolute paths, which would destabilize golden
                        -- files. Doc ('--|') and 'tag' lines are path-free
                        -- and not gated.
                        RWCore    -> do
                              if conf^.locators && not (conf^.noLocators)
                                    then writeOutput p
                                    else writeOutput $ scrubSpans p
                              certifyOutput p
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

            -- | --certify: write the certified pair beside the output --
            --   the machine-mode Eidos IR (<out>.eir, the --eidos dump,
            --   written by ReWire.ModCache) and the final backend-consumed
            --   Hyle program (<out>.certify.rwc) -- run the verified
            --   validator on it, and surface the verdict: a one-line
            --   confirmation on VALIDATED, otherwise a warning (fatal
            --   under -Werror), never a silent pass. See doc/certify.md.
            certifyOutput :: (MonadError AstError m, MonadIO m) => Program -> m ()
            certifyOutput p11 = when (conf^.Config.certify) $ case conf^.source of
                  Haskell -> do
                        let fout    = fromMaybe filename $ conf^.Config.outFile
                            eirFile = fout -<.> "eir"
                            rwcFile = fout -<.> "certify.rwc"
                        verb $ "certify: writing the final (backend-consumed) Hyle IR to file: " <> pack rwcFile
                        liftIO $ T.writeFile rwcFile $ prettyPrint p11
                        liftIO findRwv >>= \ case
                              Nothing  -> warnAt conf (filePath filename) $ "certify: not validated: the validator (" <> pack rwvExe
                                    <> ") was not found next to rwc, on the PATH, or in verify/.lake/build/bin; build it with"
                                    <> " 'cd verify && lake build " <> pack rwvExe <> "' in a ReWire checkout (or set RWC_RWV to its location)."
                              Just rwv -> do
                                    verb $ "certify: running the validator: " <> pack rwv <> " " <> pack eirFile <> " " <> pack rwcFile
                                    (_, out, err) <- liftIO $ readCreateProcessWithExitCode (proc rwv [eirFile, rwcFile]) ""
                                    case [ s | l <- lines out, Just s <- [stripPrefix "summary: " l] ] of
                                          s : _ | "VALIDATED" `isPrefixOf` s -> liftIO $ T.putStrLn
                                                $ "certify: VALIDATED: the compiled device (" <> pack rwcFile
                                                <> ") implements the Eidos machine (" <> pack eirFile <> ")."
                                          s : _ -> warnAt conf (filePath filename)
                                                $ "certify: not validated: " <> pack s
                                                <> " (artifacts: " <> pack eirFile <> ", " <> pack rwcFile <> ")."
                                          []    -> warnAt conf (filePath filename)
                                                $ "certify: not validated: the validator produced no verdict: " <> lastLine out err
                                                <> " (artifacts: " <> pack eirFile <> ", " <> pack rwcFile <> ")."
                  -- Under --from-core the Eidos pipeline never runs, so
                  -- there is no machine IR to validate against.
                  _       -> warnAt conf noAnn "certify: not validated: nothing to certify (certification requires compiling from Haskell source; no Eidos IR exists under --from-core)."
                  where lastLine :: String -> String -> Text
                        lastLine out err = maybe "(no output)" pack
                              $ lastMaybe (filter (not . null) $ lines out) <|> lastMaybe (filter (not . null) $ lines err)

            verb :: MonadIO m => Text -> m ()
            verb = pDebug conf

-- | Strip all provenance from the program's annotations (a generic sweep;
--   Hyle types derive Data) so the printed .rwc carries no '--@' locator
--   lines.
scrubSpans :: Program -> Program
scrubSpans = unAnn

-- | The verified Eidos-to-Hyle validator executable (built from verify/
--   with Lake; see doc/certify.md).
rwvExe :: String
rwvExe = "rwv-cstep-validate"

-- | Locate the validator: RWC_RWV, then next to the rwc executable, then
--   the PATH, then the in-checkout Lake build directory relative to the
--   current directory (mirroring the rwcry discovery chain in
--   ReWire.Eidos.ToHyle).
findRwv :: IO (Maybe FilePath)
findRwv = lookupEnv "RWC_RWV" >>= \ case
      Just r  -> pure $ Just r
      Nothing -> do
            cand <- (</> rwvExe) . takeDirectory <$> getExecutablePath
            doesFileExist cand >>= \ case
                  True  -> pure $ Just cand
                  False -> findExecutable rwvExe >>= \ case
                        Just r  -> pure $ Just r
                        Nothing -> do
                              let local = "verify" </> ".lake" </> "build" </> "bin" </> rwvExe
                              ex <- doesFileExist local
                              pure $ if ex then Just local else Nothing

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
