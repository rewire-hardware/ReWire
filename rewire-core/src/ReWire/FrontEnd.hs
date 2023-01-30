{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.FrontEnd
      ( loadProgram
      , LoadPath
      , compileFile
      ) where

import ReWire.Annotation (unAnn, noAnn)
import ReWire.Config (Config, Language (..), getOutFile, verbose, target, dump, cycles, inputsFile, source)
import ReWire.Core.Interp (interp, Ins, run)
import ReWire.Core.Parse (parseCore)
import ReWire.Core.Syntax (Program)
import ReWire.Core.Transform (mergeSlices, purgeUnused, partialEval, dedupe)
import ReWire.Error (MonadError, AstError, runSyntaxError, failAt)
import ReWire.ModCache (printHeader, runCache, getProgram, LoadPath)
import ReWire.Pretty (Pretty, prettyPrint, fastPrint, showt)
import qualified ReWire.Config         as Config
import qualified ReWire.Core.Check     as Core
import qualified ReWire.Core.Syntax    as Core
import qualified ReWire.Core.ToVHDL    as VHDL
import qualified ReWire.Core.ToVerilog as Verilog

import Control.Arrow ((>>>))
import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import Data.Either (fromRight)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Numeric.Natural (Natural)
import System.Exit (exitFailure)
import System.IO (stderr)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text.IO        as T
import qualified Data.Yaml           as YAML

-- | Opens and parses a file and, recursively, its imports.
loadProgram :: (MonadFail m, MonadError AstError m, MonadState AstError m, MonadIO m) => Config -> FilePath -> m Program
loadProgram conf fp = runCache $ getProgram conf fp

compileFile :: MonadIO m => Config -> FilePath -> m ()
compileFile conf filename = do
      when (conf^.verbose) $ liftIO $ T.putStrLn $ "Compiling: " <> pack filename

      runSyntaxError (loadCore >>= compile)
            >>= either (liftIO . (>> exitFailure) . T.hPutStrLn stderr . prettyPrint) pure

      where loadCore :: (MonadError AstError m, MonadState AstError m, MonadFail m, MonadIO m) => m Core.Program
            loadCore = case conf^.source of
                  Haskell -> loadProgram conf filename
                  RWCore  -> parseCore filename >>= Core.check
                  s       -> failAt noAnn $ "Not a supported source language: " <> pack (show s)

            compile :: (MonadFail m, MonadError AstError m, MonadIO m) => Core.Program -> m ()
            compile a = do
                  let b = ( mergeSlices
                        >>> mergeSlices
                        >>> partialEval
                        >>> mergeSlices
                        >>> dedupe
                        >>> purgeUnused
                        ) a
                  when (conf^.verbose)   $ liftIO $ T.putStrLn "Debug: [Pass 13] Reduced core."
                  when (conf^.dump $ 13) $ liftIO $ do
                        printHeader "[Pass 13] Reduced Core"
                        liftIO $ T.putStrLn $ prettyPrint b
                        when (conf^.verbose) $ do
                              liftIO $ T.putStrLn "\n## Show core:\n"
                              liftIO $ T.putStrLn $ showt $ unAnn b
                  case conf^.target of
                        FIRRTL    -> liftIO $ T.putStrLn "FIRRTL backend currently out-of-order. Use '--verilog' or '--interpret'."
                                     -- compileProgram flags b >>= toLoFirrtl >>= writeOutput
                        VHDL      -> VHDL.compileProgram conf b >>= writeOutput
                        RWCore    -> writeOutput b
                        Interpret -> do
                              when (conf^.verbose) $ liftIO $ T.putStrLn $ "Debug: Interpreting core: reading inputs: " <> pack (conf^.inputsFile)
                              ips  <- boundInput (conf^.cycles) . fromRight mempty <$> liftIO (YAML.decodeFileEither $ conf^.inputsFile)
                              when (conf^.verbose) $ liftIO $ T.putStrLn $ "Debug: Interpreting core: running for " <> showt (conf^.cycles) <> " cycles."
                              outs <- run conf (interp conf b) ips
                              let fout = getOutFile conf filename
                              when (conf^.verbose) $ liftIO $ T.putStrLn $ "Debug: Interpreting core: done running; writing YAML output to file: " <> pack fout
                              liftIO $ YAML.encodeFile fout outs
                        Verilog   -> Verilog.compileProgram conf b >>= writeOutput
                        Haskell   -> failAt noAnn "Haskell is not a supported target language."

            writeOutput :: (MonadError AstError m, MonadIO m, Pretty a) => a -> m ()
            writeOutput a = do
                  let fout = getOutFile conf filename
                  liftIO $ T.writeFile fout $ if | conf^.Config.pretty -> prettyPrint a
                                                 | otherwise           -> fastPrint a

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
