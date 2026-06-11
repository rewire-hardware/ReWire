{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Config
      ( interpret, Config, getOutFile
      , Language (..), ResetFlag (..), OutFlag (..)
      , verbose, pretty, flatten
      , target, clock, reset
      , resetFlags, outFlags
      , inputSigs, stateSigs, outputSigs
      , vhdlPackages, inputsFile, outFile
      , start, top, loadPath, cycles, depth, dump, source, typecheck, rtlOpt
      , testbench
      , pDebug
      ) where

import ReWire.Flags (Flag (..))
import ReWire.Pretty (showt)

import Control.Lens (makeLenses, over, (.~), (^.), Lens', lens)
import Control.Monad (when, foldM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack, splitOn)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.FilePath ((-<.>))

import qualified Data.HashSet as Set
import qualified Data.Text.IO as T

data Language = Interpret | VHDL | Verilog | Cryptol | RWCore | Haskell
      deriving (Eq, Ord, Show)
data ResetFlag = Inverted | Synchronous
      deriving (Eq, Ord, Show, Generic)
instance Hashable ResetFlag
data OutFlag   = Flatten | Pretty | Verbose
      deriving (Eq, Ord, Show, Generic)
instance Hashable OutFlag

data Config = Config
      { _source       :: Language
      , _target       :: Language
      , _clock        :: Text -- No clock if null.
      , _reset        :: Text -- No reset if null.
      , _resetFlags   :: HashSet ResetFlag
      , _outFlags     :: HashSet OutFlag
      , _inputSigs    :: [Text]
      , _stateSigs    :: [Text]
      , _outputSigs   :: [Text]
      , _vhdlPackages :: [Text]
      , _inputsFile   :: FilePath
      , _outFile      :: Maybe FilePath
      , _start        :: Text
      , _top          :: Text
      , _loadPath     :: [FilePath]
      , _cycles       :: Natural
      , _depth        :: Natural
      , _dump         :: Natural -> Bool
      , _typecheck    :: Bool
      , _rtlOpt       :: Natural
      , _testbench    :: Bool
      }

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config
      { _source       = Haskell
      , _target       = Verilog
      , _clock        = "clk"
      , _reset        = "rst"
      , _resetFlags   = mempty
      , _outFlags     = mempty
      , _inputSigs    = map (("__in" <>) . showt) [0::Int ..]
      , _stateSigs    = map (("__st" <>) . showt) [0::Int ..]
      , _outputSigs   = map (("__out" <>) . showt) [0::Int ..]
      , _vhdlPackages = ["ieee.std_logic_1164.all"]
      , _inputsFile   = "inputs.yaml"
      , _outFile      = Nothing
      , _start        = "Main.start"
      , _top          = "top_level"
      , _loadPath     = []
      , _cycles       = 10
      , _depth        = 8
      , _dump         = const False
      , _typecheck    = False
      , _rtlOpt       = 8
      , _testbench    = False
      }

verbose :: Lens' Config Bool
verbose = lens (getOutFlag Verbose) (setOutFlag Verbose)

pretty :: Lens' Config Bool
pretty = lens (getOutFlag Pretty) (setOutFlag Pretty)

flatten :: Lens' Config Bool
flatten = lens (getOutFlag Flatten) (setOutFlag Flatten)

getOutFlag :: OutFlag -> Config -> Bool
getOutFlag f conf = f `Set.member` (conf^.outFlags)

setOutFlag :: OutFlag -> Config -> Bool -> Config
setOutFlag f conf ins | ins       = over outFlags (Set.insert f) conf
                      | otherwise = over outFlags (Set.delete f) conf

type ErrorMsg = Text

getOutFile :: Config -> FilePath -> FilePath
getOutFile c filename = flip fromMaybe (c^.outFile) $ case c^.target of
      Verilog   -> filename -<.> "sv"
      VHDL      -> filename -<.> "vhdl"
      Cryptol   -> filename -<.> "cry"
      Interpret -> filename -<.> "yaml"
      RWCore    -> filename -<.> "rwc"
      Haskell   -> filename -<.> "hs"

-- TODO(chathhorn): separate validation pass.
interpret :: [Flag] -> Either ErrorMsg Config
interpret = foldM interp defaultConfig
      where interp :: Config -> Flag -> Either ErrorMsg Config
            interp c = \ case
                  FlagHelp                        -> Left ""
                  FlagLoadPath (pack -> p)        -> pure $ over loadPath (<> map unpack (splitOn' "," p)) c
                  FlagO p | Nothing <- c^.outFile -> pure $ outFile .~ pure p   $ c
                          | otherwise             -> Left "Multiple output files specified on the command line."
                  FlagVerilog                     -> pure $ target .~ Verilog   $ c
                  FlagVhdl                        -> pure $ target .~ VHDL      $ c
                  FlagCryptol                     -> pure $ target .~ Cryptol   $ c
                  FlagInterpret Nothing           -> pure $ target .~ Interpret $ c
                  FlagInterpret (Just ip)         -> pure $ target .~ Interpret $ inputsFile .~ ip $ c
                  FlagTestbench Nothing           -> pure $ testbench .~ True   $ c
                  FlagTestbench (Just ip)         -> pure $ testbench .~ True   $ inputsFile .~ ip $ c
                  FlagCore                        -> pure $ target .~ RWCore    $ c
                  FlagFromCore                    -> pure $ source .~ RWCore    $ c
                  FlagClockName (pack -> n)       -> pure $ clock  .~ n         $ c
                  FlagNoClock                     -> pure $ clock  .~ ""        $ reset .~ ""      $ c
                  FlagResetName (pack -> n)       -> pure $ reset  .~ n         $ c
                  FlagNoReset                     -> pure $ reset  .~ ""        $ c
                  FlagInvertReset                 -> pure $ over resetFlags (Set.insert Inverted) c
                  FlagSyncReset                   -> pure $ over resetFlags (Set.insert Synchronous) c
                  FlagFlatten                     -> pure $ over outFlags (Set.insert Flatten) c
                  FlagPretty                      -> pure $ over outFlags (Set.insert Pretty) c
                  FlagVerbose                     -> pure $ over outFlags (Set.insert Verbose) c
                  FlagDump (pack -> d)            -> pure $ over dump (augment $ map (read . unpack) $ splitOn' "," d) c
                  FlagVhdlPkgs (pack -> p)        -> pure $ over vhdlPackages (<> splitOn' "," p) c
                  FlagInputNames (pack -> n)      -> pure $ over inputSigs  (splitOn' "," n <>) c
                  FlagStateNames (pack -> n)      -> pure $ over stateSigs  (splitOn' "," n <>) c
                  FlagOutputNames (pack -> n)     -> pure $ over outputSigs (splitOn' "," n <>) c
                  FlagStart (pack -> n)           -> pure $ start .~ n $ c
                  FlagTop (pack -> n)             -> pure $ top .~ n $ c
                  FlagCycles n                    -> pure $ cycles .~ read n $ c
                  FlagEvalDepth n                 -> pure $ depth .~ read n $ c
                  FlagDebugTypeCheck              -> pure $ typecheck .~ True $ c
                  FlagRtlOpt n                    -> pure $ rtlOpt .~ read n $ c

            augment :: [Natural] -> (Natural -> Bool) -> Natural -> Bool
            augment ns f n | n `elem` ns = True
                           | otherwise   = f n

            -- | Version of splitOn that returns '[]' instead of '[""]' when the second argument is empty.
            splitOn' :: Text -> Text -> [Text]
            splitOn' sep = \ case
                  "" -> []
                  s  -> splitOn sep s

pDebug :: MonadIO m => Config -> Text -> m ()
pDebug conf s = when (conf^.verbose) $ liftIO $ T.putStrLn $ "Debug: " <> s
