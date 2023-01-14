{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Config
      ( interpret, Config, getOutFile
      , Target (..), ResetFlag (..), OutFlag (..)
      , verbose, pretty, flatten
      , target, clock, reset
      , resetFlags, outFlags
      , inputSigs, stateSigs, outputSigs
      , vhdlPackages, inputsFile, outFile
      , top, loadPath, cycles, depth, dump
      ) where

import ReWire.Flags (Flag (..))
import ReWire.Pretty (showt)

import Control.Lens (makeLenses, over, (.~), (^.), Lens', lens)
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text, pack, unpack, splitOn)
import Numeric.Natural (Natural)
import System.FilePath ((-<.>))

import qualified Data.Set as Set

data Target = Interpret | FIRRTL | VHDL | Verilog
      deriving (Eq, Ord, Show)
data ResetFlag = Inverted | Synchronous
      deriving (Eq, Ord, Show)
data OutFlag   = Flatten | Pretty | Verbose
      deriving (Eq, Ord, Show)

data Config = Config
      { _target       :: Target
      , _clock        :: Text -- No clock if null.
      , _reset        :: Text -- No reset if null.
      , _resetFlags   :: Set ResetFlag
      , _outFlags     :: Set OutFlag
      , _inputSigs    :: [Text]
      , _stateSigs    :: [Text]
      , _outputSigs   :: [Text]
      , _vhdlPackages :: [Text]
      , _inputsFile   :: FilePath
      , _outFile      :: Maybe FilePath
      , _top          :: Text
      , _loadPath     :: [FilePath]
      , _cycles       :: Natural
      , _depth        :: Natural
      , _dump         :: Natural -> Bool
      }

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config
      { _target       = Verilog
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
      , _top          = "Main.start"
      , _loadPath     = []
      , _cycles       = 10
      , _depth        = 8
      , _dump         = const False
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
setOutFlag f conf True  = over outFlags (Set.insert f) conf
setOutFlag f conf False = over outFlags (Set.delete f) conf

type ErrorMsg = Text

getOutFile :: Config -> FilePath -> FilePath
getOutFile c filename = flip fromMaybe (c^.outFile) $ case c^.target of
      Verilog   -> filename -<.> "v"
      FIRRTL    -> filename -<.> "fir"
      VHDL      -> filename -<.> "vhdl"
      Interpret -> filename -<.> "yaml"

-- TODO(chathhorn): separate validation pass.
interpret :: [Flag] -> Either ErrorMsg Config
interpret = foldM interp defaultConfig
      where interp :: Config -> Flag -> Either ErrorMsg Config
            interp c = \ case
                  FlagHelp                        -> Left ""
                  FlagLoadPath (pack -> p)        -> pure $ over loadPath (<> map unpack (splitOn' "," p)) c
                  FlagO p | Nothing <- c^.outFile -> pure $ outFile .~ pure p   $ c
                          | otherwise             -> Left "Multiple output files specified on the command line."
                  FlagFirrtl                      -> pure $ target .~ FIRRTL    $ c
                  FlagVerilog                     -> pure $ target .~ Verilog   $ c
                  FlagVhdl                        -> pure $ target .~ VHDL      $ c
                  FlagInterpret Nothing           -> pure $ target .~ Interpret $ c
                  FlagInterpret (Just ip)         -> pure $ target .~ Interpret $ inputsFile .~ ip $ c
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
                  FlagTop (pack -> n)             -> pure $ top .~ n $ c
                  FlagCycles n                    -> pure $ cycles .~ read n $ c
                  FlagEvalDepth n                 -> pure $ depth .~ read n $ c

            augment :: [Natural] -> (Natural -> Bool) -> Natural -> Bool
            augment ns f n | n `elem` ns = True
                           | otherwise   = f n

            -- | Version of splitOn that returns '[]' instead of '[""]' when the second argument is empty.
            splitOn' :: Text -> Text -> [Text]
            splitOn' sep = \ case
                  "" -> []
                  s  -> splitOn sep s
