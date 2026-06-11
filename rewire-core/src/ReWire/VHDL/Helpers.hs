{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
-- | The rw_helpers VHDL package, embedded at compile time from
--   rewire-core/vhdl/rw_helpers.vhdl. It implements Verilog expression
--   semantics (unsigned operations, self-determined width rules per the
--   Verilog standard) over std_logic_vector; the VHDL backend emits it with
--   every design and only ever assigns through rw_resize, mirroring Verilog
--   assignment truncation/extension.
module ReWire.VHDL.Helpers (helpersPackage) where

import Data.Text (Text)
import Language.Haskell.TH (runIO, stringE)
import Language.Haskell.TH.Syntax (addDependentFile)

import qualified Data.Text as T

-- | The contents of vhdl/rw_helpers.vhdl (the path is relative to the
--   rewire-core package root, where GHC runs during a build).
helpersPackage :: Text
helpersPackage = T.pack $(do
      addDependentFile "vhdl/rw_helpers.vhdl"
      runIO (readFile "vhdl/rw_helpers.vhdl") >>= stringE)
