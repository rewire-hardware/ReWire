{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
-- | The rw_helpers VHDL package, embedded at compile time from
--   rewire-backend/vhdl/rw_helpers.vhdl. It implements the Hyle primitive
--   denotations of doc/hyle.md, section 5.2 (unsigned operations over
--   std_logic_vector, which has no arithmetic of its own; SMT-LIB
--   division by zero in rw_div/rw_mod). The VHDL backend emits it with
--   every design and calls rw_resize only at the explicit zext and trunc
--   coercions -- no assignment resizes implicitly, since operand and
--   result widths are equal by construction everywhere else.
module ReWire.VHDL.Helpers (helpersPackage) where

import Data.Text (Text)
import Language.Haskell.TH (runIO, stringE)
import Language.Haskell.TH.Syntax (addDependentFile)

import qualified Data.Text as T

-- | The contents of vhdl/rw_helpers.vhdl (the path is relative to the
--   rewire-backend package root, where GHC runs during a build).
helpersPackage :: Text
helpersPackage = T.pack $(do
      addDependentFile "vhdl/rw_helpers.vhdl"
      runIO (readFile "vhdl/rw_helpers.vhdl") >>= stringE)
