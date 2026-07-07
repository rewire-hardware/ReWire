-- | Calling Cryptol functions from ReWire programs. Unlike
--   'ReWire.extern' -- a black box realized by hand-written HDL -- a
--   'cryptol' function is compiled to hardware along with the rest of the
--   program: it is present in the generated Verilog, VHDL, and Cryptol,
--   and the interpreter can evaluate it.
module ReWire.Cryptol (cryptol) where

import RWC.Primitives (rwPrimCryptol)

-- | @cryptol file fn impl@ compiles the Cryptol function @fn@ from the
--   Cryptol module @file@ (resolved against the importing source file's
--   directory and the loadpath) at the type of this expression, which
--   must be monomorphic at each use site (after inlining). The Cryptol
--   module is loaded and typechecked by Cryptol itself (this requires
--   @z3@ on the PATH), and the use-site type is checked against the
--   function's Cryptol type scheme.
--
--   The third argument is the implementation used when the program runs
--   under GHC; rwc ignores it (the Cryptol source is the meaning). Pass
--   the definition itself if there is none:
--
--   > f :: W 8 -> W 8
--   > f = cryptol "f.cry" "f" f
--
--   Both String arguments must be literals (after inlining).
{-# INLINE cryptol #-}
cryptol :: String -> String -> a -> a
cryptol = rwPrimCryptol
