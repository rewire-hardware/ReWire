{-# LANGUAGE DataKinds #-}
-- Cryptol FFI: `crystep` is compiled from cry/crymix.cry (words, where
-- bindings, take/drop/concat, comparison, if-then-else) by rwcry and
-- realized in the generated HDL alongside the rest of the program.
import ReWire
import ReWire.Bits (lit)
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

crystep :: W 8 -> W 8 -> W 8
crystep = cryptol "cry/crymix.cry" "step" crystep

go :: (W 8, W 8) -> W 8
go (m, x) = crystep m x

start :: Dev (W 8, W 8) (W 8)
start = iter go (lit 0, lit 0)

main :: IO ()
main = undefined
