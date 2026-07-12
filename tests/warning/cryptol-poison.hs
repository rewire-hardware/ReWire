-- EXPECT-WARNING: compiled to a zero constant
-- EXPECT-WARNING: trace/traceVal is ignored
-- The Cryptol FFI realizes `error`/`undefined` as a zero "poison"
-- constant (Hyle is total, with no bottom) and treats `trace` as the
-- identity on its result; both carry a compile-time warning from rwcry,
-- surfaced through rwc.
{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits (lit, (+))
import ReWire.Monad (iter, Dev)
import ReWire.Cryptol (cryptol)

safeDiv :: W 8 -> W 8 -> W 8
safeDiv = cryptol "cry/cryerr.cry" "safeDiv" safeDiv

traced :: W 8 -> W 8
traced = cryptol "cry/cryerr.cry" "traced" traced

go :: (W 8, W 8) -> W 8
go (a, b) = safeDiv a b + traced a

start :: Dev (W 8, W 8) (W 8)
start = iter go (lit 0, lit 0)

main :: IO ()
main = undefined
