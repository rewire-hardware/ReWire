{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits

-- A reactive `if` on the resumed input whose taken branch uses the
-- scrutinee again: GHC's case-binder swap turns that use into the case
-- binder, which a terminator case has no place for — it aliases the
-- scrutinee.
dev :: ReacT Bit Bit (StateT Bit Identity) ()
dev = signal True >>= \ i ->
      (if i then lift (put i) else lift get >>= \ s -> lift (put (not s))) >>= \ _ ->
      lift get >>= \ s -> signal s >>= \ _ -> dev

start :: ReacT Bit Bit Identity ()
start = extrude dev False

main = undefined
