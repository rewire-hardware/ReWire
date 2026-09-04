{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits

-- An inner stateful phase written inline under extrude, inside an outer
-- stateless loop: no reactive definition carries the StateT layer, so
-- the state cell is visible only in expression types. Each iteration
-- re-enters the phase with its state reset to False.
start :: ReacT Bit Bit Identity ()
start = extrude (signal True >>= \ i -> lift (put i) >>= \ _ -> lift get >>= \ s -> signal (not s) >>= \ j -> lift get >>= \ t -> return (t &&& j)) False
      >>= \ r -> signal r >>= \ _ -> start

main = undefined
