-- The start device pauses (signals) before extruding its state: the state
-- registers have no program-determined initial values (they're zero-filled
-- and unobservable until the extrude), and the first input is captured as an
-- argument of the generated R_ continuation constructor.
{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits

dev :: W 8 -> ReacT (W 8) (W 8) (StateT (W 8) Identity) ()
dev k = do
      s <- lift get
      i <- signal (s + k)
      lift (put i)
      dev k

start :: ReacT (W 8) (W 8) Identity ()
start = signal (lit 0) >>= \ i -> extrude (dev i) (lit 0)

main :: IO ()
main = undefined
