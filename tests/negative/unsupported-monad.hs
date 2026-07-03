-- The reactive stack (ReacT/StateT/Identity) is the only monad rwc supports:
-- a monadic operator at any other concrete monad is rejected by the GHC
-- front end (a monadic operator at a still-polymorphic monad type is fine --
-- see tests/golden/monadpoly.hs).
-- EXPECT-ERROR: monadic operator at unsupported monad: GHC.Internal.Maybe.Maybe
import ReWire

f :: Bit -> Maybe Bit
f b = return b >>= \ b' -> Just b'

start :: ReacT Bit Bit Identity ()
start = case f True of
      Just b  -> signal b >>= \ _ -> start
      Nothing -> start

main :: IO ()
main = undefined
