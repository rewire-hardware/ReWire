-- EXPECT-ERROR: No instance for ‘GHC.Internal.Control.Monad.Fix.MonadFix
{-# LANGUAGE RecursiveDo #-}
import ReWire
import ReWire.Bits
start :: ReacT Bit Bit Identity ()
start = do
  rec { signal zero }
  start
main = undefined
