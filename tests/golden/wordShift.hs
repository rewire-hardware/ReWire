{-# LANGUAGE DataKinds #-}
import Prelude hiding ((-))
import ReWire ( signal, Identity, ReacT, W)
import ReWire.Monad (Dev)
import ReWire.Bits (lit, (-), (<<.), (>>.), (>>>), (.|.), (%))

start :: Dev (W 8) (W 8)
start = loop (lit 52)

loop :: W 8 -> ReacT (W 8) (W 8) Identity ()
loop i = return (myArithRotr (myRotr i (lit 3)) (lit 5)) >>= signal >>= loop

-- | Bits
-- (LShift, "<<."), (RShift, ">>."), (RShiftArith, ">>>"),
myRotr :: W 8 -> W 8 -> W 8
myRotr w n = let n' = n % lit 8
             in w <<. (lit 8 - n') .|. (w >>. n')

myArithRotr :: W 8 -> W 8 -> W 8
myArithRotr w n = let n' = n % lit 8
                  in w <<. (lit 8 - n') .|. (w >>> n')

main = undefined

