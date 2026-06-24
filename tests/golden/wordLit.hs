{-# LANGUAGE DataKinds #-}
import Prelude hiding ((^))
import ReWire ( signal, Identity, ReacT, W)
import ReWire.Bits ( lit, resize, (^)) 
import ReWire.Monad (Dev)


start :: Dev (W 8) (W 16)
start = loop (lit 255)

loop :: (W 8) -> ReacT (W 8) (W 16) Identity ()
loop i = return ((resize i) ^ (lit 1)) >>= signal >>= loop

main = undefined

-- | Bits
{-
      (Resize, "resize"),
      also lit, 
-}