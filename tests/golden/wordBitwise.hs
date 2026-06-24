{-# LANGUAGE DataKinds #-}
import Prelude hiding ((^))
import ReWire ( signal, Identity, ReacT, W)
import ReWire.Monad (Dev)
import ReWire.Bits (lit, (.&.), (.|.), bnot, (^), (~^))

start :: Dev (W 8,W 8,W 8) (W 8)
start = loop (lit 0,lit 1,lit 2)

loop :: (W 8,W 8,W 8) -> ReacT (W 8,W 8,W 8) (W 8) Identity ()
loop i = return (compute i) >>= signal >>= loop

-- | Bits
-- (And, ".&."), (Or, ".|."), (Not, "bnot"), (XOr, "^"), (XNor, "~^")
compute :: (W 8,W 8,W 8) -> W 8
compute (u,v,w) = (u .&. v ^ w) .|. (bnot u .&. bnot v ~^ w)

main = undefined