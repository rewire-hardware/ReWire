{-# LANGUAGE DataKinds #-}
import ReWire ( signal, Identity, ReacT, W )
import ReWire.Monad (Dev)
import ReWire.Bits (lit, (@@), (@.), msbit)

start :: Dev (W 16) (W 8)
start = loop (lit 0)

loop :: (W 16) -> ReacT (W 16) (W 8) Identity ()
loop i = return (compute i) >>= signal >>= loop

-- | Bits
-- (BitSlice, "finBitSlice"),(BitIndex, "finBitIndex"), (MSBit, "msbit")
compute :: W 16 -> W 8
compute w = if msbit w && w @. 8 
            then w @@ (7,0)
            else w @@ (15,8)

main = undefined