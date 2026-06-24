{-# LANGUAGE DataKinds #-}
import Prelude hiding ((*),(**),(+),(/),(-))
import ReWire ( signal, Identity, ReacT, W)
import ReWire.Monad (Dev)
import ReWire.Bits (lit, (%), (*), (**), (+), (/),(-))

start :: Dev (W 8) (W 8)
start = loop (lit 0)

loop :: W 8 -> ReacT (W 8) (W 8) Identity ()
loop i = return (compute i) >>= signal >>= loop

-- | Bits
-- (Add, "+"), (Sub, "-"), (Mul, "*"), (Div, "//"), (Mod, "%"), (Pow, "**")
compute :: W 8 -> W 8
compute w = ((w + lit 1) ** lit 2 * (w - lit 2) / lit 3) % (w  + lit 1)

main = undefined

