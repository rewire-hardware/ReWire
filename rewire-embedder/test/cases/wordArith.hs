{-# LANGUAGE DataKinds #-}
import Prelude hiding ((*),(**),(+),(/),(-))
import ReWire ( signal, Identity, ReacT, W)
import ReWire.Monad (Dev, iter)
import ReWire.Bits (lit, (%), (*), (**), (+), (/),(-))

start :: Dev (W 8) (W 8)
start = iter compute (lit 0)

-- | Bits
-- (Add, "+"), (Sub, "-"), (Mul, "*"), (Div, "/"), (Mod, "%"), (Pow, "**")
compute :: W 8 -> W 8
compute w = ((w + lit 1) ** lit 2 * (w - lit 2) / lit 3) % (w + lit 1)

main = undefined

