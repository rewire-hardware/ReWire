{-# LANGUAGE DataKinds #-}
import Prelude hiding (replicate, map, head, tail, init, last)
import ReWire ( signal, Identity, ReacT, Vec, W)
import ReWire.Monad (Dev)
import ReWire.Vectors (replicate, map, cons, snoc, head, tail, init, last)
import ReWire.Bits (lit)

type Input = (Vec 8 (W 8), Vec 8 (W 8))
type Output = (Vec 8 (W 8), Vec 8 (W 8))

start :: Dev Input Output
start = loop (initVec, initVec)

initVec :: Vec 8 (W 8)
initVec = replicate (lit 0)


loop :: Input -> ReacT Input Output Identity ()
loop i = return (compute i) >>= signal >>= loop

-- | Vectors
-- head, last, init, tail, cons, snoc
compute :: Input -> Output
compute (v,w) = let v0 = head v
                    vs = (tail v :: Vec 7 (W 8))
                    w7 = last w
                    ws = (init w :: Vec 7 (W 8))
                in (snoc vs w7, cons v0 ws)

main = undefined
