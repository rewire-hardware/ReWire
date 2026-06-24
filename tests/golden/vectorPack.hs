{-# LANGUAGE DataKinds #-}
import Prelude hiding (replicate, map)
import ReWire ( signal, Identity, ReacT, Vec, W)
import ReWire.Monad (Dev)
import ReWire.Vectors (map, replicate, packlo, packhi, unpacklo, unpackhi)
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
-- reverse, `(++), empty, singleton, take, drop
compute :: Input -> Output
compute (v,w) = let (lo,hi) = (packlo v w, packhi v w)
                    (lo',hi') = (packlo hi lo, packhi hi lo)
                    (v',w') = (unpacklo lo' hi', unpackhi lo' hi')
                in (v',w')

main = undefined




-- | Vectors
-- packlo, packhi, unpacklo, unpackhi
