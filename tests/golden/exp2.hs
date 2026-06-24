{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
import Prelude hiding ((+), (*), head, last, tail, map, replicate, Word)
import ReWire
import ReWire.Bits
import ReWire.Vectors

type Word = W 100
type N = 10

x2 :: Word -> Word
x2 x = x * lit 2

ss' :: Vec N Word -> Word -> Vec N Word
ss' ss i = map x2 $ tail ss `snoc` i

dev :: Monad m => ReacT Word Word (StateT (Vec N Word) m) ()
dev = do
      ss <- lift get
      i  <- signal $ head ss
      lift $ put $ ss' ss i
      dev

start :: ReacT Word Word Identity ()
start = extrude dev $ replicate $ lit 0

main :: IO ()
main = undefined
