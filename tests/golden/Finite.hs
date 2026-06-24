{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
import Prelude hiding ((+), (*), head, last, tail, map, replicate, Word)
import ReWire
import ReWire.Bits
import ReWire.Finite (finite, toFinite', fromFinite)

type Word = W 100
type Packed = (Word, Word, Word, Word)

f :: Finite 100
f = finite 99

dev :: Finite 100 -> ReacT (Finite 100) (Finite 100) Identity ()
dev a = do
      a' <- signal a
      dev (toFinite' (fromFinite a' + (lit 1 :: W 7)))

start :: ReacT (Finite 100) (Finite 100) Identity ()
start = dev f

main :: IO ()
main = undefined
