{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits

data DWord = DWord W' W' W' W'

data DWord' a b c d = DWord' a b c d

type DWord'' a = DWord' a a a a

type W' = W 8

type SW = StateT W'

type S x = SW Identity

type RU i o s = ReacT i o s ()

type RU' o i s = ReacT i o s ()

type RT s = RU Bit W' s

type RT' s = RU' W' Bit s

zeroW8 :: W'
zeroW8 = lit 0

oneW8 :: W'
oneW8 = lit 1

dwordZero :: DWord
dwordZero = DWord zeroW8 zeroW8 zeroW8 zeroW8

dwordZero' :: DWord'' W'
dwordZero' = DWord' zeroW8 zeroW8 zeroW8 zeroW8

begin :: RT (StateT (W 8) (StateT (W 8) Identity))
begin = lift (put zeroW8) >>= \zz ->
        lift (lift (put oneW8)) >>= \zz ->
        sig

sig :: RT' (SW (S DWord))
sig = do
      r0 <- lift get
      i <- signal r0
      if i then incr else sig

incr :: RT (StateT (W 8) (S W'))
incr = do
      r0 <- lift get
      r1 <- lift (lift get)
      lift (put r1)
      lift (lift (put (r0 + r1)))
      sig

start :: RT' Identity
start = extrude (extrude begin zeroW8) oneW8

main = undefined
