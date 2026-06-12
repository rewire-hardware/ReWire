-- Externs with user-supplied Haskell models: the implementation argument
-- (rwPrimExtern's seventh) references a top-level definition, so the
-- interpreter (and the Cryptol backend) can evaluate the extern. The
-- hardware implementations live in verilog/w32ops.sv and vhdl/w32ops.vhdl,
-- and must agree with the models (the cosimulation check enforces this).
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit, (+), (.&.), (^), bnot)
import ReWire.Monad (iter, Dev)
import Prelude hiding ((+), (^))

type W32 = W 32

plusW32 :: W32 -> W32 -> W32
plusW32 = extern "plusW32" plusModel

plusModel :: W32 -> W32 -> W32
plusModel a b = a + b

andW32 :: W32 -> W32 -> W32
andW32 = extern "andW32" andModel

andModel :: W32 -> W32 -> W32
andModel a b = a .&. b

xorW32 :: W32 -> W32 -> W32
xorW32 = extern "xorW32" xorModel

xorModel :: W32 -> W32 -> W32
xorModel a b = a ^ b

notW32 :: W32 -> W32
notW32 = extern "notW32" notModel

notModel :: W32 -> W32
notModel = bnot

f :: W32 -> W32
f x = plusW32 (andW32 x (lit 0xF0F0F0F0)) (xorW32 (notW32 x) (lit 0x12345678))

start :: Dev W32 W32
start = iter f (lit 0)

main = undefined
