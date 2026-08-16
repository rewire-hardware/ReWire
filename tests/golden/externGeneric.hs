{-# LANGUAGE DataKinds #-}
-- Model-less combinational extern with static module parameters: two
-- instantiations of the same extern at different parameter values (the
-- hand-written implementations are verilog/addk.sv and vhdl/addk.vhdl,
-- which add the parameter K to the input). The parameter rides the
-- extern declaration's generics and each call's generic values, so the
-- two calls must remain distinct instantiations end to end.
import Prelude hiding ((+))
import ReWire
import ReWire.Bits

type W8 = W 8

add3 :: W8 -> W8
add3 = externWithSig [("K", 3)] "" "" [] [] "addk" add3 ""

add5 :: W8 -> W8
add5 = externWithSig [("K", 5)] "" "" [] [] "addk" add5 ""

dev :: W8 -> ReacT W8 W8 Identity ()
dev a = do
      a' <- signal a
      dev (add3 a' + add5 a')

start :: ReacT W8 W8 Identity ()
start = dev (lit 0)

main :: IO ()
main = undefined
