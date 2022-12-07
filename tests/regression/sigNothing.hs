import ReWire
import ReWire.Bits

data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit

zeroW8 :: W8
zeroW8 = W8 zero zero zero zero zero zero zero zero

start' :: ReacT W8 (Maybe W8) (StateT W8 Identity) ()
start' = signal Nothing >>= ((\ _ -> start') :: W8 -> ReacT W8 (Maybe W8) (StateT W8 Identity) ())

start :: ReacT W8 (Maybe W8) Identity ()
start = extrude start' zeroW8

main = undefined
