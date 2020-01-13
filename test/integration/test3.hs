data Bit = Zero | One

data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit

tick :: ReT Bit W8 (StT W8 I) Bit
tick = lift get >>= \ x -> signal x

main :: ReT Bit W8 (StT W8 I) ()
main = do
      b <- tick
      case b of
            One  -> main
            Zero -> main


start :: ReT Bit W8 I ((),W8)
start = extrude main (W8 Zero Zero Zero Zero Zero Zero Zero Zero)
