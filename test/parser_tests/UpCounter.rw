data Tuple2 a b is Tuple2 a b end
data Unit is Unit end
data Bit is Zero | One end

data W8 is W8 Bit Bit Bit Bit Bit Bit Bit Bit end

vhdl plusOne :: W8 -> W8 is prim_plusOne
vhdl rotl :: W8 -> W8 is prim_rotl

tick :: <ReT Bit W8 (StT W8 I)><Bit>
is
     bind n <- lift get
  in signal n
end

main :: <ReT Bit W8 (StT W8 I)><()>
is
     bind b   <- tick
  in bind zzz <-
     case b of
     { One -> bind n   <- lift get
           in lift (put (plusOne n))
     ; Zero -> bind n  <- lift get
           in lift (put (rotl n))
     }
  in main
end

start :: <ReT Bit W8 I><((),W8)>
is
  extrude main (W8 Zero Zero Zero Zero Zero Zero Zero Zero)
end
