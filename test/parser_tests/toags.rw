data Bit is Zero | One end

vhdl bitAnd :: Bit -> Bit -> Bit is prim_bitAnd
vhdl bitNot :: Bit -> Bit is prim_bitNot

go :: <ReT Bit Bit (StT Bit (StT Bit I))><Bit>
is
     bind x  <- signal Zero
  in bind y  <- case x of
                { One  -> signal Zero
                ; Zero -> lift get
                }
  in bind z  <- signal (bitAnd (case y of { One -> x ; Zero -> bitAnd x x }) y)
  in bind zz <- lift (lift (put y))
  in            go
end

start :: <ReT Bit Bit I><((Bit,Bit),Bit)>
is
  extrude (extrude go Zero) One
end
