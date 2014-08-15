data Bit is Zero | One end

vhdl bitAnd :: Bit -> Bit -> Bit is prim_bitAnd
vhdl bitNot :: Bit -> Bit is prim_bitNot

go :: <ReT Bit Bit I><Bit>
is
     bind x <- signal Zero
  in bind y <- case x of
               { One  -> signal Zero
               ; Zero -> signal x
               }
  in bind z <- signal (bitAnd x y)
  in           go
end

start :: <ReT Bit Bit I><Bit>
is
  go
end
