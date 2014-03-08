data React i o a is D a | P o (i -> React i o a) end
data Bit is Zero | One end

not :: Bit -> Bit
is
  \ b -> case b of
         { One -> Zero
         ; Zero -> One
         }
end

loop :: Bit -> React Bit Bit a
is
  \ b -> P b (\ sw -> case sw of
                      { One  -> loop (not b)
                      ; Zero -> loop b
                      })
end

main :: React Bit Bit a
is
  P Zero (\ sw -> case sw of
                 { One  -> loop One
                  ; Zero -> loop Zero
                  })
end