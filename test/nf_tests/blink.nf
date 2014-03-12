data React i o a is D a | P o (i -> React i o a) end
data Bit is Zero | One end
data Nil is end

not :: Bit -> Bit
is
  \ b -> case b of
         { One -> Zero
         ; Zero -> One
         }
end

-- This is a "continuer".
-- It has to be of the form \ x1 -> \ x2 -> .. -> \ xn -> \ xi -> {case producing a pauser call}.
-- Where n >= 0.
k :: Bit -> Bit -> React Bit Bit Nil
is
  \ b -> \ sw -> case sw of
                 { One  -> loop (not b)
                 ; Zero -> loop b
                 }
end

-- This is a "pauser".
-- It has to be of the form \ x1 -> \ x2 -> .. -> \ xn -> {case producing a pause}.
-- Where n >= 0.
--
-- A pause is an expression of the form P e1 e2 where e2 is a continuer call.
loop :: Bit -> React Bit Bit Nil
is
  \ b -> P b (k b)
end

-- Main is a "pauser" with arity 0.
main :: React Bit Bit Nil
is
  P Zero (k Zero)
end