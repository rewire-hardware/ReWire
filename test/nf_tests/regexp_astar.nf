data React i o a is D a | P o (i -> React i o a) end
data Bit is Zero | One end
data Nil is end
data Char is Char Bit Bit Bit Bit Bit Bit Bit Bit end

not :: Bit -> Bit
is
  \ b -> case b of
         { One -> Zero
         ; Zero -> One
         }
end

failstate :: Char -> React Char Bit Nil
is
  \ xxx -> P Zero failstate
end


--End Templating

state0 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P One state3
 ; fail -> P One failstate
}
end
state3 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P One state3
 ; fail -> P One failstate
}
end


main :: React Char Bit Nil
is
P Zero state0
end


