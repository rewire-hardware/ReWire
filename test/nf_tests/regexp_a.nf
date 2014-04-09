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



--End Templating

state0 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
(Char  One Zero Zero Zero Zero One One Zero) -> P Zero state2
 ; fail -> P Zero state0
}
end
state2 :: Char -> React Char Bit Nil
is
\ input -> case input of
  {
fail -> P One state0
}
end


main :: React Char Bit Nil
is
P Zero state0
end

