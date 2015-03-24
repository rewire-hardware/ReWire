data Bit is Zero | One end
data Tuple4 a b c d is Tuple4 a b c d end
data Unit is Unit end

start :: <ReT Bit Bit (StT (Bit,Bit,Bit,Bit) I)><()>
is
  return ()
end
