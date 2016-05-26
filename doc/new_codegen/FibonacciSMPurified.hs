-- State-monad version of Fibonacci

--
-- The compiler doesn't yet support a "prelude" so we will have to define a
-- few things ourselves!
--
data Bit        = Zero | One
data W8         = W8 Bit Bit Bit Bit Bit Bit Bit Bit

plusW8 :: W8 -> W8 -> W8
{-# INLINE plusW8 #-}
plusW8 = nativeVhdl "plusW8" plusW8

zeroW8 :: W8
zeroW8 = W8 Zero Zero Zero Zero Zero Zero Zero Zero

oneW8 :: W8
oneW8 = W8 Zero Zero Zero Zero Zero Zero Zero One

--
-- End stuff that will eventually be in the prelude.
--

start_pure :: Either () (W8,R)
start_pure = loop_pure zeroW8 oneW8

getN_pure :: W8 -> W8 -> (W8,(W8,W8))
getN_pure s1 s2 = (s1,(s1,s2))

putN_pure :: W8 -> W8 -> W8 -> ((),(W8,W8))
putN_pure x s1 s2 = ((),(x,s2))

getM_pure :: W8 -> W8 -> (W8,(W8,W8))
getM_pure s1 s2 = (s2,(s1,s2))

putM_pure :: W8 -> W8 -> W8 -> ((),(W8,W8))
putM_pure x s1 s2 = ((),(s1,x))

upd_pure :: Bit -> W8 -> W8 -> ((),(W8,W8))
upd_pure One  s1 s2 = ((),(s1,s2))
upd_pure Zero s1 s2 = let (n,(s1,s2)) = getN_pure s1 s2
                          (m,(s1,s2)) = getM_pure s1 s2
                          (_,(s1,s2)) = putN_pure m s1 s2
                      in putM_pure (plusW8 n m) s1 s2

loop_pure :: W8 -> W8 -> Either () (W8,R)
loop_pure s1 s2 = let (n,(s1,s2)) = getN_pure s1 s2
                  in  (Right (n,R_k s1 s2))

k_pure :: Bit -> W8 -> W8 -> Either () (W8,R)
k_pure b s1 s2 = let (_,(s1,s2)) = upd_pure b s1 s2
                 in  loop_pure s1 s2

data R = R_k W8 W8

dispatch :: R -> Bit -> Either () (W8,R)
dispatch (R_k n m) i = k_pure i n m

start :: ReT Bit W8 I ()
start = unfold dispatch start_pure
