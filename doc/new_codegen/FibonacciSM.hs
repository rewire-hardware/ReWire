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

start :: ReT Bit W8 I (((),W8),W8)
start = extrude (extrude loop zeroW8) oneW8

getN :: StT W8 (StT W8 I) W8
getN = get

putN :: W8 -> StT W8 (StT W8 I) ()
putN x = put x

getM :: StT W8 (StT W8 I) W8
getM = lift get

putM :: W8 -> StT W8 (StT W8 I) ()
putM x = lift (put x)

upd :: Bit -> StT W8 (StT W8 I) ()
upd One  = return ()
upd Zero = do n <- getN
              m <- getM
              putN m
              putM (plusW8 n m)

loop :: ReT Bit W8 (StT W8 (StT W8 I)) ()
loop = do n <- lift getN
          b <- signal n
          lift (upd b)
          loop
