data Bit = Zero | One
data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit

grunt :: StT W8 (StT W8 I) ()
grunt = do
  r0 <- get
  lift (put r0)

incr :: ReT Bit W8 (StT W8 (StT W8 I)) ()
incr = do
      --r0 <- lift get
      --lift (lift (put r0))
      lift grunt
      signal (W8 Zero Zero Zero Zero Zero Zero Zero Zero)
      incr

start :: ReT Bit W8 I (((),W8),W8)
start = extrude (extrude incr (W8 Zero Zero Zero Zero Zero Zero Zero Zero)) (W8 Zero Zero Zero Zero Zero Zero Zero One)
