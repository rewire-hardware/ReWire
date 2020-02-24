data Bit = Zero | One
data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit

incr :: ReT Bit W8 (StT W8 I) ()
incr = do
      r0 <- lift get
      lift (put r0)
      signal r0
      incr

start :: ReT Bit W8 I ()
start = extrude incr (W8 Zero Zero Zero Zero Zero Zero Zero Zero)
