data Bit = Zero | One
data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit

grunt :: StT W8 I ()
grunt = do
  x <- get
  put x

incr :: ReT Bit W8 (StT W8 I) ()
incr = do
      lift grunt
      incr

start :: ReT Bit W8 I ((),W8)
start = extrude incr (W8 Zero Zero Zero Zero Zero Zero Zero Zero)
