import ReWire
import ReWire.Bits
import OD19Filter.ProgramCounter

-------
-- Tests 1-4.
-------

-- Each of the following is implemented in VHDL in vhdl/filterprims.vhd.

test1 :: W32 -> W32 -> Bit
test1 = nativeVhdl "test1" test1

test2 :: W32 -> W32 -> Bit
test2 = nativeVhdl "test2" test2

test3 :: W32 -> W32 -> Bit
test3 = nativeVhdl "test3" test3

test4 :: W32 -> W32 -> Bit
test4 = nativeVhdl "test4" test4


-------
-- end of tests
-------

data YN a  = Yes a | No

filterOD19 :: ReT (YN W32) (YN Bit) (StT (W32,PC) I) (YN W32)
filterOD19 = repl No

putReg :: W32 -> StT (W32,PC) I ()
putReg w32 = do
  (d,pc) <- get
  put (w32,pc)

nextPC :: StT (W32,PC) I ()
nextPC = do
  (r,pc) <- get
  put (r, incPC pc)

getPC :: StT (W32,PC) I PC
getPC = do
  (_,pc) <- get
  return pc

getReg :: StT (W32,PC) I W32
getReg = do
  (r,_) <- get
  return r

store_wait :: W32 -> ReT (YN W32) (YN Bit) (StT (W32, PC) I) (YN W32)
store_wait w32 = do
                   lift (putReg w32)
                   lift nextPC
                   signal No

continue :: W32 -> ReT (YN W32) (YN Bit) (StT (W32, PC) I) (YN W32)
continue w32   = do
                   lift nextPC
                   signal No

{-# INLINE check #-}
check :: (W32 -> W32 -> Bit) -> W32 -> ReT (YN W32) (YN Bit) (StT (W32, PC) I) (YN W32)
check tst w32   = do
                    r <- lift getReg
                    signal (Yes (tst w32 r))

repl :: YN W32 -> ReT (YN W32) (YN Bit) (StT (W32,PC) I) (YN W32)
repl (Yes w32) = do
                   (_,pc) <- lift get
                   case pc of
                     PC0 -> do
                              i <- store_wait w32
                              repl i
                     PC1 -> do
                              i <- check test1 w32
                              repl i
                     PC2 -> do
                              i <- store_wait w32
                              repl i
                     PC3 -> do
                              i <- check test1 w32
                              repl i
                     PC4 -> do
                              i <- check test2 w32
                              repl i
                     PC5 -> do
                              i <- check test3 w32
                              repl i
                     PC6 -> do
                              i <- check test4 w32
                              repl i
                     _   -> do
                              i <- continue w32
                              repl i
repl No        = do
                   i <- signal No
                   repl i

start :: ReT (YN W32) (YN Bit) I (YN W32)
start = extrude filterOD19 (zeroW32,PC0)

main = undefined
