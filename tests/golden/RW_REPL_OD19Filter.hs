{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits
import OD19.ProgramCounter

type W32 = W 32

zeroW32 :: W32
zeroW32 = lit 0

-------
-- Tests 1-4.
-------

-- Each of the following is implemented in VHDL in vhdl/filterprims.vhd.

test1 :: W32 -> W32 -> Bit
test1 = extern "test1" test1

test2 :: W32 -> W32 -> Bit
test2 = extern "test2" test2

test3 :: W32 -> W32 -> Bit
test3 = extern "test3" test3

test4 :: W32 -> W32 -> Bit
test4 = extern "test4" test4

-------
-- end of tests
-------

data YN a  = Yes a | No

filterOD19 :: ReacT (YN W32) (YN Bit) (StateT (W32,PC) Identity) (YN W32)
filterOD19 = repl No

putReg :: W32 -> StateT (W32,PC) Identity ()
putReg w32 = do
  (d,pc) <- get
  put (w32,pc)

nextPC :: StateT (W32,PC) Identity ()
nextPC = do
  (r,pc) <- get
  put (r, incPC pc)

getPC :: StateT (W32,PC) Identity PC
getPC = do
  (_,pc) <- get
  return pc

getReg :: StateT (W32,PC) Identity W32
getReg = do
  (r,_) <- get
  return r

store_wait :: W32 -> ReacT (YN W32) (YN Bit) (StateT (W32, PC) Identity) (YN W32)
store_wait w32 = do
                   lift (putReg w32)
                   lift nextPC
                   signal No

continue :: W32 -> ReacT (YN W32) (YN Bit) (StateT (W32, PC) Identity) (YN W32)
continue w32   = do
                   lift nextPC
                   signal No

{-# INLINE check #-}
check :: (W32 -> W32 -> Bit) -> W32 -> ReacT (YN W32) (YN Bit) (StateT (W32, PC) Identity) (YN W32)
check tst w32   = do
                    r <- lift getReg
                    signal (Yes (tst w32 r))

repl :: YN W32 -> ReacT (YN W32) (YN Bit) (StateT (W32,PC) Identity) (YN W32)
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

start :: ReacT (YN W32) (YN Bit) Identity (YN W32)
start = extrude filterOD19 (zeroW32,PC0)

main = undefined
