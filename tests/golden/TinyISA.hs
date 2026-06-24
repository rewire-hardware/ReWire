{-# LANGUAGE DataKinds #-}
import Prelude hiding ((^), (+), (==), (&&))
import ReWire.Bits
import ReWire

data Reg   = R0 | R1 | R2 | R3
type Addr  = W 6
data Instr = NOP
           | LD Addr
           | ST Addr
           | NAND Reg Reg Reg
           | BNZ Addr

data Ins     = Ins { instrIn :: Instr,
                     dataIn  :: W 8 }
data Out     = Out { weOut   :: Bit,
                     addrOut :: Addr,
                     dataOut :: W 8 }
data RegFile = RegFile { r0 :: W 8, r1 :: W 8, r2 :: W 8, r3 :: W 8,
                         pc :: Addr, inputs :: Ins, outputs :: Out }

type S   = StateT RegFile Identity
type Dev = ReacT Ins Out S

getPC :: S Addr
getPC = do s <- get
           return (pc s)

putPC :: Addr -> S ()
putPC a = do s <- get
             put (s { pc = a })

incrPC :: S ()
incrPC = do pc <- getPC
            putPC (pc + lit 1)

getReg :: Reg -> S (W 8)
getReg R0 = get >>= return . r0
getReg R1 = get >>= return . r1
getReg R2 = get >>= return . r2
getReg R3 = get >>= return . r3

putReg :: Reg -> W 8 -> S ()
putReg R0 b = get >>= \ s -> put (s { r0 = b })
putReg R1 b = get >>= \ s -> put (s { r1 = b })
putReg R2 b = get >>= \ s -> put (s { r2 = b })
putReg R3 b = get >>= \ s -> put (s { r3 = b })

getOut :: S Out
getOut   = do
             s <- get
             return (outputs s)

putOut :: Out -> S ()
putOut o = do
             s <- get
             put (s { outputs = o })

getIns :: S Ins
getIns   = do
             s <- get
             return (inputs s)

getDataIn :: S (W 8)
getDataIn = do
              i <- getIns
              return (dataIn i)

getInstr :: S Instr
getInstr = do
             i <- getIns
             return (instrIn i)

putIns :: Ins -> S ()
putIns i = do
             s <- get
             put (s { inputs = i })

tick :: Dev ()
tick     = do o <- lift getOut
              i <- signal o
              lift (putIns i)

putWeOut :: Bit -> S ()
putWeOut b = do o <- getOut
                putOut (o { weOut = b })

putAddrOut :: Addr -> S ()
putAddrOut a = do o <- getOut
                  putOut (o { addrOut = a })

putDataOut :: W 8 -> S ()
putDataOut d = do o <- getOut
                  putOut (o { dataOut = d })

finishInstr :: Dev ()
finishInstr = do
                 lift $ do pc <- getPC
                           putAddrOut pc
                           putWeOut zero
                 tick

-- | Microcode

nop :: Dev ()
nop  = do
         lift incrPC
         finishInstr

ld :: Addr -> Dev ()
ld a = do
         lift $ do putAddrOut a
                   putWeOut zero
         tick
         lift incrPC
         finishInstr
         lift $ do d <- getDataIn
                   putReg R0 d

st :: Addr -> Dev ()
st a = do
          lift $ do
                    d <- getReg R0
                    putAddrOut a
                    putDataOut d
                    putWeOut one
          tick
          lift incrPC
          finishInstr

nand :: Reg -> Reg -> Reg -> Dev ()
nand rd ra rb = do
                   lift $ do
                     a <- getReg ra
                     b <- getReg rb
                     putReg rd (bnot (a .&. b))
                     incrPC
                   finishInstr

bnz :: Addr -> Dev ()
bnz a = do
           lift $ do
             v <- getReg R0
             if v == lit 0 then incrPC else putPC a
           finishInstr


reset :: Dev ()
reset = do
           lift $ do
             putPC (lit 0)
             putDataOut (lit 0)
           tick
           finishInstr
           loop

loop :: ReacT Ins Out (StateT RegFile Identity) ()
loop = do instr <- lift getInstr
          s     <- lift get
          case instr of
            NOP           -> nop
            LD a          -> ld a
            ST a          -> st a
            NAND rd r1 r2 -> nand rd r1 r2
            BNZ a         -> bnz a
          loop

start :: ReacT Ins Out Identity ()
start = extrude reset initState
   where
     initOut :: Out
     initOut = Out { weOut = zero,
                             addrOut = lit 0,
                             dataOut = lit 0 }
     initIns :: Ins
     initIns  = Ins  { instrIn = NOP,
                             dataIn = lit 0 }
     initState :: RegFile
     initState = RegFile { r0 = lit 0,
                           r1 = lit 0,
                           r2 = lit 0,
                           r3 = lit 0,
                           pc = lit 0,
                           outputs = initOut,
                           inputs  = initIns }

main = undefined
