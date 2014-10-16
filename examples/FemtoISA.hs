{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,UndecidableInstances #-}

module FemtoISA where

import Control.Monad.Resumption.Reactive
import Control.Monad.State
import Control.Monad.Identity
import Data.Word
import Data.Bits
import Data.Bits.Extras
import Debug.Trace (trace)

type ReactT = ReacT
type ReT    = ReacT
type StT    = StateT
type I      = Identity

data Bit  = Zero | One deriving (Eq,Show)
data Addr = Addr Bit Bit Bit Bit Bit Bit deriving (Eq,Show)
data Byte = Byte Bit Bit Bit Bit Bit Bit Bit Bit deriving (Eq,Show)
data Reg  = R0 | R1 | R2 | R3 deriving (Eq,Show)

--data Instr = LD Addr | ST Addr | NAND Reg Reg Reg | BZ Addr deriving (Eq,Show)

newtype Inputs = Inputs { dataIn :: Byte }
                    deriving (Eq,Show)
data Outputs   = Outputs { weOut   :: Bit,
                           addrOut :: Addr,
                           dataOut :: Byte }
                    deriving (Eq,Show)
data CPUState = CPUState { r0 :: Byte, r1 :: Byte, r2 :: Byte, r3 :: Byte,
                           pc :: Addr, inputs :: Inputs, outputs :: Outputs }
                         deriving (Eq,Show)

instance MonadState s m => MonadState s (ReactT i o m) where
  get = lift get
  put = lift . put
  
type M = ReT Inputs Outputs (StT CPUState I)

byteToW8 :: Byte -> Word8
byteToW8 (Byte b0 b1 b2 b3 b4 b5 b6 b7) = w8 (bb b0 * 128
                                            + bb b1 * 64
                                            + bb b2 * 32
                                            + bb b3 * 16
                                            + bb b4 * 8
                                            + bb b5 * 4
                                            + bb b6 * 2
                                            + bb b7)
  where bb Zero = 0
        bb One  = 1

w8ToByte :: Word8 -> Byte
w8ToByte w = Byte b0 b1 b2 b3 b4 b5 b6 b7
  where b0 = bb (testBit w 7)
        b1 = bb (testBit w 6)
        b2 = bb (testBit w 5)
        b3 = bb (testBit w 4)
        b4 = bb (testBit w 3)
        b5 = bb (testBit w 2)
        b6 = bb (testBit w 1)
        b7 = bb (testBit w 0)
        bb False = Zero
        bb True  = One

                      
incrByte :: Byte -> Byte
incrByte b = w8ToByte (byteToW8 b + 1)

byteToAddr (Byte _ _ b2 b3 b4 b5 b6 b7) = Addr b2 b3 b4 b5 b6 b7
addrToByte (Addr a0 a1 a2 a3 a4 a5) = Byte Zero Zero a0 a1 a2 a3 a4 a5

incrAddr :: Addr -> Addr
incrAddr a = byteToAddr (incrByte (addrToByte a))

nandBit :: Bit -> Bit -> Bit
nandBit Zero _   = One
nandBit One Zero = One
nandBit One One  = Zero

nandByte :: Byte -> Byte -> Byte
nandByte (Byte a0 a1 a2 a3 a4 a5 a6 a7) (Byte b0 b1 b2 b3 b4 b5 b6 b7)
            = Byte (nandBit a0 b0)
                   (nandBit a1 b1)
                   (nandBit a2 b2)
                   (nandBit a3 b3)
                   (nandBit a4 b4)
                   (nandBit a5 b5)
                   (nandBit a6 b6)
                   (nandBit a7 b7)

getPC :: M Addr
getPC = do s <- get
           return (pc s)
           
putPC :: Addr -> M ()
putPC a = do s <- get
             put (s { pc = a })

incrPC :: M ()
incrPC = do pc <- getPC
            putPC (incrAddr pc)

getReg :: Reg -> M Byte
getReg R0 = do { s <- get ; return (r0 s) }
getReg R1 = do { s <- get ; return (r1 s) }
getReg R2 = do { s <- get ; return (r2 s) }
getReg R3 = do { s <- get ; return (r3 s) }
            
putReg :: Reg -> Byte -> M ()
putReg R0 b = do { s <- get ; put (s { r0 = b }) }
putReg R1 b = do { s <- get ; put (s { r1 = b }) }
putReg R2 b = do { s <- get ; put (s { r2 = b }) }
putReg R3 b = do { s <- get ; put (s { r3 = b }) }

getOutputs :: M Outputs
getOutputs = do { s <- get ; return (outputs s) }

putOutputs :: Outputs -> M ()
putOutputs o = do { s <- get ; put (s { outputs = o }) }

getInputs :: M Inputs
getInputs = do { s <- get ; return (inputs s) }

getDataIn :: M Byte
getDataIn = do { i <- getInputs ; return (dataIn i) }

putInputs :: Inputs -> M ()
putInputs i = do { s <- get ; put (s { inputs = i }) }

tick :: M ()
tick = do o <- getOutputs
          i <- signal o
          putInputs i

putWeOut :: Bit -> M ()
putWeOut b = do o <- getOutputs
                putOutputs (o { weOut = b })

putAddrOut :: Addr -> M ()
putAddrOut a = do o <- getOutputs
                  putOutputs (o { addrOut = a })

putDataOut :: Byte -> M ()
putDataOut d = do o <- getOutputs
                  putOutputs (o { dataOut = d })

finishInstr :: M ()
finishInstr = do pc <- getPC
                 putAddrOut pc
                 putWeOut Zero
                 tick

ld :: Addr -> M ()
ld a = --trace "ld" $
        do putAddrOut a
           putWeOut Zero
           tick
          
           incrPC
           finishInstr
           d <- getDataIn
           putReg R0 d

st :: Addr -> M ()
st a = --trace "st" $
        do d <- getReg R0
           putAddrOut a
           putDataOut d
           putWeOut One
           tick
          
           incrPC
           finishInstr

nand :: Reg -> Reg -> Reg -> M ()
nand rd ra rb = --trace "nand" $
                 do a <- getReg ra
                    b <- getReg rb
                    putReg rd (nandByte a b)
                    incrPC
                    finishInstr

bnz :: Addr -> M ()
bnz a = --trace "bnz" $
         do v <- getReg R0
            case v of
             Byte Zero Zero Zero Zero Zero Zero Zero Zero -> incrPC
             _                                            -> putPC a
            finishInstr

mkReg Zero Zero = R0
mkReg Zero  One = R1
mkReg  One Zero = R2
mkReg  One  One = R3

loop :: M ()
loop = do instr <- getDataIn
          --trace (show instr) $
          case instr of
            Byte Zero Zero a0 a1 a2 a3 a4 a5 -> ld (Addr a0 a1 a2 a3 a4 a5)
            Byte Zero  One a0 a1 a2 a3 a4 a5 -> st (Addr a0 a1 a2 a3 a4 a5)
            Byte  One Zero d0 d1 a0 a1 b0 b1 -> nand (mkReg d0 d1) (mkReg a0 a1) (mkReg b0 b1)
            Byte  One  One a0 a1 a2 a3 a4 a5 -> bnz (Addr a0 a1 a2 a3 a4 a5)
          loop

reset :: M ()
reset = do putPC (Addr Zero Zero Zero Zero Zero Zero)
           putDataOut (Byte Zero Zero Zero Zero Zero Zero Zero Zero)
           finishInstr
           loop

extrude :: Monad m => ReT i o (StT sto m) a -> sto -> ReT i o m (a,sto)
extrude (ReacT m) s = ReacT (runStateT m s >>= \ (dp,s') ->
                             return 
                                 (case dp of
                                       Left v      -> Left (v,s')
                                       Right (q,k) -> Right (q,\ i -> extrude (k i) s')))

start :: ReT Inputs Outputs I ((),CPUState)
start = extrude reset initState
  where initState = CPUState { r0 = byteZero, r1 = byteZero, r2 = byteZero, r3 = byteZero, pc = addrZero, outputs = initOutputs, inputs = initInputs }
        initOutputs = Outputs { weOut = Zero, addrOut = addrZero, dataOut = byteZero }
        initInputs  = Inputs { dataIn = byteZero }
        byteZero = Byte Zero Zero Zero Zero Zero Zero Zero Zero
        addrZero = Addr Zero Zero Zero Zero Zero Zero

--test :: [Outputs]
test = glukk start
  where glukk phi = case runIdentity (deReacT phi) of
                      Left _      -> []
                      Right (o,k) -> (f o,o) : glukk (k (f o))
        f o = --trace (show (weOut o)) $
               case addrOut o of
                Addr Zero Zero Zero Zero Zero Zero -> Inputs (Byte One Zero Zero Zero Zero Zero Zero Zero)
                Addr Zero Zero Zero Zero Zero  One -> Inputs (Byte Zero One  One  One  One  One  One  One)
                _                                  -> Inputs (Byte One  One Zero Zero Zero Zero Zero Zero)