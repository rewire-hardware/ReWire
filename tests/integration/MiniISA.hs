{-# LANGUAGE DataKinds #-}

-- A small CPU / ISA model. Refactored to use the rewire-user bitvector type
-- (W 8) and the ReWire.Bits operators in place of a hand-rolled Bit/W8 algebra
-- and uninterpreted "prim_*" externs, so the whole device is now
-- interpreter-evaluable (cf. tests/golden/Sha256.hs). Bit is ReWire's Bit
-- (= Bool); False/True replace the old C/S. The carry-producing operations are
-- given their conventional semantics via 9-bit arithmetic. The instruction
-- decoder matches on the eight bits of the opcode word (MSB-first, b1 = @.7)
-- instead of a W8 constructor.

import Prelude hiding ((+), (-), (^), (==))
import ReWire
import ReWire.Bits hiding (xor)

type W8 = W 8

zeroW8 :: W8
zeroW8 = lit 0

oneW8 :: W8
oneW8 = lit 1

notb :: Bit -> Bit
notb b = case b of
      True  -> False
      False -> True

msbW8 :: W8 -> Bit
msbW8 v = v @. 7

lsbW8 :: W8 -> Bit
lsbW8 v = v @. 0

-- Add with carry: a + b + cin, returning (carry-out, sum).
plusCW8 :: W8 -> W8 -> Bit -> (Bit, W8)
plusCW8 a b cin = (s @. 8, resize s :: W8)
   where s :: W 9
         s = (resize a :: W 9) + (resize b :: W 9) + (resize (fromList [cin] :: W 1) :: W 9)

-- Subtract with borrow: a - b - cin, returning (borrow-out, difference). The
-- borrow-out (bit 8 of the 9-bit two's-complement result) is set iff a < b + cin.
minusCW8 :: W8 -> W8 -> Bit -> (Bit, W8)
minusCW8 a b cin = (s @. 8, resize s :: W8)
   where s :: W 9
         s = (resize a :: W 9) - (resize b :: W 9) - (resize (fromList [cin] :: W 1) :: W 9)

-- Shift left by one, cin into the LSB; carry-out is the old MSB.
shlCW8 :: W8 -> Bit -> (Bit, W8)
shlCW8 v cin = (msbW8 v, (v <<. lit 1) .|. (resize (fromList [cin] :: W 1) :: W8))

-- Shift right by one, cin into the MSB; carry-out is the old LSB.
shrCW8 :: W8 -> Bit -> (Bit, W8)
shrCW8 v cin = (lsbW8 v, (v >>. lit 1) .|. ((resize (fromList [cin] :: W 1) :: W8) <<. lit 7))

-- Tuple-typed I/O so rwc splits each field into its own top-level port:
--   inputs  __in0 = dataIn (W8), __in1 = rstIn, __in2 = intIn
--   outputs __out0 = addrOut (W8), __out1 = dataOut (W8), __out2 = weOut, __out3 = iackOut
type Inputs = (W8, Bit, Bit)             -- (dataIn,rstIn,intIn)
type Outputs = (W8, W8, Bit, Bit)        -- (addrOut,dataOut,weOut,iackOut)
data CPUState = CPUState Inputs Outputs  -- (inputs,outputs)
                          Bit Bit Bit W8 -- (zFlag,cFlag,ieFlag,pc) (28,29,30,31-38)
                          Bit Bit W8     -- (zsFlag,csFlag,pcSave) (39,40,41-48)
                          W8 W8 W8 W8    -- (r0,r1,r2,r3) (49-56,57-64,65-72,73-80)

data Register = R0 | R1 | R2 | R3

mkReg :: Bit -> Bit -> Register
mkReg False False = R0
mkReg False True  = R1
mkReg True  False = R2
mkReg True  True  = R3

{-# INLINE when #-}
when :: Monad m => Bit -> m () -> m ()
when = \ b -> \ m -> case b of
                       True  -> m
                       False -> return ()

getState :: ReacT Inputs Outputs (StateT CPUState Identity) CPUState
{-# INLINE getState #-}
getState = lift get

outputs :: CPUState -> Outputs
outputs (CPUState _ o _ _ _ _ _ _ _ _ _ _ _) = o

getOutputs :: ReacT Inputs Outputs (StateT CPUState Identity) Outputs
{-# INLINE getOutputs #-}
getOutputs = getState >>= \s -> return (outputs s)

setInputs :: CPUState -> Inputs -> CPUState
setInputs (CPUState _ o z c ie pc zs cs pcs r0 r1 r2 r3) i = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putState :: CPUState -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putState #-}
putState s = lift (put s)

putInputs :: Inputs -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putInputs #-}
putInputs i = getState >>= \s -> putState (setInputs s i)

tick :: ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE tick #-}
tick = getOutputs >>= \o -> signal o >>= \i -> putInputs i

pc :: CPUState -> W8
pc (CPUState _ _ _ _ _ pc _ _ _ _ _ _ _) = pc

getPC :: ReacT Inputs Outputs (StateT CPUState Identity) W8
{-# INLINE getPC #-}
getPC = getState >>= \s -> return (pc s)

setPC :: CPUState -> W8 -> CPUState
setPC (CPUState i o z c ie  _ zs cs pcs r0 r1 r2 r3) pc = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putPC :: W8 -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putPC #-}
putPC pc = getState >>= \s -> putState (setPC s pc)

incrPC :: ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE incrPC #-}
incrPC = getPC >>= \pc -> putPC (snd (plusCW8 pc oneW8 False))

r0 :: CPUState -> W8
r0 (CPUState _ _ _ _ _ _ _ _ _ r0 _ _ _) = r0

r1 :: CPUState -> W8
r1 (CPUState _ _ _ _ _ _ _ _ _ _ r1 _ _) = r1

r2 :: CPUState -> W8
r2 (CPUState _ _ _ _ _ _ _ _ _ _ _ r2 _) = r2

r3 :: CPUState -> W8
r3 (CPUState _ _ _ _ _ _ _ _ _ _ _ _ r3) = r3

getReg :: Register -> ReacT Inputs Outputs (StateT CPUState Identity) W8
{-# INLINE getReg #-}
getReg R0 = getState >>= \s -> return (r0 s)
getReg R1 = getState >>= \s -> return (r1 s)
getReg R2 = getState >>= \s -> return (r2 s)
getReg R3 = getState >>= \s -> return (r3 s)

setR0 :: CPUState -> W8 -> CPUState
setR0 (CPUState i o z c ie pc zs cs pcs _ r1 r2 r3) r0 = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

setR1 :: CPUState -> W8 -> CPUState
setR1 (CPUState i o z c ie pc zs cs pcs r0 _ r2 r3) r1 = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

setR2 :: CPUState -> W8 -> CPUState
setR2 (CPUState i o z c ie pc zs cs pcs r0 r1 _ r3) r2 = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

setR3 :: CPUState -> W8 -> CPUState
setR3 (CPUState i o z c ie pc zs cs pcs r0 r1 r2 _) r3 = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putReg :: Register -> W8 -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putReg #-}
putReg R0 v = getState >>= \s -> putState (setR0 s v)
putReg R1 v = getState >>= \s -> putState (setR1 s v)
putReg R2 v = getState >>= \s -> putState (setR2 s v)
putReg R3 v = getState >>= \s -> putState (setR3 s v)

setPCSave :: CPUState -> W8 -> CPUState
setPCSave (CPUState i o z c ie pc zs cs _ r0 r1 r2 r3) pcs = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putPCSave :: W8 -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putPCSave #-}
putPCSave v = getState >>= \s -> putState (setPCSave s v)

pcSave :: CPUState -> W8
pcSave (CPUState _ _ _ _ _ _ _ _ pcs _ _ _ _) = pcs

getPCSave :: ReacT Inputs Outputs (StateT CPUState Identity) W8
{-# INLINE getPCSave #-}
getPCSave = getState >>= \s -> return (pcSave s)

zFlag :: CPUState -> Bit
zFlag (CPUState _ _ z _ _ _ _ _ _ _ _ _ _) = z

getZFlag :: ReacT Inputs Outputs (StateT CPUState Identity) Bit
{-# INLINE getZFlag #-}
getZFlag = getState >>= \s -> return (zFlag s)

zsFlag :: CPUState -> Bit
zsFlag (CPUState _ _ _ _ _ _ zs _ _ _ _ _ _) = zs

getZSave :: ReacT Inputs Outputs (StateT CPUState Identity) Bit
{-# INLINE getZSave #-}
getZSave = getState >>= \s -> return (zsFlag s)

setZFlag :: CPUState -> Bit -> CPUState
setZFlag (CPUState i o _ c ie pc zs cs pcs r0 r1 r2 r3) z = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putZFlag :: Bit -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putZFlag #-}
putZFlag b = getState >>= \s -> putState (setZFlag s b)

setZSave :: CPUState -> Bit -> CPUState
setZSave (CPUState i o z c ie pc  _ cs pcs r0 r1 r2 r3) zs = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putZSave :: Bit -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putZSave #-}
putZSave b = getState >>= \s -> putState (setZSave s b)

cFlag :: CPUState -> Bit
cFlag (CPUState _ _ _ c _ _ _ _ _ _ _ _ _) = c

getCFlag :: ReacT Inputs Outputs (StateT CPUState Identity) Bit
{-# INLINE getCFlag #-}
getCFlag = getState >>= \s -> return (cFlag s)

csFlag :: CPUState -> Bit
csFlag (CPUState _ _ _ _ _ _ _ cs _ _ _ _ _) = cs

getCSave :: ReacT Inputs Outputs (StateT CPUState Identity) Bit
{-# INLINE getCSave #-}
getCSave = getState >>= \s -> return (csFlag s)

setCFlag :: CPUState -> Bit -> CPUState
setCFlag (CPUState i o z _ ie pc zs cs pcs r0 r1 r2 r3) c = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putCFlag :: Bit -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putCFlag #-}
putCFlag b = getState >>= \s -> putState (setCFlag s b)

setCSave :: CPUState -> Bit -> CPUState
setCSave (CPUState i o z c ie pc zs  _ pcs r0 r1 r2 r3) cs = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putCSave :: Bit -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putCSave #-}
putCSave b = getState >>= \s -> putState (setCSave s b)

setIEFlag :: CPUState -> Bit -> CPUState
setIEFlag (CPUState i o z c  _ pc zs cs pcs r0 r1 r2 r3) ie = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putIEFlag :: Bit -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putIEFlag #-}
putIEFlag b = getState >>= \s -> putState (setIEFlag s b)

ieFlag :: CPUState -> Bit
{-# INLINE ieFlag #-}
ieFlag (CPUState _ _ _ _ ie _ _ _ _ _ _ _ _) = ie

getIEFlag :: ReacT Inputs Outputs (StateT CPUState Identity) Bit
{-# INLINE getIEFlag #-}
getIEFlag = getState >>= \s -> return (ieFlag s)

inputs :: CPUState -> Inputs
inputs (CPUState i _ _ _ _ _ _ _ _ _ _ _ _) = i

getInputs :: ReacT Inputs Outputs (StateT CPUState Identity) Inputs
{-# INLINE getInputs #-}
getInputs = getState >>= \s -> return (inputs s)

setOutputs :: CPUState -> Outputs -> CPUState
setOutputs (CPUState i _ z c ie pc zs cs pcs r0 r1 r2 r3) o = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putOutputs :: Outputs -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putOutputs #-}
putOutputs o = getState >>= \s -> putState (setOutputs s o)

setAddrOut :: Outputs -> W8 -> Outputs
setAddrOut (_, d_o, we_o, iack_o) a_o = (a_o, d_o, we_o, iack_o)

putAddrOut :: W8 -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putAddrOut #-}
putAddrOut a = getOutputs >>= \o -> putOutputs (setAddrOut o a)

setDataOut :: Outputs -> W8 -> Outputs
setDataOut (a_o, _, we_o, iack_o) d_o = (a_o, d_o, we_o, iack_o)

putDataOut :: W8 -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putDataOut #-}
putDataOut d = getOutputs >>= \o -> putOutputs (setDataOut o d)

dataIn :: Inputs -> W8
dataIn (d_i, _, _) = d_i

getDataIn :: ReacT Inputs Outputs (StateT CPUState Identity) W8
{-# INLINE getDataIn #-}
getDataIn = getInputs >>= \i -> return (dataIn i)

setWeOut :: Outputs -> Bit -> Outputs
setWeOut (a_o, d_o, _, iack_o) we_o = (a_o, d_o, we_o, iack_o)

putWeOut :: Bit -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putWeOut #-}
putWeOut we = getOutputs >>= \o -> putOutputs (setWeOut o we)

setIackOut :: Outputs -> Bit -> Outputs
setIackOut (a_o, d_o, we_o, _) iack_o = (a_o, d_o, we_o, iack_o)

putIackOut :: Bit -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putIackOut #-}
putIackOut iack = getOutputs >>= \o -> putOutputs (setIackOut o iack)

mem :: Bit -> Bit -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE mem #-}
mem rEn wEn r = do
      pc <- getPC
      putAddrOut pc
      tick
      a <- getDataIn
      putAddrOut a
      putWeOut wEn
      when wEn (getReg r >>= \ x -> putDataOut x)
      incrPC
      tick
      when rEn (getDataIn >>= \ x -> putReg r x)

ld :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE ld #-}
ld rD rS = do
      a <- getReg rS
      putWeOut False
      putAddrOut a
      tick
      v <- getDataIn
      putReg rD v

st :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE st #-}
st rD rS = do
      a <- getReg rS
      v <- getReg rD
      putWeOut True
      putDataOut v
      putAddrOut a
      tick

add :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE add #-}
add rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let p    =  plusCW8 vD vS False
      let cout =  fst p
      let vD'  =  snd p
      putCFlag cout
      putReg rD vD'
      tick

addc :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE addc #-}
addc rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      cin <- getCFlag
      let p    =  plusCW8 vD vS cin
      let cout =  fst p
      let vD'  =  snd p
      putCFlag cout
      putReg rD vD'
      tick

sub :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE sub #-}
sub rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let p            =  minusCW8 vD vS False
      let cout         =  fst p
      let vD'          =  snd p
      putCFlag cout
      putReg rD vD'
      tick

subb :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE subb #-}
subb rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      cin <- getCFlag
      let p               =  minusCW8 vD vS cin
      let cout            =  fst p
      let vD'             =  snd p
      putCFlag cout
      putReg rD vD'
      tick

mov :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE mov #-}
mov rD rS = getReg rS >>= \ x -> putReg rD x >>= \ zzz -> tick

or' :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE or' #-}
or' rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let vD'      =  vD .|. vS
      putReg rD vD'
      putCFlag False
      putZFlag (vD' == lit 0)
      tick

and' :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE and' #-}
and' rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let vD'      =  vD .&. vS
      putReg rD vD'
      putCFlag False
      putZFlag (vD' == lit 0)
      tick

xor :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE xor #-}
xor rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let vD'      =  vD ^ vS
      putReg rD vD'
      putCFlag False
      putZFlag (vD' == lit 0)
      tick

cmp :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE cmp #-}
cmp rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let p    =  minusCW8 vD vS False
      let c    =  fst p
      let r    =  snd p
      putCFlag c
      putZFlag (r == lit 0)
      tick

brz :: Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE brz #-}
brz r = do
      z <- getZFlag
      when z (getReg r >>= \ x -> putPC x)
      tick

brnz :: Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE brnz #-}
brnz r = do
      z <- getZFlag
      when (notb z) (getReg r >>= \ x -> putPC x)
      tick

brc :: Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE brc #-}
brc r = do
      c <- getCFlag
      when c (getReg r >>= \ x -> putPC x)
      tick

brnc :: Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE brnc #-}
brnc r = do
      c <- getCFlag
      when (notb c) (getReg r >>= \ x -> putPC x)
      tick

jmp :: Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE jmp #-}
jmp r = getReg r >>= \ x -> putPC x >>= \zzz -> tick

ien :: Bit -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE ien #-}
ien b = putIEFlag b >>= \zzz -> tick

iack :: ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE iack #-}
iack = putIackOut True >>= \zzz -> tick

iret :: ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE iret #-}
iret = do
      putIEFlag True
      pc <- getPCSave
      putPC pc
      z <- getZSave
      putZFlag z
      c <- getCSave
      putCFlag c
      tick

not' :: Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE not' #-}
not' r = do
      v <- getReg r
      putReg r (bnot v)
      tick

clrr :: Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE clrr #-}
clrr r = putReg r zeroW8 >>= \zzz -> tick

incr :: Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE incr #-}
incr r = do
      v <- getReg r
      let p    =  plusCW8 v oneW8 False
      let cout =  fst p
      let v'   =  snd p
      putReg r v'
      putCFlag cout
      putZFlag (v' == lit 0)
      tick

decr :: Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE decr #-}
decr r = do
      v <- getReg r
      let p    =  minusCW8 v oneW8 False
      let cout =  fst p
      let v'   =  snd p
      putReg r v'
      putCFlag cout
      putZFlag (v' == lit 0)
      tick

rot :: Bit -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE rot #-}
rot False r = do
      v <- getReg r
      putReg r (rotL (lit 1) v)
      tick
rot True  r = do
      v <- getReg r
      putReg r (rotR (lit 1) v)
      tick

shft :: Bit -> Bit -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE shft #-}
shft l d r = do
      v <- getReg r
      let p    = case (d,l) of
            (False, False) -> shlCW8 v (msbW8 v)
            (False, True)  -> shlCW8 v False
            (True,  False) -> shrCW8 v (lsbW8 v)
            (True,  True)  -> shrCW8 v False
      let cout =  fst p
      let v'   =  snd p
      putReg r v'
      putCFlag cout
      putZFlag (v' == lit 0)
      tick

initOutputs :: Outputs
initOutputs = (zeroW8, zeroW8, False, False)

reset :: ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE reset #-}
reset = do
      putCFlag False
      putZFlag False
      putOutputs initOutputs
      tick

interrupt :: ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE interrupt #-}
interrupt = do
      putIEFlag False
      pc <- getPC
      z <- getZFlag
      c <- getCFlag
      putPCSave pc
      putZSave z
      putCSave c
      tick

rstIn :: Inputs -> Bit
rstIn (_, r_i, _) = r_i

intIn :: Inputs -> Bit
intIn (_, _, i_i) = i_i

loop :: ReacT Inputs Outputs (StateT CPUState Identity) ()
loop = do
      inp <- getInputs
      case rstIn inp of
            True  -> reset
            False -> getIEFlag >>= \ie ->
                  case (ie, intIn inp) of
                        (True, True) -> interrupt
                        _            -> decode (dataIn inp)
      loop

-- Decode and execute one instruction. The eight bits are taken MSB-first
-- (b1 = @.7 .. b8 = @.0), matching the old W8 constructor field order.
decode :: W8 -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE decode #-}
decode i = case (i @. 7, i @. 6, i @. 5, i @. 4, i @. 3, i @. 2, i @. 1, i @. 0) of
      (False, False, False, False, rEn, wEn, b0, b1) -> mem rEn wEn (mkReg b0 b1)
      (False, False, False, True,  b0,  b1,  c0, c1) -> ld   (mkReg b0 b1) (mkReg c0 c1)
      (False, False, True,  False, b0,  b1,  c0, c1) -> st   (mkReg b0 b1) (mkReg c0 c1)
      (False, False, True,  True,  b0,  b1,  c0, c1) -> add  (mkReg b0 b1) (mkReg c0 c1)
      (False, True,  False, False, b0,  b1,  c0, c1) -> addc (mkReg b0 b1) (mkReg c0 c1)
      (False, True,  False, True,  b0,  b1,  c0, c1) -> sub  (mkReg b0 b1) (mkReg c0 c1)
      (False, True,  True,  False, b0,  b1,  c0, c1) -> subb (mkReg b0 b1) (mkReg c0 c1)
      (False, True,  True,  True,  b0,  b1,  c0, c1) -> mov  (mkReg b0 b1) (mkReg c0 c1)
      (True,  False, False, False, b0,  b1,  c0, c1) -> or'  (mkReg b0 b1) (mkReg c0 c1)
      (True,  False, False, True,  b0,  b1,  c0, c1) -> and' (mkReg b0 b1) (mkReg c0 c1)
      (True,  False, True,  False, b0,  b1,  c0, c1) -> xor  (mkReg b0 b1) (mkReg c0 c1)
      (True,  False, True,  True,  b0,  b1,  c0, c1) -> cmp  (mkReg b0 b1) (mkReg c0 c1)
      (True,  True,  False, False, False, False, b0, b1) -> brz  (mkReg b0 b1)
      (True,  True,  False, False, False, True,  b0, b1) -> brnz (mkReg b0 b1)
      (True,  True,  False, False, True,  False, b0, b1) -> brc  (mkReg b0 b1)
      (True,  True,  False, False, True,  True,  b0, b1) -> brnc (mkReg b0 b1)
      (True,  True,  False, True,  False, False, b0, b1) -> jmp  (mkReg b0 b1)
      (True,  True,  False, True,  False, True,  False, b0) -> ien b0
      (True,  True,  False, True,  False, True,  True,  False) -> iack
      (True,  True,  False, True,  False, True,  True,  True)  -> iret
      (True,  True,  False, True,  True,  False, b0, b1) -> not'  (mkReg b0 b1)
      (True,  True,  False, True,  True,  True,  b0, b1) -> clrr  (mkReg b0 b1)
      (True,  True,  True,  False, False, False, b0, b1) -> incr  (mkReg b0 b1)
      (True,  True,  True,  False, False, True,  b0, b1) -> decr  (mkReg b0 b1)
      (True,  True,  True,  False, True,  d,     b0, b1) -> rot d  (mkReg b0 b1)
      (True,  True,  True,  True,  l,     d,     b0, b1) -> shft l d (mkReg b0 b1)

go :: ReacT Inputs Outputs (StateT CPUState Identity) ()
go = reset >> loop

initInputs :: Inputs
initInputs = (zeroW8, False, False)

initState :: CPUState
initState = CPUState initInputs initOutputs False False False zeroW8 False False zeroW8 zeroW8 zeroW8 zeroW8 zeroW8

start :: ReacT Inputs Outputs Identity ()
start = extrude go initState

main = undefined
