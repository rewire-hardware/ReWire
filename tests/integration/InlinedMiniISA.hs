import ReWire
import ReWire.Bits

data Inputs = Inputs W8 Bit Bit          -- (dataIn,rstIn,intIn)
data Outputs = Outputs W8 W8 Bit Bit     -- (addrOut,dataOut,weOut,iackOut)
data CPUState = CPUState Inputs Outputs  -- (inputs,outputs) (0-9,10-27)
                          Bit Bit Bit W8 -- (zFlag,cFlag,ieFlag,pc) (28,29,30,31-38)
                          Bit Bit W8     -- (zsFlag,csFlag,pcSave) (39,40,41-48)
                          W8 W8 W8 W8    -- (r0,r1,r2,r3) (49-56,57-64,65-72,73-80)
data Register = R0 | R1 | R2 | R3

plusCW8  :: W8 -> W8 -> Bit -> (Bit,W8)
plusCW8  =  nativeVhdl "prim_plusCW8" plusCW8

minusCW8 :: W8 -> W8 -> Bit -> (Bit,W8)
minusCW8 =  nativeVhdl "prim_minusCW8" minusCW8

shlCW8   :: W8 -> Bit -> (Bit,W8)
shlCW8   =  nativeVhdl "prim_shlCW8" shlCW8

shrCW8   :: W8 -> Bit -> (Bit,W8)
shrCW8   =  nativeVhdl "prim_shrCW8" shrCW8

msbW8    :: W8 -> Bit
msbW8    =  nativeVhdl "prim_msbW8" msbW8

lsbW8    :: W8 -> Bit
lsbW8    =  nativeVhdl "prim_lsbW8" lsbW8

mkReg :: Bit -> Bit -> Register
mkReg C C = R0
mkReg C S = R1
mkReg S C = R2
mkReg S S = R3

{-
{-# INLINE when #-}
when :: Monad m => Bit -> m () -> m ()
when = \ b -> \ m -> case b of
                       S  -> m
                       C -> return ()
-}

getState :: ReT Inputs Outputs (StT CPUState I) CPUState
{-# INLINE getState #-}
getState = lift get

outputs :: CPUState -> Outputs
outputs (CPUState _ o _ _ _ _ _ _ _ _ _ _ _) = o

getOutputs :: ReT Inputs Outputs (StT CPUState I) Outputs
{-# INLINE getOutputs #-}
getOutputs = getState >>= \s -> return (outputs s)

setInputs :: CPUState -> Inputs -> CPUState
setInputs (CPUState _ o z c ie pc zs cs pcs r0 r1 r2 r3) i = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putState :: CPUState -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE putState #-}
putState s = lift (put s)

putInputs :: Inputs -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE putInputs #-}
putInputs i = getState >>= \s -> putState (setInputs s i)

tick :: ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE tick #-}
tick = getOutputs >>= \o -> signal o >>= \i -> putInputs i

pc :: CPUState -> W8
pc (CPUState _ _ _ _ _ pc _ _ _ _ _ _ _) = pc

getPC :: ReT Inputs Outputs (StT CPUState I) W8
{-# INLINE getPC #-}
getPC = getState >>= \s -> return (pc s)

setPC :: CPUState -> W8 -> CPUState
setPC (CPUState i o z c ie  _ zs cs pcs r0 r1 r2 r3) pc = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putPC :: W8 -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE putPC #-}
putPC pc = getState >>= \s -> putState (setPC s pc)

incrPC :: ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE incrPC #-}
incrPC = getPC >>= \pc -> putPC (snd (plusCW8 pc oneW8 C))

r0 :: CPUState -> W8
r0 (CPUState _ _ _ _ _ _ _ _ _ r0 _ _ _) = r0

r1 :: CPUState -> W8
r1 (CPUState _ _ _ _ _ _ _ _ _ _ r1 _ _) = r1

r2 :: CPUState -> W8
r2 (CPUState _ _ _ _ _ _ _ _ _ _ _ r2 _) = r2

r3 :: CPUState -> W8
r3 (CPUState _ _ _ _ _ _ _ _ _ _ _ _ r3) = r3

getReg :: Register -> ReT Inputs Outputs (StT CPUState I) W8
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

putReg :: Register -> W8 -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE putReg #-}
putReg R0 v = getState >>= \s -> putState (setR0 s v)
putReg R1 v = getState >>= \s -> putState (setR1 s v)
putReg R2 v = getState >>= \s -> putState (setR2 s v)
putReg R3 v = getState >>= \s -> putState (setR3 s v)

setPCSave :: CPUState -> W8 -> CPUState
setPCSave (CPUState i o z c ie pc zs cs _ r0 r1 r2 r3) pcs = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putPCSave :: W8 -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE putPCSave #-}
putPCSave v = getState >>= \s -> putState (setPCSave s v)

pcSave :: CPUState -> W8
pcSave (CPUState _ _ _ _ _ _ _ _ pcs _ _ _ _) = pcs

getPCSave :: ReT Inputs Outputs (StT CPUState I) W8
{-# INLINE getPCSave #-}
getPCSave = getState >>= \s -> return (pcSave s)

zFlag :: CPUState -> Bit
zFlag (CPUState _ _ z _ _ _ _ _ _ _ _ _ _) = z

getZFlag :: ReT Inputs Outputs (StT CPUState I) Bit
{-# INLINE getZFlag #-}
getZFlag = getState >>= \s -> return (zFlag s)

zsFlag :: CPUState -> Bit
zsFlag (CPUState _ _ _ _ _ _ zs _ _ _ _ _ _) = zs

getZSave :: ReT Inputs Outputs (StT CPUState I) Bit
{-# INLINE getZSave #-}
getZSave = getState >>= \s -> return (zsFlag s)

setZFlag :: CPUState -> Bit -> CPUState
setZFlag (CPUState i o _ c ie pc zs cs pcs r0 r1 r2 r3) z = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putZFlag :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE putZFlag #-}
putZFlag b = getState >>= \s -> putState (setZFlag s b)

setZSave :: CPUState -> Bit -> CPUState
setZSave (CPUState i o z c ie pc  _ cs pcs r0 r1 r2 r3) zs = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putZSave :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE putZSave #-}
putZSave b = getState >>= \s -> putState (setZSave s b)

cFlag :: CPUState -> Bit
cFlag (CPUState _ _ _ c _ _ _ _ _ _ _ _ _) = c

getCFlag :: ReT Inputs Outputs (StT CPUState I) Bit
{-# INLINE getCFlag #-}
getCFlag = getState >>= \s -> return (cFlag s)

csFlag :: CPUState -> Bit
csFlag (CPUState _ _ _ _ _ _ _ cs _ _ _ _ _) = cs

getCSave :: ReT Inputs Outputs (StT CPUState I) Bit
{-# INLINE getCSave #-}
getCSave = getState >>= \s -> return (csFlag s)

setCFlag :: CPUState -> Bit -> CPUState
setCFlag (CPUState i o z _ ie pc zs cs pcs r0 r1 r2 r3) c = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putCFlag :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE putCFlag #-}
putCFlag b = getState >>= \s -> putState (setCFlag s b)

setCSave :: CPUState -> Bit -> CPUState
setCSave (CPUState i o z c ie pc zs  _ pcs r0 r1 r2 r3) cs = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putCSave :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE putCSave #-}
putCSave b = getState >>= \s -> putState (setCSave s b)

setIEFlag :: CPUState -> Bit -> CPUState
setIEFlag (CPUState i o z c  _ pc zs cs pcs r0 r1 r2 r3) ie = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putIEFlag :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE putIEFlag #-}
putIEFlag b = getState >>= \s -> putState (setIEFlag s b)

ieFlag :: CPUState -> Bit
{-# INLINE ieFlag #-}
ieFlag (CPUState _ _ _ _ ie _ _ _ _ _ _ _ _) = ie

getIEFlag :: ReT Inputs Outputs (StT CPUState I) Bit
{-# INLINE getIEFlag #-}
getIEFlag = getState >>= \s -> return (ieFlag s)

inputs :: CPUState -> Inputs
inputs (CPUState i _ _ _ _ _ _ _ _ _ _ _ _) = i

getInputs :: ReT Inputs Outputs (StT CPUState I) Inputs
{-# INLINE getInputs #-}
getInputs = getState >>= \s -> return (inputs s)

setOutputs :: CPUState -> Outputs -> CPUState
setOutputs (CPUState i _ z c ie pc zs cs pcs r0 r1 r2 r3) o = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putOutputs :: Outputs -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE putOutputs #-}
putOutputs o = getState >>= \s -> putState (setOutputs s o)

setAddrOut :: Outputs -> W8 -> Outputs
setAddrOut (Outputs _ d_o we_o iack_o) a_o = Outputs a_o d_o we_o iack_o

putAddrOut :: W8 -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE putAddrOut #-}
putAddrOut a = getOutputs >>= \o -> putOutputs (setAddrOut o a)

setDataOut :: Outputs -> W8 -> Outputs
setDataOut (Outputs a_o _ we_o iack_o) d_o = Outputs a_o d_o we_o iack_o

putDataOut :: W8 -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE putDataOut #-}
putDataOut d = getOutputs >>= \o -> putOutputs (setDataOut o d)

dataIn :: Inputs -> W8
dataIn (Inputs d_i _ _) = d_i

getDataIn :: ReT Inputs Outputs (StT CPUState I) W8
{-# INLINE getDataIn #-}
getDataIn = getInputs >>= \i -> return (dataIn i)

setWeOut :: Outputs -> Bit -> Outputs
setWeOut (Outputs a_o d_o _ iack_o) we_o = Outputs a_o d_o we_o iack_o

putWeOut :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE putWeOut #-}
putWeOut we = getOutputs >>= \o -> putOutputs (setWeOut o we)

setIackOut :: Outputs -> Bit -> Outputs
setIackOut (Outputs a_o d_o we_o _) iack_o = Outputs a_o d_o we_o iack_o

putIackOut :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE putIackOut #-}
putIackOut iack = getOutputs >>= \o -> putOutputs (setIackOut o iack)

mem :: Bit -> Bit -> Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE mem #-}
mem rEn wEn r = do
      pc <- getPC
      putAddrOut pc
      tick
      a <- getDataIn
      putAddrOut a
      putWeOut wEn
      case wEn of
       S  -> (getReg r >>= \ x -> putDataOut x)
       C -> return ()
      incrPC
      tick
      case rEn of
       S  -> (getDataIn >>= \ x -> putReg r x)
       C -> return ()

ld :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE ld #-}
ld rD rS = do
      a <- getReg rS
      putWeOut C
      putAddrOut a
      tick
      v <- getDataIn
      putReg rD v

st :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE st #-}
st rD rS = do
      a <- getReg rS
      v <- getReg rD
      putWeOut S
      putDataOut v
      putAddrOut a
      tick

add :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE add #-}
add rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let p    =  plusCW8 vD vS C
      let cout =  fst p
      let vD'  =  snd p
      putCFlag cout
      putReg rD vD'
      tick

addc :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
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

sub :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE sub #-}
sub rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let p            =  minusCW8 vD vS C
      let cout         =  fst p
      let vD'          =  snd p
      putCFlag cout
      putReg rD vD'
      tick

subb :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
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

mov :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE mov #-}
mov rD rS = getReg rS >>= \ x -> putReg rD x >>= \ zzz -> tick

or' :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE or' #-}
or' rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let vD'      =  orW8 vD vS
      putReg rD vD'
      putCFlag C
      putZFlag (toBit $ eqW8 vD' zeroW8)
      tick

and' :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE and' #-}
and' rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let vD'      =  andW8 vD vS
      putReg rD vD'
      putCFlag C
      putZFlag (toBit $ eqW8 vD' zeroW8)
      tick

xor :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE xor #-}
xor rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let vD'      =  xorW8 vD vS
      putReg rD vD'
      putCFlag C
      putZFlag (toBit $ eqW8 vD' zeroW8)
      tick

cmp :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE cmp #-}
cmp rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let p    =  minusCW8 vD vS C
      let c    =  fst p
      let r    =  snd p
      putCFlag c
      putZFlag (toBit $ eqW8 r zeroW8)
      tick

brz :: Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE brz #-}
brz r = do
      z <- getZFlag
      case z of
        S -> (getReg r >>= \ x -> putPC x)
        C -> return ()
      tick

brnz :: Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE brnz #-}
brnz r = do
      z <- getZFlag
      case notb z of
        S -> (getReg r >>= \ x -> putPC x)
        C -> return ()
      tick

brc :: Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE brc #-}
brc r = do
      c <- getCFlag
      case c of
        S -> (getReg r >>= \ x -> putPC x)
        C -> return ()
      tick

brnc :: Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE brnc #-}
brnc r = do
      c <- getCFlag
      case notb c of
        S -> (getReg r >>= \ x -> putPC x)
        C -> return ()
      tick

jmp :: Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE jmp #-}
jmp r = getReg r >>= \ x -> putPC x >>= \zzz -> tick

ien :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE ien #-}
ien b = putIEFlag b >>= \zzz -> tick

iack :: ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE iack #-}
iack = putIackOut S >>= \zzz -> tick

iret :: ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE iret #-}
iret = do
      putIEFlag S
      pc <- getPCSave
      putPC pc
      z <- getZSave
      putZFlag z
      c <- getCSave
      putCFlag c
      tick

not' :: Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE not' #-}
not' r = do
      v <- getReg r
      putReg r (notW8 v)
      tick

clrr :: Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE clrr #-}
clrr r = putReg r zeroW8 >>= \zzz -> tick

incr :: Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE incr #-}
incr r = do
      v <- getReg r
      let p    =  plusCW8 v oneW8 C
      let cout =  fst p
      let v'   =  snd p
      putReg r v'
      putCFlag cout
      putZFlag (toBit $ eqW8 v' zeroW8)
      tick

decr :: Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE decr #-}
decr r = do
      v <- getReg r
      let p    =  minusCW8 v oneW8 C
      let cout =  fst p
      let v'   =  snd p
      putReg r v'
      putCFlag cout
      putZFlag (toBit $ eqW8 v' zeroW8)
      tick

rot :: Bit -> Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE rot #-}
rot C r = do
      v <- getReg r
      putReg r (rolW8 v)
      tick
rot S  r = do
      v <- getReg r
      putReg r (rorW8 v)
      tick

shft :: Bit -> Bit -> Register -> ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE shft #-}
shft l d r = do
      v <- getReg r
      let p    = case (d,l) of
            (C, C) -> shlCW8 v (msbW8 v)
            (C, S) -> shlCW8 v C
            (S, C) -> shrCW8 v (lsbW8 v)
            (S, S) -> shrCW8 v C
      let cout =  fst p
      let v'   =  snd p
      putReg r v'
      putCFlag cout
      putZFlag (toBit $ eqW8 v' zeroW8)
      tick

initOutputs :: Outputs
initOutputs = Outputs zeroW8 zeroW8 C C

reset :: ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE reset #-}
reset = do
      putCFlag C
      putZFlag C
      putOutputs initOutputs
      tick

interrupt :: ReT Inputs Outputs (StT CPUState I) ()
{-# INLINE interrupt #-}
interrupt = do
      putIEFlag C
      pc <- getPC
      z <- getZFlag
      c <- getCFlag
      putPCSave pc
      putZSave z
      putCSave c
      tick

rstIn :: Inputs -> Bit
rstIn (Inputs _ r_i _) = r_i

intIn :: Inputs -> Bit
intIn (Inputs _ _ i_i) = i_i

loop :: ReT Inputs Outputs (StT CPUState I) ()
loop = do
      inp <- getInputs
      case rstIn inp of
            S  -> reset
            C -> getIEFlag >>= \ie ->
                  case (ie,intIn inp) of
                        { (S,S) -> interrupt
                        ; _         -> case dataIn inp of
                                    { W8 C C C C rEn wEn b0 b1 -> mem rEn wEn (mkReg b0 b1)
                                    ; W8 C C C S b0  b1  c0 c1 -> ld (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 C C S C b0  b1  c0 c1 -> st (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 C C S S b0  b1  c0 c1 -> add (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 C S C C b0  b1  c0 c1 -> addc (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 C S C S b0  b1  c0 c1 -> sub (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 C S S C b0  b1  c0 c1 -> subb (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 C S S S b0  b1  c0 c1 -> mov (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 S C C C b0  b1  c0 c1 -> or' (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 S C C S b0  b1  c0 c1 -> and' (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 S C S C b0  b1  c0 c1 -> xor (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 S C S S b0  b1  c0 c1 -> cmp (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 S S C C C   C   b0 b1 -> brz (mkReg b0 b1)
                                    ; W8 S S C C C   S   b0 b1 -> brnz (mkReg b0 b1)
                                    ; W8 S S C C S   C   b0 b1 -> brc (mkReg b0 b1)
                                    ; W8 S S C C S   S   b0 b1 -> brnc (mkReg b0 b1)
                                    ; W8 S S C S C   C   b0 b1 -> jmp (mkReg b0 b1)
                                    ; W8 S S C S C   S   C  b0 -> ien b0
                                    ; W8 S S C S C   S   S  C  -> iack
                                    ; W8 S S C S C   S   S  S  -> iret
                                    ; W8 S S C S S   C   b0 b1 -> not' (mkReg b0 b1)
                                    ; W8 S S C S S   S   b0 b1 -> clrr (mkReg b0 b1)
                                    ; W8 S S S C C   C   b0 b1 -> incr (mkReg b0 b1)
                                    ; W8 S S S C C   S   b0 b1 -> decr (mkReg b0 b1)
                                    ; W8 S S S C S   d   b0 b1 -> rot d (mkReg b0 b1)
                                    ; W8 S S S S l   d   b0 b1 -> shft l d (mkReg b0 b1)
                                    ; _                        -> reset
                                    }
                        }
      loop

go :: ReT Inputs Outputs (StT CPUState I) ()
go = do
      reset
      loop

initInputs :: Inputs
initInputs = Inputs zeroW8 C C

initState :: CPUState
initState = CPUState initInputs initOutputs C C C zeroW8 C C zeroW8 zeroW8 zeroW8 zeroW8 zeroW8

start :: ReT Inputs Outputs I ()
start = extrude go initState

main = undefined
