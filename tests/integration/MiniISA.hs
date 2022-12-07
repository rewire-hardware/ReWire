import ReWire

data Bit = C | S
data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit

zeroW8 :: W8
zeroW8 = W8 S S S S S S S S

oneW8 :: W8
oneW8 = W8 C C C C C C C S

toBit :: Bool -> Bit
toBit False = C
toBit True  = S

eqb :: Bit -> Bit -> Bool
eqb C C = True
eqb S S = True
eqb _ _ = False

notb :: Bit -> Bit
notb C = S
notb S = C

orb :: Bit -> Bit -> Bit
orb a b | eqb a S || eqb b S = S
        | otherwise          = C

xorb :: Bit -> Bit -> Bit
xorb a b | eqb a S && eqb b C = S
         | eqb a C && eqb b S = S
         | otherwise          = C

andb :: Bit -> Bit -> Bit
andb a b | eqb a S && eqb b S = S
         | otherwise          = C

eqW8 :: W8 -> W8 -> Bool
eqW8 (W8 b1 b2 b3 b4 b5 b6 b7 b8) (W8 b1' b2' b3' b4' b5' b6' b7' b8')
      = eqb b1 b1'
      && eqb b2 b2'
      && eqb b3 b3'
      && eqb b4 b4'
      && eqb b5 b5'
      && eqb b6 b6'
      && eqb b7 b7'
      && eqb b8 b8'

rolW8 :: W8 -> W8
rolW8 (W8 b1 b2 b3 b4 b5 b6 b7 b8)
      = W8 b2
           b3
           b4
           b5
           b6
           b7
           b8
           b1

rorW8 :: W8 -> W8
rorW8 (W8 b1 b2 b3 b4 b5 b6 b7 b8)
      = W8 b8
           b1
           b2
           b3
           b4
           b5
           b6
           b7

notW8 :: W8 -> W8
notW8 (W8 b1 b2 b3 b4 b5 b6 b7 b8)
      = W8 (notb b1)
           (notb b2)
           (notb b3)
           (notb b4)
           (notb b5)
           (notb b6)
           (notb b7)
           (notb b8)

orW8 :: W8 -> W8 -> W8
orW8 (W8 b1 b2 b3 b4 b5 b6 b7 b8) (W8 b1' b2' b3' b4' b5' b6' b7' b8')
      = W8 (orb b1 b1')
           (orb b2 b2')
           (orb b3 b3')
           (orb b4 b4')
           (orb b5 b5')
           (orb b6 b6')
           (orb b7 b7')
           (orb b8 b8')

xorW8 :: W8 -> W8 -> W8
xorW8 (W8 b1 b2 b3 b4 b5 b6 b7 b8) (W8 b1' b2' b3' b4' b5' b6' b7' b8')
      = W8 (xorb b1 b1')
           (xorb b2 b2')
           (xorb b3 b3')
           (xorb b4 b4')
           (xorb b5 b5')
           (xorb b6 b6')
           (xorb b7 b7')
           (xorb b8 b8')

andW8 :: W8 -> W8 -> W8
andW8 (W8 b1 b2 b3 b4 b5 b6 b7 b8) (W8 b1' b2' b3' b4' b5' b6' b7' b8')
      = W8 (andb b1 b1')
           (andb b2 b2')
           (andb b3 b3')
           (andb b4 b4')
           (andb b5 b5')
           (andb b6 b6')
           (andb b7 b7')
           (andb b8 b8')

data Inputs = Inputs W8 Bit Bit           -- (dataIn,rstIn,intIn)
data Outputs = Outputs W8 W8 Bit Bit      -- (addrOut,dataOut,weOut,iackOut)
data CPUState = CPUState Inputs Outputs   -- (inputs,outputs) (0-9,10-27)
                          Bit Bit Bit W8  -- (zFlag,cFlag,ieFlag,pc) (28,29,30,31-38)
                          Bit Bit W8      -- (zsFlag,csFlag,pcSave) (39,40,41-48)
                          W8 W8 W8 W8     -- (r0,r1,r2,r3) (49-56,57-64,65-72,73-80)

data Register = R0 | R1 | R2 | R3

plusCW8  :: W8 -> W8 -> Bit -> (Bit,W8)
plusCW8  =  extern "prim_plusCW8" plusCW8

minusCW8 :: W8 -> W8 -> Bit -> (Bit,W8)
minusCW8 =  extern "prim_minusCW8" minusCW8

shlCW8   :: W8 -> Bit -> (Bit,W8)
shlCW8   =  extern "prim_shlCW8" shlCW8

shrCW8   :: W8 -> Bit -> (Bit,W8)
shrCW8   =  extern "prim_shrCW8" shrCW8

msbW8    :: W8 -> Bit
msbW8    =  extern "prim_msbW8" msbW8

lsbW8    :: W8 -> Bit
lsbW8    =  extern "prim_lsbW8" lsbW8

mkReg :: Bit -> Bit -> Register
mkReg  C  C = R0
mkReg  C  S  = R1
mkReg  S   C = R2
mkReg  S   S  = R3

{-# INLINE when #-}
when :: Monad m => Bit -> m () -> m ()
when = \ b -> \ m -> case b of
                       S  -> m
                       C -> return ()

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
incrPC = getPC >>= \pc -> putPC (snd (plusCW8 pc oneW8 C))

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
setAddrOut (Outputs _ d_o we_o iack_o) a_o = Outputs a_o d_o we_o iack_o

putAddrOut :: W8 -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putAddrOut #-}
putAddrOut a = getOutputs >>= \o -> putOutputs (setAddrOut o a)

setDataOut :: Outputs -> W8 -> Outputs
setDataOut (Outputs a_o _ we_o iack_o) d_o = Outputs a_o d_o we_o iack_o

putDataOut :: W8 -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putDataOut #-}
putDataOut d = getOutputs >>= \o -> putOutputs (setDataOut o d)

dataIn :: Inputs -> W8
dataIn (Inputs d_i _ _) = d_i

getDataIn :: ReacT Inputs Outputs (StateT CPUState Identity) W8
{-# INLINE getDataIn #-}
getDataIn = getInputs >>= \i -> return (dataIn i)

setWeOut :: Outputs -> Bit -> Outputs
setWeOut (Outputs a_o d_o _ iack_o) we_o = Outputs a_o d_o we_o iack_o

putWeOut :: Bit -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE putWeOut #-}
putWeOut we = getOutputs >>= \o -> putOutputs (setWeOut o we)

setIackOut :: Outputs -> Bit -> Outputs
setIackOut (Outputs a_o d_o we_o _) iack_o = Outputs a_o d_o we_o iack_o

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
      putWeOut C
      putAddrOut a
      tick
      v <- getDataIn
      putReg rD v

st :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE st #-}
st rD rS = do
      a <- getReg rS
      v <- getReg rD
      putWeOut S
      putDataOut v
      putAddrOut a
      tick

add :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
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
      let p            =  minusCW8 vD vS C
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
      let vD'      =  orW8 vD vS
      putReg rD vD'
      putCFlag C
      putZFlag (toBit $ eqW8 vD' zeroW8)
      tick

and' :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE and' #-}
and' rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let vD'      =  andW8 vD vS
      putReg rD vD'
      putCFlag C
      putZFlag (toBit $ eqW8 vD' zeroW8)
      tick

xor :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE xor #-}
xor rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let vD'      =  xorW8 vD vS
      putReg rD vD'
      putCFlag C
      putZFlag (toBit $ eqW8 vD' zeroW8)
      tick

cmp :: Register -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
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
iack = putIackOut S >>= \zzz -> tick

iret :: ReacT Inputs Outputs (StateT CPUState Identity) ()
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

not' :: Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE not' #-}
not' r = do
      v <- getReg r
      putReg r (notW8 v)
      tick

clrr :: Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE clrr #-}
clrr r = putReg r zeroW8 >>= \zzz -> tick

incr :: Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
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

decr :: Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
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

rot :: Bit -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE rot #-}
rot C r = do
      v <- getReg r
      putReg r (rolW8 v)
      tick
rot S  r = do
      v <- getReg r
      putReg r (rorW8 v)
      tick

shft :: Bit -> Bit -> Register -> ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE shft #-}
shft l d r = do
      v <- getReg r
      let p    = case (d,l) of
            (C,C) -> shlCW8 v (msbW8 v)
            (C, S) -> shlCW8 v C
            ( S,C) -> shrCW8 v (lsbW8 v)
            ( S, S) -> shrCW8 v C
      let cout =  fst p
      let v'   =  snd p
      putReg r v'
      putCFlag cout
      putZFlag (toBit $ eqW8 v' zeroW8)
      tick

initOutputs :: Outputs
initOutputs = Outputs zeroW8 zeroW8 C C

reset :: ReacT Inputs Outputs (StateT CPUState Identity) ()
{-# INLINE reset #-}
reset = do
      putCFlag C
      putZFlag C
      putOutputs initOutputs
      tick

interrupt :: ReacT Inputs Outputs (StateT CPUState Identity) ()
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

loop :: ReacT Inputs Outputs (StateT CPUState Identity) ()
loop = do
      inp <- getInputs
      case rstIn inp of
            S  -> reset
            C -> getIEFlag >>= \ie ->
                  case (ie,intIn inp) of
                        { (S,S) -> interrupt
                        ; _         -> case dataIn inp of
                                    { W8 C C C C  rEn  wEn   b0   b1 -> mem rEn wEn (mkReg b0 b1)
                                    ; W8 C C C  S   b0   b1   c0   c1 -> ld (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 C C  S C   b0   b1   c0   c1 -> st (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 C C  S  S   b0   b1   c0   c1 -> add (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 C  S C C   b0   b1   c0   c1 -> addc (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 C  S C  S   b0   b1   c0   c1 -> sub (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 C  S  S C   b0   b1   c0   c1 -> subb (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 C  S  S  S   b0   b1   c0   c1 -> mov (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8  S C C C   b0   b1   c0   c1 -> or' (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8  S C C  S   b0   b1   c0   c1 -> and' (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8  S C  S C   b0   b1   c0   c1 -> xor (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8  S C  S  S   b0   b1   c0   c1 -> cmp (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8  S  S C C C C   b0   b1 -> brz (mkReg b0 b1)
                                    ; W8  S  S C C C  S   b0   b1 -> brnz (mkReg b0 b1)
                                    ; W8  S  S C C  S C   b0   b1 -> brc (mkReg b0 b1)
                                    ; W8  S  S C C  S  S   b0   b1 -> brnc (mkReg b0 b1)
                                    ; W8  S  S C  S C C   b0   b1 -> jmp (mkReg b0 b1)
                                    ; W8  S  S C  S C  S C   b0 -> ien b0
                                    ; W8  S  S C  S C  S  S C -> iack
                                    ; W8  S  S C  S C  S  S  S -> iret
                                    ; W8  S  S C  S  S C   b0   b1 -> not' (mkReg b0 b1)
                                    ; W8  S  S C  S  S  S   b0   b1 -> clrr (mkReg b0 b1)
                                    ; W8  S  S  S C C C   b0   b1 -> incr (mkReg b0 b1)
                                    ; W8  S  S  S C C  S   b0   b1 -> decr (mkReg b0 b1)
                                    ; W8  S  S  S C  S    d   b0   b1 -> rot d (mkReg b0 b1)
                                    ; W8  S  S  S  S    l    d   b0   b1 -> shft l d (mkReg b0 b1)
                                    ; _                                          -> reset
                                    }
                        }
      loop

go :: ReacT Inputs Outputs (StateT CPUState Identity) ()
go = do
      reset
      loop

initInputs :: Inputs
initInputs = Inputs zeroW8 C C

initState :: CPUState
initState = CPUState initInputs initOutputs C C C zeroW8 C C zeroW8 zeroW8 zeroW8 zeroW8 zeroW8

start :: ReacT Inputs Outputs Identity ()
start = extrude go initState

main = undefined
