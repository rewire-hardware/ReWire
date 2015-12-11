-- kludge, should be in a prelude
data Tuple2 a b = Tuple2 a b
data Tuple3 a b c = Tuple3 a b c
data Tuple4 a b c d = Tuple4 a b c d
data Unit = Unit
-- end kludge
data Bit = Zero | One
data W8  = W8 Bit Bit Bit Bit Bit Bit Bit Bit
data Inputs = Inputs W8 Bit Bit           -- (dataIn,rstIn,intIn)
data Outputs = Outputs W8 W8 Bit Bit      -- (addrOut,dataOut,weOut,iackOut)
data CPUState = CPUState Inputs Outputs   -- (inputs,outputs) (0-9,10-27)
                          Bit Bit Bit W8  -- (zFlag,cFlag,ieFlag,pc) (28,29,30,31-38)
                          Bit Bit W8      -- (zsFlag,csFlag,pcSave) (39,40,41-48)
                          W8 W8 W8 W8     -- (r0,r1,r2,r3) (49-56,57-64,65-72,73-80)
data Register = R0 | R1 | R2 | R3

undefined :: a
undefined = undefined

notBit   :: Bit -> Bit
notBit   =  nativeVhdl "prim_notBit" undefined
eqBit    :: Bit -> Bit -> Bit
eqBit    =  nativeVhdl "prim_eqBit" undefined
andBit   :: Bit -> Bit -> Bit
andBit   =  nativeVhdl "prim_andBit" undefined
orBit    :: Bit -> Bit -> Bit
orBit    =  nativeVhdl "prim_orBit" undefined
xorBit   :: Bit -> Bit -> Bit
xorBit   =  nativeVhdl "prim_xorBit" undefined

zeroW8   :: W8
zeroW8   =  nativeVhdl "prim_zeroW8" undefined
oneW8    :: W8
oneW8    =  nativeVhdl "prim_oneW8" undefined
notW8    :: W8 -> W8
notW8    =  nativeVhdl "prim_notW8" undefined
andW8    :: W8 -> W8 -> W8
andW8    =  nativeVhdl "prim_andW8" undefined
orW8     :: W8 -> W8 -> W8
orW8     =  nativeVhdl "prim_orW8" undefined
xorW8    :: W8 -> W8 -> W8
xorW8    =  nativeVhdl "prim_xorW8" undefined
eqW8     :: W8 -> W8 -> Bit
eqW8     =  nativeVhdl "prim_eqW8" undefined
rolW8    :: W8 -> W8
rolW8    =  nativeVhdl "prim_rolW8" undefined
rorW8    :: W8 -> W8
rorW8    =  nativeVhdl "prim_rorW8" undefined
plusCW8  :: W8 -> W8 -> Bit -> (Bit,W8)
plusCW8  =  nativeVhdl "prim_plusCW8" undefined
plusW8   :: W8 -> W8 -> (Bit,W8)
plusW8   =  nativeVhdl "prim_plusW8" undefined
negW8    :: W8 -> W8 -> W8
negW8    =  nativeVhdl "prim_negW8" undefined
minusCW8 :: W8 -> W8 -> Bit -> (Bit,W8)
minusCW8 =  nativeVhdl "prim_minusCW8" undefined
shlCW8   :: W8 -> Bit -> (Bit,W8)
shlCW8   =  nativeVhdl "prim_shlCW8" undefined
shrCW8   :: W8 -> Bit -> (Bit,W8)
shrCW8   =  nativeVhdl "prim_shrCW8" undefined
msbW8    :: W8 -> Bit
msbW8    =  nativeVhdl "prim_msbW8" undefined
lsbW8    :: W8 -> Bit
lsbW8    =  nativeVhdl "prim_lsbW8" undefined

mkReg :: Bit -> Bit -> Register
mkReg  Zero  Zero = R0
mkReg  Zero  One  = R1
mkReg  One   Zero = R2
mkReg  One   One  = R3

when :: Monad m => Bit -> m () -> m ()
when One  m = m
when Zero _ = return ()

getState :: ReT Inputs Outputs (StT CPUState I) CPUState
getState = lift get

outputs :: CPUState -> Outputs
outputs (CPUState _ o _ _ _ _ _ _ _ _ _ _ _) = o

getOutputs :: ReT Inputs Outputs (StT CPUState I) Outputs
getOutputs = getState >>= \s -> return (outputs s)

setInputs :: CPUState -> Inputs -> CPUState
setInputs (CPUState _ o z c ie pc zs cs pcs r0 r1 r2 r3) i = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putState :: CPUState -> ReT Inputs Outputs (StT CPUState I) ()
putState s = lift (put s)

putInputs :: Inputs -> ReT Inputs Outputs (StT CPUState I) ()
putInputs i = getState >>= \s -> putState (setInputs s i)

tick :: ReT Inputs Outputs (StT CPUState I) ()
tick = getOutputs >>= \o -> signal o >>= \i -> putInputs i

pc :: CPUState -> W8
pc (CPUState _ _ _ _ _ pc _ _ _ _ _ _ _) = pc

getPC :: ReT Inputs Outputs (StT CPUState I) W8
getPC = getState >>= \s -> return (pc s)

setPC :: CPUState -> W8 -> CPUState
setPC (CPUState i o z c ie  _ zs cs pcs r0 r1 r2 r3) pc = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putPC :: W8 -> ReT Inputs Outputs (StT CPUState I) ()
putPC pc = getState >>= \s -> putState (setPC s pc)

fst :: (a,b) -> a
fst (x, _) = x

snd :: (a,b) -> b
snd (_, x) = x

incrPC :: ReT Inputs Outputs (StT CPUState I) ()
incrPC = getPC >>= \pc -> putPC (snd (plusW8 pc oneW8))

r0 :: CPUState -> W8
r0 (CPUState _ _ _ _ _ _ _ _ _ r0 _ _ _) = r0

r1 :: CPUState -> W8
r1 (CPUState _ _ _ _ _ _ _ _ _ _ r1 _ _) = r1

r2 :: CPUState -> W8
r2 (CPUState _ _ _ _ _ _ _ _ _ _ _ r2 _) = r2

r3 :: CPUState -> W8
r3 (CPUState _ _ _ _ _ _ _ _ _ _ _ _ r3) = r3

getReg :: Register -> ReT Inputs Outputs (StT CPUState I) W8
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
putReg R0 v = getState >>= \s -> putState (setR0 s v)
putReg R1 v = getState >>= \s -> putState (setR1 s v)
putReg R2 v = getState >>= \s -> putState (setR2 s v)
putReg R3 v = getState >>= \s -> putState (setR3 s v)

setPCSave :: CPUState -> W8 -> CPUState
setPCSave (CPUState i o z c ie pc zs cs _ r0 r1 r2 r3) pcs = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putPCSave :: W8 -> ReT Inputs Outputs (StT CPUState I) ()
putPCSave v = getState >>= \s -> putState (setPCSave s v)

pcSave :: CPUState -> W8
pcSave (CPUState _ _ _ _ _ _ _ _ pcs _ _ _ _) = pcs

getPCSave :: ReT Inputs Outputs (StT CPUState I) W8
getPCSave = getState >>= \s -> return (pcSave s)

zFlag :: CPUState -> Bit
zFlag (CPUState _ _ z _ _ _ _ _ _ _ _ _ _) = z

getZFlag :: ReT Inputs Outputs (StT CPUState I) Bit
getZFlag = getState >>= \s -> return (zFlag s)

zsFlag :: CPUState -> Bit
zsFlag (CPUState _ _ _ _ _ _ zs _ _ _ _ _ _) = zs

getZSave :: ReT Inputs Outputs (StT CPUState I) Bit
getZSave = getState >>= \s -> return (zsFlag s)

setZFlag :: CPUState -> Bit -> CPUState
setZFlag (CPUState i o _ c ie pc zs cs pcs r0 r1 r2 r3) z = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putZFlag :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
putZFlag b = getState >>= \s -> putState (setZFlag s b)

setZSave :: CPUState -> Bit -> CPUState
setZSave (CPUState i o z c ie pc  _ cs pcs r0 r1 r2 r3) zs = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putZSave :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
putZSave b = getState >>= \s -> putState (setZSave s b)

cFlag :: CPUState -> Bit
cFlag (CPUState _ _ _ c _ _ _ _ _ _ _ _ _) = c

getCFlag :: ReT Inputs Outputs (StT CPUState I) Bit
getCFlag = getState >>= \s -> return (cFlag s)

csFlag :: CPUState -> Bit
csFlag (CPUState _ _ _ _ _ _ _ cs _ _ _ _ _) = cs

getCSave :: ReT Inputs Outputs (StT CPUState I) Bit
getCSave = getState >>= \s -> return (csFlag s)

setCFlag :: CPUState -> Bit -> CPUState
setCFlag (CPUState i o z _ ie pc zs cs pcs r0 r1 r2 r3) c = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putCFlag :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
putCFlag b = getState >>= \s -> putState (setCFlag s b)

setCSave :: CPUState -> Bit -> CPUState
setCSave (CPUState i o z c ie pc zs  _ pcs r0 r1 r2 r3) cs = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putCSave :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
putCSave b = getState >>= \s -> putState (setCSave s b)

setIEFlag :: CPUState -> Bit -> CPUState
setIEFlag (CPUState i o z c  _ pc zs cs pcs r0 r1 r2 r3) ie = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putIEFlag :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
putIEFlag b = getState >>= \s -> putState (setIEFlag s b)

ieFlag :: CPUState -> Bit
ieFlag (CPUState _ _ _ _ ie _ _ _ _ _ _ _ _) = ie

getIEFlag :: ReT Inputs Outputs (StT CPUState I) Bit
getIEFlag = getState >>= \s -> return (ieFlag s)

inputs :: CPUState -> Inputs
inputs (CPUState i _ _ _ _ _ _ _ _ _ _ _ _) = i

getInputs :: ReT Inputs Outputs (StT CPUState I) Inputs
getInputs = getState >>= \s -> return (inputs s)

setOutputs :: CPUState -> Outputs -> CPUState
setOutputs (CPUState i _ z c ie pc zs cs pcs r0 r1 r2 r3) o = CPUState i o z c ie pc zs cs pcs r0 r1 r2 r3

putOutputs :: Outputs -> ReT Inputs Outputs (StT CPUState I) ()
putOutputs o = getState >>= \s -> putState (setOutputs s o)

setAddrOut :: Outputs -> W8 -> Outputs
setAddrOut (Outputs _ d_o we_o iack_o) a_o = Outputs a_o d_o we_o iack_o

putAddrOut :: W8 -> ReT Inputs Outputs (StT CPUState I) ()
putAddrOut a = getOutputs >>= \o -> putOutputs (setAddrOut o a)

setDataOut :: Outputs -> W8 -> Outputs
setDataOut (Outputs a_o _ we_o iack_o) d_o = Outputs a_o d_o we_o iack_o

putDataOut :: W8 -> ReT Inputs Outputs (StT CPUState I) ()
putDataOut d = getOutputs >>= \o -> putOutputs (setDataOut o d)

dataIn :: Inputs -> W8
dataIn (Inputs d_i _ _) = d_i

getDataIn :: ReT Inputs Outputs (StT CPUState I) W8
getDataIn = getInputs >>= \i -> return (dataIn i)

setWeOut :: Outputs -> Bit -> Outputs
setWeOut (Outputs a_o d_o _ iack_o) we_o = Outputs a_o d_o we_o iack_o

putWeOut :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
putWeOut we = getOutputs >>= \o -> putOutputs (setWeOut o we)

setIackOut :: Outputs -> Bit -> Outputs
setIackOut (Outputs a_o d_o we_o _) iack_o = Outputs a_o d_o we_o iack_o

putIackOut :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
putIackOut iack = getOutputs >>= \o -> putOutputs (setIackOut o iack)

mem :: Bit -> Bit -> Register -> ReT Inputs Outputs (StT CPUState I) ()
mem rEn wEn r = do
      pc <- getPC
      putAddrOut pc
      tick
      a <- getDataIn
      putAddrOut a
      putWeOut wEn
      when wEn (getReg r >>= putDataOut)
      incrPC
      tick
      when rEn (getDataIn >>= putReg r)

ld :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
ld rD rS = do
      a <- getReg rS
      putWeOut Zero
      putAddrOut a
      tick
      v <- getDataIn
      putReg rD v

st :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
st rD rS = do
      a <- getReg rS
      v <- getReg rD
      putWeOut One
      putDataOut v
      putAddrOut a
      tick

add :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
add rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let p    =  plusCW8 vD vS Zero
      let cout =  fst p
      let vD'  =  snd p
      putCFlag cout
      putReg rD vD'
      tick

addc :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
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
sub rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let p            =  minusCW8 vD vS Zero
      let cout         =  fst p
      let vD'          =  snd p
      putCFlag cout
      putReg rD vD'
      tick

subb :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
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
mov rD rS = getReg rS >>= putReg rD >>= \zzz -> tick

or :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
or rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let vD'      =  orW8 vD vS
      putReg rD vD'
      putCFlag Zero
      putZFlag (eqW8 vD' zeroW8)
      tick

and :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
and rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let vD'      =  andW8 vD vS
      putReg rD vD'
      putCFlag Zero
      putZFlag (eqW8 vD' zeroW8)
      tick

xor :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
xor rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let vD'      =  xorW8 vD vS
      putReg rD vD'
      putCFlag Zero
      putZFlag (eqW8 vD' zeroW8)
      tick

cmp :: Register -> Register -> ReT Inputs Outputs (StT CPUState I) ()
cmp rD rS = do
      vD <- getReg rD
      vS <- getReg rS
      let p    =  minusCW8 vD vS Zero
      let c    =  fst p
      let r    =  snd p
      putCFlag c
      putZFlag (eqW8 r zeroW8)
      tick

brz :: Register -> ReT Inputs Outputs (StT CPUState I) ()
brz r = do
      z <- getZFlag
      when z (getReg r >>= putPC)
      tick

brnz :: Register -> ReT Inputs Outputs (StT CPUState I) ()
brnz r = do
      z <- getZFlag
      when (notBit z) (getReg r >>= putPC)
      tick

brc :: Register -> ReT Inputs Outputs (StT CPUState I) ()
brc r = do
      c <- getCFlag
      when c (getReg r >>= putPC)
      tick

brnc :: Register -> ReT Inputs Outputs (StT CPUState I) ()
brnc r = do
      c <- getCFlag
      when (notBit c) (getReg r >>= putPC)
      tick

jmp :: Register -> ReT Inputs Outputs (StT CPUState I) ()
jmp r = getReg r >>= putPC >>= \zzz -> tick

ien :: Bit -> ReT Inputs Outputs (StT CPUState I) ()
ien b = putIEFlag b >>= \zzz -> tick

iack :: ReT Inputs Outputs (StT CPUState I) ()
iack = putIackOut One >>= \zzz -> tick

iret :: ReT Inputs Outputs (StT CPUState I) ()
iret = do
      putIEFlag One
      pc <- getPCSave
      putPC pc
      z <- getZSave
      putZFlag z
      c <- getCSave
      putCFlag c
      tick

not :: Register -> ReT Inputs Outputs (StT CPUState I) ()
not r = do
      v <- getReg r
      putReg r (notW8 v)
      tick

clrr :: Register -> ReT Inputs Outputs (StT CPUState I) ()
clrr r = putReg r zeroW8 >>= \zzz -> tick

incr :: Register -> ReT Inputs Outputs (StT CPUState I) ()
incr r = do
      v <- getReg r
      let p    =  plusCW8 v oneW8 Zero
      let cout =  fst p
      let v'   =  snd p
      putReg r v'
      putCFlag cout
      putZFlag (eqW8 v' zeroW8)
      tick

decr :: Register -> ReT Inputs Outputs (StT CPUState I) ()
decr r = do
      v <- getReg r
      let p    =  minusCW8 v oneW8 Zero
      let cout =  fst p
      let v'   =  snd p
      putReg r v'
      putCFlag cout
      putZFlag (eqW8 v' zeroW8)
      tick

rot :: Bit -> Register -> ReT Inputs Outputs (StT CPUState I) ()
rot Zero r = do
      v <- getReg r
      putReg r (rolW8 v)
      tick
rot One  r = do
      v <- getReg r
      putReg r (rorW8 v)
      tick

shft :: Bit -> Bit -> Register -> ReT Inputs Outputs (StT CPUState I) ()
shft l d r = do
      v <- getReg r
      let p    = case (d,l) of
            (Zero,Zero) -> shlCW8 v (msbW8 v)
            (Zero, One) -> shlCW8 v Zero
            ( One,Zero) -> shrCW8 v (lsbW8 v)
            ( One, One) -> shrCW8 v Zero
      let cout =  fst p
      let v'   =  snd p
      putReg r v'
      putCFlag cout
      putZFlag (eqW8 v' zeroW8)
      tick

initOutputs :: Outputs
initOutputs = Outputs zeroW8 zeroW8 Zero Zero

reset :: ReT Inputs Outputs (StT CPUState I) ()
reset = do
      putCFlag Zero
      putZFlag Zero
      putOutputs initOutputs
      tick

interrupt :: ReT Inputs Outputs (StT CPUState I) ()
interrupt = do
      putIEFlag Zero
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
            One  -> reset
            Zero -> getIEFlag >>= \ie ->
                  case (ie,intIn inp) of
                        { (One,One) -> interrupt
                        ; _         -> case dataIn inp of
                                    { W8 Zero Zero Zero Zero  rEn  wEn   b0   b1 -> mem rEn wEn (mkReg b0 b1)
                                    ; W8 Zero Zero Zero  One   b0   b1   c0   c1 -> ld (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 Zero Zero  One Zero   b0   b1   c0   c1 -> st (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 Zero Zero  One  One   b0   b1   c0   c1 -> add (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 Zero  One Zero Zero   b0   b1   c0   c1 -> addc (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 Zero  One Zero  One   b0   b1   c0   c1 -> sub (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 Zero  One  One Zero   b0   b1   c0   c1 -> subb (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8 Zero  One  One  One   b0   b1   c0   c1 -> mov (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8  One Zero Zero Zero   b0   b1   c0   c1 -> or (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8  One Zero Zero  One   b0   b1   c0   c1 -> and (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8  One Zero  One Zero   b0   b1   c0   c1 -> xor (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8  One Zero  One  One   b0   b1   c0   c1 -> cmp (mkReg b0 b1) (mkReg c0 c1)
                                    ; W8  One  One Zero Zero Zero Zero   b0   b1 -> brz (mkReg b0 b1)
                                    ; W8  One  One Zero Zero Zero  One   b0   b1 -> brnz (mkReg b0 b1)
                                    ; W8  One  One Zero Zero  One Zero   b0   b1 -> brc (mkReg b0 b1)
                                    ; W8  One  One Zero Zero  One  One   b0   b1 -> brnc (mkReg b0 b1)
                                    ; W8  One  One Zero  One Zero Zero   b0   b1 -> jmp (mkReg b0 b1)
                                    ; W8  One  One Zero  One Zero  One Zero   b0 -> ien b0
                                    ; W8  One  One Zero  One Zero  One  One Zero -> iack
                                    ; W8  One  One Zero  One Zero  One  One  One -> iret
                                    ; W8  One  One Zero  One  One Zero   b0   b1 -> not (mkReg b0 b1)
                                    ; W8  One  One Zero  One  One  One   b0   b1 -> clrr (mkReg b0 b1)
                                    ; W8  One  One  One Zero Zero Zero   b0   b1 -> incr (mkReg b0 b1)
                                    ; W8  One  One  One Zero Zero  One   b0   b1 -> decr (mkReg b0 b1)
                                    ; W8  One  One  One Zero  One    d   b0   b1 -> rot d (mkReg b0 b1)
                                    ; W8  One  One  One  One    l    d   b0   b1 -> shft l d (mkReg b0 b1)
                                    ; _                                          -> reset
                                    }
                        }
      loop

begin :: ReT Inputs Outputs (StT CPUState I) ()
begin = do
      reset
      loop

initInputs :: Inputs
initInputs = Inputs zeroW8 Zero Zero

initState :: CPUState
initState = CPUState initInputs initOutputs Zero Zero Zero zeroW8 Zero Zero zeroW8 zeroW8 zeroW8 zeroW8 zeroW8

start :: ReT Inputs Outputs I ((),CPUState)
start = extrude begin initState
