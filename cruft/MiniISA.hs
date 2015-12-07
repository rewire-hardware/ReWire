{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module MiniISA where

import Prelude hiding (not,or,and)
import Control.Monad.Resumption.Reactive
import Control.Monad.State hiding (when)
import Control.Monad.Identity hiding (when)

-- begin primitive boilerplate
data Bit = Zero | One deriving Show
notBit One  = Zero
notBit Zero = One
eqBit One  One  = One
eqBit Zero Zero = One
eqBit _    _    = Zero
andBit One b  = b
andBit Zero _ = Zero
orBit Zero b = b
orBit One _  = One
xorBit One b  = notBit b
xorBit Zero b = b
plusCBit Zero Zero Zero = (Zero,Zero)
plusCBit Zero Zero  One = (Zero, One)
plusCBit Zero  One Zero = (Zero, One)
plusCBit Zero  One  One = ( One,Zero)
plusCBit  One Zero Zero = (Zero, One)
plusCBit  One Zero  One = ( One,Zero)
plusCBit  One  One Zero = ( One,Zero)
plusCBit  One  One  One = ( One, One)
minusCBit  Zero Zero Zero = (Zero,Zero)
minusCBit  Zero Zero  One = ( One, One)
minusCBit  Zero  One Zero = ( One, One)
minusCBit  Zero  One  One = ( One,Zero)
minusCBit   One Zero Zero = (Zero, One)
minusCBit   One Zero  One = (Zero,Zero)
minusCBit   One  One Zero = (Zero,Zero)
minusCBit   One  One  One = ( One,Zero)

data W8  = W8 Bit Bit Bit Bit Bit Bit Bit Bit deriving Show
zeroW8 = W8 Zero Zero Zero Zero Zero Zero Zero Zero
oneW8  = W8 Zero Zero Zero Zero Zero Zero Zero  One
notW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) = W8 (notBit b0) (notBit b1) (notBit b2) (notBit b3) (notBit b4) (notBit b5) (notBit b6) (notBit b7)
andW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = W8 (andBit b0 c0) (andBit b1 c1) (andBit b2 c2) (andBit b3 c3) (andBit b4 c4) (andBit b5 c5) (andBit b6 c6) (andBit b7 c7)
orW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = W8 (orBit b0 c0) (orBit b1 c1) (orBit b2 c2) (orBit b3 c3) (orBit b4 c4) (orBit b5 c5) (orBit b6 c6) (orBit b7 c7)
xorW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = W8 (xorBit b0 c0) (xorBit b1 c1) (xorBit b2 c2) (xorBit b3 c3) (xorBit b4 c4) (xorBit b5 c5) (xorBit b6 c6) (xorBit b7 c7)
eqW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = andBit (andBit (andBit (eqBit b0 c0) (eqBit b1 c1)) (andBit (eqBit b2 c2) (eqBit b3 c3))) (andBit (andBit (eqBit b4 c4) (eqBit b5 c5)) (andBit (eqBit b6 c6) (eqBit b7 c7)))
rolW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) = W8 b1 b2 b3 b4 b5 b6 b7 b0
rorW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) = W8 b7 b0 b1 b2 b3 b4 b5 b6
plusCW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) ci = (co0,W8 d0 d1 d2 d3 d4 d5 d6 d7)
 where (co0,d0) = plusCBit b0 c0 co1
       (co1,d1) = plusCBit b1 c1 co2
       (co2,d2) = plusCBit b2 c2 co3
       (co3,d3) = plusCBit b3 c3 co4
       (co4,d4) = plusCBit b4 c4 co5
       (co5,d5) = plusCBit b5 c5 co6
       (co6,d6) = plusCBit b6 c6 co7
       (co7,d7) = plusCBit b7 c7 ci
plusW8 a b = plusCW8 a b Zero
negW8 w = snd $ plusW8 (notW8 w) oneW8
minusCW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) ci = (co0,W8 d0 d1 d2 d3 d4 d5 d6 d7)
 where (co0,d0) = minusCBit b0 c0 co1
       (co1,d1) = minusCBit b1 c1 co2
       (co2,d2) = minusCBit b2 c2 co3
       (co3,d3) = minusCBit b3 c3 co4
       (co4,d4) = minusCBit b4 c4 co5
       (co5,d5) = minusCBit b5 c5 co6
       (co6,d6) = minusCBit b6 c6 co7
       (co7,d7) = minusCBit b7 c7 ci
shlCW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) ci = (b0,W8 b1 b2 b3 b4 b5 b6 b7 ci)
shrCW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) ci = (b7,W8 ci b0 b1 b2 b3 b4 b5 b6)
msbW8 (W8 b _ _ _ _ _ _ _) = b
lsbW8 (W8 _ _ _ _ _ _ _ b) = b

extrudeStateT :: Monad m => ReacT i o (StateT s m) a -> s -> ReacT i o m (a,s)
extrudeStateT (ReacT phi) s = ReacT $ do (res,s') <- runStateT phi s
                                         case res of
                                           Left x      -> return (Left (x,s'))
                                           Right (o,k) -> return (Right (o,\ i -> extrudeStateT (k i) s'))
-- end primitive boilerplate

data Inputs = Inputs { dataIn :: W8,
                       rstIn  :: Bit,
                       intIn  :: Bit }

data Outputs = Outputs { addrOut :: W8,
                         dataOut :: W8,
                         weOut   :: Bit,
                         iackOut :: Bit }

data CPUState = CPUState { inputs  :: Inputs,
                           outputs :: Outputs,

                           zFlag   :: Bit,
                           cFlag   :: Bit,
                           ieFlag  :: Bit,
                           pc      :: W8,

                           zsFlag  :: Bit,
                           csFlag  :: Bit,
                           pcSave  :: W8,

                           r0      :: W8,
                           r1      :: W8,
                           r2      :: W8,
                           r3      :: W8 }

data Register = R0 | R1 | R2 | R3

mkReg Zero Zero = R0
mkReg Zero  One = R1
mkReg  One Zero = R2
mkReg  One  One = R3

when :: Monad m => Bit -> m () -> m ()
when One m  = m
when Zero m = return ()

type CPUM = ReacT Inputs Outputs (StateT CPUState Identity)

tick :: CPUM ()
tick = do o <- getOutputs
          i <- signal o
          putInputs i

incrPC :: CPUM ()
incrPC = do pc <- getPC
            putPC (snd $ plusW8 pc oneW8)

getState :: CPUM CPUState
getState = lift get

putState :: CPUState -> CPUM ()
putState = lift . put

getReg :: Register -> CPUM W8
getReg r = do s <- getState
              case r of
                R0 -> return (r0 s)
                R1 -> return (r1 s)
                R2 -> return (r2 s)
                R3 -> return (r3 s)

putReg :: Register -> W8 -> CPUM ()
putReg r v = do s <- getState
                case r of
                  R0 -> putState (s { r0 = v })
                  R1 -> putState (s { r1 = v })
                  R2 -> putState (s { r2 = v })
                  R3 -> putState (s { r3 = v })

getPC :: CPUM W8
getPC = do s <- getState
           return (pc s)

putPC :: W8 -> CPUM ()
putPC v = do s <- getState
             putState (s { pc = v })

putPCSave :: W8 -> CPUM ()
putPCSave v = do s <- getState
                 putState (s { pcSave = v })

getPCSave :: CPUM W8
getPCSave = do s <- getState
               return (pcSave s)

getZFlag :: CPUM Bit
getZFlag = do s <- getState
              return (zFlag s)

getZSave :: CPUM Bit
getZSave = do s <- getState
              return (zsFlag s)

putZFlag :: Bit -> CPUM ()
putZFlag b = do s <- getState
                putState (s { zFlag = b })

putZSave :: Bit -> CPUM ()
putZSave b = do s <- getState
                putState (s { zsFlag = b })

getCFlag :: CPUM Bit
getCFlag = do s <- getState
              return (cFlag s)

getCSave :: CPUM Bit
getCSave = do s <- getState
              return (csFlag s)

putCFlag :: Bit -> CPUM ()
putCFlag b = do s <- getState
                putState (s { cFlag = b })

putCSave :: Bit -> CPUM ()
putCSave b = do s <- getState
                putState (s { csFlag = b })

putIEFlag :: Bit -> CPUM ()
putIEFlag b = do s <- getState
                 putState (s { ieFlag = b })

getIEFlag :: CPUM Bit
getIEFlag = do s <- getState
               return (ieFlag s)

getInputs :: CPUM Inputs
getInputs = do s <- getState
               return (inputs s)

putInputs :: Inputs -> CPUM ()
putInputs i = do s <- getState
                 putState (s { inputs = i })

getOutputs :: CPUM Outputs
getOutputs = do s <- getState
                return (outputs s)

putOutputs :: Outputs -> CPUM ()
putOutputs o = do s <- getState
                  putState (s { outputs = o })

putAddrOut :: W8 -> CPUM ()
putAddrOut a = do o <- getOutputs
                  putOutputs (o { addrOut = a })

putDataOut :: W8 -> CPUM ()
putDataOut v = do o <- getOutputs
                  putOutputs (o { dataOut = v })

getDataIn :: CPUM W8
getDataIn = do i <- getInputs
               return (dataIn i)

putWeOut :: Bit -> CPUM ()
putWeOut b = do o <- getOutputs
                putOutputs (o { weOut = b })

putIackOut :: Bit -> CPUM ()
putIackOut b = do o <- getOutputs
                  putOutputs (o { iackOut = b })

loop :: CPUM ()
loop = do inp <- getInputs
          case rstIn inp of
            One  -> reset
            Zero -> do ie   <- getIEFlag
                       case (ie,intIn inp) of
                         (One,One) -> interrupt
                         _         -> case dataIn inp of
                           W8 Zero Zero Zero Zero  rEn  wEn   b0   b1 -> mem rEn wEn (mkReg b0 b1)
                           W8 Zero Zero Zero  One   b0   b1   c0   c1 -> ld (mkReg b0 b1) (mkReg c0 c1)
                           W8 Zero Zero  One Zero   b0   b1   c0   c1 -> st (mkReg b0 b1) (mkReg c0 c1)
                           W8 Zero Zero  One  One   b0   b1   c0   c1 -> add (mkReg b0 b1) (mkReg c0 c1)
                           W8 Zero  One Zero Zero   b0   b1   c0   c1 -> addc (mkReg b0 b1) (mkReg c0 c1)
                           W8 Zero  One Zero  One   b0   b1   c0   c1 -> sub (mkReg b0 b1) (mkReg c0 c1)
                           W8 Zero  One  One Zero   b0   b1   c0   c1 -> subb (mkReg b0 b1) (mkReg c0 c1)
                           W8 Zero  One  One  One   b0   b1   c0   c1 -> mov (mkReg b0 b1) (mkReg c0 c1)
                           W8  One Zero Zero Zero   b0   b1   c0   c1 -> or (mkReg b0 b1) (mkReg c0 c1)
                           W8  One Zero Zero  One   b0   b1   c0   c1 -> and (mkReg b0 b1) (mkReg c0 c1)
                           W8  One Zero  One Zero   b0   b1   c0   c1 -> xor (mkReg b0 b1) (mkReg c0 c1)
                           W8  One Zero  One  One   b0   b1   c0   c1 -> cmp (mkReg b0 b1) (mkReg c0 c1)
                           W8  One  One Zero Zero Zero Zero   b0   b1 -> brz (mkReg b0 b1)
                           W8  One  One Zero Zero Zero  One   b0   b1 -> brnz (mkReg b0 b1)
                           W8  One  One Zero Zero  One Zero   b0   b1 -> brc (mkReg b0 b1)
                           W8  One  One Zero Zero  One  One   b0   b1 -> brnc (mkReg b0 b1)
                           W8  One  One Zero  One Zero Zero   b0   b1 -> jmp (mkReg b0 b1)
                           W8  One  One Zero  One Zero  One Zero   b0 -> ien b0
                           W8  One  One Zero  One Zero  One  One Zero -> iack
                           W8  One  One Zero  One Zero  One  One  One -> iret
                           W8  One  One Zero  One  One Zero   b0   b1 -> not (mkReg b0 b1)
                           W8  One  One Zero  One  One  One   b0   b1 -> clrr (mkReg b0 b1)
                           W8  One  One  One Zero Zero Zero   b0   b1 -> incr (mkReg b0 b1)
                           W8  One  One  One Zero Zero  One   b0   b1 -> decr (mkReg b0 b1)
                           W8  One  One  One Zero  One    d   b0   b1 -> rot d (mkReg b0 b1)
                           W8  One  One  One  One    l    d   b0   b1 -> shft l d (mkReg b0 b1)
          loop

mem :: Bit -> Bit -> Register -> CPUM ()
mem rEn wEn r = do pc <- getPC
                   putAddrOut pc
                   tick

                   a <- getDataIn

                   putAddrOut a
                   putWeOut wEn
                   when wEn $ do
                     d <- getReg r
                     putDataOut d
                   incrPC
                   tick
                   when rEn $ do
                     v <- getDataIn
                     putReg r v

ld :: Register -> Register -> CPUM ()
ld rD rS = do a <- getReg rS
              putWeOut Zero
              putAddrOut a
              tick

              v <- getDataIn
              putReg rD v

st :: Register -> Register -> CPUM ()
st rD rS = do a <- getReg rS
              v <- getReg rD
              putWeOut One
              putDataOut v
              putAddrOut a
              tick

add :: Register -> Register -> CPUM ()
add rD rS = do vD             <- getReg rD
               vS             <- getReg rS
               let (cout,vD') =  plusCW8 vD vS Zero
               putCFlag cout
               putReg rD vD'
               tick

addc :: Register -> Register -> CPUM ()
addc rD rS = do vD             <- getReg rD
                vS             <- getReg rS
                cin            <- getCFlag
                let (cout,vD') =  plusCW8 vD vS cin
                putCFlag cout
                putReg rD vD'
                tick

sub :: Register -> Register -> CPUM ()
sub rD rS = do vD          <- getReg rD
               vS          <- getReg rS
               let (cout,vD') =  minusCW8 vD vS Zero
               putCFlag cout
               putReg rD vD'
               tick

subb :: Register -> Register -> CPUM ()
subb rD rS = do vD             <- getReg rD
                vS             <- getReg rS
                cin            <- getCFlag
                let (cout,vD') =  minusCW8 vD vS cin
                putCFlag cout
                putReg rD vD'
                tick

mov :: Register -> Register -> CPUM ()
mov rD rS = do v <- getReg rS
               putReg rD v
               tick

or :: Register -> Register -> CPUM ()
or rD rS = do vD      <- getReg rD
              vS      <- getReg rS
              let vD' =  orW8 vD vS
              putReg rD vD'
              putCFlag Zero
              putZFlag (eqW8 vD' zeroW8)
              tick

and :: Register -> Register -> CPUM ()
and rD rS = do vD <- getReg rD
               vS <- getReg rS
               let vD' = andW8 vD vS
               putReg rD vD'
               putCFlag Zero
               putZFlag (eqW8 vD' zeroW8)
               tick

xor :: Register -> Register -> CPUM ()
xor rD rS = do vD <- getReg rD
               vS <- getReg rS
               let vD' = xorW8 vD vS
               putReg rD vD'
               putCFlag Zero
               putZFlag (eqW8 vD' zeroW8)
               tick

cmp :: Register -> Register -> CPUM ()
cmp rD rS = do vD <- getReg rD
               vS <- getReg rS
               let (c,r) =  minusCW8 vD vS Zero
               putCFlag c
               putZFlag (eqW8 r zeroW8)
               tick

brz :: Register -> CPUM ()
brz r = do z <- getZFlag
           when z $ do
             a <- getReg r
             putPC a
           tick

brnz :: Register -> CPUM ()
brnz r = do z <- getZFlag
            when (notBit z) $ do
              a <- getReg r
              putPC a
            tick

brc :: Register -> CPUM ()
brc r = do c <- getCFlag
           when c $ do
             a <- getReg r
             putPC a
           tick

brnc :: Register -> CPUM ()
brnc r = do c <- getCFlag
            when (notBit c) $ do
              a <- getReg r
              putPC a
            tick

jmp :: Register -> CPUM ()
jmp r = do a <- getReg r
           putPC a
           tick

ien :: Bit -> CPUM ()
ien b = do putIEFlag b
           tick

iack :: CPUM ()
iack = do putIackOut One
          tick

iret :: CPUM ()
iret = do putIEFlag One
          pc <- getPCSave
          putPC pc
          z  <- getZSave
          putZFlag z
          c  <- getCSave
          putCFlag c
          tick

not :: Register -> CPUM ()
not r = do v <- getReg r
           putReg r (notW8 v)
           tick

clrr :: Register -> CPUM ()
clrr r = do putReg r (zeroW8)
            tick

incr :: Register -> CPUM ()
incr r = do v             <- getReg r
            let (cout,v') =  plusCW8 v oneW8 Zero
            putReg r v'
            putCFlag cout
            putZFlag (eqW8 v' zeroW8)
            tick

decr :: Register -> CPUM ()
decr r = do v             <- getReg r
            let (cout,v') =  minusCW8 v oneW8 Zero
            putReg r v'
            putCFlag cout
            putZFlag (eqW8 v' zeroW8)
            tick

rot :: Bit -> Register -> CPUM ()
rot Zero r = do v <- getReg r
                putReg r (rolW8 v)
                tick
rot One r  = do v <- getReg r
                putReg r (rorW8 v)
                tick

shft :: Bit -> Bit -> Register -> CPUM ()
shft l d r = do v             <- getReg r
                let shf       =  case d of
                                   Zero -> shlCW8
                                   One  -> shrCW8
                    cin       =  case l of
                                   Zero -> case d of
                                             Zero -> msbW8 v
                                             One  -> lsbW8 v
                                   One  -> Zero
                    (cout,v') =  shf v cin
                putReg r v'
                putCFlag cout
                putZFlag (eqW8 v' zeroW8)
                tick

reset :: CPUM ()
reset = do putCFlag Zero
           putZFlag Zero
           putOutputs initOutputs

interrupt :: CPUM ()
interrupt = do putIEFlag Zero
               pc <- getPC
               z  <- getZFlag
               c  <- getCFlag
               putPCSave pc
               putZSave z
               putCSave c
               return ()

initInputs  = Inputs  { dataIn  = zeroW8, rstIn = Zero, intIn = Zero }
initOutputs = Outputs { addrOut = zeroW8, dataOut = zeroW8, weOut = Zero, iackOut = Zero }

begin :: CPUM ()
begin = do tick
           loop

start :: ReacT Inputs Outputs Identity ((),CPUState)
start = extrudeStateT begin initState
  where initState = CPUState { inputs  = initInputs,
                               outputs = initOutputs,
                               zFlag   = Zero,
                               cFlag   = Zero,
                               ieFlag  = Zero,
                               pc      = zeroW8,
                               zsFlag  = Zero,
                               csFlag  = Zero,
                               pcSave  = zeroW8,
                               r0      = zeroW8,
                               r1      = zeroW8,
                               r2      = zeroW8,
                               r3      = zeroW8 }
