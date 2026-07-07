{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module RWC.Primitives
      ( Identity, ReacT, A_, R_, StateT, Vec, Finite, PuRe, Proxy (..)
      , rwPrimAdd
      , rwPrimAnd
      , rwPrimBind
      , rwPrimBitIndex
      , rwPrimBitSlice
      , rwPrimBits
      , rwPrimCryptol
      , rwPrimDiv
      , rwPrimEq
      , rwPrimError
      , rwPrimExtern
      , rwPrimExtrude
      , rwPrimGet
      , rwPrimGt
      , rwPrimGtEq
      , rwPrimLAnd
      , rwPrimLNot
      , rwPrimLOr
      , rwPrimLShift
      , rwPrimLift
      , rwPrimLt
      , rwPrimLtEq
      , rwPrimMSBit
      , rwPrimMod
      , rwPrimMul
      , rwPrimNatVal
      , rwPrimNot
      , rwPrimOr
      , rwPrimPow
      , rwPrimPut
      , rwPrimRAnd
      , rwPrimRNAnd
      , rwPrimRNor
      , rwPrimROr
      , rwPrimRShift
      , rwPrimRShiftArith
      , rwPrimRXNor
      , rwPrimRXOr
      , rwPrimResize
      , rwPrimReturn
      , rwPrimSignal
      , rwPrimSignextend
      , rwPrimSub
      , rwPrimUnfold
      , rwPrimFinite
      , rwPrimFiniteMinBound
      , rwPrimFiniteMaxBound
      , rwPrimToFinite
      , rwPrimToFiniteMod
      , rwPrimFromFinite
      -- , rwPrimVecBulkUpdate
      , rwPrimVecConcat
      , rwPrimVecFromList
      , rwPrimVecIndex
      , rwPrimVecIndexProxy
      , rwPrimVecMap
      , rwPrimVecGenerate
      -- , rwPrimVecIterate
      -- , rwPrimVecZip
      , rwPrimVecRSlice
      , rwPrimVecReplicate
      , rwPrimVecReverse
      , rwPrimVecSlice
      , rwPrimXNor
      , rwPrimXOr
      , rwPrimToInteger
      , type (+), type GHC.Monad, type GHC.MonadTrans, KnownNat
      ) where

-- Imports in this file are ignored by rwc.
import Prelude ((.),($))
import qualified Prelude                           as GHC
import qualified Control.Monad.Identity            as GHC
import qualified Control.Monad.Resumption.Reactive as GHC
import qualified Control.Monad.State               as GHC
import qualified Data.Bits                         as GHC
import GHC.TypeLits (Nat, type (+), natVal)
import qualified GHC.TypeLits                      as TL
import qualified Data.Finite                       as F
import qualified Data.Vector.Sized                 as V
import qualified Data.Vector                       as VU
import qualified ReWire.BitWord                    as BW

type Identity   = GHC.Identity
type ReacT      = GHC.ReacT
type StateT     = GHC.StateT
type Integer    = GHC.Integer
type String     = GHC.String
type Bool       = GHC.Bool
type Vec        = V.Vector
type KnownNat   = TL.KnownNat
type Finite     = F.Finite

-- ReWire primitives.

-- Primitive types:
-- data (->) a b
-- data ReacT i o m a
-- data StateT s m a
-- data Identity a
-- data Integer
-- data Bit

-- Also tuples:
-- data () = ()
-- data (a, b) = (a, b)
-- ...

data R_ -- Ctors generated during program build.
data A_ -- Ctors generated during program build.
data Proxy (n :: Nat) = Proxy
data PuRe s o = Done (A_, s) | Pause (o, (R_, s))

-- Definitions in this file are for ghc compat and ignored by rwc.

{-# OPAQUE rwPrimError #-}
rwPrimError :: String -> a
rwPrimError = GHC.error

-- | The String and list arguments must be literals (after inlining).
{-# OPAQUE rwPrimExtern #-}
rwPrimExtern :: [(String, Integer)] -- ^ Module parameters (name and integer literal value).
             -> String              -- ^ Clock signal name or empty for no clock.
             -> String              -- ^ Reset signal name or empty for no reset.
             -> [(String, Integer)] -- ^ Module inputs (name and integer literal bitwidth).
             -> [(String, Integer)] -- ^ Module outputs (name and integer literal bitwidth).
             -> String              -- ^ Module name.
             -> a                   -- ^ Haskell definition to use when interpreting.
             -> String              -- ^ Instance name to use in generated Verilog.
             -> a
rwPrimExtern _ _ _ _ _ _ f _ = f

-- | The String arguments must be literals (after inlining).
{-# OPAQUE rwPrimCryptol #-}
rwPrimCryptol :: String -- ^ Cryptol module file.
              -> String -- ^ Function name.
              -> a      -- ^ Haskell definition to use when running under GHC.
              -> a
rwPrimCryptol _ _ f = f

{-# OPAQUE rwPrimBind #-}
rwPrimBind :: GHC.Monad m => m a -> (a -> m b) -> m b
rwPrimBind = (GHC.>>=)

{-# OPAQUE rwPrimReturn #-}
rwPrimReturn :: GHC.Monad m => a -> m a
rwPrimReturn = GHC.return

{-# OPAQUE rwPrimPut #-}
rwPrimPut :: GHC.Monad m => s -> StateT s m ()
rwPrimPut = GHC.put

{-# OPAQUE rwPrimGet #-}
rwPrimGet :: GHC.Monad m => StateT s m s
rwPrimGet = GHC.get

{-# OPAQUE rwPrimSignal #-}
rwPrimSignal :: GHC.Monad m => o -> ReacT i o m i
rwPrimSignal = GHC.signal

{-# OPAQUE rwPrimLift #-}
rwPrimLift :: (GHC.MonadTrans t, GHC.Monad m) => m a -> t m a
rwPrimLift = GHC.lift

{-# OPAQUE rwPrimExtrude #-}
rwPrimExtrude :: GHC.Monad m => ReacT i o (StateT s m) a -> s -> ReacT i o m a
rwPrimExtrude (GHC.ReacT (GHC.StateT m)) s =
   GHC.ReacT GHC.$
     do (res,s') <- m s
        case res of
            GHC.Left y -> GHC.return (GHC.Left y)
            GHC.Right (o,k) -> GHC.return (GHC.Right (o, \ i -> rwPrimExtrude (k i) s'))

-- | The seed is a function of the initial state so that a device may pause
--   before its state registers are initialized (by extrude); the registers
--   are unobservable until then (the compiler zero-fills them).
{-# OPAQUE rwPrimUnfold #-}
rwPrimUnfold :: ((R_, s) -> i -> PuRe s o) -> (s -> PuRe s o) -> ReacT i o Identity ()
rwPrimUnfold f s0 = go (s0 (GHC.error "rwPrimUnfold: the initial state is unobservable"))
      where go (Done _)      = GHC.return ()
            go (Pause (o,b)) = do i <- GHC.signal o
                                  go (f b i)

-- | Convert an Integer into a @'Finite' n@, throws an error if >= @n@.
{-# OPAQUE rwPrimFinite #-}
rwPrimFinite :: KnownNat n => Integer -> Finite n
rwPrimFinite = F.finite

{-# OPAQUE rwPrimFiniteMinBound #-}
rwPrimFiniteMinBound :: KnownNat n => Finite n
rwPrimFiniteMinBound = GHC.minBound

{-# OPAQUE rwPrimFiniteMaxBound #-}
rwPrimFiniteMaxBound :: KnownNat n => Finite n
rwPrimFiniteMaxBound = GHC.maxBound

{-# OPAQUE rwPrimToFinite #-}
rwPrimToFinite :: KnownNat n => Vec m Bool -> Finite n
rwPrimToFinite = F.finite . BW.toInteger'

{-# OPAQUE rwPrimToFiniteMod #-}
rwPrimToFiniteMod :: forall m n. KnownNat n => Vec m Bool -> Finite n
rwPrimToFiniteMod v = F.finite (BW.toInteger' v `GHC.mod` (natVal (Proxy :: Proxy n)))

{-# OPAQUE rwPrimFromFinite #-}
rwPrimFromFinite :: KnownNat m => Finite n -> Vec m Bool
rwPrimFromFinite = rwPrimResize . rwPrimBits . F.getFinite

-- *** Built-in Vec functions. ***

-- | Turns a List literal into a Vec with fixed length. I.e.,
-- > [x, y, z] :: Vec 3 a
{-# OPAQUE rwPrimVecFromList #-}
rwPrimVecFromList :: KnownNat n => [a] -> Vec n a
rwPrimVecFromList v = case V.fromList v of
       GHC.Just v' -> v'
       GHC.Nothing -> GHC.error "failed fromList: list is a different length than expected"

{-# OPAQUE rwPrimVecReplicate #-}
rwPrimVecReplicate :: KnownNat n => a -> Vec n a
rwPrimVecReplicate = V.replicate

{-# OPAQUE rwPrimVecReverse #-}
rwPrimVecReverse :: Vec n a -> Vec n a
rwPrimVecReverse = V.reverse

{-# OPAQUE rwPrimVecSlice #-}
rwPrimVecSlice :: (KnownNat i, KnownNat n) => Proxy i -> Vec ((i + n) + m) a -> Vec n a
rwPrimVecSlice = V.slice

-- | Slice indexed from the end of the Vec.
{-# OPAQUE rwPrimVecRSlice #-}
rwPrimVecRSlice :: (KnownNat i, KnownNat n) => Proxy i -> Vec ((i + n) + m) a -> Vec n a
rwPrimVecRSlice i = V.reverse . V.slice i . V.reverse

{-# OPAQUE rwPrimVecIndex #-}
rwPrimVecIndex :: Vec n a -> Finite n -> a
rwPrimVecIndex = V.index

{-# OPAQUE rwPrimVecIndexProxy #-}
rwPrimVecIndexProxy :: KnownNat n => Vec ((n + m) + 1) a -> Proxy n -> a
rwPrimVecIndexProxy = V.index'

{-# OPAQUE rwPrimVecMap #-}
rwPrimVecMap :: (a -> b) -> Vec n a -> Vec n b
rwPrimVecMap = V.map

-- rwPrimVecZip :: Vec n a -> Vec n b -> Vec n (a , b)
-- rwPrimVecZip = V.zip

{-# OPAQUE rwPrimVecGenerate #-}
rwPrimVecGenerate :: KnownNat n => (Finite n -> a) -> Vec n a
rwPrimVecGenerate = V.generate

-- rwPrimVecIterate :: KnownNat n => Proxy n -> (a -> a) -> a -> Vec n a
-- rwPrimVecIterate = V.iterateN'

-- | Concatenate vectors.
{-# OPAQUE rwPrimVecConcat #-}
rwPrimVecConcat :: Vec n a -> Vec m a -> Vec (n + m) a
rwPrimVecConcat = (V.++)

-- | Interpret an Integer as a bit vector.
{-# OPAQUE rwPrimBits #-}
rwPrimBits :: Integer -> Vec 128 Bool
rwPrimBits =
      rwPrimVecFromList . BW.padTrunc' i
                        . GHC.reverse . BW.int2bits'
        where
          i = GHC.fromIntegral (natVal (Proxy :: Proxy 128))

-- | Truncates or zero-pads most significant bits.
{-# OPAQUE rwPrimResize #-}
rwPrimResize :: forall m n . KnownNat m => Vec n Bool -> Vec m Bool
rwPrimResize v = rwPrimVecFromList vs'
    where
      vs = V.toList v
      vs' = BW.resize' (GHC.fromEnum (natVal (Proxy :: Proxy m))) vs

{-# OPAQUE rwPrimSignextend #-}
rwPrimSignextend :: forall m n . KnownNat m => Vec n Bool -> Vec m Bool
rwPrimSignextend v = rwPrimVecFromList vs'
    where
      vs = V.toList v
      vs' = BW.signextend (GHC.fromEnum (natVal (Proxy :: Proxy m))) vs

-- | Update multiple indices
-- rwPrimVecBulkUpdate :: KnownNat n => Vec n a -> Vec m (Finite n,a) -> Vec n a
-- rwPrimVecBulkUpdate v a = V.update v (V.map (BF.first fromEnum) a)

-- | Produce integer associated with type-level natural.
{-# OPAQUE rwPrimNatVal #-}
rwPrimNatVal :: KnownNat n => Proxy n -> Integer
rwPrimNatVal = natVal

-- | bitSlice a j i returns bits j (most significant) to i (least significant) from a (j >= i).
--   Bits are numbered with the least significant bit at 0 (Verilog convention);
--   the head of the Vec is the most significant bit.
--   The Finite arguments must be known/literals (after inlining).
{-# OPAQUE rwPrimBitSlice #-}
rwPrimBitSlice :: KnownNat m => Vec n Bool -> Finite n -> Finite n -> Vec m Bool
rwPrimBitSlice v j i = case V.toSized (VU.slice start len (V.fromSized v)) of
      GHC.Nothing -> GHC.error "rwPrimBitSlice: slice size mismatch"
      GHC.Just w  -> w
      where start = VU.length (V.fromSized v) GHC.- 1 GHC.- GHC.fromIntegral (F.getFinite j)
            len   = GHC.fromIntegral (F.getFinite j GHC.- F.getFinite i GHC.+ 1)

-- | bitIndex a i == bitSlice a i i.
--   The Finite argument must be known/literal (after inlining).
{-# OPAQUE rwPrimBitIndex #-}
rwPrimBitIndex :: Vec n Bool -> Finite n -> Bool
rwPrimBitIndex v i = V.fromSized v VU.! (VU.length (V.fromSized v) GHC.- 1 GHC.- GHC.fromIntegral (F.getFinite i))

-- *** Primitive bitwise operations based on Verilog operators. ***

-- | Add.
{-# OPAQUE rwPrimAdd #-}
rwPrimAdd :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimAdd v w = rwPrimResize $ rwPrimBits $ BW.toInteger' v GHC.+ BW.toInteger' w

-- | Subtract.
{-# OPAQUE rwPrimSub #-}
rwPrimSub :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimSub v w = rwPrimResize $ rwPrimBits $ BW.toInteger' v GHC.- BW.toInteger' w

-- | Multiply.
{-# OPAQUE rwPrimMul #-}
rwPrimMul :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimMul v w = rwPrimResize $ rwPrimBits $ BW.toInteger' v GHC.* BW.toInteger' w

-- | Divide.
{-# OPAQUE rwPrimDiv #-}
rwPrimDiv :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimDiv v w = rwPrimResize $ rwPrimBits $ rwPrimToInteger v `GHC.div` rwPrimToInteger w


-- | Modulus.
{-# OPAQUE rwPrimMod #-}
rwPrimMod :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimMod v w = rwPrimResize $ rwPrimBits $ BW.toInteger' v `GHC.mod` BW.toInteger' w

-- | Exponentiation.
{-# OPAQUE rwPrimPow #-}
rwPrimPow :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimPow v w = rwPrimVecFromList $ BW.power' (V.toList v) (V.toList w)

-- | Logical and.
{-# OPAQUE rwPrimLAnd #-}
rwPrimLAnd :: Vec n Bool -> Vec n Bool -> Bool
rwPrimLAnd v w = V.or v GHC.&& V.or w

-- | Logical or.
{-# OPAQUE rwPrimLOr #-}
rwPrimLOr :: Vec n Bool -> Vec n Bool -> Bool
rwPrimLOr v w = V.or v GHC.|| V.or w

-- | Logical not.
{-# OPAQUE rwPrimLNot #-}
rwPrimLNot :: Vec n Bool -> Bool
rwPrimLNot v = GHC.not (V.or v)  -- note that 'or' acts as 'toBool'

-- | Bitwise and.
{-# OPAQUE rwPrimAnd #-}
rwPrimAnd :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimAnd = V.zipWith (GHC.&&)

-- | Bitwise or.
{-# OPAQUE rwPrimOr #-}
rwPrimOr :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimOr = V.zipWith (GHC.||)

-- | Bitwise not.
{-# OPAQUE rwPrimNot #-}
rwPrimNot :: Vec n Bool -> Vec n Bool
rwPrimNot = V.map GHC.not

-- | Bitwise exclusive or.
{-# OPAQUE rwPrimXOr #-}
rwPrimXOr :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimXOr = V.zipWith GHC.xor

-- | Bitwise exclusive nor.
{-# OPAQUE rwPrimXNor #-}
rwPrimXNor :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimXNor = V.zipWith (\ x y -> GHC.not (GHC.xor x y))

-- | Shift left.
{-# OPAQUE rwPrimLShift #-}
rwPrimLShift :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimLShift v i = rwPrimVecFromList $ BW.shiftL' (V.toList v) (V.toList i)

-- | Shift right.
{-# OPAQUE rwPrimRShift #-}
rwPrimRShift :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimRShift v i = rwPrimVecFromList $ BW.shiftR' (V.toList v) (V.toList i)

-- | Shift right, sign-extend.
{-# OPAQUE rwPrimRShiftArith #-}
rwPrimRShiftArith :: KnownNat n => Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimRShiftArith v i = rwPrimVecFromList $ BW.arithShiftR' (V.toList v) (V.toList i)

-- | Equal.
{-# OPAQUE rwPrimEq #-}
rwPrimEq :: Vec n Bool -> Vec n Bool -> Bool
rwPrimEq v w = BW.toInteger' v GHC.== BW.toInteger' w

-- | Greater-than.
{-# OPAQUE rwPrimGt #-}
rwPrimGt :: Vec n Bool -> Vec n Bool -> Bool
rwPrimGt v w = BW.toInteger' v GHC.> BW.toInteger' w

-- | Greater-than or equal.
{-# OPAQUE rwPrimGtEq #-}
rwPrimGtEq :: Vec n Bool -> Vec n Bool -> Bool
rwPrimGtEq v w = BW.toInteger' v GHC.>= BW.toInteger' w

-- | Less-than.
{-# OPAQUE rwPrimLt #-}
rwPrimLt :: Vec n Bool -> Vec n Bool -> Bool
rwPrimLt v w = BW.toInteger' v GHC.< BW.toInteger' w

-- | Less-than or equal.
{-# OPAQUE rwPrimLtEq #-}
rwPrimLtEq :: Vec n Bool -> Vec n Bool -> Bool
rwPrimLtEq v w = BW.toInteger' v GHC.<= BW.toInteger' w

-- | Reduction and.
{-# OPAQUE rwPrimRAnd #-}
rwPrimRAnd :: Vec n Bool -> Bool
rwPrimRAnd = V.and

-- | Reduction nand (NOT of the and-reduction, following the Verilog ~& operator).
{-# OPAQUE rwPrimRNAnd #-}
rwPrimRNAnd :: Vec (1 + n) Bool -> Bool
rwPrimRNAnd = GHC.not . V.and

-- | Reduction or.
{-# OPAQUE rwPrimROr #-}
rwPrimROr :: Vec n Bool -> Bool
rwPrimROr = V.or

-- | Reduction nor (NOT of the or-reduction, following the Verilog ~| operator).
{-# OPAQUE rwPrimRNor #-}
rwPrimRNor :: Vec (1 + n) Bool -> Bool
rwPrimRNor = GHC.not . V.or

-- | Reduction xor.
{-# OPAQUE rwPrimRXOr #-}
rwPrimRXOr :: Vec (1 + n) Bool -> Bool
rwPrimRXOr = V.foldl1 GHC.xor

-- | Reduction xnor (NOT of the xor-reduction, following the Verilog ~^ operator).
{-# OPAQUE rwPrimRXNor #-}
rwPrimRXNor :: Vec (1 + n) Bool -> Bool
rwPrimRXNor = GHC.not . V.foldl1 GHC.xor

-- | Most significant bit.
{-# OPAQUE rwPrimMSBit #-}
rwPrimMSBit :: Vec (1 + n) Bool -> Bool
rwPrimMSBit = V.head

-- rwPrimToInteger :: Vec n Bool -> Integer
-- rwPrimToInteger = V.foldr (\ b iacc -> if b then 1 GHC.+ 2 GHC.* iacc else 2 GHC.* iacc) 0

{-# OPAQUE rwPrimToInteger #-}
rwPrimToInteger :: Vec n Bool -> Integer
rwPrimToInteger = V.foldl (\ iacc b -> if b then 1 GHC.+ 2 GHC.* iacc else 2 GHC.* iacc) 0
