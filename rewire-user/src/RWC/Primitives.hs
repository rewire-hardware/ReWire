{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module RWC.Primitives
      ( Identity, ReacT, StateT, Vec, R_, A_, PuRe (..), Ref (..), Proxy (..)
      , rwPrimError
      , rwPrimExtern
      , rwPrimSetRef
      , rwPrimGetRef
      , rwPrimBind
      , rwPrimReturn
      , rwPrimPut
      , rwPrimGet
      , rwPrimSignal
      , rwPrimLift
      , rwPrimExtrude
      , rwPrimUnfold
      , rwPrimVecFromList
      , rwPrimVecReplicate
      , rwPrimVecReverse
      , rwPrimVecSlice
      , rwPrimVecRSlice
      , rwPrimVecIndex
      , rwPrimVecConcat
      , rwPrimBits
      , rwPrimResize
      , rwPrimBitSlice
      , rwPrimBitIndex
      , rwPrimAdd
      , rwPrimSub
      , rwPrimMul
      , rwPrimDiv
      , rwPrimMod
      , rwPrimPow
      , rwPrimLAnd
      , rwPrimLOr
      , rwPrimAnd
      , rwPrimOr
      , rwPrimXOr
      , rwPrimXNor
      , rwPrimLShift
      , rwPrimRShift
      , rwPrimRShiftArith
      , rwPrimEq
      , rwPrimGt
      , rwPrimGtEq
      , rwPrimLt
      , rwPrimLtEq
      , rwPrimConcat
      , rwPrimLNot
      , rwPrimNot
      , rwPrimRAnd
      , rwPrimRNAnd
      , rwPrimROr
      , rwPrimRNor
      , rwPrimRXOr
      , rwPrimRXNor
      , rwPrimMSBit
      , type (+), type GHC.Monad, type GHC.MonadTrans
      ) where

-- Imports in this file are ignored by rwc.
import Prelude ()
import qualified Prelude                           as GHC
import qualified Control.Monad.Identity            as GHC
import qualified Control.Monad.Resumption.Reactive as GHC
import qualified Control.Monad.State               as GHC
import GHC.TypeLits (Nat, type (+))

type Identity = GHC.Identity
type ReacT    = GHC.ReacT
type StateT   = GHC.StateT
type Integer  = GHC.Integer
type String   = GHC.String
type Bool     = GHC.Bool

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
data Vec (n :: Nat) a -- Ctors built in.
data Proxy (n :: Nat) = Proxy
data Ref a = Ref String
data PuRe s o = Done (A_, s) | Pause (o, (R_, s))

-- Definitions in this file are for ghc compat and ignored by rwc.

rwPrimError :: String -> a
rwPrimError = GHC.error

-- | The String and list arguments must be literals (after inlining).
rwPrimExtern :: [(String, Integer)] -- ^ Module parameters (name and integer literal value).
             -> String              -- ^ Clock signal name or empty for no clock.
             -> [(String, Integer)] -- ^ Module inputs (name and integer literal bitwidth).
             -> [(String, Integer)] -- ^ Module outputs (name and integer literal bitwidth).
             -> String              -- ^ Module name.
             -> a                   -- ^ Haskell definition to use when interpreting.
             -> String              -- ^ Instance name to use in generated Verilog.
             -> a
rwPrimExtern _ _ _ _ _ f _ = f

rwPrimSetRef :: Ref a -> a -> b -> b
rwPrimSetRef _ _ b = b

rwPrimGetRef :: Ref a -> a
rwPrimGetRef = GHC.error "Prim: get reference"

rwPrimBind :: GHC.Monad m => m a -> (a -> m b) -> m b
rwPrimBind = (GHC.>>=)

rwPrimReturn :: GHC.Monad m => a -> m a
rwPrimReturn = GHC.return

rwPrimPut :: GHC.Monad m => s -> StateT s m ()
rwPrimPut = GHC.put

rwPrimGet :: GHC.Monad m => StateT s m s
rwPrimGet = GHC.get

rwPrimSignal :: GHC.Monad m => o -> ReacT i o m i
rwPrimSignal = GHC.signal

rwPrimLift :: (GHC.MonadTrans t, GHC.Monad m) => m a -> t m a
rwPrimLift = GHC.lift

rwPrimExtrude :: ReacT i o (StateT s m) a -> s -> ReacT i o m a
rwPrimExtrude = GHC.error "Prim: extrude"

rwPrimUnfold :: ((R_, s) -> i -> PuRe s o) -> PuRe s o -> ReacT i o Identity a
rwPrimUnfold = GHC.error "Prim: unfold"

-- *** Built-in Vec functions. ***

-- | Turns a List literal into a Vec with fixed length. I.e.,
-- > [x, y, z] :: Vec 3 a
rwPrimVecFromList :: [a] -> Vec n a
rwPrimVecFromList = GHC.error "Prim: construct vector"

rwPrimVecReplicate :: a -> Vec n a
rwPrimVecReplicate = GHC.error "Prim: vector replicate"

rwPrimVecReverse :: Vec n a -> Vec n a
rwPrimVecReverse = GHC.error "Prim: vector reverse"

rwPrimVecSlice :: Proxy i -> Vec ((i + n) + m) a -> Vec n a
rwPrimVecSlice = GHC.error "Prim: vector slice"

-- | Slice indexed from the end of the Vec. Could be defined as:
-- > slice' i = reverse . slice i . reverse
rwPrimVecRSlice :: Proxy i -> Vec ((i + n) + m) a -> Vec n a
rwPrimVecRSlice = GHC.error "Prim: rear vector slice"

rwPrimVecIndex :: Vec ((n + m) + 1) a -> Proxy n -> a
rwPrimVecIndex = GHC.error "Prim: vector indexing"

-- | Concatenate vectors.
rwPrimVecConcat :: Vec n a -> Vec m a -> Vec (n + m) a
rwPrimVecConcat = GHC.error "Prim: vector concatenation"

-- | Interpret anything as a bit vector.
rwPrimBits :: a -> Vec n Bool
rwPrimBits = GHC.error "Prim: bitvector representation"

-- | Truncates or zero-pads most significant bits.
rwPrimResize :: Vec n Bool -> Vec m Bool
rwPrimResize = GHC.error "Prim: bitvector resize"

-- | bitSlice a j i returns bits j (most significant) to i (least significant) from a (j >= i).
--   The Integer arguments must be non-negative integer literals (after inlining).
rwPrimBitSlice :: Vec n Bool -> Integer -> Integer -> Vec m Bool
rwPrimBitSlice = GHC.error "Prim: bit slice"

-- | bitIndex a i == bitSlice a i i.
--   The Integer argument must be a non-negative integer literal (after inlining).
rwPrimBitIndex :: Vec n Bool -> Integer -> Bool
rwPrimBitIndex = GHC.error "Prim: bit extraction"

-- *** Primitive bitwise operations based on Verilog operators. ***

-- | Add.
rwPrimAdd :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimAdd = GHC.error "Prim: add"

-- | Subtract.
rwPrimSub :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimSub = GHC.error "Prim: sub"

-- | Multiply.
rwPrimMul :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimMul = GHC.error "Prim: multiply"

-- | Divide.
rwPrimDiv :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimDiv = GHC.error "Prim: divide"

-- | Modulus.
rwPrimMod :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimMod = GHC.error "Prim: modulus"

-- | Exponentiation.
rwPrimPow :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimPow = GHC.error "Prim: exponentiation"

-- | Logical and.
rwPrimLAnd :: Vec n Bool -> Vec n Bool -> Bool
rwPrimLAnd = GHC.error "Prim: logical and"

-- | Logical or.
rwPrimLOr :: Vec n Bool -> Vec n Bool -> Bool
rwPrimLOr = GHC.error "Prim: logical or"

-- | Bitwise and.
rwPrimAnd :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimAnd = GHC.error "Prim: bitwise and"

-- | Bitwise or.
rwPrimOr :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimOr = GHC.error "Prim: bitwise or"

-- | Bitwise exclusive or.
rwPrimXOr :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimXOr = GHC.error "Prim: bitwise exclusive or"

-- | Bitwise exclusive nor.
rwPrimXNor :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimXNor = GHC.error "Prim: bitwise exclusive nor"

-- | Shift left.
rwPrimLShift :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimLShift = GHC.error "Prim: bitwise shift left"

-- | Shift right.
rwPrimRShift :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimRShift = GHC.error "Prim: bitwise shift right"

-- | Shift right, sign-extend.
rwPrimRShiftArith :: Vec n Bool -> Vec n Bool -> Vec n Bool
rwPrimRShiftArith = GHC.error "Prim: bitwise shift right, sign-extend"

-- | Equal.
rwPrimEq :: Vec n Bool -> Vec m Bool -> Bool
rwPrimEq = GHC.error "Prim: equal"

-- | Greater-than.
rwPrimGt :: Vec n Bool -> Vec m Bool -> Bool
rwPrimGt = GHC.error "Prim: greater-than"

-- | Greater-than or equal.
rwPrimGtEq :: Vec n Bool -> Vec m Bool -> Bool
rwPrimGtEq = GHC.error "Prim: greater-than or equal"

-- | Less-than.
rwPrimLt :: Vec n Bool -> Vec m Bool -> Bool
rwPrimLt = GHC.error "Prim: less-than"

-- | Less-than or equal.
rwPrimLtEq :: Vec n Bool -> Vec m Bool -> Bool
rwPrimLtEq = GHC.error "Prim: less-than or equal"

-- | Concatenate.
rwPrimConcat :: Vec n Bool -> Vec m Bool -> Vec (n + m) Bool
rwPrimConcat = GHC.error "Prim: bitvector concatenation"

-- | Logical not.
rwPrimLNot :: Vec n Bool -> Bool
rwPrimLNot = GHC.error "Prim: logical not"

-- | Bitwise not.
rwPrimNot :: Vec n Bool -> Vec n Bool
rwPrimNot = GHC.error "Prim: bitwise not"

-- | Reduction and.
rwPrimRAnd :: Vec n Bool -> Bool
rwPrimRAnd = GHC.error "Prim: bitwise reduction and"

-- | Reduction nand.
rwPrimRNAnd :: Vec n Bool -> Bool
rwPrimRNAnd = GHC.error "Prim: bitwise reduction nand"

-- | Reduction or.
rwPrimROr :: Vec n Bool -> Bool
rwPrimROr = GHC.error "Prim: bitwise reduction or"

-- | Reduction nor.
rwPrimRNor :: Vec n Bool -> Bool
rwPrimRNor = GHC.error "Prim: bitwise reduction nor"

-- | Reduction xor.
rwPrimRXOr :: Vec n Bool -> Bool
rwPrimRXOr = GHC.error "Prim: bitwise reduction exclusive or"

-- | Reduction xnor.
rwPrimRXNor :: Vec n Bool -> Bool
rwPrimRXNor = GHC.error "Prim: bitwise reduction exclusive nor"

-- | Most significant bit.
rwPrimMSBit :: Vec n Bool -> Bool
rwPrimMSBit = GHC.error "Prim: most significant bit"
