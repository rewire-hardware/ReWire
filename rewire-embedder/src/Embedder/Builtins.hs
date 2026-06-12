{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Embedder.Builtins 
    (Builtin(..), builtinName, builtins,
     TyBuiltin(..), s2tb, tb2s, tybuiltins,
     builtinUserQName, builtinUserName,
     RWUserOp(..), rwu2s, s2rwu, rwu2qn, qn2rwu) where

import GHC.Generics (Generic)
import Data.Data (Typeable, Data)
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)
import Control.Arrow ((&&&))

import ReWire.Builtins (Builtin (..), builtinName, builtins)
import ReWire.Pretty (Pretty (..), TextShow, FromGeneric(..))
import ReWire.Unbound (Alpha)

-- | Note: this is not injective (e.g., VecConcat has two notations)
-- | Note: there are rwPrim defs in ReWire that don't have Builtins (e.g. ToInteger)
-- | Note: there are Builtins that don't have direct corresponding notations (e.g. Bits is used in lit, but not directly)
builtinUserQName :: [(Builtin,Text)]
builtinUserQName = [
      -- | ReWire
      (Error, "ReWire.error"), (Extern, "ReWire.externWithSig"),
      (NatVal, "ReWire.natVal"),
      (Put, "ReWire.put"), (Get, "ReWire.get"), (Signal, "ReWire.signal"),
      (Lift, "ReWire.lift"), (Extrude, "ReWire.extrude"), (Unfold, "ReWire.unfold"),
      (VecFromList,"ReWire.fromList"),      
      -- | Prelude
      (Bind, "ReWire.Prelude.>>="), (Return, "ReWire.Prelude.return"), (Return, "ReWire.Prelude.pure"),
      -- | Vectors
      (VecReplicate, "ReWire.Vectors.replicate"), (VecReverse, "ReWire.Vectors.reverse"), 
      (VecSlice, "ReWire.Vectors.slice"), (VecRSlice, "ReWire.Vectors.rslice"), (VecIndex, "ReWire.Vectors.index"), 
      (VecIndexProxy, "ReWire.Vectors.index'"), (VecIndexProxy, "ReWire.Vectors.!"),
      (VecConcat, "ReWire.Vectors.++"), (VecMap, "ReWire.Vectors.map"), (VecGenerate, "ReWire.Vectors.generate"),
      -- Missing prims: VecUpdate, VecBulkUpdate, VecIterate, VecZip,
      -- Missing Builtins: VecFoldR, VecFoldL, VecFromList
      -- | Finites
      (Finite, "ReWire.Finite.finite"), (FiniteMinBound, "ReWire.Finite.minBound"), (FiniteMaxBound, "ReWire.Finite.maxBound"), 
      (ToFinite, "ReWire.Finite.toFinite"), (ToFiniteMod, "ReWire.Finite.toFinite'"), (FromFinite, "ReWire.Finite.fromFinite"),
      -- | Bits
      (Bits, "RWC.Primitives.rwPrimBits"),
      (Add, "ReWire.Bits.+"), (Sub, "ReWire.Bits.-"), (Mul, "ReWire.Bits.*"), (Div, "ReWire.Bits./"), (Mod, "ReWire.Bits.%"), (Pow, "ReWire.Bits.**"), 
      (Resize, "ReWire.Bits.resize"), (BitSlice, "ReWire.Bits.finBitSlice"),(BitIndex, "ReWire.Bits.finBitIndex"), 
      (LAnd, "ReWire.Bits.&&."), (LOr, "ReWire.Bits.||."), (LNot, "ReWire.Bits.lnot"),
      (And, "ReWire.Bits..&."), (Or, "ReWire.Bits..|."), (Not, "ReWire.Bits.bnot"), (XOr, "ReWire.Bits.^"), (XNor, "ReWire.Bits.~^"), 
      (LShift, "ReWire.Bits.<<."), (RShift, "ReWire.Bits.>>."), (RShiftArith, "ReWire.Bits.>>>"), 
      (Eq, "ReWire.Bits.=="), (Gt, "ReWire.Bits.>"), (GtEq, "ReWire.Bits.>="), (Lt, "ReWire.Bits.<"), (LtEq, "ReWire.Bits.<="),  
      (RAnd, "ReWire.Bits.rAnd"), (RNAnd, "ReWire.Bits.rNAnd"), (ROr, "ReWire.Bits.rOr"), (RNor, "ReWire.Bits.rNor"), (RXOr, "ReWire.Bits.rXOr"), (RXNor, "ReWire.Bits.rXNor"), 
      (MSBit, "ReWire.Bits.msbit"), (MSBit, "ReWire.Bits.bit"), (VecConcat, "ReWire.Bits.<>") ]
      -- Missing prims: ToInteger,
      -- Missing Builtins: Bits,

builtinUserName :: [(Builtin,Text)]
builtinUserName = [
      -- | ReWire
      (Error, "error"), (Extern, "externWithSig"),
      (NatVal, "natVal"),
      (Put, "put"), (Get, "get"), (Signal, "signal"),
      (Lift, "lift"), (Extrude, "extrude"), (Unfold, "unfold"),
      (VecFromList,"fromList"),      
      -- | Prelude
      (Bind, ">>="), (Return, "return"), (Return, "pure"),
      -- | Vectors
      (VecReplicate, "replicate"), (VecReverse, "reverse"), 
      (VecSlice, "slice"), (VecRSlice, "rslice"), (VecIndex, "index"), 
      (VecIndexProxy, "index'"), (VecIndexProxy, "!"),
      (VecConcat, "++"), (VecMap, "map"), (VecGenerate, "generate"),
      -- Missing prims: VecUpdate, VecBulkUpdate, VecIterate, VecZip,
      -- Missing Builtins: VecFoldR, VecFoldL, VecFromList
      -- | Finites
      (Finite, "finite"), (FiniteMinBound, "minBound"), (FiniteMaxBound, "maxBound"), 
      (ToFinite, "toFinite"), (ToFiniteMod, "toFinite'"), (FromFinite, "fromFinite"),
      -- | Bits
      (Bits, "rwPrimBits"),
      (Add, "+"), (Sub, "-"), (Mul, "*"), (Div, "/"), (Mod, "%"), (Pow, "**"), 
      (Resize, "resize"), (BitSlice, "finBitSlice"),(BitIndex, "finBitIndex"), 
      (LAnd, "&&."), (LOr, "||."), (LNot, "lnot"),
      (And, ".&."), (Or, ".|."), (Not, "bnot"), (XOr, "^"), (XNor, "~^"), 
      (LShift, "<<."), (RShift, ">>."), (RShiftArith, ">>>"), 
      (Eq, "=="), (Gt, ">"), (GtEq, ">="), (Lt, "<"), (LtEq, "<="),  
      (RAnd, "rAnd"), (RNAnd, "rNAnd"), (ROr, "rOr"), (RNor, "rNor"), (RXOr, "rXOr"), (RXNor, "rXNor"), 
      (MSBit, "msbit"), (MSBit, "bit")
      -- Missing prims: ToInteger,
      -- Missing Builtins: Bits, 
      ]


-- Cases where we need to include specific handling for rewire-user definitions:
-- 1. We need to print punctuation and infix notations carefully
-- 2. We may need to differentiate based on type information in some circumstances:
--    a. We can't define a single operator over different monads (or can we?)
--    b. We need to differentiate between vector and word operations
-- 3. We include Builtins because we handle them at the same junctures
-- We can handle other cases by writing a function or synonym in rewire-isabelle
data RWUserOp = 
      -- No need to repeat most builtins
      RWBuiltin Builtin
      -- ops for functions
      | CompDot | CompDol 
      -- ops that match builtins: specified typing
      -- + Monads
      | BindI | BindS | BindR | BindRInf 
      | Seq | SeqI | SeqS | SeqR | SeqRInf
      | RBindI | RBindS | RBindR | RBindRInf -- reverse bind =<<
      | ReturnI | ReturnS | ReturnR
      | LiftS | LiftR
      -- + Words
      | WordIndexProxy
      | WordIndexFin
      -- + Finites
      -- + Vectors
      -- constructors from haskell prelude for builtin datatypes
      -- Data structures: Embedder.Prelude: Maybe a = Nothing | Just a, Either a b = Left a | Right b, Bool = True | False
      -- ops for builtin datatypes
      | BAnd | BOr | BXOr 
      | FinAdd | FinSub | FinMul | FinDiv | FinEq | FinLt
      | WordSlice | WordIndex | NEq | Update 
      -- (.), ($), (&&), (||), (=<<), (>>),
      -- (+), (-), (*), (div), (==), (<)
      -- `xor`, (@@)=bitSlice, (@.)=bitIndex, (/=)=not Eq, (!=)=update
      -- Additional notations:
      -- wordconcat: (VecConcat, "Embedder.Vectors.<>")
      -- Prelude: id, const, flip, not, otherwise, maybe, either, fst, snd, curry, uncurry, undefined, 
      -- rewire-user notations that do not directly inline to builtins
      -- ReWire: Bit=Bool, W n=Vec n Bit, extern, modify, length, len
      -- Monad: iter, iterSt
      -- Vectors: 
      | VecLastIndexProxy -- in Isabelle, we can't overload notations that end in '
      -- | VecEmpty | VecSingleton | VecCons | VecSnoc | VecHead 
      -- | VecLastIndex | VecLastIndexProxy | VecTake | VecDrop | VecInit | VecTail 
      -- | VecZipWith | VecZipWith3 | VecPackLo | VecPackHi | VecUnpackLo | VecUnpackHi
      -- Additional notation: FiniteComp: even, odd
      -- Bits: zero, one, bit, lit, rotR, rotL, even, odd, Lit=W128
      deriving (Eq, Ord, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric RWUserOp

instance Hashable RWUserOp
instance Alpha RWUserOp
instance NFData RWUserOp

rwUserQName :: [(RWUserOp,Text)]
rwUserQName = [
      (BindI     , "ReWire.Prelude.>>=I"),  (BindS     , "ReWire.Prelude.>>=$"),
      (BindR     , "ReWire.Prelude.>>=~"),  (BindRInf  , "ReWire.Prelude.>>>=~"),
      (RBindI    , "ReWire.Prelude.=<<I"),  (RBindS    , "ReWire.Prelude.=<<$"),
      (RBindR    , "ReWire.Prelude.=<<~"),  (RBindRInf , "ReWire.Prelude.=<<<~"),
      (Seq       , "ReWire.Prelude.>>"),    (SeqI      , "ReWire.Prelude.>>I"),
      (SeqS      , "ReWire.Prelude.>>$"),   (SeqR      , "ReWire.Prelude.>>~"),
      (SeqRInf   , "ReWire.Prelude.>>>~"),  (CompDot   , "ReWire.Prelude.."),
      (CompDol   , "ReWire.Prelude.$"),     (BAnd      , "ReWire.Prelude.&&"),
      (BOr       , "ReWire.Prelude.||"),    (BXOr      , "ReWire.Bits.xor"),
      (FinAdd    , "ReWire.FiniteComp.+"),  (FinSub    , "ReWire.FiniteComp.-"),
      (FinMul    , "ReWire.FiniteComp.*"),  (FinDiv    , "ReWire.FiniteComp.div"),
      (FinEq     , "ReWire.FiniteComp.=="), (FinLt     , "ReWire.FiniteComp.<"),
      (WordSlice , "ReWire.Bits.@@"),       (WordIndex , "ReWire.Bits.@."),
      (NEq       , "ReWire.Bits./="),       (Update    , "ReWire.Vectors.!="),
      (VecLastIndexProxy, "ReWire.Vectors.lastIndex'"),
      (WordIndexProxy, "ReWire.Bits.!."), (WordIndexFin, "ReWire.Bits.!%")
      ]

rwUserName :: [(RWUserOp,Text)]
rwUserName = [
      (BindI     , ">>=I"),  (BindS     , ">>=$"),  (BindR     , ">>=~"),
      (BindRInf  , ">>>=~"), (RBindI    , "=<<I"),  (RBindS    , "=<<$"),
      (RBindR    , "=<<~"),  (RBindRInf , "=<<<~"), (Seq       , ">>"),
      (SeqI      , ">>I"),   (SeqS      , ">>$"),   (SeqR      , ">>~"),
      (SeqRInf   , ">>>~"),  (CompDot   , "."),     (CompDol   , "$"),
      (BAnd      , "&&"),    (BOr       , "||"),    (BXOr      , "xor"),
      (FinAdd   , "+"),      (FinSub    , "-"),     (FinMul    , "*"),
      (FinDiv    , "div"),   (FinEq     , "=="),    (FinLt     , "<"),
      (WordSlice , "@@"),    (WordIndex , "@."),    (NEq       , "/="),
      (Update    , "!="),    (VecLastIndexProxy, "lastIndex'"),
      (WordIndexProxy, "!."), (WordIndexFin, "!%")
      ]

rwu2s :: RWUserOp -> Maybe Text
rwu2s = \ case
      RWBuiltin b -> lookup b builtinUserName
      b -> lookup b rwUserName

s2rwu :: Text -> Maybe RWUserOp
s2rwu = \ case
      x | Just b <- lookup x (map swap builtinUserName) -> Just (RWBuiltin b)
      x -> lookup x (map swap rwUserName)

rwu2qn :: RWUserOp -> Maybe Text
rwu2qn = \ case
      RWBuiltin b -> lookup b builtinUserQName
      b -> lookup b rwUserQName

qn2rwu :: Text -> Maybe RWUserOp
qn2rwu = \ case
      x | Just b <- lookup x (map swap builtinUserQName) -> Just (RWBuiltin b)
      x -> lookup x (map swap rwUserQName)


-- | Type builtins
-- type (+), type GHC.Monad, type GHC.MonadTrans, KnownNat
-- Identity, ReacT, A_, R_, StateT, Vec, Finite, PuRe, Ref (..), Proxy (..)
-- Maybe, Either, Bool
-- Products?
-- | Primitives
-- Defined Types/Data structs: Monad, MonadTrans, A_=, R_=, PuRe s o=Done(A_,s)|Pause(o,(R_,s)), Ref a=Ref String, Proxy (n::Nat)=Proxy
-- Imported Types/Data structs: type(+),type(Nat),Identity, ReacT, StateT, Integer, String, Bool, Vec=Vector, KnownNat, Finite
      
data TyBuiltin =
          TyInteger
        | TyString
        | TyBool
        | TyUnit
        | TyFun
        | TyReacT
        | TyStateT
        | TyIdentity
        | TyState
        | TyRe
        | TyDev
        | TyStateDev
        | TyS
        | TyProd
        | TyList
        | TyVec
        | TyProxy
        | TyFin
        | TyPlus
        | TyNeg
        | TyRef
      deriving (Eq, Ord, Generic, Typeable, Data, Bounded, Enum, Show)
      deriving TextShow via FromGeneric TyBuiltin

tb2s :: TyBuiltin -> Text
tb2s = \ case
      TyInteger -> "Integer"
      TyString -> "String"
      TyBool -> "Bool"
      TyUnit -> "()"
      TyFun -> "->"
      TyReacT -> "ReacT"
      TyStateT -> "StateT"
      TyIdentity -> "Identity"
      TyState -> "State"
      TyS -> "S"
      TyRe -> "Re"
      TyDev -> "Dev"
      TyStateDev -> "StateDev"
      TyList -> "[_]"
      TyVec -> "Vec"
      TyProxy -> "Proxy"
      TyFin -> "Finite"
      TyProd -> "(,)"
      TyPlus -> "+"
      TyNeg -> "-"
      TyRef -> "Ref"

s2tb :: Text -> Maybe TyBuiltin
s2tb = \ case
      "Integer"   -> Just TyInteger
      "String"    -> Just TyString
      "Bool"      -> Just TyBool
      "()"        -> Just TyUnit
      "->"        -> Just TyFun
      "ReacT"     -> Just TyReacT
      "StateT"    -> Just TyStateT
      "Identity"  -> Just TyIdentity
      "State"     -> Just TyState
      "Re"        -> Just TyRe
      "Dev"       -> Just TyDev
      "StateDev"  -> Just TyStateDev
      "S"         -> Just TyS
      "(,)"       -> Just TyProd
      "[_]"       -> Just TyList
      "Vec"       -> Just TyVec
      "Proxy"     -> Just TyProxy
      "Finite"    -> Just TyFin
      "+"         -> Just TyPlus
      "-"         -> Just TyNeg
      "Ref"       -> Just TyRef
      _           -> Nothing

tybuiltins :: [(Text, TyBuiltin)]
tybuiltins = map (tb2s &&& id) [minBound .. maxBound]

instance Pretty TyBuiltin where
      pretty = pretty . tb2s

instance NFData TyBuiltin
instance Hashable TyBuiltin
instance Alpha TyBuiltin
