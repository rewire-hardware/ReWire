{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
-- | The compiler's primitive operations, shared between the rwc
--   pipeline (Eidos IR) and the rewire-embedder pipeline (Atmo IR).
module ReWire.Builtins (Builtin (..), builtins, builtinName) where

import ReWire.Orphans ()
import ReWire.Pretty (TextShow (showt), FromGeneric (..), Pretty (pretty))
import ReWire.Unbound (Alpha (..))

import Control.Arrow ((&&&))
import Control.DeepSeq (NFData (..))
import Data.Data (Typeable, Data (..))
import Data.Hashable (Hashable (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Tuple (swap)
import GHC.Generics (Generic (..))

data Builtin = Error | Extern
             | Bind | Return
             | Put | Get
             | Signal | Lift | Extrude | Unfold
             | VecFromList | VecReplicate | VecReverse | VecSlice | VecRSlice
             | VecIndex | VecIndexProxy
             | VecConcat
             | VecMap | VecFoldR | VecFoldL | VecGenerate
             | Finite | FiniteMinBound | FiniteMaxBound | ToFinite | ToFiniteMod | FromFinite
             | NatVal
             | Bits | Resize | BitSlice | BitIndex
             | Add | Sub | Mul | Div | Mod | Pow
             | LAnd | LOr
             | And | Or
             | XOr | XNor
             | LShift | RShift | RShiftArith
             | Eq | Gt | GtEq | Lt | LtEq
             | LNot | Not
             | RAnd | RNAnd | ROr | RNor | RXOr | RXNor
             | MSBit
             | UsingExtern
      deriving (Eq, Ord, Generic, Show, Typeable, Data, Bounded, Enum)
      deriving TextShow via FromGeneric Builtin

instance Hashable Builtin
instance Alpha Builtin
instance NFData Builtin

instance Pretty Builtin where
      pretty = pretty . builtinName

builtinName :: Builtin -> Text
builtinName b = fromMaybe "" $ lookup b $ map swap builtins

builtins :: [(Text, Builtin)]
builtins = map ((("rwPrim" <>) . showt) &&& id) [minBound .. maxBound]
