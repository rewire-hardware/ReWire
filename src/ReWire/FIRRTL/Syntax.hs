{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DerivingVia #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.FIRRTL.Syntax where

-- Syntax for LoFIRRTL.

import safe Data.Data (Typeable, Data (..))
import safe Data.Text (Text)
import TextShow (TextShow (..))
import TextShow.Generic (FromGeneric (..))
import safe GHC.Generics (Generic (..))
import safe Prettyprinter
      ( Doc, nest, hsep, parens, dquotes
      , braces, vcat, (<+>), Pretty (..)
      )

data Id = Id !Text
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Id

instance Pretty Id where
      pretty (Id x) = text x

data Exp = UInt !Int !Int
      | UIntBits !Int !Text
      | SInt !Int !Int
      | SIntBits !Int !Text
      | Ref !Id
      | Mux !Exp !Exp !Exp
      | Validif !Exp !Exp
      -- Primitive operations
      | Add !Exp !Exp
      | Sub !Exp !Exp
      | Mul !Exp !Exp
      | Div !Exp !Exp
      | Mod !Exp !Exp
      | Lt !Exp !Exp
      | Leq !Exp !Exp
      | Gt !Exp !Exp
      | Geq !Exp !Exp
      | Eq !Exp !Exp
      | Neq !Exp !Exp
      | Pad !Exp !Int
      | AsUInt !Exp
      | AsSInt !Exp
      | AsClock !Exp
      -- Bit shift
      | Shl !Exp !Int
      | Shr !Exp !Int
      -- Dynamic shift
      | Dshl !Exp !Exp 
      | Dshr !Exp !Exp
      -- Convert to signed
      | Cvt !Exp
      | Neg !Exp
      | Not !Exp
      | And !Exp !Exp
      | Or !Exp !Exp
      | Xor !Exp !Exp
      -- Bitwise reduction
      | Andr !Exp
      | Orr !Exp
      | Xorr !Exp
      | Cat !Exp !Exp
      | Bits !Exp !Int !Int
      | Head !Exp !Int
      | Tail !Exp !Int
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Exp

-- TODO
instance Pretty Exp where
      pretty = \ case
            _ -> text "exp"

data Type = UIntTy !Int
          | SIntTy !Int
          | ClockTy
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Type

-- TODO
instance Pretty Type where
      pretty = \ case
            _ -> text "type"

-- Read Under Write flag
data RUW = Old | New | Undefined
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric RUW

instance Pretty RUW where
      pretty = \ case
            Old       -> text "old"
            New       -> text "new"
            Undefined -> text "undefined"

data Stmt = Wire !Id !Type
          | Reg !Id !Type !Exp
          | RegReset !Id !Type !Exp !Exp !Exp
          -- id, type, depth, read latency, write latency, RUW, readers, writers, readwriters
          | Mem !Id !Type !Int !Int !Int !RUW ![Id] ![Id] ![Id]
          | Inst !Id !Id
          | Node !Id !Exp
          | Connect !Exp !Exp
          | Invalidate !Exp
          | Stop !Exp !Exp !Int
          | Printf !Exp !Exp !Text ![Exp]
          | Skip
          | Parens ![Stmt]
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Stmt

data Port = Input !Id !Type | Output !Id !Type
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Port

data Module = ModuleDef !Id ![Port] !Stmt
            | ExtModule !Id ![Port]
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Module

data Circuit = Circuit !Id ![Module]
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Circuit
