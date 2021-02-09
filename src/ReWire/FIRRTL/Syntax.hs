{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DerivingVia #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.FIRRTL.Syntax where

import safe Data.Data (Typeable, Data (..))
import safe Data.Text (Text)
import TextShow (TextShow (..))
import TextShow.Generic (FromGeneric (..))
import safe GHC.Generics (Generic (..))

data Id = Id !Text
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Id

data Exp = UInt !(Maybe Int) !Int
      | UIntBits !(Maybe Int) !Text
      | SInt !(Maybe Int) !Int
      | SIntBits !(Maybe Int) !Text
      | Ref !Id
      | Subfield !Exp !Id
      | Subindex !Exp !Int
      | Subaccess !Exp !Exp
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

data Field = Forward !Id !Type
           | Flipped !Id !Type
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Field

data Type = UIntTy !(Maybe Int)
          | SIntTy !(Maybe Int)
          | ClockTy
          | BundleTy ![Field]
          | VecTy !Type !Int
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Type

-- Read Under Write flag
data RUW = Old | New | Undefined
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric RUW

data Stmt = Wire !Id !Type
          | Reg !Id !Type !Exp
          | RegReset !Id !Type !Exp !Exp !Exp
          -- id, type, depth, read latency, write latency, RUW, readers, writers, readwriters
          | Mem !Id !Type !Int !Int !Int !RUW ![Id] ![Id] ![Id]
          | Inst !Id !Id
          | Node !Id !Exp
          | Connect !Exp !Exp
          | ConnectPartial !Exp !Exp
          | Invalidate !Exp
          | If !Exp !Stmt
          | IfElse !Exp !Stmt !Stmt
          | Stop !Exp !Exp !Int
          | Printf !Exp !Exp !Text ![Exp]
          | Skip
          | Parens ![Stmt]
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Stmt

data Dir = Input | Output
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Dir

data Port = Port !Dir !Id !Type
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Port

data Module = ModuleDef !Id ![Port] !Stmt
            | ExtModule !Id ![Port]
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Module

data Circuit = Circuit !Id ![Module]
      deriving (Eq, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Circuit
