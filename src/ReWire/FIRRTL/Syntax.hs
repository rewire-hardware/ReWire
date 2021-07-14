{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DerivingVia, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.FIRRTL.Syntax where

-- Syntax for FullFIRRTL.

import safe Data.Data (Typeable, Data (..))
import safe Data.Text (Text)
import TextShow (TextShow (..))
import TextShow.Generic (FromGeneric (..))
import safe GHC.Generics (Generic (..))

type Id = Text

data Circuit = Circuit !Id !(Maybe Info) ![Module]
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Circuit

data Module = Module !Id !(Maybe Info) ![Port] ![Stmt]
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Module

data Port = Input !Id !Type !(Maybe Info)
          | Output !Id !Type !(Maybe Info)
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Port

data Type = UIntTy !(Maybe Integer)
          | SIntTy !(Maybe Integer)
          | BundleTy ![Member]
          | VectorTy !Type !Integer
          | ClockTy
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Type

data Member = Binding !Id !Type | Flip !Id !Type
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Member

data RUW = Old | New | Undefined
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric RUW

data Info = Annotation !Text
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Info

data Stmt = Wire !Id !Type !(Maybe Info)
          | Reg !Id !Type !Exp !(Maybe (Exp, Exp)) !(Maybe Info)
          | CMem !Id !Type !(Maybe Info)
          | SMem !Id !Type !(Maybe Info)
          | Node !Id !Exp !(Maybe Info)
          | InferMPort !Id !Exp !Exp !(Maybe Info)
          | ReadMPort !Id !Exp !Exp !(Maybe Info)
          | WriteMPort !Id !Exp !Exp !(Maybe Info)
          | Memory !Id !(Maybe Info) !Type !Integer !Integer !Integer ![Id] ![Id] ![Id] !RUW
          | Instance !Id !Id !(Maybe Info)
          | Connect !Exp !Exp !(Maybe Info)
          | PartialConnect !Exp !Exp !(Maybe Info)
          | Invalid !Exp !(Maybe Info)
          | When !Exp !(Maybe Info) ![Stmt] !(Maybe [Stmt])
          | Skip
          | Stop !Exp !Exp !Integer
          | Printf !Exp !Exp !Text ![Exp] !(Maybe Info)
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Stmt

data Exp = UInt !(Maybe Integer) !Integer -- examining the example code generated
         | SInt !(Maybe Integer) !Integer -- these are in the format UInt<3>("hA")
         | Ref !Id                        -- reference
         | Field !Exp !Id
         | Index !Exp !Integer
         | Unbundle !Exp !Exp
         | Mux !Exp !Exp !Exp               -- multiplexor
         | Validif !Exp !Exp               -- conditionally valid
         | LitInt !Integer
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
         | Pad !Exp !Exp
         | AsUInt !Exp
         | AsSInt !Exp
         | AsClock !Exp
         -- Bit shift
         | Shl !Exp !Exp
         | Shr !Exp !Exp
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
         | Bits !Exp !Exp !Exp
         | Head !Exp !Exp
         | Tail !Exp !Exp
      deriving (Eq, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Exp
