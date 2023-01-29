{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.FIRRTL.Syntax where

-- Syntax for FullFIRRTL.

import safe Data.Data (Typeable, Data (..))
import safe Data.Text (Text)
import safe GHC.Generics (Generic (..))

type Id = Text

data Circuit = Circuit !Id !(Maybe Info) ![Module]
      deriving (Eq, Generic, Typeable, Data, Show)

data Module = Module !Id !(Maybe Info) ![Port] ![Stmt]
      deriving (Eq, Generic, Typeable, Data, Show)

data Port = Input !Id !Type !(Maybe Info)
          | Output !Id !Type !(Maybe Info)
      deriving (Eq, Generic, Typeable, Data, Show)

data Type = UIntTy !(Maybe Integer)
          | SIntTy !(Maybe Integer)
          | BundleTy ![Member]
          | VectorTy !Type !Integer
          | ClockTy
      deriving (Eq, Generic, Typeable, Data, Show)

data Member = Binding !Id !Type | Flip !Id !Type
      deriving (Eq, Generic, Typeable, Data, Show)

data RUW = Old | New | Undefined
      deriving (Eq, Generic, Typeable, Data, Show)

newtype Info = Annotation Text
      deriving (Eq, Generic, Typeable, Data, Show)

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
         | XOr !Exp !Exp
         -- Bitwise reduction
         | Andr !Exp
         | Orr !Exp
         | XOrr !Exp
         | Cat !Exp !Exp
         | Bits !Exp !Exp !Exp
         | Head !Exp !Exp
         | Tail !Exp !Exp
      deriving (Eq, Generic, Typeable, Data, Show)
