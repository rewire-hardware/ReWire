module ReWire.FIRRTL.Sintax where

type Id = String

data Circuit   = Circuit (Maybe Info) Id [Module] deriving (Show, Eq)

data Module    = Module (Maybe Info) Id [Port] Block deriving (Show, Eq)

data Port = Input (Maybe Info) Id Type
          | Output (Maybe Info) Id Type
          deriving (Show, Eq)

data Type = UIntTy (Maybe Integer)
          | SIntTy (Maybe Integer)
          | Bundle [Field]
          | Vector Type Integer
          | Clock
          deriving (Show, Eq)

data Field = Binding Id Type | Flip Id Type deriving (Show, Eq)

data Mem = Mem {
  data_type      :: Type,
  depth          :: Integer,
  readers        :: [Id],
  writers        :: [Id],
  readWriters    :: [Id],
  readLatency    :: Integer,
  writeLatency   :: Integer,
  readUnderWrite :: RUWBehavior
} deriving (Show, Eq)

data RUWBehavior = Old | New | Undefined deriving (Show, Eq)

data Info = Annotation String deriving (Show, Eq)

type Block = [Stmt]

data Stmt = Wire (Maybe Info) Id Type
          | Reg (Maybe Info) Id Type Exp (Maybe (Exp, Exp))
          | CMem (Maybe Info) Id Type
          | SMem (Maybe Info) Id Type
          | Node (Maybe Info) Id Exp
          | InferMPort (Maybe Info) Id Exp Exp
          | ReadMPort (Maybe Info) Id Exp Exp
          | WriteMPort (Maybe Info) Id Exp Exp
          | Memory (Maybe Info) Id Mem
          | Instance (Maybe Info) Id Id
          | Connect (Maybe Info) Exp Exp
          | PartialConnect (Maybe Info) Exp Exp
          | Invalid Exp (Maybe Info)
          | WhenElse Exp Block (Maybe Block)
          | Skip
          | Stop Exp Exp Integer
          | Printf Exp Exp String [Exp] (Maybe Info)
          deriving (Show, Eq)

type Width = Integer

data Exp = LitUInt (Maybe Width) Integer -- examining the example code generated
         | LitSInt (Maybe Width) Integer -- these are in the format UInt<3>("hA")
         | Ref Id                        -- reference
         | Exp :. Id
         | Subind Exp Integer
         | Subacc Exp Exp
         | Mux Exp Exp Exp               -- multiplexor
         | Validif Exp Exp               -- conditionally valid
         | LitInt Integer
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
         deriving (Show, Eq)

-- data Op = Add | Sub | Mul | Div | Mod | LT | LEQ
--         | GT  | GEQ | EQ  | NEQ | Pad | AsUInt | AsSInt | AsClock
--         | Shl | Shr | DShl | DShr | Cvt | Neg | Not | And | Or | Xor | Andr| Orr | Xorr
--         | Cat | Bits | Head | Tail
--         deriving (Show, Eq)

