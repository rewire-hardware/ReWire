{-# OPTIONS -fwarn-incomplete-patterns #-}

module ReWire.PreHDL.Syntax where

import Data.List (intercalate)

type Label = String
type Loc   = String

data Prog = Prog { progHeader :: Header, progBody :: Cmd }

type Header = () -- FIXME

-- Allowed in:
--
--   CLFS
--
--   C - initial control flow graph
--   L - linearized control flow graph
--   F - flattened program
--   S - structured program
--
data Cmd = Rem String                -- [CLFS] comment
         | Assign Loc RHS            -- [CLFS] assignment
         | NextState Int             -- [ LFS] next state assignment
         | If BoolExp Cmd            -- [   S] if-then (no else)
         | Seq Cmd Cmd               -- [CLFS] s1 ; s2
         | Skip                      -- [CLFS] no-op
         | Goto BoolExp Label        -- [  F ] conditional branch to label
         | Lbl Label                 -- [  F ] label
         deriving Eq

data Bit = Zero | One deriving (Eq,Ord)

instance Show Bit where
  show Zero = "0"
  show One  = "1"

data RHS = BoolRHS BoolExp
         | LocRHS Loc
         | FunCallRHS String [Loc]
         | ConstRHS [Bit]
         deriving Eq

data BoolExp = And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Not BoolExp
             | BoolVar Loc
             | BoolConst Bool
             | InState Int
             deriving Eq

flattenSeq :: Cmd -> [Cmd]
flattenSeq (Seq c1 c2) = flattenSeq c1 ++ flattenSeq c2
flattenSeq c           = [c]

mkSeq :: Cmd -> Cmd -> Cmd
mkSeq c1 c2 = foldr1 Seq (flattenSeq c1 ++ flattenSeq c2)

instance Show BoolExp where
  show (And e1 e2)       = "(" ++ show e1 ++ " && " ++ show e2 ++ ")"
  show (Or e1 e2)        = "(" ++ show e1 ++ " || " ++ show e2 ++ ")"
  show (Not e)           = "(!" ++ show e ++ ")"
  show (BoolVar l)       = l
  show (BoolConst True)  = "true"
  show (BoolConst False) = "false"
  show (InState n)       = "(state == STATE" ++ show n ++ ")"

instance Show RHS where
  show (BoolRHS b)       = show b
  show (LocRHS l)        = l
  show (FunCallRHS f ls) = f ++ "(" ++ intercalate "," ls ++ ")"
  show (ConstRHS bs)     = "\"" ++ concatMap show bs ++ "\""
    
instance Show Cmd where
  show (Rem s)          = "/* " ++ s ++ " */"
  show (Assign l rhs)   = l ++ " := " ++ show rhs ++ ";"
  show (Lbl l)          = l ++ ":"
  show (Goto b l)       = "when " ++ show b ++ " goto " ++ l ++ ";"
  show (If b c)         = "if " ++ show b ++ " {\n"
                       ++ indent (show c) ++ "\n"
                       ++ "}"
    where indent :: String -> String
          indent s = "  " ++ idt s
            where idt "\n"     = "\n"
                  idt ('\n':s) = "\n  " ++ idt s
                  idt (c:s)    = c:idt s
                  idt ""       = ""
  show (Seq c1 c2)      = show c1 ++ "\n" ++ show c2
  show Skip             = "skip;"
  show (NextState n)    = "next state is " ++ show n ++ ";"

instance Show Prog where
  show p = show (progHeader p) ++ "\n" ++ show (progBody p)