{-# LANGUAGE TemplateHaskell,FlexibleInstances,MultiParamTypeClasses,FlexibleContexts,UndecidableInstances #-}

-- Some scribbles as I try to figure out what form ReWire Core will take. Note
-- that the term ReWire Core as used here is different from the term as used
-- in the ICFPT paper; rather than being a normal form of Haskell it is a
-- separate language like GHC Core. This is the input to the partial
-- evaluator.

module ReWire.Core where

import Unbound.LocallyNameless

-- Identifier is used instead of Name for anything that occurs at the top
-- level of the program (type names, constructor names, class names,
-- functions defined at top level) -- i.e. things that cannot be substituted
-- for.
type Identifier = String

data RWCTy = RWCTyApp RWCTy RWCTy
           | RWCTyCon Identifier
           | RWCTyVar (Name RWCTy)
           deriving Show

data RWCExp = RWCApp RWCTy RWCExp RWCExp
            | RWCLam RWCTy (Bind (Name RWCExp,Embed RWCTy) RWCExp)
            | RWCVar RWCTy (Name RWCExp)
            | RWCCon RWCTy Identifier
            | RWCLiteral RWCTy RWCLit
            | RWCCase RWCTy RWCExp [RWCAlt]
            deriving Show

data RWCLit = RWCLitInteger Integer
            | RWCLitFloat Double
            | RWCLitChar Char
            deriving Show

data RWCAlt = RWCAlt (Bind RWCPat (RWCExp,RWCExp)) -- (guard,body)
              deriving Show

data RWCPat = RWCPatCon (Embed RWCTy) Identifier [RWCPat]
            | RWCPatLiteral (Embed RWCTy) RWCLit
            | RWCPatVar (Embed RWCTy) (Name RWCExp)
            deriving Show

data RWCConstraint = RWCConstraint Identifier [RWCTy] deriving Show

-- Note that this is used both for non-overloaded functions *and* for method
-- definitions inside an instance; that should eliminate the need for class
-- declarations and simplify instance declarations (see RWCInstance below).
data RWCDefn = RWCDefn Identifier (Bind [Name RWCTy]
                                    ([RWCConstraint],
                                     RWCTy,
                                     RWCExp))
               deriving Show

data RWCData = RWCData Identifier (Bind [Name RWCTy]
                                    [RWCDataCon])
               deriving Show

data RWCDataCon = RWCDataCon Identifier [RWCTy]
                  deriving Show

data RWCNewtype = RWCNewtype Identifier (Bind [Name RWCTy]
                                          RWCNewtypeCon)
                  deriving Show

data RWCNewtypeCon = RWCNewtypeCon Identifier RWCTy
                     deriving Show

-- This is simpler than the instance declarations in Haskell. A RWCInstance
-- simply records the header of a Haskell instance declaration (e.g.
-- "instance Blop t => Blep t Int where"). The definitions of the
-- implementations are given as ordinary RWCDefns.
data RWCInstance = RWCInstance Identifier (Bind [Name RWCTy]
                                            ([RWCConstraint],RWCTy))
                   deriving Show

-- Random remark: if we want fundeps, we will need to encode them here. (I
-- will probably add this at some point.)
data RWCProg = RWCProg { dataDecls    :: [RWCData],
                         newtypeDecls :: [RWCNewtype],
                         instances    :: [RWCInstance],
                         defns        :: [RWCDefn] }
                       deriving Show

-- Boilerplate for Unbound.
instance Alpha RWCExp where
instance Alpha RWCAlt where
instance Alpha RWCPat where
instance Alpha RWCLit where
instance Alpha RWCTy where
instance Alpha RWCData where
instance Alpha RWCDataCon where
instance Alpha RWCNewtype where
instance Alpha RWCNewtypeCon where
instance Alpha RWCConstraint where
instance Alpha RWCDefn where
instance Alpha RWCInstance where
instance Alpha RWCProg where
  
$(derive [''RWCExp,''RWCAlt,''RWCPat,''RWCTy,''RWCLit,''RWCData,''RWCDataCon,''RWCNewtype,''RWCNewtypeCon,''RWCConstraint,''RWCDefn,''RWCInstance,''RWCProg])
