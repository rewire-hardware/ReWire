{-# LANGUAGE TemplateHaskell,FlexibleInstances,MultiParamTypeClasses,FlexibleContexts,UndecidableInstances #-}

module ReWire.Core where

import Unbound.LocallyNameless
import Unbound.LocallyNameless.Types (SetPlusBind)

-- Identifier is used instead of Name for anything that occurs at the top
-- level of the program (type names, constructor names, class names,
-- functions defined at top level) -- i.e. things that cannot be substituted
-- for.
type Identifier = String

data RWCTy = RWCTyApp RWCTy RWCTy
           | RWCTyCon Identifier
           | RWCTyVar (Name RWCTy)
           deriving Show

data RWCExp = RWCApp RWCExp RWCExp
            | RWCLam (Bind (Name RWCExp) RWCExp)
            | RWCVar RWCTy (Name RWCExp)
            | RWCCon RWCTy Identifier
            | RWCLiteral RWCTy RWCLit
            | RWCCase RWCExp [RWCAlt]
            deriving Show

data RWCLit = RWCLitInteger Integer
            | RWCLitFloat Double
            | RWCLitChar Char
            deriving (Eq,Show)

data RWCAlt = RWCAlt (Bind RWCPat RWCExp)
              deriving Show

data RWCPat = RWCPatCon Identifier [RWCPat]
            | RWCPatLiteral RWCLit
            | RWCPatVar (Name RWCExp)
            deriving Show

data RWCDefn = RWCDefn (Name RWCExp) (Embed (SetPlusBind [Name RWCTy]
                                                 (RWCTy,
                                                  RWCExp)))
               deriving Show

data RWCData = RWCData Identifier (Bind [Name RWCTy]
                                    [RWCDataCon])
               deriving Show

data RWCDataCon = RWCDataCon Identifier [RWCTy]
                  deriving Show

data RWCProg = RWCProg { dataDecls    :: [RWCData],
                         defns        :: TRec [RWCDefn] }
                       deriving Show

-- Boilerplate for Unbound.
instance Alpha RWCExp where
instance Alpha RWCAlt where
instance Alpha RWCPat where
instance Alpha RWCLit where
instance Alpha RWCTy where
instance Alpha RWCData where
instance Alpha RWCDataCon where
instance Alpha RWCDefn where
--instance Alpha RWCProg where      (can't have this anymore due to recbind)
  
instance Subst RWCExp RWCDefn where
  isvar _ = Nothing
  
instance Subst RWCExp RWCExp where
  isvar (RWCVar _ n) = Just (SubstName n)
  isvar _            = Nothing

instance Subst RWCExp RWCAlt where
  isvar _ = Nothing

instance Subst RWCExp RWCPat where
  isvar _ = Nothing

instance Subst a RWCLit where
  isvar _ = Nothing

instance Subst RWCExp RWCTy where
  isvar _ = Nothing

instance Subst RWCTy RWCTy where
  isvar (RWCTyVar n) = Just (SubstName n)
  isvar _            = Nothing
  
instance Subst RWCTy RWCExp where
  isvar _ = Nothing

instance Subst RWCTy RWCAlt where
  isvar _ = Nothing

instance Subst RWCTy RWCPat where
  isvar _ = Nothing

$(derive [''RWCExp,''RWCAlt,''RWCPat,''RWCTy,''RWCLit,''RWCData,''RWCDataCon,''RWCDefn{-,''RWCProg-}])
