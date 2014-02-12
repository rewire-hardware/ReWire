{-# LANGUAGE TemplateHaskell,FlexibleInstances,MultiParamTypeClasses,FlexibleContexts,UndecidableInstances #-}

-- Some scribbles as I try to figure out what form ReWire Core will take. Note
-- that the term ReWire Core as used here is different from the term as used
-- in the ICFPT paper; rather than being a normal form of Haskell it is a
-- separate language like GHC Core. This is the input to the partial
-- evaluator.

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

data RWCAlt = RWCAlt (Bind RWCPat (RWCExp,RWCExp)) -- (guard,body)
              deriving Show

data RWCPat = RWCPatCon Identifier [RWCPat]
            | RWCPatLiteral RWCLit
            | RWCPatVar (Name RWCExp)
            deriving Show

data RWCConstraint = RWCConstraint Identifier [RWCTy] deriving Show

-- Unlike Haskell's surface syntax, the entire bundle of type constraints
-- (including those imposed by the class context) must be reflected here.
-- For example, instead of:
--
-- class Num a where
--   (+) :: a -> a -> a
--
-- you have:
--
-- class Num a where
--   (+) :: Num a => a -> a -> a
--
-- Another example:
--
-- class Monad m => MonadState s m where
--   get :: (Monad m,MonadState s m) => m s
--   put :: (Monad m,MonadState s m) => s -> m ()
--   upd :: (Monad m,MonadState s m) => (s -> s) -> m ()
--   upd f = get >>= \ s -> put (f s)
--
-- (This is because I couldn't figure out how to get the type variables to
-- scope over the class methods while still exporting the method names as
-- binders. There is probably a way to do this, but... oh well!)
data RWCClassMethod = RWCClassMethod (Name RWCExp) (Embed (SetPlusBind [Name RWCTy]
                                                               ([RWCConstraint],
                                                                RWCTy,
                                                                Maybe RWCExp)))
                      deriving Show

data RWCInstance = RWCInstance (SetPlusBind [Name RWCTy] ([RWCConstraint],RWCTy,[RWCInstanceMethod]))
                   deriving Show

data RWCInstanceMethod = RWCInstanceMethod (Name RWCExp) (SetPlusBind [Name RWCTy]
                                                              ([RWCConstraint],
                                                               RWCTy,
                                                               RWCExp))
                         deriving Show

data RWCDefn = RWCDefn (Name RWCExp) (Embed (SetPlusBind [Name RWCTy]
                                                 ([RWCConstraint],
                                                   RWCTy,
                                                   RWCExp)))
             | RWCClass Identifier (Embed (SetPlusBind [Name RWCTy]
                                               ([RWCConstraint],
                                                [RWCTy])))
                                   [RWCClassMethod]
                                   (Embed [RWCInstance])
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

data RWCProg = RWCProg { dataDecls    :: [RWCData],
                         newtypeDecls :: [RWCNewtype],
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
instance Alpha RWCNewtype where
instance Alpha RWCNewtypeCon where
instance Alpha RWCConstraint where
instance Alpha RWCClassMethod where
instance Alpha RWCInstance where
instance Alpha RWCInstanceMethod where
instance Alpha RWCDefn where
--instance Alpha RWCProg where      (can't have this anymore due to recbind)
  
instance Subst RWCExp RWCDefn where
  isvar _ = Nothing
  
instance Subst RWCExp RWCInstance where
  isvar _ = Nothing
  
instance Subst RWCExp RWCClassMethod where
  isvar _ = Nothing

instance Subst RWCExp RWCInstanceMethod where
  isvar _ = Nothing

instance Subst RWCExp RWCConstraint where
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

$(derive [''RWCExp,''RWCAlt,''RWCPat,''RWCTy,''RWCLit,''RWCData,''RWCDataCon,''RWCNewtype,''RWCNewtypeCon,''RWCConstraint,''RWCClassMethod,''RWCInstanceMethod,''RWCInstance,''RWCDefn{-,''RWCProg-}])
