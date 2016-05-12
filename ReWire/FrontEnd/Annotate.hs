{-# LANGUAGE ScopedTypeVariables #-}
module ReWire.FrontEnd.Annotate
      ( annotate
      , Annote
      ) where

import ReWire.Annotation (Annote (..), toSrcSpanInfo)
import ReWire.SYB

import Control.Monad.Identity (Identity (..))
import Data.Data (Data (..), cast)
import Data.Maybe (fromJust)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)

import Language.Haskell.Exts.Annotated.Syntax

annotate :: (Data (ast Annote), Functor ast, Monad m) => ast SrcSpanInfo -> m (ast Annote)
annotate m = return $ runIdentity $ runPureT nodes $ LocAnnote <$> m

type SF a = a Annote -> Identity (a Annote)

nodes :: Transform Identity
nodes =   (s :: SF Module)
      ||> (s :: SF ModuleHead)
      ||> (s :: SF WarningText)
      ||> (s :: SF ExportSpecList)
      ||> (s :: SF ExportSpec)
      ||> (s :: SF ImportDecl)
      ||> (s :: SF ImportSpecList)
      ||> (s :: SF ImportSpec)
      ||> (s :: SF Assoc)
      ||> (s :: SF Decl)
      ||> (s :: SF DeclHead)
      ||> (s :: SF InstRule)
      ||> (s :: SF InstHead)
      ||> (s :: SF IPBind)
      ||> (s :: SF ClassDecl)
      ||> (s :: SF InstDecl)
      ||> (s :: SF Deriving)
      ||> (s :: SF DataOrNew)
      ||> (s :: SF ConDecl)
      ||> (s :: SF FieldDecl)
      ||> (s :: SF QualConDecl)
      ||> (s :: SF GadtDecl)
      ||> (s :: SF BangType)
      ||> (s :: SF Match)
      ||> (s :: SF Rhs)
      ||> (s :: SF GuardedRhs)
      ||> (s :: SF Context)
      ||> (s :: SF FunDep)
      ||> (s :: SF Asst)
      ||> (s :: SF Type)
      ||> (s :: SF Kind)
      ||> (s :: SF TyVarBind)
      ||> (s :: SF Exp)
      ||> (s :: SF Stmt)
      ||> (s :: SF QualStmt)
      ||> (s :: SF FieldUpdate)
      ||> (s :: SF Alt)
      ||> (s :: SF XAttr)
      ||> (s :: SF Pat)
      ||> (s :: SF PatField)
      ||> (s :: SF PXAttr)
      ||> (s :: SF RPat)
      ||> (s :: SF RPatOp)
      ||> (s :: SF Literal)
      ||> (s :: SF ModuleName)
      ||> (s :: SF QName)
      ||> (s :: SF Name)
      ||> (s :: SF QOp)
      ||> (s :: SF Op)
      ||> (s :: SF CName)
      ||> (s :: SF IPName)
      ||> (s :: SF XName)
      ||> (s :: SF Bracket)
      ||> (s :: SF Splice)
      ||> (s :: SF Safety)
      ||> (s :: SF CallConv)
      ||> (s :: SF ModulePragma)
      ||> (s :: SF Rule)
      ||> (s :: SF RuleVar)
      ||> (s :: SF Activation)
      ||> (s :: SF Annotation)
      ||> TId
      where s n = return $ gmapT (\ t -> case cast t :: Maybe Annote of
                  Just _  -> fromJust $ cast $ AstAnnote (toSrcSpanInfo <$> n)
                  Nothing -> t) n

