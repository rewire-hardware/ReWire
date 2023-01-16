{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
module ReWire.HSE.Annotate
      ( annotate
      , Annote
      ) where

import ReWire.Annotation (Annote (..), toSrcSpanInfo)
import ReWire.HSE.Orphans ()
import ReWire.SYB (transform, gmapT)

import Data.Data (cast)
import Data.Maybe (fromJust)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)

import Language.Haskell.Exts.Syntax

annotate :: Module SrcSpanInfo -> Module Annote
annotate m = nodes $ LocAnnote <$> m

type SF a = a Annote -> a Annote

nodes :: Module Annote -> Module Annote
nodes =           (s :: SF Module)
      . transform (s :: SF ModuleHead)
      . transform (s :: SF ExportSpecList)
      . transform (s :: SF ExportSpec)
      . transform (s :: SF ImportDecl)
      . transform (s :: SF ImportSpecList)
      . transform (s :: SF ImportSpec)
      . transform (s :: SF Assoc)
      . transform (s :: SF Decl)
      . transform (s :: SF DeclHead)
      . transform (s :: SF InstRule)
      . transform (s :: SF InstHead)
      . transform (s :: SF IPBind)
      . transform (s :: SF ClassDecl)
      . transform (s :: SF InstDecl)
      . transform (s :: SF Deriving)
      . transform (s :: SF DataOrNew)
      . transform (s :: SF ConDecl)
      . transform (s :: SF FieldDecl)
      . transform (s :: SF QualConDecl)
      . transform (s :: SF GadtDecl)
      . transform (s :: SF BangType)
      . transform (s :: SF Match)
      . transform (s :: SF Rhs)
      . transform (s :: SF GuardedRhs)
      . transform (s :: SF Context)
      . transform (s :: SF FunDep)
      . transform (s :: SF Asst)
      . transform (s :: SF Type)
      . transform (s :: SF Kind)
      . transform (s :: SF TyVarBind)
      . transform (s :: SF Exp)
      . transform (s :: SF Stmt)
      . transform (s :: SF QualStmt)
      . transform (s :: SF FieldUpdate)
      . transform (s :: SF Alt)
      . transform (s :: SF XAttr)
      . transform (s :: SF Pat)
      . transform (s :: SF PatField)
      . transform (s :: SF PXAttr)
      . transform (s :: SF RPat)
      . transform (s :: SF RPatOp)
      . transform (s :: SF Literal)
      . transform (s :: SF ModuleName)
      . transform (s :: SF QName)
      . transform (s :: SF Name)
      . transform (s :: SF QOp)
      . transform (s :: SF Op)
      . transform (s :: SF CName)
      . transform (s :: SF IPName)
      . transform (s :: SF XName)
      . transform (s :: SF Bracket)
      . transform (s :: SF Splice)
      . transform (s :: SF Safety)
      . transform (s :: SF CallConv)
      . transform (s :: SF ModulePragma)
      . transform (s :: SF Rule)
      . transform (s :: SF RuleVar)
      . transform (s :: SF Activation)
      . transform (s :: SF Annotation)
      where s n = gmapT (\ t -> case cast t :: Maybe Annote of
                  Just _  -> fromJust $ cast $ AstAnnote (toSrcSpanInfo <$> n)
                  Nothing -> t) n
