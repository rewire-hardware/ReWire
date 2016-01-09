{-# LANGUAGE Rank2Types, GADTs, DeriveDataTypeable, ScopedTypeVariables #-}
module ReWire.FrontEnd.Annotate
      ( annotate
      , Annote (..)
      ) where

import ReWire.SYB

import Control.Monad.Identity (Identity(..))
import Data.Data (Data(..), Typeable, cast)
import Data.Foldable (Foldable)
import Data.Functor ((<$>))
import Data.Maybe (fromJust)
import Data.Traversable (Traversable)
import GHC.Generics (Generic)
import Language.Haskell.Exts.Annotated.ExactPrint (ExactP)
import Language.Haskell.Exts.Pretty (Pretty)
import Language.Haskell.Exts.SrcLoc (SrcInfo(..), SrcSpanInfo)

import Language.Haskell.Exts.Annotated.Syntax

-- | The point of this is just to hide the annotation from traversals.
data Annote where
      AnnoteLoc :: SrcSpanInfo -> Annote
      Annote :: forall ast.
            ( Functor ast
            , Foldable ast
            , Traversable ast
            , Annotated ast
            , ExactP ast
            , Eq (ast SrcSpanInfo)
            , Data (ast SrcSpanInfo)
            , Ord (ast SrcSpanInfo)
            , Show (ast SrcSpanInfo)
            , Generic (ast SrcSpanInfo)
            , Pretty (ast SrcSpanInfo)
            ) => ast SrcSpanInfo -> Annote
      deriving Typeable

instance SrcInfo Annote where
      toSrcInfo a b c = AnnoteLoc $ toSrcInfo a b c
      fromSrcInfo     = AnnoteLoc . fromSrcInfo
      fileName        = fileName . unAnnote
      startLine       = startLine . unAnnote
      startColumn     = startColumn . unAnnote

-- | Using the default definition of gfoldl.
instance Data Annote where
      gunfold    = undefined
      toConstr   = undefined
      dataTypeOf = undefined

unAnnote :: Annote -> SrcSpanInfo
unAnnote (AnnoteLoc l) = l
unAnnote (Annote a)    = ann a

annotate :: (Data (ast Annote), Functor ast) => ast SrcSpanInfo -> ast Annote
annotate m = runIdentity $ runPureT nodes $ AnnoteLoc <$> m

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
                  Just _  -> fromJust $ cast $ Annote (fmap unAnnote n)
                  Nothing -> t) n

