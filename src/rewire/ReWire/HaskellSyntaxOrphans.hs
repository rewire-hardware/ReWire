{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DerivingVia #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ReWire.HaskellSyntaxOrphans () where

import Language.Haskell.Exts.Fixity (Fixity (..))
import Language.Haskell.Exts.SrcLoc (SrcSpan, SrcSpanInfo)
import Language.Haskell.Exts.Syntax
import TextShow (TextShow (..))
import TextShow.Generic (genericShowbPrec)

import GHC.Generics (Generic (..))

deriving instance Generic Fixity

instance TextShow Fixity where
      showbPrec = genericShowbPrec
instance TextShow SrcSpan where
      showbPrec = genericShowbPrec
instance TextShow SrcSpanInfo where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Role a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (BooleanFormula a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Unpackedness a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (PatternSynDirection a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Promoted a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Overlap a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (TypeEqn a) where
      showbPrec = genericShowbPrec
instance TextShow Tool where
      showbPrec = genericShowbPrec
instance TextShow Boxed where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (InjectivityInfo a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (EWildcard a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (DerivStrategy a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (MaybePromotedName a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Binds a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Sign a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (SpecialCon a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (ResultSig a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Namespace a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (WarningText a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Module a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (ModuleHead a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (ExportSpecList a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (ExportSpec a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (ImportDecl a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (ImportSpecList a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (ImportSpec a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Assoc a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Decl a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (DeclHead a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (InstRule a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (InstHead a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (IPBind a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (ClassDecl a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (InstDecl a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Deriving a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (DataOrNew a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (ConDecl a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (FieldDecl a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (QualConDecl a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (GadtDecl a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (BangType a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Match a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Rhs a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (GuardedRhs a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Context a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (FunDep a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Asst a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Type a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (TyVarBind a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Exp a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Stmt a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (QualStmt a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (FieldUpdate a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Alt a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (XAttr a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Pat a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (PatField a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (PXAttr a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (RPat a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (RPatOp a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Literal a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (ModuleName a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (QName a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Name a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (QOp a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Op a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (CName a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (IPName a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (XName a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Bracket a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Splice a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Safety a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (CallConv a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (ModulePragma a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Rule a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (RuleVar a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Activation a) where
      showbPrec = genericShowbPrec
instance TextShow a => TextShow (Annotation a) where
      showbPrec = genericShowbPrec

