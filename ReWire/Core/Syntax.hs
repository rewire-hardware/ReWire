{-# LANGUAGE MultiParamTypeClasses,GeneralizedNewtypeDeriving,FlexibleInstances,
             DeriveDataTypeable,DeriveGeneric,StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ReWire.Core.Syntax
  ( DataConId(..),TyConId(..),Poly(..)
  , RWCTy(..)
  , RWCExp(..)
  , RWCPat(..)
  , RWCDefn(..)
  , RWCData(..)
  , RWCDataCon(..)
  , RWCProgram(..)
  , mkArrow,arrowRight
  , flattenTyApp,flattenApp,typeOf
  , (|->)
  ) where

import ReWire.Pretty
import ReWire.Annotation

import Data.Data (Typeable,Data(..))
import Data.List (nub)
import Text.PrettyPrint
import GHC.Generics (Generic)

import Unbound.Generics.LocallyNameless (name2String,aeq,bind,Alpha,Subst(..),SubstName(..))
import Unbound.Generics.LocallyNameless.Name (Name(..))
import Unbound.Generics.LocallyNameless.Bind (Bind(..))

newtype DataConId = DataConId { deDataConId :: String } deriving (Eq,Ord,Generic,Show,Typeable,Data)
newtype TyConId   = TyConId   { deTyConId :: String } deriving (Eq,Ord,Generic,Show,Typeable,Data)

instance Alpha TyConId
instance Alpha DataConId

data Poly = Poly (Bind [Name RWCTy] RWCTy)
      deriving (Generic,Show,Typeable,Data)

(|->) :: [Name RWCTy] -> RWCTy -> Poly
vs |-> t = Poly $ bind vs t

infix 0 |->

instance Alpha Poly

instance Eq Poly where
  (==) = aeq

instance Pretty Poly where
  pretty _ = text "TODO(pretty Poly)"

instance Pretty DataConId where
  pretty = text . deDataConId

instance Pretty TyConId where
  pretty = text . deTyConId

---

data RWCTy = RWCTyApp Annote RWCTy RWCTy
           | RWCTyCon Annote TyConId
           | RWCTyVar Annote (Name RWCTy)
           | RWCTyComp Annote RWCTy RWCTy -- application of a monad
           deriving (Eq,Generic,Show,Typeable,Data)

instance Alpha RWCTy

instance Subst RWCTy RWCTy where
      isvar (RWCTyVar _ x) = Just $ SubstName x
      isvar _              = Nothing
instance Subst RWCTy Annote
instance Subst RWCTy SrcSpanInfo
instance Subst RWCTy SrcSpan
instance Subst RWCTy TyConId
instance Subst RWCTy DataConId

instance Annotated RWCTy where
  ann (RWCTyApp a _ _)  = a
  ann (RWCTyCon a _)    = a
  ann (RWCTyVar a _)    = a
  ann (RWCTyComp a _ _) = a

instance Pretty (Name RWCTy) where
  pretty n = text $ name2String n

instance Pretty RWCTy where
  pretty (RWCTyApp _ (RWCTyApp _ (RWCTyCon _ (TyConId "->")) t1) t2) = ppTyArrowL t1 <+> text "->" <+> pretty t2
    where ppTyArrowL t@(RWCTyApp _ (RWCTyApp _ (RWCTyCon _ (TyConId "->")) _) _) = parens $ pretty t
          ppTyArrowL t                                                           = pretty t
  pretty (RWCTyApp _ t1 t2)  = pretty t1 <+> ppTyAppR t2
  pretty (RWCTyCon _ n)      = text (deTyConId n)
  pretty (RWCTyVar _ n)      = pretty n
  pretty (RWCTyComp _ t1 t2) = pretty t1 <+> ppTyAppR t2

ppTyAppR :: RWCTy -> Doc
ppTyAppR t@RWCTyApp {} = parens $ pretty t
ppTyAppR t             = pretty t

---

data RWCExp = RWCApp Annote RWCExp RWCExp
            | RWCGVar Annote RWCTy String
            | RWCLVar Annote RWCTy Int
            | RWCCon Annote RWCTy DataConId
            | RWCCase Annote RWCExp RWCPat RWCExp RWCExp
            | RWCMatch Annote RWCExp DataConId String [String] RWCExp
            | RWCNativeVHDL Annote RWCTy String
            | RWCError Annote RWCTy String
            deriving (Eq,Show,Typeable,Data)

instance Annotated RWCExp where
  ann (RWCApp a _ _)        = a
  ann (RWCGVar a _ _)       = a
  ann (RWCLVar a _ _)       = a
  ann (RWCCon a _ _)        = a
  ann (RWCCase a _ _ _ _)   = a
  ann (RWCNativeVHDL a _ _) = a
  ann (RWCError a _ _)      = a

instance Pretty RWCExp where
  pretty (RWCApp _ e1 e2)      = parens $ hang (pretty e1) 4 (pretty e2)
  pretty (RWCCon _ _ n)        = text (deDataConId n)
  pretty (RWCGVar _ _ n)       = text n
  pretty (RWCLVar _ _ n)       = text $ "$" ++ show n
  pretty (RWCCase _ e p e1 e2) = parens $
                                 foldr ($+$) empty
                                   [ text "case" <+> pretty e <+> text "of"
                                   , nest 4 (braces $ vcat $ punctuate (space <> text ";" <> space)
                                     [ parens (pretty p) <+> text "->" <+> pretty e1
                                     , text "_" <+> text "->" <+> pretty e2
                                     ])
                                   ]
  pretty (RWCNativeVHDL _ _ n) = parens (text "nativeVHDL" <+> doubleQuotes (text n))
  pretty (RWCError _ _ m)      = parens (text "primError" <+> doubleQuotes (text m))

---

data RWCPat = RWCPatCon Annote DataConId [RWCPat]
            | RWCPatVar Annote RWCTy
            deriving (Eq,Show,Typeable,Data)

instance Annotated RWCPat where
  ann (RWCPatCon a _ _) = a
  ann (RWCPatVar a _)   = a

instance Pretty RWCPat where
  pretty (RWCPatCon _ n ps) = parens (text (deDataConId n) <+> hsep (map pretty ps))
  pretty (RWCPatVar _ _)    = text "*"

---

data RWCDefn = RWCDefn { defnAnnote :: Annote,
                         defnName   :: String,
                         defnPolyTy :: Poly,
                         defnArity  :: Int,
                         defnBody   :: RWCExp }
               deriving (Eq,Show,Typeable,Data)

instance Annotated RWCDefn where
  ann (RWCDefn a _ _ _ _) = a

instance Pretty RWCDefn where
  pretty (RWCDefn _ n ty arr e) = foldr ($+$) empty
                                           (  [text n <+> text "::" <+> pretty ty]
                                           ++ [text n <+> hsep (map (text . ("$"++) . show) [1..arr]) <+> text "=", nest 4 $ pretty e])

---

data RWCData = RWCData { dataAnnote :: Annote,
                         dataName   :: TyConId,
                         dataTyVars :: [Name RWCTy],
                         dataCons   :: [RWCDataCon] }
               deriving (Eq,Show,Typeable,Data)

instance Annotated RWCData where
  ann (RWCData a _ _ _) = a

-- FIXME: just ignoring the kind here
instance Pretty RWCData where
  pretty (RWCData _ n tvs dcs) = foldr ($+$) empty
                                     [text "data" <+> text (deTyConId n) <+> hsep (map pretty tvs) <+> (if null (map pretty dcs) then empty else char '='),
                                     nest 4 (hsep (punctuate (char '|') $ map pretty dcs))]

---

data RWCDataCon = RWCDataCon Annote DataConId [RWCTy]
                  deriving (Generic,Eq,Show,Typeable,Data)

instance Alpha RWCDataCon

instance Annotated RWCDataCon where
  ann (RWCDataCon a _ _) = a

instance Pretty RWCDataCon where
  pretty (RWCDataCon _ n ts) = text (deDataConId n) <+> hsep (map pretty ts)

---

data RWCProgram = RWCProgram { dataDecls  :: [RWCData],
                               defns      :: [RWCDefn] }
                  deriving (Eq,Show,Typeable,Data)

instance Monoid RWCProgram where
  mempty = RWCProgram mempty mempty
  mappend (RWCProgram ts vs) (RWCProgram ts' vs') = RWCProgram (nub $ ts ++ ts') $ nub $ vs ++ vs'

instance Pretty RWCProgram where
  pretty p = ppDataDecls (dataDecls p) $+$ ppDefns (defns p)
    where ppDefns = foldr ($+$) empty . map pretty
          ppDataDecls = foldr ($+$) empty . map pretty

---

flattenTyApp :: RWCTy -> [RWCTy]
flattenTyApp (RWCTyApp _ t1 t2) = flattenTyApp t1 ++ [t2]
flattenTyApp t                  = [t]

flattenApp :: RWCExp -> [RWCExp]
flattenApp (RWCApp _ e e') = flattenApp e++[e']
flattenApp e               = [e]

mkArrow :: RWCTy -> RWCTy -> RWCTy
mkArrow t = RWCTyApp (ann t) (RWCTyApp (ann t) (RWCTyCon (ann t) (TyConId "->")) t)

infixr `mkArrow`

arrowRight :: RWCTy -> RWCTy
arrowRight (RWCTyApp _ (RWCTyApp _ (RWCTyCon _ (TyConId "->")) _) t2) = t2
arrowRight t                                                          = error $ "arrowRight: got non-arrow type: " ++ show t

typeOf :: RWCExp -> RWCTy
typeOf (RWCApp _ e _)        = arrowRight (typeOf e)
typeOf (RWCGVar _ t _)       = t
typeOf (RWCLVar _ t _)       = t
typeOf (RWCCon _ t _)        = t
typeOf (RWCCase _ _ _ e _)   = typeOf e
typeOf (RWCNativeVHDL _ t _) = t
typeOf (RWCError _ t _)      = t

-- Orphans.

deriving instance Data a => Data (Name a)
deriving instance (Data a, Data b) => Data (Bind a b)
