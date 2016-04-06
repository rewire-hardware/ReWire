{-# LANGUAGE MultiParamTypeClasses,GeneralizedNewtypeDeriving,FlexibleInstances,
             DeriveDataTypeable,DeriveGeneric,StandaloneDeriving,LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ReWire.Core.Syntax
  ( DataConId(..),TyConId(..)
  , RWCTy(..)
  , RWCExp(..)
  , RWCPat(..)
  , RWCDefn(..)
  , RWCDataCon(..)
  , RWCProgram(..)
  , mkArrow,arrowRight
  , flattenTyApp,flattenApp,typeOf
  , GId, LId
  ) where

import ReWire.Pretty
import ReWire.Annotation

import Data.Data (Typeable,Data(..))
import Data.List (nub)
import Text.PrettyPrint
import GHC.Generics (Generic)

newtype DataConId = DataConId { deDataConId :: String } deriving (Eq,Ord,Generic,Show,Typeable,Data)
newtype TyConId   = TyConId   { deTyConId :: String } deriving (Eq,Ord,Generic,Show,Typeable,Data)

type GId = String
type LId = Int

instance Pretty DataConId where
  pretty = text . deDataConId

instance Pretty TyConId where
  pretty = text . deTyConId

---

data RWCTy = RWCTyApp  Annote RWCTy RWCTy
           | RWCTyCon  Annote TyConId
           | RWCTyComp Annote RWCTy RWCTy
           deriving (Eq,Generic,Show,Typeable,Data)

instance Annotated RWCTy where
  ann (RWCTyApp a _ _)  = a
  ann (RWCTyCon a _)    = a
  ann (RWCTyComp a _ _) = a

instance Pretty RWCTy where
  pretty (RWCTyApp _ (RWCTyApp _ (RWCTyCon _ (TyConId "->")) t1) t2) = ppTyArrowL t1 <+> text "->" <+> pretty t2
    where ppTyArrowL t@(RWCTyApp _ (RWCTyApp _ (RWCTyCon _ (TyConId "->")) _) _) = parens $ pretty t
          ppTyArrowL t                                                           = pretty t
  pretty (RWCTyApp _ t1 t2)  = pretty t1 <+> ppTyAppR t2
  pretty (RWCTyCon _ n)      = text (deTyConId n)
  pretty (RWCTyComp _ t1 t2) = pretty t1 <+> ppTyAppR t2

ppTyAppR :: RWCTy -> Doc
ppTyAppR t@RWCTyApp {} = parens $ pretty t
ppTyAppR t             = pretty t

---

data RWCExp = RWCApp        Annote RWCExp RWCExp
            | RWCGVar       Annote RWCTy  GId
            | RWCLVar       Annote RWCTy  LId
            | RWCCon        Annote RWCTy  DataConId
            | RWCMatch      Annote RWCExp RWCPat GId (Maybe RWCExp)
            | RWCNativeVHDL Annote RWCTy  String
            deriving (Eq,Show,Typeable,Data)

-- The semantics of Match:
-- Match _ e p g e' = if p matches e, then call g with
--    (1) the current in-scope locals (i.e., the args of the current
--        definition) followed by
--    (2) the values that would be bound by the pattern (if the "holes" were
--        variables).

instance Annotated RWCExp where
  ann (RWCApp a _ _)        = a
  ann (RWCGVar a _ _)       = a
  ann (RWCLVar a _ _)       = a
  ann (RWCCon a _ _)        = a
  ann (RWCMatch a _ _ _ _)  = a
  ann (RWCNativeVHDL a _ _) = a

instance Pretty RWCExp where
  pretty (RWCApp _ e1 e2)       = parens $ hang (pretty e1) 4 (pretty e2)
  pretty (RWCCon _ _ n)         = text (deDataConId n)
  pretty (RWCGVar _ _ n)        = text n
  pretty (RWCLVar _ _ n)        = text $ "$" ++ show n
  pretty (RWCMatch _ e p e1 Nothing) = parens $
                                 foldr ($+$) empty
                                   [ text "case" <+> pretty e <+> text "of"
                                   , nest 4 (braces $ vcat $ punctuate (space <> text ";" <> space)
                                     -- TODO(chathhorn)
                                     [ parens (pretty p) <+> text "->" <+> text e1
                                     ])
                                   ]
  pretty (RWCMatch _ e p e1 (Just e2)) = parens $
                                 foldr ($+$) empty
                                   [ text "case" <+> pretty e <+> text "of"
                                   , nest 4 (braces $ vcat $ punctuate (space <> text ";" <> space)
                                     -- TODO(chathhorn)
                                     [ parens (pretty p) <+> text "->" <+> text e1
                                     , text "_" <+> text "->" <+> pretty e2
                                     ])
                                   ]
  pretty (RWCNativeVHDL _ _ n)  = parens (text "nativeVHDL" <+> doubleQuotes (text n))

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
                         defnName   :: GId,
                         defnTy     :: RWCTy, -- params given by the arity.
                         defnBody   :: RWCExp }
               deriving (Eq,Show,Typeable,Data)

instance Annotated RWCDefn where
  ann (RWCDefn a _ _ _) = a

instance Pretty RWCDefn where
  pretty (RWCDefn _ n ty e) = foldr ($+$) empty
                                           (  [text n <+> text "::" <+> pretty ty]
                                           ++ [text n <+> hsep (map (text . ("$"++) . show) [1..arity ty]) <+> text "=", nest 4 $ pretty e])

---

data RWCDataCon = RWCDataCon Annote DataConId [RWCTy]
                  deriving (Generic,Eq,Show,Typeable,Data)

instance Annotated RWCDataCon where
  ann (RWCDataCon a _ _) = a

instance Pretty RWCDataCon where
  pretty (RWCDataCon _ n ts) = text (deDataConId n) <+> hsep (map pretty ts)

---

data RWCProgram = RWCProgram { ctors  :: [RWCDataCon],
                               defns  :: [RWCDefn] }
                  deriving (Eq,Show,Typeable,Data)

instance Monoid RWCProgram where
  mempty = RWCProgram mempty mempty
  mappend (RWCProgram ts vs) (RWCProgram ts' vs') = RWCProgram (nub $ ts ++ ts') $ nub $ vs ++ vs'

instance Pretty RWCProgram where
  pretty p = ppDataDecls (ctors p) $+$ ppDefns (defns p)
    where ppDefns = foldr ($+$) empty . map pretty
          ppDataDecls = foldr ($+$) empty . map pretty

---

arity :: RWCTy -> Int
arity = \ case
  RWCTyApp _ (RWCTyApp _ (RWCTyCon _ (TyConId "->")) _) t2 -> 1 + arity t2
  _                                                        -> 0

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
typeOf = \ case
  RWCApp _ e _        -> arrowRight (typeOf e)
  RWCGVar _ t _       -> t
  RWCLVar _ t _       -> t
  RWCCon _ t _        -> t
  RWCMatch _ e _ _ _  -> typeOf e
  RWCNativeVHDL _ t _ -> t

