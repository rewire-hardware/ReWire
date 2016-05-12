{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving
           , Rank2Types, GADTs, ScopedTypeVariables, StandaloneDeriving, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ReWire.FrontEnd.Syntax
      ( DataConId, TyConId
      , RWMTy (..), RWMExp (..), RWMPat (..), RWMMatchPat (..)
      , RWMDefn (..), RWMData (..), RWMDataCon (..)
      , RWMProgram (..)
      , Kind (..)
      , tblank, kblank
      , flattenApp, arr0, mkArrow, arrowRight, getArrow
      , fv, fvAny
      , Fresh, Name, Embed (..), TRec, Bind
      , trec, untrec, bind, unbind
      , Poly (..), (|->), poly
      ) where

import ReWire.Annotation
import ReWire.Pretty

import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.Data (Typeable, Data(..))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Text.PrettyPrint
      ( Doc, text, nest, hsep, punctuate, parens, doubleQuotes
      , space, hang, braces, vcat, (<+>), ($+$)
      )

import Unbound.Generics.LocallyNameless
      ( Subst (..), Alpha (..), SubstName (..)
      , Bind, Name, AnyName (..), TRec (..), Embed (..)
      , name2String, string2Name
      , runFreshM, Fresh (..), FreshMT (..)
      , bind, unbind, trec, untrec, aeq
      )
import qualified Unbound.Generics.LocallyNameless as UB
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)
import Unbound.Generics.LocallyNameless.Name (Name(..))
import Unbound.Generics.LocallyNameless.Bind (Bind(..))

fv :: (Alpha a, Typeable b) => a -> [Name b]
fv = toListOf UB.fv

fvAny :: Alpha a => a -> [AnyName]
fvAny = toListOf UB.fvAny

tblank :: RWMTy
tblank = RWMTyBlank noAnn

kblank :: Kind
kblank = KVar $ string2Name "_"

newtype DataConId = DataConId String
      deriving (Generic, Typeable, Data)
newtype TyConId = TyConId String
      deriving (Generic, Typeable, Data)

data RWMDataCon = RWMDataCon Annote (Name DataConId) (Embed Poly)
      deriving (Generic, Eq, Show, Typeable, Data)

instance Alpha RWMDataCon

instance Annotated RWMDataCon where
      ann (RWMDataCon a _ _) = a

instance Pretty RWMDataCon where
      pretty (RWMDataCon _ n t) = text (name2String n) <+> text "::" <+> pretty t

instance NFData RWMDataCon

data Kind = KStar | KFun Kind Kind | KMonad | KVar (Name Kind)
      deriving (Generic, Ord, Eq, Show, Typeable, Data)

infixr `KFun`

instance Alpha Kind

instance Subst Kind Kind where
      isvar (KVar x) = Just $ SubstName x
      isvar _        = Nothing

instance NFData Kind

instance Pretty Kind where
      pretty = \ case
            KStar            -> text "*"
            KVar n           -> text $ show n
            KFun a@KFun {} b -> parens (pretty a) <+> text "->" <+> pretty b
            KFun a b         -> pretty a <+> text "->" <+> pretty b
            KMonad           -> text "M"

newtype Poly = Poly (Bind [Name RWMTy] RWMTy)
      deriving (Generic, Show, Typeable, Data)

instance NFData Poly

poly :: [Name RWMTy] -> RWMTy -> Poly
poly vs t = Poly $ bind vs t

(|->) :: [Name RWMTy] -> RWMTy -> Embed Poly
vs |-> t = Embed $ poly vs t

infix 1 |->

instance Alpha Poly

instance Eq Poly where
      (==) = aeq

instance Pretty Poly where
      pretty (Poly pt) = runFreshM $ do
            (_, t) <- unbind pt
            return $ pretty t

data RWMTy = RWMTyApp Annote RWMTy RWMTy
           | RWMTyCon Annote (Name TyConId)
           | RWMTyVar Annote Kind (Name RWMTy)
           | RWMTyComp Annote RWMTy RWMTy -- application of a monad
           | RWMTyBlank Annote
           deriving (Eq,Generic,Show,Typeable,Data)

instance Alpha RWMTy

instance Subst RWMTy RWMTy where
      isvar (RWMTyVar _ _ x) = Just $ SubstName x
      isvar _                = Nothing
instance Subst RWMTy Annote where
      subst _ _ x = x
      substs _ x = x
instance Subst RWMTy Kind

instance Annotated RWMTy where
      ann = \ case
            RWMTyApp a _ _  -> a
            RWMTyCon a _    -> a
            RWMTyVar a _ _  -> a
            RWMTyComp a _ _ -> a
            RWMTyBlank a    -> a

instance Pretty RWMTy where
      pretty = \ case
            RWMTyApp _ (RWMTyApp _ (RWMTyCon _ c) t1) t2
                  | name2String c == "->" -> ppTyArrowL t1 <+> text "->" <+> pretty t2
                  where ppTyArrowL t@(RWMTyApp _ (RWMTyApp _ (RWMTyCon _ c) _) _)
                              | name2String c == "->" = parens $ pretty t
                        ppTyArrowL t                                                           = pretty t
            RWMTyApp _ t1 t2  -> pretty t1 <+> ppTyAppR t2
            RWMTyCon _ n      -> text (name2String n)
            RWMTyVar _ _ n    -> pretty n
            RWMTyComp _ t1 t2 -> pretty t1 <+> ppTyAppR t2
            RWMTyBlank _      -> text "_"

instance NFData RWMTy

ppTyAppR :: RWMTy -> Doc
ppTyAppR t@RWMTyApp {} = parens $ pretty t
ppTyAppR t             = pretty t

----

data RWMExp = RWMApp        Annote RWMExp RWMExp
            | RWMLam        Annote RWMTy (Bind (Name RWMExp) RWMExp)
            | RWMVar        Annote RWMTy (Name RWMExp)
            | RWMCon        Annote RWMTy (Name DataConId)
            | RWMCase       Annote RWMTy RWMExp (Bind RWMPat RWMExp) (Maybe RWMExp)
            | RWMMatch      Annote RWMTy RWMExp RWMMatchPat RWMExp [RWMExp] (Maybe RWMExp)
            | RWMNativeVHDL Annote String RWMExp
            | RWMError      Annote RWMTy String
            deriving (Generic, Show, Typeable, Data)

instance Alpha RWMExp

instance Subst RWMExp RWMExp where
      isvar (RWMVar _ _ x) = Just $ SubstName x
      isvar _              = Nothing
instance Subst RWMExp Annote where
      subst _ _ x = x
      substs _ x = x
instance Subst RWMExp RWMTy
instance Subst RWMExp Kind
instance Subst RWMExp RWMPat
instance Subst RWMExp RWMDefn
instance Subst RWMExp Poly

instance Subst RWMTy RWMExp
instance Subst RWMTy RWMPat

instance NFData RWMExp

instance Annotated RWMExp where
      ann = \ case
            RWMApp a _ _           -> a
            RWMLam a _ _           -> a
            RWMVar a _ _           -> a
            RWMCon a _ _           -> a
            RWMCase a _ _ _ _      -> a
            RWMMatch a _ _ _ _ _ _ -> a
            RWMNativeVHDL a _ _    -> a
            RWMError a _ _         -> a

instance Pretty RWMExp where
      pretty = \ case
            RWMApp _ e1 e2      -> parens $ hang (pretty e1) 4 $ pretty e2
            RWMCon _ _ n        -> text $ name2String n
            RWMVar _ t n        -> text (show n) <+> text "::" <+> pretty t
            RWMLam _ _ e        -> runFreshM $ do
                  (p, e') <- unbind e
                  return $ parens $ text "\\" <+> text (show p) <+> text "->" <+> pretty e'
            RWMCase _ _ e e1 e2 -> runFreshM $ do
                  (p, e1') <- unbind e1
                  return $ parens $
                        foldr ($+$) mempty
                        [ text "case" <+> pretty e <+> text "of"
                        , nest 4 (braces $ vcat $ punctuate (space <> text ";")
                              [ pretty p <+> text "->" <+> pretty e1' ]
                              ++ maybe [] (\ e2' -> [text "_" <+> text "->" <+> pretty e2']) e2
                              )
                        ]
            RWMMatch _ _ e p e1 as e2 -> runFreshM $ do
                  return $ parens $
                        foldr ($+$) mempty
                        [ text "match" <+> pretty e <+> text "of"
                        , nest 4 (braces $ vcat $ punctuate (space <> text ";")
                              [ pretty p <+> text "->" <+> pretty e1 <+> hsep (map pretty as) ]
                              ++ maybe [] (\ e2' -> [text "_" <+> text "->" <+> pretty e2']) e2
                              )
                        ]
            RWMNativeVHDL _ n e -> parens (text "nativeVHDL" <+> doubleQuotes (text n) <+> parens (pretty e))
            RWMError _ t m      -> parens (text "primError" <+> doubleQuotes (text m) <+> text "::" <+> pretty t)

---

data RWMPat = RWMPatCon  Annote (Embed (Name DataConId)) [RWMPat]
            | RWMPatVar  Annote (Embed RWMTy) (Name RWMExp)
            deriving (Show, Generic, Typeable, Data)

instance Alpha RWMPat

instance NFData RWMPat

instance Annotated RWMPat where
      ann = \ case
            RWMPatCon  a _ _ -> a
            RWMPatVar  a _ _ -> a

instance Pretty RWMPat where
      pretty = \ case
            RWMPatCon _ (Embed n) ps -> parens $ text (name2String n) <+> hsep (map pretty ps)
            RWMPatVar _ _ n          -> text $ show n


data RWMMatchPat = RWMMatchPatCon Annote (Name DataConId) [RWMMatchPat]
                 | RWMMatchPatVar Annote RWMTy
                 deriving (Show, Generic, Typeable, Data)

instance Alpha RWMMatchPat

instance Subst RWMExp RWMMatchPat
instance Subst RWMTy RWMMatchPat

instance NFData RWMMatchPat

instance Annotated RWMMatchPat where
      ann = \ case
            RWMMatchPatCon a _ _  -> a
            RWMMatchPatVar a _    -> a

instance Pretty RWMMatchPat where
      pretty = \ case
            RWMMatchPatCon _ n ps -> parens $ text (name2String n) <+> hsep (map pretty ps)
            RWMMatchPatVar _ t    -> parens $ text "*" <+> text "::" <+> pretty t

---

data RWMDefn = RWMDefn
      { defnAnnote :: Annote
      , defnName   :: Name RWMExp
      , defnPolyTy :: Embed Poly
      , defnInline :: Bool
      , defnBody   :: Embed (Bind [Name RWMExp] RWMExp)
      } deriving (Generic, Show, Typeable, Data)

instance Alpha RWMDefn

instance NFData RWMDefn

instance Annotated RWMDefn where
      ann (RWMDefn a _ _ _ _) = a

instance Pretty RWMDefn where
      pretty (RWMDefn _ n t b (Embed e)) = runFreshM $ do
            (vs, e') <- unbind e
            return $ foldr ($+$) mempty
                  $  [text (show n) <+> text "::" <+> pretty t]
                  ++ (if b then [text "{-# INLINE" <+> text (show n) <+> text "#-}"] else [])
                  ++ [text (show n) <+> hsep (map (text . show) vs) <+> text "=", nest 4 $ pretty e']

---

data RWMData = RWMData
      { dataAnnote :: Annote
      , dataName   :: Name TyConId
      , dataKind   :: Kind
      , dataCons   :: [RWMDataCon]
      } deriving (Generic, Show, Typeable, Data)

instance Alpha RWMData

instance NFData RWMData

instance Annotated RWMData where
      ann (RWMData a _ _ _) = a

instance Pretty RWMData where
      pretty (RWMData _ n k cs) = foldr ($+$) mempty $
                  (text "data" <+> text (name2String n) <+> text "::" <+> pretty k <+> text "where")
                  : map (nest 4 . pretty) cs

---

newtype RWMProgram = RWMProgram (TRec ([RWMData], [RWMDefn]))
      deriving (Generic, Show, Typeable)

instance Alpha RWMProgram

instance NFData RWMProgram where
      rnf (RWMProgram p) = p `deepseq` ()

instance Pretty RWMProgram where
      pretty (RWMProgram p) = runFreshM $ do
            (ts, vs) <- untrec p
            return $ (foldr ($+$) mempty $ map pretty ts) $+$ (foldr ($+$) mempty $ map pretty vs)
---

flattenApp :: RWMExp -> [RWMExp]
flattenApp (RWMApp _ e e') = flattenApp e ++ [e']
flattenApp e               = [e]

arr0 :: RWMTy -> RWMTy -> RWMTy
arr0 = mkArrow $ string2Name "->"

infixr `arr0`

mkArrow :: Name TyConId -> RWMTy -> RWMTy -> RWMTy
mkArrow arr t = RWMTyApp (ann t) (RWMTyApp (ann t) (RWMTyCon (ann t) arr) t)

arrowRight :: RWMTy -> RWMTy
arrowRight (RWMTyApp _ (RWMTyApp _ (RWMTyCon _ c) _) t2)
      | name2String c == "->" = t2
arrowRight t                  = t

getArrow :: [RWMData] -> Name TyConId
getArrow = dataName . fromJust . find ((== "->") . name2String . dataName)

-- Orphans.

deriving instance Data a => Data (Embed a)
deriving instance NFData a => NFData (TRec a)
deriving instance Data a => Data (Name a)
deriving instance (Data a, Data b) => Data (Bind a b)

deriving instance MonadThrow m => MonadThrow (FreshMT m)
deriving instance MonadCatch m => MonadCatch (FreshMT m)

instance Pretty AnyName where
      pretty (AnyName n) = pretty n

instance Pretty (Name a) where
      pretty n = text $ show n

instance Pretty a => Pretty (Embed a) where
      pretty (Embed a) = pretty a
