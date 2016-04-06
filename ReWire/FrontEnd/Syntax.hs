{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving
           , Rank2Types, GADTs, ScopedTypeVariables, StandaloneDeriving, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ReWire.FrontEnd.Syntax
      ( DataConId (..), TyConId (..)
      , RWMTy (..), RWMExp (..), RWMPat (..)
      , RWMDefn (..), RWMData (..), RWMDataCon (..)
      , RWMProgram (..)
      , Kind (..)
      , kblank, tblank
      , flattenApp, mkArrow
      , typeOf
      , transProg
      , patVars, fv
      , Fresh, Name, Embed (..), TRec, Bind
      , trec, untrec, bind, unbind
      , Poly (..), (|->), poly
      , module C
      ) where

import ReWire.Annotation
import ReWire.Pretty
import ReWire.Core.Syntax as C
      ( DataConId (..)
      , TyConId (..)
      , flattenTyApp
      )
import ReWire.SYB

import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.Data (Typeable, Data(..))
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Text.PrettyPrint
      ( Doc, text, char, nest, hsep, punctuate, parens, doubleQuotes
      , space, hang, braces, vcat, (<+>), ($+$))

import Unbound.Generics.LocallyNameless
      ( Subst (..), Alpha (..), SubstName (..)
      , Bind, Name, TRec (..), Embed (..), name2String
      , runFreshM, Fresh (..), FreshMT (..)
      , bind, unbind, trec, untrec, aeq)
import qualified Unbound.Generics.LocallyNameless as UB
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)
import Unbound.Generics.LocallyNameless.Name (Name(..))
import Unbound.Generics.LocallyNameless.Bind (Bind(..))

fv :: (Alpha a, Typeable b) => a -> [Name b]
fv = toListOf UB.fv

kblank :: Kind
kblank = KBlank

tblank :: RWMTy
tblank = RWMTyBlank noAnn

data RWMDataCon = RWMDataCon Annote DataConId [RWMTy]
                  deriving (Generic,Eq,Show,Typeable,Data)

instance Alpha RWMDataCon

instance Annotated RWMDataCon where
  ann (RWMDataCon a _ _) = a

instance Pretty RWMDataCon where
  pretty (RWMDataCon _ n ts) = text (deDataConId n) <+> hsep (map pretty ts)

instance NFData RWMDataCon where
      rnf (RWMDataCon _ i ts) = i `deepseq` ts `deepseq` ()

data Kind = KStar | KFun Kind Kind | KMonad | KVar (Name Kind) | KBlank
      deriving (Generic, Ord, Eq, Show, Typeable, Data)

infixr `KFun`

instance Alpha Kind

instance Subst Kind Kind where
      isvar (KVar x) = Just $ SubstName x
      isvar _        = Nothing

instance NFData Kind where
      rnf = \ case
            KVar n     -> n `deepseq` ()
            KStar      -> ()
            KFun k1 k2 -> k1 `deepseq` k2 `deepseq` ()
            KMonad     -> ()
            KBlank     -> ()

instance Pretty Kind where
      pretty = \ case
            KStar    -> text "*"
            KVar n   -> text $ name2String n
            KFun a b -> parens $ pretty a <+> text "->" <+> pretty b
            KMonad   -> text "M"
            KBlank   -> text "_"

data Poly = Poly (Bind [Name RWMTy] RWMTy)
      deriving (Generic,Show,Typeable,Data)

instance NFData Poly where
      rnf (Poly t) = t `deepseq` ()

poly :: [Name RWMTy] -> RWMTy -> Poly
poly vs t = Poly $ bind vs t

(|->) :: [Name RWMTy] -> RWMTy -> Embed Poly
vs |-> t = Embed $ poly vs t

infix 0 |->

instance Alpha Poly

instance Eq Poly where
      (==) = aeq

instance Pretty Poly where
      pretty (Poly pt) = runFreshM $ do
            (_, t) <- unbind pt
            return $ pretty t

data RWMTy = RWMTyApp Annote RWMTy RWMTy
           | RWMTyCon Annote TyConId
           | RWMTyVar Annote (Name RWMTy)
           | RWMTyComp Annote RWMTy RWMTy -- application of a monad
           | RWMTyBlank Annote
           deriving (Eq,Generic,Show,Typeable,Data)

instance Alpha RWMTy

instance Subst RWMTy RWMTy where
      isvar (RWMTyVar _ x) = Just $ SubstName x
      isvar _              = Nothing
instance Subst RWMTy Annote
instance Subst RWMTy SrcSpanInfo
instance Subst RWMTy SrcSpan
instance Subst RWMTy TyConId
instance Subst RWMTy DataConId

instance Annotated RWMTy where
      ann = \ case
            RWMTyApp a _ _  -> a
            RWMTyCon a _    -> a
            RWMTyVar a _    -> a
            RWMTyComp a _ _ -> a
            RWMTyBlank a    -> a

instance Pretty (Name RWMTy) where
  pretty n = text $ name2String n

instance Pretty RWMTy where
      pretty = \ case
            RWMTyApp _ (RWMTyApp _ (RWMTyCon _ (TyConId "->")) t1) t2 -> ppTyArrowL t1 <+> text "->" <+> pretty t2
                  where ppTyArrowL t@(RWMTyApp _ (RWMTyApp _ (RWMTyCon _ (TyConId "->")) _) _) = parens $ pretty t
                        ppTyArrowL t                                                           = pretty t
            RWMTyApp _ t1 t2  -> pretty t1 <+> ppTyAppR t2
            RWMTyCon _ n      -> text (deTyConId n)
            RWMTyVar _ n      -> pretty n
            RWMTyComp _ t1 t2 -> pretty t1 <+> ppTyAppR t2
            RWMTyBlank _      -> text "_"

instance NFData RWMTy where
      rnf = \ case
            RWMTyApp _ t1 t2 -> t1 `deepseq` t2 `deepseq` ()
            RWMTyCon _ i     -> i  `deepseq` ()
            RWMTyVar _ x     -> x  `deepseq` ()
            RWMTyComp _ m t  -> m  `deepseq` t  `deepseq` ()
            RWMTyBlank _     -> ()

ppTyAppR :: RWMTy -> Doc
ppTyAppR t@RWMTyApp {} = parens $ pretty t
ppTyAppR t             = pretty t

----

data RWMExp = RWMApp Annote RWMExp RWMExp
            | RWMLam Annote RWMTy (Bind (Name RWMExp) RWMExp)
            | RWMVar Annote RWMTy (Name RWMExp)
            | RWMCon Annote RWMTy DataConId
            | RWMCase Annote RWMExp (Bind RWMPat RWMExp) RWMExp
            | RWMNativeVHDL Annote String RWMExp
            | RWMError Annote RWMTy String
            deriving (Generic, Show, Typeable, Data)

instance Alpha RWMExp

instance Subst RWMExp RWMExp where
      isvar (RWMVar _ _ x) = Just $ SubstName x
      isvar _              = Nothing
instance Subst RWMExp Annote
instance Subst RWMExp SrcSpanInfo
instance Subst RWMExp RWMTy
instance Subst RWMExp DataConId
instance Subst RWMExp TyConId
instance Subst RWMExp RWMPat
instance Subst RWMExp RWMDefn
instance Subst RWMExp Poly
instance Subst RWMExp SrcSpan

instance Subst RWMTy RWMExp
instance Subst RWMTy RWMPat

instance NFData RWMExp

instance Annotated RWMExp where
      ann = \ case
            RWMApp a _ _        -> a
            RWMLam a _ _        -> a
            RWMVar a _ _        -> a
            RWMCon a _ _        -> a
            RWMCase a _ _ _   -> a
            RWMNativeVHDL a _ _ -> a
            RWMError a _ _      -> a

instance Pretty RWMExp where
      pretty = \ case
            RWMApp _ e1 e2      -> parens $ hang (pretty e1) 4 $ pretty e2
            RWMCon _ _ n        -> text $ deDataConId n
            RWMVar _ _ n        -> text $ show n
            RWMLam _ _ e        -> runFreshM $ do
                  (p, e') <- unbind e
                  return $ parens $ text "\\ " <+> text (show p) <+> text "->" <+> pretty e'
            RWMCase _ e e1 e2 -> runFreshM $ do
                  (p, e1') <- unbind e1
                  return $ parens $
                        foldr ($+$) mempty
                        [ text "case" <+> pretty e <+> text "of"
                        , nest 4 (braces $ vcat $ punctuate (space <> text ";")
                              [ pretty p <+> text "->" <+> pretty e1'
                              , text "_" <+> text "->" <+> pretty e2
                              ])
                        ]
            RWMNativeVHDL _ n e -> parens (text "nativeVHDL" <+> doubleQuotes (text n) <+> parens (pretty e))
            RWMError _ _ m      -> parens (text "primError" <+> doubleQuotes (text m))

---

data RWMPat = RWMPatCon Annote DataConId [RWMPat]
            | RWMPatVar Annote (Embed RWMTy) (Name RWMExp)
            deriving (Show, Generic, Typeable, Data)

patVars :: RWMPat -> [Name RWMExp]
patVars = \ case
      RWMPatCon _ _ ps -> concatMap patVars ps
      RWMPatVar _ _ x  -> [x]

instance Alpha RWMPat

instance NFData RWMPat

instance Annotated RWMPat where
      ann = \ case
            RWMPatCon a _ _   -> a
            RWMPatVar a _ _   -> a

instance Pretty RWMPat where
      pretty = \ case
            RWMPatCon _ n ps  -> parens $ text (deDataConId n) <+> hsep (map pretty ps)
            RWMPatVar _ _ n   -> text $ show n

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
      pretty (RWMDefn _ n (Embed t) b (Embed e)) = runFreshM $ do
            (vs, e') <- unbind e
            return $ foldr ($+$) mempty
                  (  [text (show n) <+> text "::" <+> pretty t]
                  ++ (if b then [text "{-# INLINE" <+> text (show n) <+> text "#-}"] else [])
                  ++ [text (show n) <+> hsep (map (text . show) vs) <+> text "=", nest 4 $ pretty e']
                  )

---

data RWMData = RWMData
      { dataAnnote :: Annote
      , dataName   :: TyConId
      , dataTyVars :: [Name RWMTy]
      , dataKind   :: Kind
      , dataCons   :: [RWMDataCon]
      } deriving (Generic, Show, Typeable, Data)

instance Alpha RWMData

instance NFData RWMData

instance Annotated RWMData where
      ann (RWMData a _ _ _ _) = a

instance Pretty RWMData where
      pretty (RWMData _ n tvs k dcs) = foldr ($+$) mempty
            [ text "data" <+> text (deTyConId n) <+> text "::" <+> pretty k <+> hsep (map pretty tvs) <+> (if null (map pretty dcs) then mempty else char '=')
            , nest 4 (hsep (punctuate (space <+> char '|') $ map pretty dcs))
            ]

---


data RWMProgram = RWMProgram
      { dataDecls  :: [RWMData]
      , defns      :: TRec [RWMDefn]
      } deriving (Show, Typeable)

instance NFData RWMProgram where
      rnf (RWMProgram dds defs) = dds `deepseq` defs `deepseq` ()

instance Pretty RWMProgram where
      pretty p = ppDataDecls (dataDecls p) $+$ ppDefns (defns p)
            where ppDefns ds = runFreshM $ do
                        ds' <- untrec ds
                        return $ foldr ($+$) mempty $ map pretty ds'
                  ppDataDecls = foldr ($+$) mempty . map pretty

transProg :: (MonadCatch m, Fresh m) => (([RWMData], [RWMDefn]) -> Transform m) -> RWMProgram -> m RWMProgram
transProg f (RWMProgram ts vs) = do
      vs' <- untrec vs
      RWMProgram <$> runT (f (ts, vs')) ts <*> (trec <$> runT (f (ts, vs')) vs')

---

flattenApp :: RWMExp -> [RWMExp]
flattenApp (RWMApp _ e e') = flattenApp e ++ [e']
flattenApp e               = [e]

mkArrow :: RWMTy -> RWMTy -> RWMTy
mkArrow t = RWMTyApp (ann t) (RWMTyApp (ann t) (RWMTyCon (ann t) (TyConId "->")) t)

infixr `mkArrow`

mkArrow' :: RWMTy -> Bind (Name RWMExp) RWMExp -> RWMTy
mkArrow' t b = runFreshM $ do
      (_, e) <- unbind b
      return $ mkArrow t $ typeOf e

arrowRight :: RWMTy -> RWMTy
arrowRight (RWMTyApp _ (RWMTyApp _ (RWMTyCon _ (TyConId "->")) _) t2) = t2
arrowRight t                                                          = error $ "arrowRight: got non-arrow type: " ++ show t

typeOf :: RWMExp -> RWMTy
typeOf = \ case
      RWMApp _ e _        -> arrowRight (typeOf e)
      RWMLam _ t e        -> mkArrow' t e
      RWMVar _ t _        -> t
      RWMCon _ t _        -> t
      RWMCase _ _ _ e     -> typeOf e
      RWMNativeVHDL _ _ e -> typeOf e
      RWMError _ t _      -> t

-- Orphans.

deriving instance Data a => Data (Embed a)
deriving instance NFData a => NFData (TRec a)
deriving instance Data a => Data (Name a)
deriving instance (Data a, Data b) => Data (Bind a b)

deriving instance MonadThrow m => MonadThrow (FreshMT m)
deriving instance MonadCatch m => MonadCatch (FreshMT m)

-- Orphan instances for removing dependencies from Core.Syntax.

instance NFData DataConId
instance NFData TyConId

instance Alpha TyConId
instance Alpha DataConId

