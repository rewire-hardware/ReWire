{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving
           , Rank2Types, GADTs, ScopedTypeVariables, StandaloneDeriving, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ReWire.FrontEnd.Syntax
      ( DataConId (..), TyConId (..), Poly (..)
      , RWMExp (..)
      , RWMPat (..)
      , RWMDefn (..)
      , RWMData (..)
      , RWMProgram (..)
      , Kind (..)
      , kblank, tblank
      , flattenApp
      , typeOf
      , transProg
      , patVars, fvl
      , Fresh, Name, Embed (..), TRec, Bind
      , trec, untrec, bind, unbind
      , module C
      ) where

import ReWire.Annotation
import ReWire.Pretty
import ReWire.Core.Syntax as C
      ( DataConId (..)
      , TyConId (..)
      , RWCTy (..)
      , RWCDataCon (..)
      , Poly (..)
      , flattenTyApp
      , arrowRight
      , mkArrow
      , (|->)
      )
import ReWire.SYB

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.DeepSeq (NFData (..), deepseq)
import Data.Data (Typeable, Data(..))
import Data.Monoid ((<>))
import Text.PrettyPrint
      ( text, char, nest, hsep, punctuate, parens, doubleQuotes
      , space, hang, braces, vcat, (<+>), ($+$))

import Unbound.Generics.LocallyNameless
      ( Subst (..), Alpha (..), SubstName (..)
      , Bind, Name, TRec (..), Embed (..), name2String
      , runFreshM, Fresh (..), FreshMT (..), bind, unbind, trec, untrec, fv)
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)
import GHC.Generics (Generic)

fvl :: (Alpha a, Typeable b) => a -> [Name b]
fvl = toListOf fv

kblank :: Kind
kblank = KStar

tblank :: RWCTy
tblank = RWCTyCon noAnn (TyConId "_")

instance NFData RWCDataCon where
      rnf (RWCDataCon _ i ts) = i `deepseq` ts `deepseq` ()

data Kind = KStar | KFun Kind Kind | KMonad | KVar (Name Kind)
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

instance Pretty Kind where
      pretty = \ case
            KStar    -> text "*"
            KVar n   -> text $ name2String n
            KFun a b -> parens $ pretty a <+> text "->" <+> pretty b
            KMonad   -> text "'nad"

data RWMExp = RWMApp Annote RWMExp RWMExp
            | RWMLam Annote RWCTy (Bind (Name RWMExp) RWMExp)
            | RWMVar Annote RWCTy (Name RWMExp)
            | RWMCon Annote RWCTy DataConId
            | RWMCase Annote RWMExp (Bind RWMPat RWMExp) RWMExp
            | RWMNativeVHDL Annote String RWMExp
            | RWMError Annote RWCTy String
            deriving (Generic, Show, Typeable, Data)

instance Alpha RWMExp

instance Subst RWMExp RWMExp where
      isvar (RWMVar _ _ x) = Just $ SubstName x
      isvar _              = Nothing
instance Subst RWMExp Annote
instance Subst RWMExp SrcSpanInfo
instance Subst RWMExp RWCTy
instance Subst RWMExp DataConId
instance Subst RWMExp TyConId
instance Subst RWMExp RWMPat
instance Subst RWMExp SrcSpan

instance Subst RWCTy RWMExp
instance Subst RWCTy RWMPat

instance Annotated RWMExp where
      ann = \ case
            RWMApp a _ _        -> a
            RWMLam a _ _        -> a
            RWMVar a _ _        -> a
            RWMCon a _ _        -> a
            RWMCase a _ _ _   -> a
            RWMNativeVHDL a _ _ -> a
            RWMError a _ _      -> a

instance NFData RWMExp where
      rnf = \ case
            RWMApp _ e1 e2      -> e1 `deepseq` e2 `deepseq` ()
            RWMLam _ t e        -> t  `deepseq` e  `deepseq` ()
            RWMVar _ t x        -> t  `deepseq` x  `deepseq` ()
            RWMCon _ t i        -> t  `deepseq` i  `deepseq` ()
            RWMCase _ e e1 e2   -> e  `deepseq` e1 `deepseq` e2 `deepseq` ()
            RWMNativeVHDL _ n e -> n  `deepseq` e  `deepseq` ()
            RWMError _ t m      -> t  `deepseq` m  `deepseq` ()

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
                        , nest 4 (braces $ vcat $ punctuate (space <> text ";" <> space)
                              [ pretty p <+> text "->" <+> pretty e1'
                              , text "_" <+> text "->" <+> pretty e2
                              ])
                        ]
            RWMNativeVHDL _ n e -> parens (text "nativeVHDL" <+> doubleQuotes (text n) <+> parens (pretty e))
            RWMError _ _ m      -> parens (text "primError" <+> doubleQuotes (text m))

---

data RWMPat = RWMPatCon Annote DataConId [RWMPat]
            | RWMPatVar Annote RWCTy (Name RWMExp)
            deriving (Show, Generic, Typeable, Data)

patVars :: RWMPat -> [Name RWMExp]
patVars = \ case
      RWMPatCon _ _ ps -> concatMap patVars ps
      RWMPatVar _ _ x  -> [x]

instance Alpha RWMPat

instance Annotated RWMPat where
      ann = \ case
            RWMPatCon a _ _   -> a
            RWMPatVar a _ _   -> a

instance NFData RWMPat where
      rnf = \ case
            RWMPatCon _ i ps  -> i `deepseq` ps `deepseq` ()
            RWMPatVar _ x t   -> x `deepseq` t `deepseq` ()

instance Pretty RWMPat where
      pretty = \ case
            RWMPatCon _ n ps  -> parens $ text (deDataConId n) <+> hsep (map pretty ps)
            RWMPatVar _ n _   -> text $ show n

---

data RWMDefn = RWMDefn
      { defnAnnote :: Annote
      , defnName   :: Name RWMExp
      , defnPolyTy :: Poly
      , defnInline :: Bool
      , defnBody   :: Embed (Bind [Name RWMExp] RWMExp)
      } deriving (Generic, Show, Typeable, Data)

instance Alpha RWMDefn

instance Annotated RWMDefn where
      ann (RWMDefn a _ _ _ _) = a

instance NFData RWMDefn where
      rnf (RWMDefn _ n pt b e) = n `deepseq` pt `deepseq` b `deepseq` e `deepseq` ()

instance Pretty RWMDefn where
      pretty (RWMDefn _ n t b (Embed e)) = runFreshM $ do
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
      , dataTyVars :: [Name RWCTy]
      , dataKind   :: Kind
      , dataCons   :: [RWCDataCon]
      } deriving (Generic, Show, Typeable, Data)

instance Alpha RWMData

instance Annotated RWMData where
      ann (RWMData a _ _ _ _) = a

instance NFData RWMData where
      rnf (RWMData _ i tvs k dcs) = i `deepseq` tvs `deepseq` dcs `deepseq` k `deepseq` ()

instance Pretty RWMData where
      pretty (RWMData _ n tvs _ dcs) = foldr ($+$) mempty
            [ text "data" <+> text (deTyConId n) <+> hsep (map pretty tvs) <+> (if null (map pretty dcs) then mempty else char '=')
            , nest 4 (hsep (punctuate (char '|') $ map pretty dcs))
            ]

---

data RWMProgram = RWMProgram
      { dataDecls  :: [RWMData]
      , defns      :: TRec [RWMDefn]
      } deriving (Show, Typeable)

instance NFData RWMProgram where
      rnf (RWMProgram dds defs) = dds `deepseq` defs `deepseq` ()

instance Monoid RWMProgram where
      mempty = RWMProgram mempty $ trec mempty
      mappend (RWMProgram ts1 vs1) (RWMProgram ts2 vs2) = runFreshM $ do
            vs1' <- untrec vs1
            vs2' <- untrec vs2
            return $ RWMProgram (ts1 ++ ts2) $ trec $ vs1' ++ vs2'

instance Pretty RWMProgram where
      pretty p = ppDataDecls (dataDecls p) $+$ ppDefns (defns p)
            where ppDefns ds = runFreshM $ do
                        ds' <- untrec ds
                        return $ foldr ($+$) mempty $ map pretty ds'
                  ppDataDecls = foldr ($+$) mempty . map pretty

-- | Because no Data instance for RWMProgram...
transProg :: (MonadCatch m, Fresh m) => Transform m -> RWMProgram -> m RWMProgram
transProg f (RWMProgram ts vs) = do
      vs' <- untrec vs
      RWMProgram <$> runT f ts <*> (trec <$> runT f vs')

---

flattenApp :: RWMExp -> [RWMExp]
flattenApp (RWMApp _ e e') = flattenApp e ++ [e']
flattenApp e               = [e]

mkArrow' :: RWCTy -> Bind (Name RWMExp) RWMExp -> RWCTy
mkArrow' t b = runFreshM $ do
      (_, e) <- unbind b
      return $ mkArrow t $ typeOf e

typeOf :: RWMExp -> RWCTy
typeOf = \ case
      RWMApp _ e _        -> arrowRight (typeOf e)
      RWMLam _ t e        -> mkArrow' t e
      RWMVar _ t _        -> t
      RWMCon _ t _        -> t
      RWMCase _ _ _ e     -> typeOf e
      RWMNativeVHDL _ _ e -> typeOf e
      RWMError _ t _      -> t


-- Somewhat sketchy orphans.

deriving instance Data a => Data (Embed a)
deriving instance NFData a => NFData (TRec a)

deriving instance MonadThrow m => MonadThrow (FreshMT m)
deriving instance MonadCatch m => MonadCatch (FreshMT m)

-- Orphan instances for removing the NFData dependency from Core.Syntax.

deriving instance NFData DataConId
deriving instance NFData TyConId

instance NFData Poly where
      rnf (Poly t) = t `deepseq` ()

instance NFData RWCTy where
      rnf = \ case
            RWCTyApp _ t1 t2 -> t1 `deepseq` t2 `deepseq` ()
            RWCTyCon _ i     -> i  `deepseq` ()
            RWCTyVar _ x     -> x  `deepseq` ()
            RWCTyComp _ m t  -> m  `deepseq` t  `deepseq` ()
