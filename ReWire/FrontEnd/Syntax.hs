{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances
           , DeriveDataTypeable, DeriveGeneric, Rank2Types, GADTs, ScopedTypeVariables
           , StandaloneDeriving, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ReWire.FrontEnd.Syntax
      ( DataConId(..), TyConId(..), Poly(..)
      , RWMExp(..)
      , RWMPat(..)
      , RWMDefn(..)
      , RWMData(..)
      , RWMProgram(..)
      , kblank, tblank
      , flattenApp
      , typeOf, patvars
      , module C
      ) where

import ReWire.Annotation
import ReWire.FrontEnd.Kinds
import ReWire.Pretty
import ReWire.Scoping
import ReWire.Core.Syntax as C
      ( DataConId(..)
      , TyConId(..)
      , RWCTy(..)
      , RWCDataCon(..)
      , Poly(..)
      , flattenTyApp
      , mkArrow
      , arrowRight
      )

import Control.DeepSeq (NFData (..), deepseq)
import Data.ByteString.Char8 (pack)
import Data.Data (Typeable, Data(..))
import Data.List (nub)
import Data.Monoid ((<>))
import Text.PrettyPrint
      ( text, char, nest, hsep, punctuate, parens, doubleQuotes
      , space, hang, braces, vcat, (<+>), ($+$))

kblank :: Kind
kblank = Kstar

tblank :: RWCTy
tblank = RWCTyCon noAnn (TyConId "_")

data RWMExp = RWMApp Annote RWMExp RWMExp
            | RWMLam Annote (Id RWMExp) RWCTy RWMExp
            | RWMVar Annote (Id RWMExp) RWCTy
            | RWMCon Annote DataConId RWCTy
            | RWMCase Annote RWMExp RWMPat RWMExp RWMExp
            | RWMNativeVHDL Annote String RWMExp
            | RWMError Annote String RWCTy
            deriving (Ord, Eq, Show, Typeable, Data)

instance Annotated RWMExp where
      ann = \ case
            RWMApp a _ _        -> a
            RWMLam a _ _ _      -> a
            RWMVar a _ _        -> a
            RWMCon a _ _        -> a
            RWMCase a _ _ _ _   -> a
            RWMNativeVHDL a _ _ -> a
            RWMError a _ _      -> a

instance IdSort RWMExp where
      idSort _ = pack "E"

instance Subst RWMExp RWMExp where
      fv = \ case
            RWMApp _ e1 e2      -> fv e1 ++ fv e2
            RWMLam _ x _ e      -> filter (/= x) $ fv e
            RWMVar _ x _        -> [x]
            RWMCon _ _ _        -> []
            RWMCase _ e p e1 e2 -> fv e ++ filter (not . (`elem` patvars p)) (fv e1) ++ fv e2
            RWMNativeVHDL _ _ e -> fv e
            RWMError _ _ _      -> []
      bv = \ case
            RWMApp _ e1 e2      -> bv e1 ++ bv e2
            RWMLam _ x _ e      -> x : bv e
            RWMVar _ _ _        -> []
            RWMCon _ _ _        -> []
            RWMCase _ e p e1 e2 -> bv e ++ patvars p ++ bv e1 ++ bv e2
            RWMNativeVHDL _ _ e -> bv e
            RWMError _ _ _      -> []
      subst' = \ case
            RWMApp an e1 e2      -> RWMApp an <$> subst' e1 <*> subst' e2
            RWMLam an x t e      -> refresh x (fv e) $ \ x' -> do
                  e' <- subst' e
                  return $ RWMLam an x' t e'
            RWMVar an x t        -> query x >>= return . \ case
                  Just (Left y)  -> RWMVar an y t
                  Just (Right e) -> e
                  Nothing        -> RWMVar an x t
            RWMCon an i t        -> return $ RWMCon an i t
            RWMCase an e p e1 e2 -> RWMCase an <$> subst' e <*> return p <*> subst' e1 <*> subst' e2
            RWMNativeVHDL an n e -> RWMNativeVHDL an n <$> subst' e
            RWMError an m t      -> return $ RWMError an m t

instance Subst RWMExp RWCTy where
      fv = \ case
            RWMApp _ e1 e2      -> fv e1 ++ fv e2
            RWMLam _ _ t e      -> fv t ++ fv e
            RWMVar _ _ t        -> fv t
            RWMCon _ _ t        -> fv t
            RWMCase _ e p e1 e2 -> fv e ++ fv p ++ fv e1 ++ fv e2
            RWMNativeVHDL _ _ e -> fv e
            RWMError _ _ t      -> fv t
      bv _ = []
      subst' = \ case
            RWMApp an e1 e2      -> RWMApp an <$> subst' e1 <*> subst' e2
            RWMLam an x t e      -> RWMLam an x <$> subst' t <*> subst' e
            RWMVar an x t        -> RWMVar an x <$> subst' t
            RWMCon an i t        -> RWMCon an i <$> subst' t
            RWMCase an e p e1 e2 -> RWMCase an <$> subst' e <*> subst' p <*> subst' e1 <*> subst' e2
            RWMNativeVHDL an n e -> RWMNativeVHDL an n <$> subst' e
            RWMError an m t      -> RWMError an m <$> subst' t

instance Alpha RWMExp where
      aeq' (RWMApp _ e1 e2) (RWMApp _ e1' e2')             = (&&) <$> aeq' e1 e1' <*> aeq' e2 e2'
      aeq' (RWMLam _ x t e) (RWMLam _ x' t' e')            = equating x x' $ (&&) <$> aeq' t t' <*> aeq' e e'
      aeq' (RWMVar _ x _) (RWMVar _ y _)                   = varsaeq x y
      aeq' (RWMCon _ i _) (RWMCon _ j _)                   = return $ i == j
      aeq' (RWMCase _ e p e1 e2) (RWMCase _ e' p' e1' e2') = (&&) <$> aeq' e e' <*> ((&&) <$> equatingPats p p' (aeq' e1 e1') <*> aeq' e2 e2')
      aeq' (RWMNativeVHDL _ n e) (RWMNativeVHDL _ n' e')   = (&&) <$> return (n == n') <*> aeq' e e'
      aeq' (RWMError _ m _) (RWMError _ m' _)              = return $ m == m'
      aeq' _ _                                             = return False

instance NFData RWMExp where
      rnf = \ case
            RWMApp _ e1 e2      -> e1 `deepseq` e2 `deepseq` ()
            RWMLam _ x t e      -> x  `deepseq` t  `deepseq` e  `deepseq` ()
            RWMVar _ x t        -> x  `deepseq` t  `deepseq` ()
            RWMCon _ i t        -> i  `deepseq` t  `deepseq` ()
            RWMCase _ e p e1 e2 -> e  `deepseq` p  `deepseq` e1 `deepseq` e2 `deepseq` ()
            RWMNativeVHDL _ n e -> n  `deepseq` e  `deepseq` ()
            RWMError _ m t      -> m  `deepseq` t  `deepseq` ()

instance Pretty RWMExp where
      pretty = \ case
            RWMApp _ e1 e2      -> parens $ hang (pretty e1) 4 (pretty e2)
            RWMCon _ n _        -> text (deDataConId n)
            RWMVar _ n _        -> pretty n
            RWMLam _ n _ e      -> parens (char '\\' <+> pretty n <+> text "->" <+> pretty e)
            RWMCase _ e p e1 e2 -> parens $
                  foldr ($+$) mempty
                  [ text "case" <+> pretty e <+> text "of"
                  , nest 4 (braces $ vcat $ punctuate (space <> text ";" <> space)
                        [ parens (pretty p) <+> text "->" <+> pretty e1
                        , text "_" <+> text "->" <+> pretty e2
                        ])
                  ]
            RWMNativeVHDL _ n e -> parens (text "nativeVHDL" <+> doubleQuotes (text n) <+> parens (pretty e))
            RWMError _ m _      -> parens (text "primError" <+> doubleQuotes (text m))

---

data RWMPat = RWMPatCon Annote DataConId [RWMPat]
            | RWMPatVar Annote (Id RWMExp) RWCTy
            deriving (Ord, Eq, Show, Typeable, Data)

instance Annotated RWMPat where
      ann = \ case
            RWMPatCon a _ _   -> a
            RWMPatVar a _ _   -> a

patvars :: RWMPat -> [Id RWMExp]
patvars = \ case
      RWMPatCon _ _ ps  -> concatMap patvars ps
      RWMPatVar _ x _   -> [x]

equatingPats :: RWMPat -> RWMPat -> AlphaM Bool -> AlphaM Bool
equatingPats (RWMPatCon _ i ps) (RWMPatCon _ j ps') k
      | i == j    = equatingsPats ps ps' k
      | otherwise = return False
      where equatingsPats ps ps' k
                  | length ps /= length ps' = return False
                  | otherwise               = foldr (uncurry equatingPats) k (zip ps ps')
equatingPats (RWMPatVar _ x _) (RWMPatVar _ y _) k = equating x y k
equatingPats _ _ _                                 = return False

instance Subst RWMPat RWCTy where
      fv = \ case
            RWMPatCon _ _ ps  -> concatMap fv ps
            RWMPatVar _ _ t   -> fv t
      bv _ = []
      subst' = \ case
            RWMPatCon an i ps  -> RWMPatCon an i <$> subst' ps
            RWMPatVar an x t   -> RWMPatVar an x <$> subst' t

instance NFData RWMPat where
      rnf = \ case
            RWMPatCon _ i ps  -> i `deepseq` ps `deepseq` ()
            RWMPatVar _ x t   -> x `deepseq` t `deepseq` ()

instance Pretty RWMPat where
      pretty = \ case
            RWMPatCon _ n ps  -> parens $ text (deDataConId n) <+> hsep (map pretty ps)
            RWMPatVar _ n _   -> pretty n

---

data RWMDefn = RWMDefn
      { defnAnnote :: Annote
      , defnName   :: Id RWMExp
      , defnPolyTy :: Poly RWCTy
      , defnInline :: Bool
      , defnVars   :: [Id RWMExp]
      , defnBody   :: RWMExp
      } deriving (Ord, Eq, Show, Typeable, Data)

instance Annotated RWMDefn where
      ann (RWMDefn a _ _ _ _ _) = a

instance Subst RWMDefn RWMExp where
      fv (RWMDefn _ n _ _ vs e) = filter (not . (`elem` n : vs)) $ fv e
      bv (RWMDefn _ n _ _ vs e) = vs ++ n : bv e
      -- subst' (RWMDefn an n pt b vs e) = refresh n (fv e) $ \ n' -> do
      --       e' <- subst' e
      --       return $ RWMDefn an n' pt b e'

instance Subst RWMDefn RWCTy where
      fv (RWMDefn _ _ pt _ vs e) = fv pt ++ fv e
      bv (RWMDefn _ _ pt _ vs _) = bv pt
      -- subst' (RWMDefn an n (xs :-> t) b vs e) = refreshs xs (fv t ++ fv e) $ \ xs' -> do
      --       t' <- subst' t
      --       e' <- subst' e
      --       return $ RWMDefn an n (xs' :-> t') b e'

instance NFData RWMDefn where
      rnf (RWMDefn _ n pt b vs e) = n `deepseq` pt `deepseq` b `deepseq` vs `deepseq` e `deepseq` ()

instance Pretty RWMDefn where
      pretty (RWMDefn _ n (_ :-> ty) b vs e) = foldr ($+$) mempty
            (  [pretty n <+> text "::" <+> pretty ty]
            ++ (if b then [text "{-# INLINE" <+> pretty n <+> text "#-}"] else [])
            ++ [pretty n <+> hsep (map pretty vs) <+> text "=", nest 4 $ pretty e]
            )

---

data RWMData = RWMData
      { dataAnnote :: Annote
      , dataName   :: TyConId
      , dataTyVars :: [Id RWCTy]
      , dataKind   :: Kind
      , dataCons   :: [RWCDataCon]
      } deriving (Ord, Eq, Show, Typeable, Data)

instance Annotated RWMData where
      ann (RWMData a _ _ _ _) = a

instance NFData RWMData where
      rnf (RWMData _ i tvs k dcs) = i `deepseq` tvs `deepseq` dcs `deepseq` k `deepseq` ()

-- FIXME: just ignoring the kind here
instance Pretty RWMData where
      pretty (RWMData _ n tvs _ dcs) = foldr ($+$) mempty
            [ text "data" <+> text (deTyConId n) <+> hsep (map pretty tvs) <+> (if null (map pretty dcs) then mempty else char '=')
            , nest 4 (hsep (punctuate (char '|') $ map pretty dcs))
            ]

---

data RWMProgram = RWMProgram
      { dataDecls  :: [RWMData]
      , defns      :: [RWMDefn]
      } deriving (Ord, Eq, Show, Typeable, Data)

instance NFData RWMProgram where
      rnf (RWMProgram dds defs) = dds `deepseq` defs `deepseq` ()

instance Monoid RWMProgram where
      mempty = RWMProgram mempty mempty
      mappend (RWMProgram ts vs) (RWMProgram ts' vs') = RWMProgram (nub $ ts ++ ts') $ nub $ vs ++ vs'

instance Pretty RWMProgram where
      pretty p = ppDataDecls (dataDecls p) $+$ ppDefns (defns p)
            where ppDefns = foldr ($+$) mempty . map pretty
                  ppDataDecls = foldr ($+$) mempty . map pretty

---

flattenApp :: RWMExp -> [RWMExp]
flattenApp (RWMApp _ e e') = flattenApp e ++ [e']
flattenApp e               = [e]

typeOf :: RWMExp -> RWCTy
typeOf (RWMApp _ e _)        = arrowRight (typeOf e)
typeOf (RWMLam _ _ t e)      = mkArrow t (typeOf e)
typeOf (RWMVar _ _ t)        = t
typeOf (RWMCon _ _ t)        = t
typeOf (RWMCase _ _ _ e _)   = typeOf e
typeOf (RWMNativeVHDL _ _ e) = typeOf e
typeOf (RWMError _ _ t)      = t

-- Orphan instances for removing the NFData dependency from Core.Syntax.

deriving instance NFData DataConId
deriving instance NFData TyConId

instance NFData t => NFData (Poly t) where
      rnf (vs :-> x) = vs `deepseq` x `deepseq` ()

instance NFData RWCTy where
      rnf = \ case
            RWCTyApp _ t1 t2 -> t1 `deepseq` t2 `deepseq` ()
            RWCTyCon _ i     -> i  `deepseq` ()
            RWCTyVar _ x     -> x  `deepseq` ()
            RWCTyComp _ m t  -> m  `deepseq` t  `deepseq` ()

instance NFData RWCDataCon where
      rnf (RWCDataCon _ i ts) = i `deepseq` ts `deepseq` ()
