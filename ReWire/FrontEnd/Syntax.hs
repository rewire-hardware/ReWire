{-# LANGUAGE MultiParamTypeClasses
      , GeneralizedNewtypeDeriving
      , FlexibleInstances
      , DeriveDataTypeable
      , Rank2Types
      , GADTs
      , ScopedTypeVariables
      #-}

module ReWire.FrontEnd.Syntax
  ( DataConId(..),TyConId(..),Poly(..)
  , RWMExp(..)
  , RWCLit(..)
  , RWMPat(..)
  , RWMDefn(..)
  , RWMData(..)
  , RWMProgram(..)
  , flattenApp
  , module C
  ) where

import ReWire.FrontEnd.Kinds
import ReWire.Pretty
import ReWire.Scoping
import ReWire.Core.Syntax as C
      ( Annotated(..)
      , Annote(..)
      , Annotation(..)
      , DataConId(..)
      , TyConId(..)
      , RWCTy(..)
      , RWCLit(..)
      , RWCDataCon(..)
      , Poly(..)
      , noAnn
      , flattenArrow
      , flattenTyApp
      , mkArrow
      , arrowLeft
      , arrowRight
      )

import Control.DeepSeq
import Control.Monad.State
import Data.ByteString.Char8 (pack)
import Data.Data (Typeable,Data(..))
import Data.List (nub)
import Data.Monoid (Monoid(..))

import Text.PrettyPrint

data RWMExp = RWMApp Annote RWMExp RWMExp
            | RWMLam Annote (Id RWMExp) RWCTy RWMExp
            | RWMVar Annote (Id RWMExp) RWCTy
            | RWMCon Annote DataConId RWCTy
            | RWMLiteral Annote RWCLit
            | RWMCase Annote RWMExp RWMPat RWMExp RWMExp
            | RWMNativeVHDL Annote String RWMExp
            | RWMError Annote String RWCTy
            deriving (Ord,Eq,Show,Typeable,Data)

instance Annotated RWMExp where
  ann (RWMApp a _ _)        = a
  ann (RWMLam a _ _ _)      = a
  ann (RWMVar a _ _)        = a
  ann (RWMCon a _ _)        = a
  ann (RWMLiteral a _)      = a
  ann (RWMCase a _ _ _ _)   = a
  ann (RWMNativeVHDL a _ _) = a
  ann (RWMError a _ _)      = a

instance IdSort RWMExp where
  idSort _ = pack "E"

instance Subst RWMExp RWMExp where
  fv (RWMApp _ e1 e2)      = fv e1 ++ fv e2
  fv (RWMLam _ x _ e)      = filter (/= x) (fv e)
  fv (RWMVar _ x _)        = [x]
  fv (RWMCon _ _ _)        = []
  fv (RWMLiteral _ _)      = []
  fv (RWMCase _ e p e1 e2) = fv e ++ filter (not . (`elem` patvars p)) (fv e1) ++ fv e2
  fv (RWMNativeVHDL _ _ e) = fv e
  fv (RWMError _ _ _)      = []
  bv (RWMApp _ e1 e2)      = bv e1 ++ bv e2
  bv (RWMLam _ x _ e)      = x : bv e
  bv (RWMVar _ _ _)        = []
  bv (RWMCon _ _ _)        = []
  bv (RWMLiteral _ _)      = []
  bv (RWMCase _ e p e1 e2) = bv e ++ patvars p ++ bv e1 ++ bv e2
  bv (RWMNativeVHDL _ _ e) = bv e
  bv (RWMError _ _ _)      = []
  subst' (RWMApp an e1 e2)      = liftM2 (RWMApp an) (subst' e1) (subst' e2)
  subst' (RWMLam an x t e)      = refresh x (fv e) $ \ x' ->
                                    do e' <- subst' e
                                       return (RWMLam an x' t e')
  subst' (RWMVar an x t)        = do ml <- query x
                                     case ml of
                                       Just (Left y)  -> return (RWMVar an y t)
                                       Just (Right e) -> return e
                                       Nothing        -> return (RWMVar an x t)
  subst' (RWMCon an i t)        = return (RWMCon an i t)
  subst' (RWMLiteral an l)      = return (RWMLiteral an l)
  subst' (RWMCase an e p e1 e2) = liftM4 (RWMCase an) (subst' e) (return p) (subst' e1) (subst' e2)
  subst' (RWMNativeVHDL an n e) = liftM (RWMNativeVHDL an n) (subst' e)
  subst' (RWMError an m t)      = return (RWMError an m t)

instance Subst RWMExp RWCTy where
  fv (RWMApp _ e1 e2)      = fv e1 ++ fv e2
  fv (RWMLam _ _ t e)      = fv t ++ fv e
  fv (RWMVar _ _ t)        = fv t
  fv (RWMCon _ _ t)        = fv t
  fv (RWMLiteral _ _)      = []
  fv (RWMCase _ e p e1 e2) = fv e ++ fv p ++ fv e1 ++ fv e2
  fv (RWMNativeVHDL _ _ e) = fv e
  fv (RWMError _ _ t)      = fv t
  bv _ = []
  subst' (RWMApp an e1 e2)      = liftM2 (RWMApp an) (subst' e1) (subst' e2)
  subst' (RWMLam an x t e)      = liftM2 (RWMLam an x) (subst' t) (subst' e)
  subst' (RWMVar an x t)        = liftM (RWMVar an x) (subst' t)
  subst' (RWMCon an i t)        = liftM (RWMCon an i) (subst' t)
  subst' (RWMLiteral an l)      = return (RWMLiteral an l)
  subst' (RWMCase an e p e1 e2) = liftM4 (RWMCase an) (subst' e) (subst' p) (subst' e1) (subst' e2)
  subst' (RWMNativeVHDL an n e) = liftM (RWMNativeVHDL an n) (subst' e)
  subst' (RWMError an m t)      = liftM (RWMError an m) (subst' t)

instance Alpha RWMExp where
  aeq' (RWMApp _ e1 e2) (RWMApp _ e1' e2')             = liftM2 (&&) (aeq' e1 e1') (aeq' e2 e2')
  aeq' (RWMLam _ x t e) (RWMLam _ x' t' e')            = equating x x' $
                                                           liftM2 (&&) (aeq' t t') (aeq' e e')
  aeq' (RWMVar _ x _) (RWMVar _ y _)                   = varsaeq x y
  aeq' (RWMCon _ i _) (RWMCon _ j _)                   = return (i==j)
  aeq' (RWMLiteral _ l) (RWMLiteral _ l')              = return (l==l')
  aeq' (RWMCase _ e p e1 e2) (RWMCase _ e' p' e1' e2') = liftM2 (&&) (aeq' e e') (liftM2 (&&) (equatingPats p p' (aeq' e1 e1')) (aeq' e2 e2'))
  aeq' (RWMNativeVHDL _ n e) (RWMNativeVHDL _ n' e')   = liftM2 (&&) (return (n==n')) (aeq' e e')
  aeq' (RWMError _ m _) (RWMError _ m' _)              = return (m == m')
  aeq' _ _                                             = return False

instance NFData RWMExp where
  rnf (RWMApp _ e1 e2)      = e1 `deepseq` e2 `deepseq` ()
  rnf (RWMLam _ x t e)      = x `deepseq` t `deepseq` e `deepseq` ()
  rnf (RWMVar _ x t)        = x `deepseq` t `deepseq` ()
  rnf (RWMCon _ i t)        = i `deepseq` t `deepseq` ()
  rnf (RWMLiteral _ l)      = l `deepseq` ()
  rnf (RWMCase _ e p e1 e2) = e `deepseq` p `deepseq` e1 `deepseq` e2 `deepseq` ()
  rnf (RWMNativeVHDL _ n e) = n `deepseq` e `deepseq` ()
  rnf (RWMError _ m t)      = m `deepseq` t `deepseq` ()

instance Pretty RWMExp where
  pretty (RWMApp _ e1 e2)      = parens $ hang (pretty e1) 4 (pretty e2)
  pretty (RWMLiteral _ l)      = pretty l
  pretty (RWMCon _ n _)        = text (deDataConId n)
  pretty (RWMVar _ n _)        = pretty n
  pretty (RWMLam _ n _ e)      = parens (char '\\' <+> pretty n <+> text "->" <+> pretty e)
  pretty (RWMCase _ e p e1 e2) = parens $
                                 foldr ($+$) empty
                                   [ text "case" <+> pretty e <+> text "of"
                                   , nest 4 (braces $ vcat $ punctuate (space <> text ";" <> space)
                                     [ parens (pretty p) <+> text "->" <+> pretty e1
                                     , text "_" <+> text "->" <+> pretty e2
                                     ])
                                   ]
  pretty (RWMNativeVHDL _ n e) = parens (text "nativeVHDL" <+> doubleQuotes (text n) <+> parens (pretty e))
  pretty (RWMError _ m _)      = parens (text "primError" <+> doubleQuotes (text m))

---

data RWMPat = RWMPatCon Annote DataConId [RWMPat]
            | RWMPatLiteral Annote RWCLit
            | RWMPatVar Annote (Id RWMExp) RWCTy
            deriving (Ord,Eq,Show,Typeable,Data)

instance Annotated RWMPat where
  ann (RWMPatCon a _ _)   = a
  ann (RWMPatLiteral a _) = a
  ann (RWMPatVar a _ _)   = a

patvars :: RWMPat -> [Id RWMExp]
patvars (RWMPatCon _ _ ps)  = concatMap patvars ps
patvars (RWMPatLiteral _ _) = []
patvars (RWMPatVar _ x _)   = [x]

equatingPats :: RWMPat -> RWMPat -> AlphaM Bool -> AlphaM Bool
equatingPats (RWMPatCon _ i ps) (RWMPatCon _ j ps') k
  | i == j    = equatingsPats ps ps' k
  | otherwise = return False
     where equatingsPats ps ps' k | length ps /= length ps' = return False
                                  | otherwise               = foldr (uncurry equatingPats) k (zip ps ps')
equatingPats (RWMPatLiteral _ l) (RWMPatLiteral _ l') k | l == l'   = k
                                                        | otherwise = return False
equatingPats (RWMPatVar _ x _) (RWMPatVar _ y _) k                  = equating x y k
equatingPats _ _ _                                                  = return False

instance Subst RWMPat RWCTy where
  fv (RWMPatCon _ _ ps)  = concatMap fv ps
  fv (RWMPatLiteral _ _) = []
  fv (RWMPatVar _ _ t)   = fv t
  bv _ = []
  subst' (RWMPatCon an i ps)  = liftM (RWMPatCon an i) (subst' ps)
  subst' (RWMPatLiteral an l) = return (RWMPatLiteral an l)
  subst' (RWMPatVar an x t)   = liftM (RWMPatVar an x) (subst' t)

instance NFData RWMPat where
  rnf (RWMPatCon _ i ps)  = i `deepseq` ps `deepseq` ()
  rnf (RWMPatLiteral _ l) = l `deepseq` ()
  rnf (RWMPatVar _ x t)   = x `deepseq` t `deepseq` ()

instance Pretty RWMPat where
  pretty (RWMPatCon _ n ps)        = parens (text (deDataConId n) <+> hsep (map pretty ps))
  pretty (RWMPatVar _ n _)         = pretty n
  pretty (RWMPatLiteral _ l)       = pretty l

---

data RWMDefn = RWMDefn { defnAnnote :: Annote,
                         defnName   :: Id RWMExp,
                         defnPolyTy :: Poly RWCTy,
                         defnInline :: Bool,
                         defnBody   :: RWMExp }
               deriving (Ord,Eq,Show,Typeable,Data)

instance Annotated RWMDefn where
  ann (RWMDefn a _ _ _ _) = a

instance Subst RWMDefn RWMExp where
  fv (RWMDefn _ n _ _ e) = filter (/= n) (fv e)
  bv (RWMDefn _ n _ _ e) = n : bv e
  subst' (RWMDefn an n pt b e) = refresh n (fv e) $ \ n' ->
                                 do e' <- subst' e
                                    return (RWMDefn an n' pt b e')

instance Subst RWMDefn RWCTy where
  fv (RWMDefn _ _ pt _ e) = fv pt ++ fv e
  bv (RWMDefn _ _ pt _ _) = bv pt
  subst' (RWMDefn an n (xs :-> t) b e) = refreshs xs (fv t ++ fv e) $ \ xs' ->
                                         do t' <- subst' t
                                            e' <- subst' e
                                            return (RWMDefn an n (xs' :-> t') b e')

instance NFData RWMDefn where
  rnf (RWMDefn _ n pt b e) = n `deepseq` pt `deepseq` b `deepseq` e `deepseq` ()

instance Pretty RWMDefn where
  pretty (RWMDefn _ n (_ :-> ty) b e) = foldr ($+$) empty
                                          (  [pretty n <+> text "::" <+> pretty ty]
                                          ++ (if b then [text "{-# INLINE" <+> pretty n <+> text "#-}"] else [])
                                          ++ [pretty n <+> text "=", nest 4 $ pretty e])

---

data RWMData = RWMData { dataAnnote :: Annote,
                         dataName   :: TyConId,
                         dataTyVars :: [Id RWCTy],
                         dataKind   :: Kind,
                         dataCons   :: [RWCDataCon] }
               deriving (Ord,Eq,Show,Typeable,Data)

instance Annotated RWMData where
  ann (RWMData a _ _ _ _) = a

instance NFData RWMData where
  rnf (RWMData _ i tvs k dcs) = i `deepseq` tvs `deepseq` dcs `deepseq` k `deepseq` ()

-- FIXME: just ignoring the kind here
instance Pretty RWMData where
  pretty (RWMData _ n tvs _ dcs) = foldr ($+$) empty
                                     [text "data" <+> text (deTyConId n) <+> hsep (map pretty tvs) <+> (if null (map pretty dcs) then empty else char '='),
                                     nest 4 (hsep (punctuate (char '|') $ map pretty dcs))]

---

data RWMProgram = RWMProgram { dataDecls  :: [RWMData],
                               defns      :: [RWMDefn] }
                  deriving (Ord,Eq,Show,Typeable,Data)

instance NFData RWMProgram where
  rnf (RWMProgram dds defs) = dds `deepseq` defs `deepseq` ()

instance Monoid RWMProgram where
  mempty = RWMProgram mempty mempty
  mappend (RWMProgram ts vs) (RWMProgram ts' vs') = RWMProgram (nub $ ts ++ ts') $ nub $ vs ++ vs'

instance Pretty RWMProgram where
  pretty p = ppDataDecls (dataDecls p) $+$ ppDefns (defns p)
    where ppDefns = foldr ($+$) empty . map pretty
          ppDataDecls = foldr ($+$) empty . map pretty

---

flattenApp :: RWMExp -> [RWMExp]
flattenApp (RWMApp _ e e') = flattenApp e++[e']
flattenApp e               = [e]
