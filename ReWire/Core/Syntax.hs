{-# LANGUAGE MultiParamTypeClasses,GeneralizedNewtypeDeriving,FlexibleInstances,DeriveDataTypeable
      ,Rank2Types,GADTs,ScopedTypeVariables #-}

module ReWire.Core.Syntax
      ( Annote(..),toSrcSpanInfo,noAnn,unAnn,Annotation(..),Annotated(..)
      , DataConId(..),TyConId(..),Poly(..)
      , RWCTy(..)
      , RWCExp(..)
      , RWCLit(..)
      , RWCPat(..)
      , RWCDefn(..)
      , RWCData(..)
      , RWCDataCon(..)
      , RWCProgram(..)
      , flattenArrow,mkArrow,arrowLeft,arrowRight
      , flattenTyApp,flattenApp,typeOf
      ) where

import ReWire.Core.Kinds
import ReWire.Pretty
import ReWire.Scoping
import ReWire.SYB (runPureT,transform)

import Control.DeepSeq
import Control.Monad.State
import Control.Monad.Identity (Identity(..))
import Data.ByteString.Char8 (pack)
import Data.Data (Typeable,Data(..))
import Data.Foldable (Foldable)
import Data.List (nub)
--import Data.Map.Strict (Map)
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable)
import GHC.Generics (Generic)
import qualified Language.Haskell.Exts.Annotated as HS (Annotated(..))
import Language.Haskell.Exts.Annotated.ExactPrint (ExactP)
import qualified Language.Haskell.Exts.Pretty as HS (Pretty)
import Language.Haskell.Exts.SrcLoc (SrcLoc,SrcInfo(..),SrcSpanInfo(..),noLoc,noInfoSpan,mkSrcSpan)
--import qualified Data.Map.Strict as Map

import Text.PrettyPrint

data Annote where
  NoAnnote   :: Annote
  LocAnnote  :: SrcSpanInfo -> Annote
  AstAnnote  :: forall ast.
    ( Functor ast
    , Foldable ast
    , Traversable ast
    , HS.Annotated ast
    , ExactP ast
    , Eq (ast SrcSpanInfo)
    , Data (ast SrcSpanInfo)
    , Ord (ast SrcSpanInfo)
    , Show (ast SrcSpanInfo)
    , Generic (ast SrcSpanInfo)
    , HS.Pretty (ast SrcSpanInfo)
    ) => ast SrcSpanInfo -> Annote
  deriving Typeable

class Annotation a where
  toAnnote :: a -> Annote

instance Annotation Annote where
  toAnnote = id

instance Annotation SrcSpanInfo  where
  toAnnote = LocAnnote

instance Annotation SrcLoc where
  toAnnote l = fromSrcInfo $ noInfoSpan $ mkSrcSpan l l

class Annotated a where
 ann    :: a -> Annote

unAnn :: Data d => d -> d
unAnn = runIdentity . (runPureT $ transform $ \ (_::Annote) -> return noAnn)

instance SrcInfo Annote where
  toSrcInfo a b c = LocAnnote $ toSrcInfo a b c
  fromSrcInfo     = LocAnnote . fromSrcInfo
  fileName        = fileName . toSrcSpanInfo
  startLine       = startLine . toSrcSpanInfo
  startColumn     = startColumn . toSrcSpanInfo

-- | Using the default definition of gfoldl.
instance Data Annote where
  gunfold    = undefined
  toConstr   = undefined
  dataTypeOf = undefined

instance Show Annote where
  show NoAnnote      = "NoAnnote"
  show (LocAnnote l) = "LocAnnote (" ++ show l ++ ")"
  show (AstAnnote a) = "AstAnnote (" ++ show a ++ ")"

-- TODO(chathhorn): afraid of screwing stuff up if the annotation isn't
-- ignored.
instance Ord Annote where
  _ <= _ = True
instance Eq Annote where
  _ == _ = True

toSrcSpanInfo :: Annote -> SrcSpanInfo
toSrcSpanInfo NoAnnote      = fromSrcInfo $ noInfoSpan $ mkSrcSpan noLoc noLoc
toSrcSpanInfo (LocAnnote l) = l
toSrcSpanInfo (AstAnnote a) = HS.ann a

noAnn :: Annote
noAnn = NoAnnote

---

newtype DataConId = DataConId { deDataConId :: String } deriving (Eq,Ord,Show,Typeable,Data,NFData)
newtype TyConId   = TyConId   { deTyConId :: String } deriving (Eq,Ord,Show,Typeable,Data,NFData)

data Poly t = [Id t] :-> t
      deriving (Ord,Eq,Show,Typeable,Data)

infixr :->

instance NFData t => NFData (Poly t) where
  rnf (vs :-> x) = vs `deepseq` x `deepseq` ()

instance Subst t t => Subst (Poly t) t where
  fv (xs :-> t) = filter (not . (`elem` xs)) (fv t)
  bv (xs :-> t) = xs ++ bv t
  subst' (xs :-> t) = refreshs xs (fv t) $ \ xs' ->
                       do t' <- subst' t
                          return (xs' :-> t')

instance Alpha (Poly RWCTy) where
  aeq' (xs :-> t) (ys :-> u) = equatings xs ys (return False) (aeq' t u)

instance Pretty DataConId where
  pretty = text . deDataConId

instance Pretty TyConId where
  pretty = text . deTyConId

---

data RWCTy = RWCTyApp Annote RWCTy RWCTy
           | RWCTyCon Annote TyConId
           | RWCTyVar Annote (Id RWCTy)
           | RWCTyComp Annote RWCTy RWCTy -- application of a monad
           deriving (Ord,Eq,Show,Typeable,Data)

instance Annotated RWCTy where
  ann (RWCTyApp a _ _)  = a
  ann (RWCTyCon a _)    = a
  ann (RWCTyVar a _)    = a
  ann (RWCTyComp a _ _) = a

instance IdSort RWCTy where
  idSort _ = pack "T"

instance NFData RWCTy where
  rnf (RWCTyApp _ t1 t2) = t1 `deepseq` t2 `deepseq` ()
  rnf (RWCTyCon _ i)     = i `deepseq` ()
  rnf (RWCTyVar _ x)     = x `deepseq` ()
  rnf (RWCTyComp _ m t)  = m `deepseq` t `deepseq` ()

instance Subst RWCTy RWCTy where
  fv (RWCTyVar _ x)     = [x]
  fv (RWCTyCon _ _)     = []
  fv (RWCTyApp _ t1 t2) = fv t1 ++ fv t2
  fv (RWCTyComp _ m t)  = fv m ++ fv t
  bv _ = []
  subst' (RWCTyVar an x)  = do ml <- query x
                               case ml of
                                 Just (Left y)  -> return (RWCTyVar an y)
                                 Just (Right e) -> return e
                                 Nothing        -> return (RWCTyVar an x)
  subst' (RWCTyCon an i)     = return (RWCTyCon an i)
  subst' (RWCTyApp an t1 t2) = liftM2 (RWCTyApp an) (subst' t1) (subst' t2)
  subst' (RWCTyComp an m t)  = liftM2 (RWCTyComp an) (subst' m) (subst' t)

instance Alpha RWCTy where
  aeq' (RWCTyApp _ t1 t2) (RWCTyApp _ t1' t2') = liftM2 (&&) (aeq' t1 t1') (aeq' t2 t2')
  aeq' (RWCTyCon _ i) (RWCTyCon _ j)           = return (i==j)
  aeq' (RWCTyVar _ x) (RWCTyVar _ y)           = varsaeq x y
  aeq' _ _                                     = return False

instance Pretty RWCTy where
  pretty (RWCTyApp _ (RWCTyApp _ (RWCTyCon _ (TyConId "->")) t1) t2) = ppTyArrowL t1 <+> text "->" <+> pretty t2
    where ppTyArrowL t@(RWCTyApp _ (RWCTyApp _ (RWCTyCon _ (TyConId "->")) _) _) = parens $ pretty t
          ppTyArrowL t                                                           = pretty t
  pretty (RWCTyApp _ t1 t2)  = pretty t1 <+> ppTyAppR t2
  pretty (RWCTyCon _ n)      = text (deTyConId n)
  pretty (RWCTyVar _ n)      = pretty n
  pretty (RWCTyComp _ t1 t2) = text "{- computation -}" <+> pretty t1 <+> ppTyAppR t2

ppTyAppR :: RWCTy -> Doc
ppTyAppR t@RWCTyApp {} = parens $ pretty t
ppTyAppR t             = pretty t

---

data RWCExp = RWCApp Annote RWCExp RWCExp
            | RWCLam Annote (Id RWCExp) RWCTy RWCExp
            | RWCVar Annote (Id RWCExp) RWCTy
            | RWCCon Annote DataConId RWCTy
            | RWCLiteral Annote RWCLit
            | RWCCase Annote RWCExp RWCPat RWCExp RWCExp
            | RWCNativeVHDL Annote String RWCExp
            | RWCError Annote String RWCTy
            deriving (Ord,Eq,Show,Typeable,Data)

instance Annotated RWCExp where
  ann (RWCApp a _ _)        = a
  ann (RWCLam a _ _ _)      = a
  ann (RWCVar a _ _)        = a
  ann (RWCCon a _ _)        = a
  ann (RWCLiteral a _)      = a
  ann (RWCCase a _ _ _ _)   = a
  ann (RWCNativeVHDL a _ _) = a
  ann (RWCError a _ _)      = a

instance IdSort RWCExp where
  idSort _ = pack "E"

instance Subst RWCExp RWCExp where
  fv (RWCApp _ e1 e2)      = fv e1 ++ fv e2
  fv (RWCLam _ x _ e)      = filter (/= x) (fv e)
  fv (RWCVar _ x _)        = [x]
  fv (RWCCon _ _ _)        = []
  fv (RWCLiteral _ _)      = []
  fv (RWCCase _ e p e1 e2) = fv e ++ filter (not . (`elem` patvars p)) (fv e1) ++ fv e2
  fv (RWCNativeVHDL _ _ e) = fv e
  fv (RWCError _ _ _)      = []
  bv (RWCApp _ e1 e2)      = bv e1 ++ bv e2
  bv (RWCLam _ x _ e)      = x : bv e
  bv (RWCVar _ _ _)        = []
  bv (RWCCon _ _ _)        = []
  bv (RWCLiteral _ _)      = []
  bv (RWCCase _ e p e1 e2) = bv e ++ patvars p ++ bv e1 ++ bv e2
  bv (RWCNativeVHDL _ _ e) = bv e
  bv (RWCError _ _ _)      = []
  subst' (RWCApp an e1 e2)      = liftM2 (RWCApp an) (subst' e1) (subst' e2)
  subst' (RWCLam an x t e)      = refresh x (fv e) $ \ x' ->
                                    do e' <- subst' e
                                       return (RWCLam an x' t e')
  subst' (RWCVar an x t)        = do ml <- query x
                                     case ml of
                                       Just (Left y)  -> return (RWCVar an y t)
                                       Just (Right e) -> return e
                                       Nothing        -> return (RWCVar an x t)
  subst' (RWCCon an i t)        = return (RWCCon an i t)
  subst' (RWCLiteral an l)      = return (RWCLiteral an l)
  subst' (RWCCase an e p e1 e2) = liftM4 (RWCCase an) (subst' e) (return p) (subst' e1) (subst' e2)
  subst' (RWCNativeVHDL an n e) = liftM (RWCNativeVHDL an n) (subst' e)
  subst' (RWCError an m t)      = return (RWCError an m t)

instance Subst RWCExp RWCTy where
  fv (RWCApp _ e1 e2)      = fv e1 ++ fv e2
  fv (RWCLam _ _ t e)      = fv t ++ fv e
  fv (RWCVar _ _ t)        = fv t
  fv (RWCCon _ _ t)        = fv t
  fv (RWCLiteral _ _)      = []
  fv (RWCCase _ e p e1 e2) = fv e ++ fv p ++ fv e1 ++ fv e2
  fv (RWCNativeVHDL _ _ e) = fv e
  fv (RWCError _ _ t)      = fv t
  bv _ = []
  subst' (RWCApp an e1 e2)      = liftM2 (RWCApp an) (subst' e1) (subst' e2)
  subst' (RWCLam an x t e)      = liftM2 (RWCLam an x) (subst' t) (subst' e)
  subst' (RWCVar an x t)        = liftM (RWCVar an x) (subst' t)
  subst' (RWCCon an i t)        = liftM (RWCCon an i) (subst' t)
  subst' (RWCLiteral an l)      = return (RWCLiteral an l)
  subst' (RWCCase an e p e1 e2) = liftM4 (RWCCase an) (subst' e) (subst' p) (subst' e1) (subst' e2)
  subst' (RWCNativeVHDL an n e) = liftM (RWCNativeVHDL an n) (subst' e)
  subst' (RWCError an m t)      = liftM (RWCError an m) (subst' t)

instance Alpha RWCExp where
  aeq' (RWCApp _ e1 e2) (RWCApp _ e1' e2')             = liftM2 (&&) (aeq' e1 e1') (aeq' e2 e2')
  aeq' (RWCLam _ x t e) (RWCLam _ x' t' e')            = equating x x' $
                                                           liftM2 (&&) (aeq' t t') (aeq' e e')
  aeq' (RWCVar _ x _) (RWCVar _ y _)                   = varsaeq x y
  aeq' (RWCCon _ i _) (RWCCon _ j _)                   = return (i==j)
  aeq' (RWCLiteral _ l) (RWCLiteral _ l')              = return (l==l')
  aeq' (RWCCase _ e p e1 e2) (RWCCase _ e' p' e1' e2') = liftM2 (&&) (aeq' e e') (liftM2 (&&) (equatingPats p p' (aeq' e1 e1')) (aeq' e2 e2'))
  aeq' (RWCNativeVHDL _ n e) (RWCNativeVHDL _ n' e')   = liftM2 (&&) (return (n==n')) (aeq' e e')
  aeq' (RWCError _ m _) (RWCError _ m' _)              = return (m == m')
  aeq' _ _                                             = return False

instance NFData RWCExp where
  rnf (RWCApp _ e1 e2)      = e1 `deepseq` e2 `deepseq` ()
  rnf (RWCLam _ x t e)      = x `deepseq` t `deepseq` e `deepseq` ()
  rnf (RWCVar _ x t)        = x `deepseq` t `deepseq` ()
  rnf (RWCCon _ i t)        = i `deepseq` t `deepseq` ()
  rnf (RWCLiteral _ l)      = l `deepseq` ()
  rnf (RWCCase _ e p e1 e2) = e `deepseq` p `deepseq` e1 `deepseq` e2 `deepseq` ()
  rnf (RWCNativeVHDL _ n e) = n `deepseq` e `deepseq` ()
  rnf (RWCError _ m t)      = m `deepseq` t `deepseq` ()

instance Pretty RWCExp where
  pretty (RWCApp _ e1 e2)      = parens $ hang (pretty e1) 4 (pretty e2)
  pretty (RWCLiteral _ l)      = pretty l
  pretty (RWCCon _ n _)        = text (deDataConId n)
  pretty (RWCVar _ n _)        = pretty n
  pretty (RWCLam _ n _ e)      = parens (char '\\' <+> pretty n <+> text "->" <+> pretty e)
  pretty (RWCCase _ e p e1 e2) = parens $
                                 foldr ($+$) empty
                                   [ text "case" <+> pretty e <+> text "of"
                                   , nest 4 (braces $ vcat $ punctuate (space <> text ";" <> space)
                                     [ parens (pretty p) <+> text "->" <+> pretty e1
                                     , text "_" <+> text "->" <+> pretty e2
                                     ])
                                   ]
  pretty (RWCNativeVHDL _ n e) = parens (text "nativeVHDL" <+> doubleQuotes (text n) <+> parens (pretty e))
  pretty (RWCError _ m _)      = parens (text "primError" <+> doubleQuotes (text m))

---

data RWCLit = RWCLitInteger Integer
            | RWCLitFloat Double
            | RWCLitChar Char
            deriving (Ord,Eq,Show,Typeable,Data)

instance NFData RWCLit where
  rnf (RWCLitInteger i) = i `deepseq` ()
  rnf (RWCLitFloat d)   = d `deepseq` ()
  rnf (RWCLitChar c)    = c `deepseq` ()

instance Pretty RWCLit where
  pretty (RWCLitInteger n) = integer n
  pretty (RWCLitFloat x)   = double x
  pretty (RWCLitChar c)    = text (show c)

---

data RWCPat = RWCPatCon Annote DataConId [RWCPat]
            | RWCPatLiteral Annote RWCLit
            | RWCPatVar Annote (Id RWCExp) RWCTy
            deriving (Ord,Eq,Show,Typeable,Data)

instance Annotated RWCPat where
  ann (RWCPatCon a _ _)   = a
  ann (RWCPatLiteral a _) = a
  ann (RWCPatVar a _ _)   = a

patvars :: RWCPat -> [Id RWCExp]
patvars (RWCPatCon _ _ ps)  = concatMap patvars ps
patvars (RWCPatLiteral _ _) = []
patvars (RWCPatVar _ x _)   = [x]

equatingPats :: RWCPat -> RWCPat -> AlphaM Bool -> AlphaM Bool
equatingPats (RWCPatCon _ i ps) (RWCPatCon _ j ps') k
  | i == j    = equatingsPats ps ps' k
  | otherwise = return False
     where equatingsPats ps ps' k | length ps /= length ps' = return False
                                  | otherwise               = foldr (uncurry equatingPats) k (zip ps ps')
equatingPats (RWCPatLiteral _ l) (RWCPatLiteral _ l') k | l == l'   = k
                                                        | otherwise = return False
equatingPats (RWCPatVar _ x _) (RWCPatVar _ y _) k                  = equating x y k
equatingPats _ _ _                                                  = return False

instance Subst RWCPat RWCTy where
  fv (RWCPatCon _ _ ps)  = concatMap fv ps
  fv (RWCPatLiteral _ _) = []
  fv (RWCPatVar _ _ t)   = fv t
  bv _ = []
  subst' (RWCPatCon an i ps)  = liftM (RWCPatCon an i) (subst' ps)
  subst' (RWCPatLiteral an l) = return (RWCPatLiteral an l)
  subst' (RWCPatVar an x t)   = liftM (RWCPatVar an x) (subst' t)

instance NFData RWCPat where
  rnf (RWCPatCon _ i ps)  = i `deepseq` ps `deepseq` ()
  rnf (RWCPatLiteral _ l) = l `deepseq` ()
  rnf (RWCPatVar _ x t)   = x `deepseq` t `deepseq` ()

instance Pretty RWCPat where
  pretty (RWCPatCon _ n ps)        = parens (text (deDataConId n) <+> hsep (map pretty ps))
  pretty (RWCPatVar _ n _)         = pretty n
  pretty (RWCPatLiteral _ l)       = pretty l

---

data RWCDefn = RWCDefn { defnAnnote :: Annote,
                         defnName   :: Id RWCExp,
                         defnPolyTy :: Poly RWCTy,
                         defnInline :: Bool,
                         defnBody   :: RWCExp }
               deriving (Ord,Eq,Show,Typeable,Data)

instance Annotated RWCDefn where
  ann (RWCDefn a _ _ _ _) = a

instance Subst RWCDefn RWCExp where
  fv (RWCDefn _ n _ _ e) = filter (/= n) (fv e)
  bv (RWCDefn _ n _ _ e) = n : bv e
  subst' (RWCDefn an n pt b e) = refresh n (fv e) $ \ n' ->
                                 do e' <- subst' e
                                    return (RWCDefn an n' pt b e')

instance Subst RWCDefn RWCTy where
  fv (RWCDefn _ _ pt _ e) = fv pt ++ fv e
  bv (RWCDefn _ _ pt _ _) = bv pt
  subst' (RWCDefn an n (xs :-> t) b e) = refreshs xs (fv t ++ fv e) $ \ xs' ->
                                         do t' <- subst' t
                                            e' <- subst' e
                                            return (RWCDefn an n (xs' :-> t') b e')

instance NFData RWCDefn where
  rnf (RWCDefn _ n pt b e) = n `deepseq` pt `deepseq` b `deepseq` e `deepseq` ()

instance Pretty RWCDefn where
  pretty (RWCDefn _ n (_ :-> ty) b e) = foldr ($+$) empty
                                          (  [pretty n <+> text "::" <+> pretty ty]
                                          ++ (if b then [text "{-# INLINE" <+> pretty n <+> text "#-}"] else [])
                                          ++ [pretty n <+> text "=", nest 4 $ pretty e])

---

data RWCData = RWCData { dataAnnote :: Annote,
                         dataName   :: TyConId,
                         dataTyVars :: [Id RWCTy],
                         dataKind   :: Kind,
                         dataCons   :: [RWCDataCon] }
               deriving (Ord,Eq,Show,Typeable,Data)

instance Annotated RWCData where
  ann (RWCData a _ _ _ _) = a

instance NFData RWCData where
  rnf (RWCData _ i tvs k dcs) = i `deepseq` tvs `deepseq` dcs `deepseq` k `deepseq` ()

-- FIXME: just ignoring the kind here
instance Pretty RWCData where
  pretty (RWCData _ n tvs _ dcs) = foldr ($+$) empty
                                     [text "data" <+> text (deTyConId n) <+> hsep (map pretty tvs) <+> (if null (map pretty dcs) then empty else char '='),
                                     nest 4 (hsep (punctuate (char '|') $ map pretty dcs))]

---

data RWCDataCon = RWCDataCon Annote DataConId [RWCTy]
                  deriving (Ord,Eq,Show,Typeable,Data)

instance Annotated RWCDataCon where
  ann (RWCDataCon a _ _) = a

instance NFData RWCDataCon where
  rnf (RWCDataCon _ i ts) = i `deepseq` ts `deepseq` ()

instance Pretty RWCDataCon where
  pretty (RWCDataCon _ n ts) = text (deDataConId n) <+> hsep (map pretty ts)

---

data RWCProgram = RWCProgram { dataDecls  :: [RWCData],
                               defns      :: [RWCDefn] }
                  deriving (Ord,Eq,Show,Typeable,Data)

instance NFData RWCProgram where
  rnf (RWCProgram dds defs) = dds `deepseq` defs `deepseq` ()

instance Monoid RWCProgram where
  mempty = RWCProgram mempty mempty
  mappend (RWCProgram ts vs) (RWCProgram ts' vs') = RWCProgram (nub $ ts ++ ts') $ nub $ vs ++ vs'

instance Pretty RWCProgram where
  pretty p = ppDataDecls (dataDecls p) $+$ ppDefns (defns p)
    where ppDefns = foldr ($+$) empty . map pretty
          ppDataDecls = foldr ($+$) empty . map pretty

---

flattenArrow :: RWCTy -> ([RWCTy],RWCTy)
flattenArrow (RWCTyApp _ (RWCTyApp _ (RWCTyCon _ (TyConId "->")) t1) t2) = let (ts,t) = flattenArrow t2 in (t1:ts,t)
flattenArrow t                                                           = ([],t)

flattenTyApp :: RWCTy -> [RWCTy]
flattenTyApp (RWCTyApp _ t1 t2) = flattenTyApp t1 ++ [t2]
flattenTyApp t                  = [t]

flattenApp :: RWCExp -> [RWCExp]
flattenApp (RWCApp _ e e') = flattenApp e++[e']
flattenApp e               = [e]

mkArrow :: RWCTy -> RWCTy -> RWCTy
mkArrow t = RWCTyApp noAnn (RWCTyApp noAnn (RWCTyCon noAnn (TyConId "->")) t)

infixr `mkArrow`

arrowLeft :: RWCTy -> RWCTy
arrowLeft (RWCTyApp _ (RWCTyApp _ (RWCTyCon _ (TyConId "->")) t1) _) = t1
arrowLeft t                                                          = error $ "arrowLeft: got non-arrow type: " ++ show t

arrowRight :: RWCTy -> RWCTy
arrowRight (RWCTyApp _ (RWCTyApp _ (RWCTyCon _ (TyConId "->")) _) t2) = t2
arrowRight t                                                          = error $ "arrowRight: got non-arrow type: " ++ show t

typeOf :: RWCExp -> RWCTy
typeOf (RWCApp _ e _)                    = arrowRight (typeOf e)
typeOf (RWCLam _ _ t e)                  = mkArrow t (typeOf e)
typeOf (RWCVar _ _ t)                    = t
typeOf (RWCCon _ _ t)                    = t
typeOf (RWCLiteral _ (RWCLitInteger _))  = RWCTyCon noAnn (TyConId "Integer")
typeOf (RWCLiteral _ (RWCLitFloat _))    = RWCTyCon noAnn (TyConId "Float")
typeOf (RWCLiteral _ (RWCLitChar _))     = RWCTyCon noAnn (TyConId "Char")
typeOf (RWCCase _ _ _ e _)               = typeOf e
typeOf (RWCNativeVHDL _ _ e)             = typeOf e
typeOf (RWCError _ _ t)                  = t
