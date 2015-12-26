{-# LANGUAGE MultiParamTypeClasses,GeneralizedNewtypeDeriving,FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Syntax where

import ReWire.Core.Kinds
import ReWire.Scoping

import Control.DeepSeq
import Control.Monad.State
import Data.ByteString.Char8 (pack)
import Data.Map.Strict (Map)
import Data.Monoid (Monoid(..))
import Data.Set hiding (map,filter,foldr)
import qualified Data.Map.Strict as Map

newtype DataConId = DataConId { deDataConId :: String } deriving (Eq,Ord,Show,NFData)
newtype TyConId   = TyConId   { deTyConId :: String } deriving (Eq,Ord,Show,NFData)
newtype ModuleId  = ModuleId  { deModuleId :: String } deriving (Eq,Ord,Show,NFData)

data Poly t = [Id t] :-> t
      deriving (Ord,Eq,Show)

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

---

data RWCTy = RWCTyApp RWCTy RWCTy
           | RWCTyCon TyConId
           | RWCTyVar (Id RWCTy)
           | RWCTyComp RWCTy RWCTy -- application of a monad
           deriving (Ord,Eq,Show)

instance IdSort RWCTy where
  idSort _ = pack "T"

instance NFData RWCTy where
  rnf (RWCTyApp t1 t2) = t1 `deepseq` t2 `deepseq` ()
  rnf (RWCTyCon i)     = i `deepseq` ()
  rnf (RWCTyVar x)     = x `deepseq` ()
  rnf (RWCTyComp m t)  = m `deepseq` t `deepseq` ()

instance Subst RWCTy RWCTy where
  fv (RWCTyVar x)     = [x]
  fv (RWCTyCon i)     = []
  fv (RWCTyApp t1 t2) = fv t1 ++ fv t2
  fv (RWCTyComp m t)  = fv m ++ fv t
  bv _ = []
  subst' (RWCTyVar x)     = do ml <- query x
                               case ml of
                                 Just (Left y)  -> return (RWCTyVar y)
                                 Just (Right e) -> return e
                                 Nothing        -> return (RWCTyVar x)
  subst' (RWCTyCon i)     = return (RWCTyCon i)
  subst' (RWCTyApp t1 t2) = liftM2 RWCTyApp (subst' t1) (subst' t2)
  subst' (RWCTyComp m t)  = liftM2 RWCTyComp (subst' m) (subst' t)

instance Alpha RWCTy where
  aeq' (RWCTyApp t1 t2) (RWCTyApp t1' t2') = liftM2 (&&) (aeq' t1 t1') (aeq' t2 t2')
  aeq' (RWCTyCon i) (RWCTyCon j)           = return (i==j)
  aeq' (RWCTyVar x) (RWCTyVar y)           = varsaeq x y
  aeq' _ _                                 = return False

---

data RWCExp = RWCApp RWCExp RWCExp
            | RWCLam (Id RWCExp) RWCTy RWCExp
            | RWCVar (Id RWCExp) RWCTy
            | RWCCon DataConId RWCTy
            | RWCLiteral RWCLit
            | RWCCase RWCExp [RWCAlt]
            | RWCNativeVHDL String RWCExp
            deriving (Ord,Eq,Show)

instance IdSort RWCExp where
  idSort _ = pack "E"

instance Subst RWCExp RWCExp where
  fv (RWCApp e1 e2)      = fv e1 ++ fv e2
  fv (RWCLam x _ e)      = filter (/= x) (fv e)
  fv (RWCVar x _)        = [x]
  fv (RWCCon _ _)        = []
  fv (RWCLiteral l)      = []
  fv (RWCCase e alts)    = fv e ++ concatMap fv alts
  fv (RWCNativeVHDL _ e) = fv e
  bv (RWCApp e1 e2)      = bv e1 ++ bv e2
  bv (RWCLam x _ e)      = x : bv e
  bv (RWCVar _ _)        = []
  bv (RWCCon _ _)        = []
  bv (RWCLiteral _)      = []
  bv (RWCCase e alts)    = bv e ++ bv alts
  bv (RWCNativeVHDL _ e) = bv e
  subst' (RWCApp e1 e2)      = liftM2 RWCApp (subst' e1) (subst' e2)
  subst' (RWCLam x t e)      = refresh x (fv e) $ \ x' ->
                                 do e' <- subst' e
                                    return (RWCLam x' t e')
  subst' (RWCVar x t)        = do ml <- query x
                                  case ml of
                                    Just (Left y)  -> return (RWCVar y t)
                                    Just (Right e) -> return e
                                    Nothing        -> return (RWCVar x t)
  subst' (RWCCon i t)        = return (RWCCon i t)
  subst' (RWCLiteral l)      = return (RWCLiteral l)
  subst' (RWCCase e alts)    = liftM2 RWCCase (subst' e) (subst' alts)
  subst' (RWCNativeVHDL n e) = liftM (RWCNativeVHDL n) (subst' e)

instance Subst RWCExp RWCTy where
  fv (RWCApp e1 e2)      = fv e1 ++ fv e2
  fv (RWCLam _ t e)      = fv t ++ fv e
  fv (RWCVar _ t)        = fv t
  fv (RWCCon _ t)        = fv t
  fv (RWCLiteral _)      = []
  fv (RWCCase e alts)    = fv e ++ fv alts
  fv (RWCNativeVHDL n e) = fv e
  bv _ = []
  subst' (RWCApp e1 e2)      = liftM2 RWCApp (subst' e1) (subst' e2)
  subst' (RWCLam x t e)      = liftM2 (RWCLam x) (subst' t) (subst' e)
  subst' (RWCVar x t)        = liftM (RWCVar x) (subst' t)
  subst' (RWCCon i t)        = liftM (RWCCon i) (subst' t)
  subst' (RWCLiteral l)      = return (RWCLiteral l)
  subst' (RWCCase e alts)    = liftM2 RWCCase (subst' e) (subst' alts)
  subst' (RWCNativeVHDL n e) = liftM (RWCNativeVHDL n) (subst' e)

instance Alpha RWCExp where
  aeq' (RWCApp e1 e2) (RWCApp e1' e2')           = liftM2 (&&) (aeq' e1 e1') (aeq' e2 e2')
  aeq' (RWCLam x t e) (RWCLam x' t' e')          = equating x x' $
                                                     liftM2 (&&) (aeq' t t') (aeq' e e')
  aeq' (RWCVar x _) (RWCVar y _)                 = varsaeq x y
  aeq' (RWCCon i _) (RWCCon j _)                 = return (i==j)
  aeq' (RWCLiteral l) (RWCLiteral l')            = return (l==l')
  aeq' (RWCCase e alts) (RWCCase e' alts')       = liftM2 (&&) (aeq' e e') (aeq' alts alts')
  aeq' (RWCNativeVHDL n e) (RWCNativeVHDL n' e') = liftM2 (&&) (return (n==n')) (aeq' e e')
  aeq' _ _                                       = return False

instance NFData RWCExp where
  rnf (RWCApp e1 e2)      = e1 `deepseq` e2 `deepseq` ()
  rnf (RWCLam x t e)      = x `deepseq` t `deepseq` e `deepseq` ()
  rnf (RWCVar x t)        = x `deepseq` t `deepseq` ()
  rnf (RWCCon i t)        = i `deepseq` t `deepseq` ()
  rnf (RWCLiteral l)      = l `deepseq` ()
  rnf (RWCCase e alts)    = e `deepseq` alts `deepseq` ()
  rnf (RWCNativeVHDL n e) = n `deepseq` e `deepseq` ()

---

data RWCLit = RWCLitInteger Integer
            | RWCLitFloat Double
            | RWCLitChar Char
            deriving (Ord,Eq,Show)

instance NFData RWCLit where
  rnf (RWCLitInteger i) = i `deepseq` ()
  rnf (RWCLitFloat d)   = d `deepseq` ()
  rnf (RWCLitChar c)    = c `deepseq` ()

---

data RWCAlt = RWCAlt RWCPat RWCExp
              deriving (Ord,Eq,Show)

instance Subst RWCAlt RWCExp where
  fv (RWCAlt p e)     = filter (not . (`elem` patvars p)) (fv e)
  bv (RWCAlt p e)     = patvars p ++ bv e
  subst' (RWCAlt p e) = liftM (RWCAlt p) (subst' e)

instance Subst RWCAlt RWCTy where
  fv (RWCAlt p e) = fv p ++ fv e
  bv (RWCAlt p e) = []
  subst' (RWCAlt p e) = liftM2 RWCAlt (subst' p) (subst' e)

instance Alpha RWCAlt where
  aeq' (RWCAlt p e) (RWCAlt p' e') = equatingPats p p' (aeq' e e')

instance NFData RWCAlt where
  rnf (RWCAlt p e) = p `deepseq` e `deepseq` ()

---

data RWCPat = RWCPatCon DataConId [RWCPat]
            | RWCPatLiteral RWCLit
            | RWCPatVar (Id RWCExp) RWCTy
            deriving (Ord,Eq,Show)

patvars :: RWCPat -> [Id RWCExp]
patvars (RWCPatCon _ ps)  = concatMap patvars ps
patvars (RWCPatLiteral l) = []
patvars (RWCPatVar x _)   = [x]

equatingPats :: RWCPat -> RWCPat -> AlphaM Bool -> AlphaM Bool
equatingPats (RWCPatCon i ps) (RWCPatCon j ps') k
  | i == j    = equatingsPats ps ps' k
  | otherwise = return False
     where equatingsPats ps ps' k | length ps /= length ps' = return False
                                  | otherwise               = foldr (uncurry equatingPats) k (zip ps ps')
equatingPats (RWCPatLiteral l) (RWCPatLiteral l') k | l == l'   = k
                                                    | otherwise = return False
equatingPats (RWCPatVar x _) (RWCPatVar y _) k                  = equating x y k
equatingPats _ _ k                                              = return False

instance Subst RWCPat RWCTy where
  fv (RWCPatCon _ ps)  = concatMap fv ps
  fv (RWCPatLiteral l) = []
  fv (RWCPatVar _ t)   = fv t
  bv _ = []
  subst' (RWCPatCon i ps)  = liftM (RWCPatCon i) (subst' ps)
  subst' (RWCPatLiteral l) = return (RWCPatLiteral l)
  subst' (RWCPatVar x t)   = liftM (RWCPatVar x) (subst' t)

instance NFData RWCPat where
  rnf (RWCPatCon i ps)  = i `deepseq` ps `deepseq` ()
  rnf (RWCPatLiteral l) = l `deepseq` ()
  rnf (RWCPatVar x t)   = x `deepseq` t `deepseq` ()

---

data RWCDefn = RWCDefn { defnName   :: Id RWCExp,
                         defnPolyTy :: Poly RWCTy,
                         defnInline :: Bool,
                         defnBody   :: RWCExp }
               deriving (Ord,Eq,Show)

instance Subst RWCDefn RWCExp where
  fv (RWCDefn n pt _ e) = filter (/= n) (fv e)
  bv (RWCDefn n _ _ e) = n : bv e
  subst' (RWCDefn n pt b e) = refresh n (fv e) $ \ n' ->
                                do e' <- subst' e
                                   return (RWCDefn n' pt b e')

instance Subst RWCDefn RWCTy where
  fv (RWCDefn n pt _ e) = fv pt ++ fv e
  bv (RWCDefn _ pt _ _) = bv pt
  subst' (RWCDefn n (xs :-> t) b e) = refreshs xs (fv t ++ fv e) $ \ xs' ->
                                        do t' <- subst' t
                                           e' <- subst' e
                                           return (RWCDefn n (xs' :-> t') b e')

instance NFData RWCDefn where
  rnf (RWCDefn n pt b e) = n `deepseq` pt `deepseq` b `deepseq` e `deepseq` ()

---

data RWCData = RWCData { dataName   :: TyConId,
                         dataTyVars :: [Id RWCTy],
                         dataKind   :: Kind,
                         dataCons   :: [RWCDataCon] }
               deriving (Ord,Eq,Show)

instance NFData RWCData where
  rnf (RWCData i tvs k dcs) = i `deepseq` tvs `deepseq` dcs `deepseq` k `deepseq` ()

---

data RWCDataCon = RWCDataCon DataConId [RWCTy]
                  deriving (Ord,Eq,Show)

instance NFData RWCDataCon where
  rnf (RWCDataCon i ts) = i `deepseq` ts `deepseq` ()

---

data RWCProgram = RWCProgram { dataDecls  :: [RWCData],
                               defns      :: [RWCDefn] }
                  deriving (Ord,Eq,Show)

instance NFData RWCProgram where
  rnf (RWCProgram dds defs) = dds `deepseq` defs `deepseq` ()

nub' :: Ord a => [a] -> [a]
nub' = toList . fromList

instance Monoid RWCProgram where
  mempty = RWCProgram mempty mempty
  mappend (RWCProgram ts vs) (RWCProgram ts' vs') = RWCProgram (nub' $ ts ++ ts') $ nub' $ vs ++ vs'

---

flattenArrow :: RWCTy -> ([RWCTy],RWCTy)
flattenArrow (RWCTyApp (RWCTyApp (RWCTyCon (TyConId "->")) t1) t2) = let (ts,t) = flattenArrow t2 in (t1:ts,t)
flattenArrow t                                                     = ([],t)

flattenTyApp :: RWCTy -> [RWCTy]
flattenTyApp (RWCTyApp t1 t2) = flattenTyApp t1 ++ [t2]
flattenTyApp t                = [t]

flattenApp :: RWCExp -> [RWCExp]
flattenApp (RWCApp e e') = flattenApp e++[e']
flattenApp e             = [e]

mkArrow :: RWCTy -> RWCTy -> RWCTy
mkArrow t1 t2 = RWCTyApp (RWCTyApp (RWCTyCon (TyConId "->")) t1) t2

infixr `mkArrow`

arrowLeft :: RWCTy -> RWCTy
arrowLeft (RWCTyApp (RWCTyApp (RWCTyCon (TyConId "->")) t1) t2) = t1
arrowLeft t                                                     = error $ "arrowLeft: got non-arrow type: " ++ show t

arrowRight :: RWCTy -> RWCTy
arrowRight (RWCTyApp (RWCTyApp (RWCTyCon (TyConId "->")) t1) t2) = t2
arrowRight t                                                     = error $ "arrowRight: got non-arrow type: " ++ show t

typeOf :: RWCExp -> RWCTy
typeOf (RWCApp e _)                   = arrowRight (typeOf e)
typeOf (RWCLam _ t e)                 = mkArrow t (typeOf e)
typeOf (RWCVar _ t)                   = t
typeOf (RWCCon _ t)                   = t
typeOf (RWCLiteral (RWCLitInteger _)) = RWCTyCon (TyConId "Integer")
typeOf (RWCLiteral (RWCLitFloat _))   = RWCTyCon (TyConId "Float")
typeOf (RWCLiteral (RWCLitChar _))    = RWCTyCon (TyConId "Char")
typeOf (RWCCase _ (RWCAlt _ e:_))     = typeOf e
typeOf (RWCCase _ [])                 = error "typeOf: encountered case with no alts"
typeOf (RWCNativeVHDL n e)            = typeOf e
