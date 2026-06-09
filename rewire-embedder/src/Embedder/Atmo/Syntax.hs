{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module Embedder.Atmo.Syntax
      ( Module (..), DataConId (..), TyConId (..)
      , Ty (..), TyBuiltin(..), Poly (..)
      , Exp (..), Pat (..)
      , Defn (..), DefnAttr (..), DataDefn (..), RecDefn (..), TypeSynonym (..)
      , DataCon (..)
      , FreeProgram, Program (..)
      , FieldId
      , DataHeader, RecHeader, Binds(..), PatBind(..)
      , FunBinding(..) -- Rhs(..), GuardedRhs(..)
      , prettyFP, getPatVars -- getFunBody
      , untype
      ) where

import Prelude hiding (replicate)

import ReWire.Annotation (Annote, Annotated (ann))
import ReWire.Orphans ()
import ReWire.Pretty (empty, text, TextShow (showt), FromGeneric (..), Doc,
  nest, hsep, parens, dquotes, comma, brackets, vsep, (<+>), Pretty (pretty),
  punctuate, line, softline, align, braces, dot)
import ReWire.SYB (transform)
import Control.DeepSeq (NFData (..), deepseq)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Data (Typeable, Data (..))
import Data.Hashable (Hashable (..))
import Data.List (intersperse)
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Generics (Generic (..))
import Numeric.Natural (Natural)
import Embedder.Builtins (TyBuiltin (..), RWUserOp, rwu2s)
import Control.Monad.Identity (Identity(..))
import ReWire.Error (MonadError, AstError)

data Module = Module ![DataDefn] ![RecDefn] ![TypeSynonym] ![Defn]
      deriving (Show, Generic, Data)
      deriving TextShow via FromGeneric Module

instance Pretty Module where
      pretty (Module cs rs ts ds) = nest 2 $ vsep (text "module" : map pretty cs <> map pretty rs <> map pretty ts <> map pretty ds)

instance Semigroup Module where
      -- TODO(chathhorn): shouldn't be necessary
      (Module a b c d) <> (Module a' b' c' d') = Module (nubOrdOn dataName $ a <> a') (nubOrdOn recName $ b <> b') (nubOrdOn typeSynName $ c <> c') (nubOrdOn defnName $ d <> d')

instance Monoid Module where
      mempty = Module [] [] [] []

---

-- Name Handling: 
      -- Bind bs e replaced with bs e
            -- corresponding matches are adjusted
            -- unbinds are removed (slight variable renaming)
            -- untrec removed
      -- Embed removed
      -- Name X replaced with Text
      -- instance Alpha removed
      -- instance Subst removed
      -- TRec removed
      -- runFreshM removed (replaced with runIdentity in some places)
      -- n2s removed
      -- Eq on Poly types derived instead of alpha equivalence

class Parenless a where
      -- | Parts that never need to be wrapped in parens during pretty printing.
      parenless :: a -> Bool

newtype DataConId = DataConId Text
      deriving (Eq, Generic, Typeable, Data)
newtype TyConId = TyConId Text
      deriving (Eq, Generic, Typeable, Data)
newtype FieldId  = FieldId Text
      deriving (Eq, Generic, Typeable, Data)

instance Hashable DataConId
instance Hashable TyConId
instance Hashable FieldId

data DataCon = DataCon Annote !Text !Poly
      deriving (Generic, Eq, Show, Typeable, Data)
      deriving TextShow via FromGeneric DataCon

instance Annotated DataCon where
      ann (DataCon a _ _)  = a

instance Pretty DataCon where
      pretty (DataCon _ n t)  = text n <+> text "::" <+> pretty t

instance NFData DataCon


data Poly = Poly [Text] Ty
      deriving (Generic, Eq, Show, Typeable, Data)
      deriving TextShow via FromGeneric Poly

instance Hashable Poly

instance NFData Poly


instance Pretty Poly where
      pretty (Poly _tvs pt) = pretty pt

data Ty = TyApp Annote !Ty ![Ty]
        | TyCon Annote !Text
        | TyVar Annote !Text
        | TyNat Annote !Natural
        | TyTuple Annote ![Ty]
        | TyBuiltin Annote !TyBuiltin
      deriving (Eq, Ord, Generic, Typeable, Data, Show)
      deriving TextShow via FromGeneric Ty

instance Hashable Ty

instance Annotated Ty where
      ann = \ case
            TyApp a _ _   -> a
            TyCon a _     -> a
            TyVar a _     -> a
            TyNat a _     -> a
            TyTuple a _   -> a
            TyBuiltin a _ -> a

instance Parenless Ty where
      parenless = \ case
            TyTuple {}                     -> True
            TyCon {}                       -> True
            TyVar {}                       -> True
            TyNat {}                       -> True
            TyBuiltin _ TyInteger          -> True
            TyBuiltin _ TyString           -> True
            TyBuiltin _ TyBool             -> True
            TyBuiltin _ TyUnit             -> True
            _                              -> False

needsParens :: Ty -> Bool
needsParens t = case t of
      (TyApp _ (TyBuiltin _ (TyFun {})) _) -> True
      _                  -> False



instance Pretty Ty where
      pretty = \ case
            TyVar _ n        -> text n
            TyCon _ n        -> text n
            TyNat _ n        -> text $ showt n
            TyTuple _ ts     -> parens $ hsep $ punctuate comma $ map pretty ts
            TyBuiltin _ tb   -> pretty tb
            TyApp _ (TyBuiltin _ TyList) [t] -> brackets $ pretty t
            TyApp _ (TyBuiltin _ TyFun) [t1,t2] | needsParens t1
                           -> parens $ parens (pretty t1) <+> pretty TyFun <+> pretty t2
            TyApp _ (TyBuiltin _ TyFun) [t1,t2] -> parens $ pretty t1 <+> pretty TyFun <+> pretty t2
            TyApp _ (TyBuiltin _ TyPlus) [t1,t2] -> parens $ pretty t1 <+> text "+" <+> pretty t2
            TyApp _ t ts -> parens $ pretty t <+> hsep (map mparens ts)

instance NFData Ty

untype :: Data d => d -> d
untype = transform $ \ (_ :: Maybe Ty) -> Nothing

---
-- | Records
-- (not yet)



----

-- Originally includes let bindings and where-bindings...
-- should only be let-bindings here...
-- Would be nice to simplify to, say, PatBinds or something
-- Rather, let's start with PatBinds and complicate it only if we need to
newtype Binds = BDefs [Defn]
      deriving (Generic,Eq,Show,Typeable,Data)
      deriving TextShow via FromGeneric Binds

instance Hashable Binds
instance NFData Binds

data PatBind = PatBind Pat Exp
      deriving (Generic, Eq, Show, Typeable, Data)
      deriving TextShow via FromGeneric PatBind

instance Hashable PatBind
instance NFData PatBind

instance Pretty PatBind where
      pretty (PatBind p e) = brackets $ pretty p <+> pretty e

data Exp = App     Annote !(Maybe Poly) !(Maybe Ty) !Exp ![Exp]
         | Lam     Annote !(Maybe Poly) !(Maybe Ty) ![Text] !Exp
         | Var     Annote !(Maybe Poly) !(Maybe Ty) !Text
         | Con     Annote !(Maybe Poly) !(Maybe Ty) !Text
         | Case    Annote !(Maybe Poly) !(Maybe Ty) !Exp ![PatBind]
         | RWUser  Annote !(Maybe Poly) !(Maybe Ty) !RWUserOp
         | LitInt  Annote !(Maybe Poly) !Integer
         | LitStr  Annote !(Maybe Poly) !Text
         | LitVec  Annote !(Maybe Poly) !(Maybe Ty) ![Exp]
         | LitList Annote !(Maybe Poly) !(Maybe Ty) ![Exp]
         | Tuple   Annote !(Maybe Poly) !(Maybe Ty) ![Exp]
         | If      Annote !(Maybe Poly) !(Maybe Ty) !Exp !Exp !Exp
         | Let     Annote !(Maybe Poly) !(Maybe Ty) ![PatBind] !Exp
         | RecVal  Annote !(Maybe Poly) !(Maybe Ty) ![(Text, Exp)]
         | RecSel  Annote !(Maybe Poly) !(Maybe Ty) Text Exp
         | RecUpd  Annote !(Maybe Poly) !(Maybe Ty) !Exp ![(Text,Exp)]
      deriving (Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric Exp

instance Hashable Exp

instance Eq Exp where
      a == b = hash a == hash b

instance NFData Exp

instance Annotated Exp where
      ann = \ case
            App a _ _ _ _       -> a
            Lam a _ _ _ _       -> a
            Var a _ _ _         -> a
            Con a _ _ _         -> a
            Case a _ _ _ _      -> a
            RWUser a _ _ _      -> a
            LitInt a _ _        -> a
            LitStr a _ _        -> a
            LitList a _ _ _     -> a
            LitVec a _ _ _      -> a
            Tuple a _ _ _       -> a
            If a _ _ _ _ _      -> a
            Let a _ _ _ _       -> a
            RecVal a _ _ _      -> a
            RecSel a _ _ _ _    -> a
            RecUpd a _ _ _ _    -> a

-- | Does this exp have a type annotation? Avoids depending on Embedder.Atmo.Types.
typeAnnotated :: Exp -> Bool
typeAnnotated = isJust . \ case
      App _ t _ _ _       -> t
      Lam _ t _ _ _       -> t
      Var _ t _ _         -> t
      Con _ t _ _         -> t
      Case _ t _ _ _      -> t
      RWUser _ t _ _      -> t
      LitInt _ t _        -> t
      LitStr _ t _        -> t
      LitList _ t _ _     -> t
      LitVec _ t _ _      -> t
      Tuple _ t _ _       -> t
      If _ t _ _ _ _      -> t
      Let _ t _ _ _       -> t
      RecVal _ t _ _      -> t
      RecSel _ t _ _ _    -> t
      RecUpd _ t _ _ _    -> t

instance Parenless Exp where
      parenless = \ case
            -- TODO(chathhorn): rework pretty printing to handle type annotations on applications.
            e | typeAnnotated e -> False
            Con {}              -> True
            Var {}                                          -> True
            RWUser {}                                       -> True
            LitInt {}                                       -> True
            LitStr {}                                       -> True
            LitList {}                                      -> True
            LitVec {}                                       -> True
            Tuple {}                                        -> True
            _                                               -> False

instance Pretty Exp where
      pretty = \ case
            Con _ pt _ n                               -> ppTyAnn pt $ text n
            Var _ pt _ n                               -> ppTyAnn pt $ text n
            Lam _ pt _ vs e                           -> ppTyAnn pt $
                  runIdentity $ pure $ text "\\" <+> hsep (map text vs)  <+> text "->" <+> pretty e
            Case _ pt _ e pbs                          -> ppTyAnn pt $
                  runIdentity $ pure $ text "case" <+> pretty e <+> text "of" <>
                        line <> vsep (map pretty pbs)
            RWUser _ pt _ b                            -> ppTyAnn pt $ maybe (text "RWUOp??") text (rwu2s b)
            LitInt _ pt v                              -> ppTyAnn pt $ pretty v
            LitStr _ pt v                              -> ppTyAnn pt $ dquotes $ pretty v
            LitList _ pt _ vs                          -> ppTyAnn pt $ brackets $ hsep $ punctuate comma $ map pretty vs
            LitVec _ pt _ vs                           -> ppTyAnn pt $ brackets $ hsep $ punctuate comma $ map pretty vs
            Tuple _ pt _ es                            -> ppTyAnn pt $ parens $ hsep $ punctuate comma $ map pretty es
            App _ _ _ e es                             -> nest 2 $ hsep $ map mparens (e : es)
            If _ _pt _mt t c a                         -> parens $ "if" <+> pretty t <+> "then" <+> pretty c <+> "else" <+> pretty a
            Let _ _pt _mt pbs e                        -> parens $ "let" <> softline <> align (vsep $ map pLetBind pbs ++ [ "in" <+> pretty e])
            RecVal _ _pt _mt fs                        -> braces $ hsep $ punctuate comma $ map (\(f, e) -> text f <+> "=" <+> pretty e) fs
            RecSel _ _pt _mt f e                       -> pretty e <> dot <> text f
            RecUpd _ _pt _mt e fs                      -> pretty e <+> braces (hsep $ punctuate comma $ map (\(f, e') -> text f <+> "=" <+> pretty e') fs)
            where
            pLetBind :: PatBind -> Doc ann
            pLetBind (PatBind p e) = pretty p <+> "=" <+> pretty e

---

data Pat = PatCon      Annote !(Maybe Poly) !(Maybe Ty) !Text ![Pat]
         | PatVar      Annote !(Maybe Poly) !(Maybe Ty) !Text
         | PatWildCard Annote !(Maybe Poly) !(Maybe Ty)
         | PatTuple    Annote !(Maybe Poly) !(Maybe Ty) ![Pat]
         | PatAs       Annote !(Maybe Poly) !(Maybe Ty) !Text !Pat -- should only appear in FunBindings
         | PatRec      Annote !(Maybe Poly) !(Maybe Ty) ![(Text,Pat)]
      deriving (Eq, Show, Generic, Typeable, Data)
      deriving TextShow via FromGeneric Pat

instance Hashable Pat

instance NFData Pat

instance Annotated Pat where
      ann = \ case
            PatCon a _ _ _ _  -> a
            PatVar a _ _ _    -> a
            PatWildCard a _ _ -> a
            PatTuple a _ _ _  -> a
            PatAs a _ _ _ _   -> a
            PatRec a _ _ _    -> a

instance Parenless Pat where
      parenless = \ case
            PatCon _ _ _ _ []  -> True
            PatVar {}          -> True
            PatWildCard {}     -> True
            PatTuple {}        -> True
            PatAs {}           -> True
            _                  -> False

instance Pretty Pat where
      pretty = \ case
            PatCon _ pt _ n ps   -> ppTyAnn pt $ text n <+> hsep (map mparens ps)
            PatVar _ pt _ n      -> ppTyAnn pt $ text n
            PatWildCard _ pt _   -> ppTyAnn pt $ text "_"
            PatTuple _ pt _ ps   -> ppTyAnn pt $ parens $ hsep $ punctuate comma $ map pretty ps
            PatAs _ pt _ n p     -> ppTyAnn pt $ parens $ text n <+> "@" <+> parens (pretty p)
            PatRec _ pt _ fs     -> ppTyAnn pt $ parens $ hsep $ punctuate comma $ map pretty fs

---

-- data Rhs = UnGuardedRhs Annote !Exp
--          | GuardedRhss Annote ![GuardedRhs]
--       deriving (Eq, Generic, Show, Typeable, Data)
--       deriving TextShow via FromGeneric Rhs

-- instance Hashable Rhs
-- instance NFData Rhs

-- data GuardedRhs = GuardedRhs Annote ![Exp] Exp
--       deriving (Eq, Generic, Show, Typeable, Data)
--       deriving TextShow via FromGeneric GuardedRhs

-- instance Hashable GuardedRhs
-- instance NFData GuardedRhs

data FunBinding = FunBinding Annote ![Pat] !Exp -- Rhs
      deriving (Eq, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric FunBinding

instance Hashable FunBinding
instance NFData FunBinding

instance Pretty FunBinding where
      pretty (FunBinding _ ps rhs) = pFunCase ps rhs
            -- case rhs of
            -- UnGuardedRhs _ e -> pFunCase n ps e
            -- GuardedRhss _ grhss -> vsep $ map (pGuardedFunCase n ps) grhss

pFunCase :: [Pat] -> Exp -> Doc ann
pFunCase ps e = hsep (map pretty ps) <+> "|->" <+> pretty e

-- pGuardedFunCase :: Text -> [Pat] -> GuardedRhs -> Doc ann
-- pGuardedFunCase n ps (GuardedRhs _ gs e) = text n <+> hsep (map pretty ps) <+> pGuards gs
--                                    <> "=" <+> pretty e
--       where
--       pGuards :: [Exp] -> Doc ann
--       pGuards [] = mempty
--       pGuards (g:gs) = "|" <+> pretty g <+> pGuards gs

getPatVars :: (MonadError AstError m) => [Pat] -> m [Text]
getPatVars [] = return []
getPatVars (PatVar _ _ _ v : ps) = getPatVars ps >>= \ vs -> return $ v : vs
getPatVars (PatCon _ _ _ _n ps : ps') = getPatVars ps' >>= \ vs' -> getPatVars ps >>= \ vs -> return $ vs ++ vs'
getPatVars (PatTuple _ _ _ ps : ps') = getPatVars ps' >>= \ vs' -> getPatVars ps >>= \ vs -> return $ vs ++ vs'
getPatVars (PatWildCard {} : ps) = getPatVars ps
getPatVars (PatAs _ _ _ n p : ps) = getPatVars [p] >>= \ vs' -> getPatVars ps >>= \ vs -> return $ n : vs ++ vs'
getPatVars (PatRec _ _ _ fs : ps) = getPatVars (map snd fs) >>= \ vs' -> getPatVars ps >>= \ vs -> return $ vs ++ vs'

-- getFunBody :: (MonadError AstError m) => Rhs -> m Exp
-- getFunBody (UnGuardedRhs _ e) = return e
-- getFunBody (GuardedRhss l _) = failAt l "All FunBindings should be unguarded until we resugar function guards"


data Defn = Defn
            { defnAnnote :: Annote
            , defnName   :: !Text
            , defnPolyTy :: !Poly
            , defnAttr   :: !(Maybe DefnAttr)
            , defnBinds  :: ![FunBinding]
            }
      deriving (Eq, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric Defn

instance Hashable Defn
instance NFData Defn

instance Annotated Defn where
      ann (Defn a _ _ _ _) = a

instance Pretty Defn where
      pretty (Defn _ n t attr bs) =
            runIdentity $ pure $ vsep $ map (nest 2)
                 $  [ text "{-# INLINE" <+> text n <+> text "#-}"   | attr == Just Inline   ]
                 <> [ text "{-# NOINLINE" <+> text n <+> text "#-}" | attr == Just NoInline ]
                 <> [ text n <+> text "::" <+> pretty t ]
                 <> map pretty bs

---

data DefnAttr = Inline | NoInline
      deriving (Eq, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric DefnAttr

instance Hashable DefnAttr

instance NFData DefnAttr

---

-- this is just a synonym for the dataName and dataVars of a DataDefn
type DataHeader = (Text,[Text])

data DataDefn = DataDefn
      { dataAnnote :: Annote
      , dataName   :: !Text
      , dataVars   :: ![Text]
      , dataCons   :: ![DataCon]
      }
      deriving (Eq, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric DataDefn

instance NFData DataDefn

instance Annotated DataDefn where
      ann (DataDefn a _ _ _) = a

instance Pretty DataDefn where
      pretty (DataDefn _ n tvs cs) = nest 2 $ vsep
            $ (text "data" <+> text n <+> hsep (map text tvs) <+> text "where")
            : map (nest 2 . pretty) cs

---

-- RecDefn, a specialized version of DataDefn

-- this is just a synonym for the dataName and dataVars of a DataDefn
type RecHeader = (Text,[Text])

data RecDefn = RecDefn
      { recAnnote :: Annote
      , recName   :: !Text
      , recVars   :: ![Text]
      , recPoly   :: !Poly
      , recFields :: ![(Text, Ty)]
      }
      deriving (Eq, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric RecDefn

instance NFData RecDefn

instance Annotated RecDefn where
      ann (RecDefn a _ _ _ _) = a

instance Pretty RecDefn where
      pretty (RecDefn _ n tvs _ fs) = nest 2 $ vsep
            $ (text "record" <+> text n <+> hsep (map text tvs) <+> text "where")
            : map (\ (f,t) -> nest 2 $ pretty f <+> "::" <+> pretty t) fs

---

data TypeSynonym = TypeSynonym
      { typeSynAnnote :: Annote
      , typeSynName   :: !Text
      , typeSynType   :: !Poly
      }
      deriving (Eq, Generic, Show, Typeable, Data)
      deriving TextShow via FromGeneric TypeSynonym

instance Hashable TypeSynonym

instance NFData TypeSynonym

instance Annotated TypeSynonym where
      ann (TypeSynonym a _ _) = a

instance Pretty TypeSynonym where
      pretty (TypeSynonym _ n (Poly tvs t)) = runIdentity $
            pure (text "type" <+> text n <+> hsep (map text tvs) <+> "=" <+> pretty t)
---

type FreeProgram = ([DataDefn], [RecDefn], [TypeSynonym], [Defn])

newtype Program = Program ([DataDefn], [RecDefn], [TypeSynonym], [Defn])
      deriving (Generic, Show, Typeable)

instance NFData Program where
      rnf (Program p) = p `deepseq` ()

instance Pretty Program where
      pretty (Program p) = prettyFP p

---

mparens :: (Pretty a, Parenless a) => a -> Doc ann
mparens a = if parenless a then pretty a else parens $ pretty a

ppTyAnn :: Maybe Poly -> Doc ann -> Doc ann
ppTyAnn Nothing d = d
ppTyAnn (Just pt) d = d <+> text "::" <+> pretty pt

-- TODO(chathhorn): make FreeProgram newtype.
prettyFP :: FreeProgram -> Doc ann
prettyFP (ts, rs, syns, vs) = vsep $ intersperse empty $ map pretty ts <> map pretty rs <> map pretty syns <> map pretty vs

