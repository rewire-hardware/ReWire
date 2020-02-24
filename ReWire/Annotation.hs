{-# LANGUAGE Rank2Types, ScopedTypeVariables, GADTs, TypeOperators, TypeFamilies, LambdaCase, DeriveDataTypeable #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module ReWire.Annotation
      ( Annote (..)
      , Annotation (..)
      , Annotated (..)
      , toSrcSpanInfo
      , SrcSpanInfo (..), SrcSpan
      , noAnn, unAnn
      ) where

import ReWire.SYB (runPureT,transform)
import ReWire.Unbound (Alpha (..))

import Control.DeepSeq (NFData (..))
import Control.Monad.Identity (Identity(..))
import Data.Data (Typeable,Data(..))
import Language.Haskell.Exts.ExactPrint (ExactP)
import Language.Haskell.Exts.SrcLoc
      ( SrcLoc, SrcInfo (..), SrcSpanInfo (..), SrcSpan (..)
      , noLoc, noInfoSpan, mkSrcSpan)

import qualified Language.Haskell.Exts.Syntax as HS (Annotated(..))
import qualified Language.Haskell.Exts.Pretty as HS (Pretty)

import safe GHC.Generics (Generic (..))

data Annote where
      NoAnnote  :: Annote
      MsgAnnote :: String -> Annote
      LocAnnote :: SrcSpanInfo -> Annote
      AstAnnote :: forall ast.
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
      ann :: a -> Annote

unAnn :: Data d => d -> d
unAnn = runIdentity . runPureT (transform $ \ (_ :: Annote) -> pure noAnn)

instance SrcInfo Annote where
      toSrcInfo a b c = LocAnnote $ toSrcInfo a b c
      fromSrcInfo     = LocAnnote . fromSrcInfo
      fileName        = fileName . toSrcSpanInfo
      startLine       = startLine . toSrcSpanInfo
      startColumn     = startColumn . toSrcSpanInfo

-- | Using the default definition of gfoldl.
instance Data Annote

instance Show Annote where
      show NoAnnote      = "NoAnnote"
      show (MsgAnnote m) = "MsgAnnote (" ++ show m ++ ")"
      show (LocAnnote l) = "LocAnnote (" ++ show l ++ ")"
      show (AstAnnote a) = "AstAnnote (" ++ show a ++ ")"

-- TODO(chathhorn): afraid of screwing stuff up if the annotation isn't
-- ignored.
instance Ord Annote where
      _ <= _ = True
instance Eq Annote where
      _ == _ = True

instance Alpha Annote where
      aeq' _ _ _         = True
      acompare' _ _ _    = EQ
      fvAny' _ _         = pure
      close _ _          = id
      open _ _           = id
      isPat _            = mempty
      isTerm _           = mempty
      nthPatFind _       = mempty
      namePatFind _      = mempty
      swaps' _ _         = id
      freshen' _ i       = pure (i, mempty)
      lfreshen' _ i cont = cont i mempty

instance NFData Annote where
      rnf _ = () -- Probably not ideal.

toSrcSpanInfo :: Annote -> SrcSpanInfo
toSrcSpanInfo = \ case
      NoAnnote    -> fromSrcInfo $ noInfoSpan $ mkSrcSpan noLoc noLoc
      MsgAnnote _ -> toSrcSpanInfo NoAnnote
      LocAnnote l -> l
      AstAnnote a -> HS.ann a

noAnn :: Annote
noAnn = NoAnnote
