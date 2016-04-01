{-# LANGUAGE Rank2Types, ScopedTypeVariables, GADTs #-}
module ReWire.Annotation
      ( Annote(..)
      , Annotation(..)
      , Annotated(..)
      , toSrcSpanInfo
      , noAnn, unAnn
      ) where

import ReWire.SYB (runPureT,transform)

import Control.Monad.Identity (Identity(..))
import Data.Data (Typeable,Data(..))
import GHC.Generics (Generic)
import Language.Haskell.Exts.Annotated.ExactPrint (ExactP)
import Language.Haskell.Exts.SrcLoc (SrcLoc,SrcInfo(..),SrcSpanInfo(..),noLoc,noInfoSpan,mkSrcSpan)

import qualified Language.Haskell.Exts.Annotated as HS (Annotated(..))
import qualified Language.Haskell.Exts.Pretty as HS (Pretty)

data Annote where
      NoAnnote  :: Annote
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
