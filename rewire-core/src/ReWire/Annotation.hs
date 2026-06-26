{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module ReWire.Annotation
      ( Annote (Annote, NoAnnote, MsgAnnote)
      , Provenance (..)
      , Span (..)
      , Annotation (..)
      , Annotated (..)
      , fromSrcSpanInfo, spanSrcSpanInfo, srcAnnote
      , primSpan, annProv, annContext
      , toSrcSpanInfo
      , SrcSpanInfo (..), SrcSpan
      , noAnn, unAnn
      ) where

import ReWire.SYB (transform)
import ReWire.Unbound (Alpha (..))
import ReWire.HSE.Orphans ()
import ReWire.Pretty (TextShow (showb), fromString)

import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import Data.Data (Typeable, Data (..))
import Data.Hashable (Hashable (..))
import Language.Haskell.Exts.SrcLoc
      ( SrcLoc, SrcInfo (..), SrcSpanInfo (..), SrcSpan (..)
      , noLoc, noInfoSpan, mkSrcSpan)

import GHC.Generics (Generic (..))

-- | A source region: a file together with 1-based (line, column) start and end
--   positions. Carries the full extent of a node (unlike a single point), which
--   is what lets a diagnostic underline the offending text.
data Span = Span
      { spanFile  :: !FilePath
      , spanStart :: !(Int, Int)
      , spanEnd   :: !(Int, Int)
      } deriving (Eq, Ord, Show, Generic)

-- | Where an annotated node came from. Nodes parsed from user source are
--   'FromSource'; nodes a pass invents are 'Synthesized', recording the pass
--   name and the source spans of the inputs it was built from (so errors on
--   generated code can still point back at the user's source).
data Provenance
      = FromSource  !Span
      | Synthesized !Text ![Span]
      | NoProvenance
      deriving (Eq, Ord, Show, Generic)

-- | The annotation carried by every IR node: its provenance plus any
--   human-readable context breadcrumbs accumulated as it travels through the
--   passes.
data Annote = Annote { annProv :: !Provenance, annContext :: ![Text] }
      deriving (Show, Typeable)

-- | The empty annotation: a node with no known origin.
pattern NoAnnote :: Annote
pattern NoAnnote = Annote NoProvenance []

-- | A locationless annotation carrying a single explanatory message. Matches
--   the first context message of any annotation.
pattern MsgAnnote :: Text -> Annote
pattern MsgAnnote m <- Annote _ (m : _)
      where MsgAnnote m = Annote NoProvenance [m]

class Annotation a where
      toAnnote :: a -> Annote

instance Annotation Annote where
      toAnnote = id

instance Annotation SrcSpanInfo where
      toAnnote = locAnnote

instance Annotation SrcLoc where
      toAnnote l = locAnnote $ noInfoSpan $ mkSrcSpan l l

class Annotated a where
      ann :: a -> Annote

-- | Strip every annotation from a value, replacing each with 'noAnn'. Used to
--   make dumps readable and to compare terms structurally.
unAnn :: Data d => d -> d
unAnn = transform $ \ (_ :: Annote) -> noAnn

-- | Build an annotation from a haskell-src-exts source span.
locAnnote :: SrcSpanInfo -> Annote
locAnnote ssi = Annote (FromSource $ fromSrcSpanInfo ssi) []

-- | Build an annotation for an explicit source region: a file with 1-based
--   (line, column) start and end positions.
srcAnnote :: FilePath -> (Int, Int) -> (Int, Int) -> Annote
srcAnnote f start end = Annote (FromSource $ Span f start end) []

-- | The source span underlying an annotation, if it has one. Synthesized nodes
--   yield the span of their first input.
primSpan :: Annote -> Maybe Span
primSpan = \ case
      Annote (FromSource s)          _ -> Just s
      Annote (Synthesized _ (s : _)) _ -> Just s
      _                                -> Nothing

fromSrcSpanInfo :: SrcSpanInfo -> Span
fromSrcSpanInfo (SrcSpanInfo (SrcSpan f sl sc el ec) _) = Span f (sl, sc) (el, ec)

spanSrcSpanInfo :: Span -> SrcSpanInfo
spanSrcSpanInfo (Span f (sl, sc) (el, ec)) = noInfoSpan $ SrcSpan f sl sc el ec

noSpanInfo :: SrcSpanInfo
noSpanInfo = noInfoSpan $ mkSrcSpan noLoc noLoc

instance SrcInfo Annote where
      toSrcInfo a b c = locAnnote $ toSrcInfo a b c
      fromSrcInfo     = locAnnote . fromSrcInfo
      fileName        = fileName . toSrcSpanInfo
      startLine       = startLine . toSrcSpanInfo
      startColumn     = startColumn . toSrcSpanInfo

-- | Opaque to SYB: 'gfoldl' uses its default (no children), so 'unAnn' and the
--   generic traversals treat an annotation as an indivisible leaf rather than
--   recursing into its provenance.
instance Data Annote

instance TextShow Annote where
      showb = fromString . show

-- Annotations are metadata about a node, not part of its meaning. Equality,
-- ordering, hashing, and alpha-equivalence are therefore intentionally blind to
-- them: two terms that differ only in source location must still compare equal,
-- or type comparison and term deduplication (which run over annotated IR) would
-- wrongly distinguish them.
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

instance Hashable Annote where
      hashWithSalt s _ = s

instance NFData Span where
      rnf (Span f a b) = rnf f `seq` rnf a `seq` rnf b
instance NFData Provenance where
      rnf (FromSource s)    = rnf s
      rnf (Synthesized t s) = rnf t `seq` rnf s
      rnf NoProvenance      = ()
instance NFData Annote where
      rnf (Annote p c) = rnf p `seq` rnf c

-- | Recover a haskell-src-exts span for the haskell-src-exts machinery and for
--   the error renderer. Locationless annotations map to 'noLoc'.
toSrcSpanInfo :: Annote -> SrcSpanInfo
toSrcSpanInfo an = maybe noSpanInfo spanSrcSpanInfo $ primSpan an

noAnn :: Annote
noAnn = NoAnnote
