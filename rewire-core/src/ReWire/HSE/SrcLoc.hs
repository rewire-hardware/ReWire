{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-methods #-}
-- | The boundary between haskell-src-exts source locations and ReWire's native
--   'Annote'/'Span'. It lives in the HSE layer so the base annotation module
--   (ReWire.Annotation) and the diagnostics renderer (ReWire.Error) carry no
--   haskell-src-exts dependency: only modules that actually manipulate the
--   haskell-src-exts AST import this conversion glue.
module ReWire.HSE.SrcLoc () where

import ReWire.Annotation (Annote, Annotation (..), Span (..), srcAnnote, primSpan)

import Language.Haskell.Exts.SrcLoc
      ( SrcLoc, SrcInfo (..), SrcSpanInfo (..), SrcSpan (..)
      , noLoc, noInfoSpan, mkSrcSpan)

-- | Build an annotation from a haskell-src-exts source span.
instance Annotation SrcSpanInfo where
      toAnnote = locAnnote

instance Annotation SrcLoc where
      toAnnote l = locAnnote $ noInfoSpan $ mkSrcSpan l l

locAnnote :: SrcSpanInfo -> Annote
locAnnote ssi = let Span f s e = fromSrcSpanInfo ssi in srcAnnote f s e

fromSrcSpanInfo :: SrcSpanInfo -> Span
fromSrcSpanInfo (SrcSpanInfo (SrcSpan f sl sc el ec) _) = Span f (sl, sc) (el, ec)

spanSrcSpanInfo :: Span -> SrcSpanInfo
spanSrcSpanInfo (Span f (sl, sc) (el, ec)) = noInfoSpan $ SrcSpan f sl sc el ec

noSpanInfo :: SrcSpanInfo
noSpanInfo = noInfoSpan $ mkSrcSpan noLoc noLoc

-- | Recover a haskell-src-exts span for the haskell-src-exts machinery.
--   Locationless annotations map to 'noLoc'.
toSrcSpanInfo :: Annote -> SrcSpanInfo
toSrcSpanInfo an = maybe noSpanInfo spanSrcSpanInfo $ primSpan an

instance SrcInfo Annote where
      toSrcInfo a b c = locAnnote $ toSrcInfo a b c
      fromSrcInfo     = locAnnote . fromSrcInfo
      fileName        = fileName . toSrcSpanInfo
      startLine       = startLine . toSrcSpanInfo
      startColumn     = startColumn . toSrcSpanInfo
