{-# LANGUAGE Safe #-}
module ReWire.HSE.Annotate
      ( annotate
      , Annote
      ) where

import ReWire.Annotation (Annote, Annotation (toAnnote))
import ReWire.HSE.SrcLoc () -- the `Annotation SrcSpanInfo` instance.

import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax (Module)

-- | Replace every haskell-src-exts source span in a parsed module with the
--   compiler's own annotation, recording each node's origin span.
annotate :: Module SrcSpanInfo -> Module Annote
annotate = fmap toAnnote
