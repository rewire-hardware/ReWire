{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ReWire.Pretty
      ( ($$), text, int, empty, P.Pretty
      , prettyPrint, prettyPrint'
      , fastPrint, fastPrint'
      ) where

import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as P
import TextShow
import Data.Text (Text)

($$) :: P.Doc ann -> P.Doc ann -> P.Doc ann
a $$ b = P.vsep [a, b]

infixl 5 $$

text :: Text -> P.Doc ann
text = P.pretty

int :: Int -> P.Doc ann
int = P.pretty

empty :: P.Doc ann
empty = P.emptyDoc

prettyPrint :: P.Pretty a => a -> Text
prettyPrint = prettyPrint' . P.pretty

prettyPrint' :: P.Doc ann -> Text
prettyPrint' = P.renderStrict . P.layoutSmart (P.defaultLayoutOptions
      { P.layoutPageWidth = P.AvailablePerLine 120 1.0 })

fastPrint :: P.Pretty a => a -> Text
fastPrint = fastPrint' . P.pretty

fastPrint' :: P.Doc ann -> Text
fastPrint' = P.renderStrict . P.layoutCompact

-- TODO(chathhorn): orphan
instance TextShow (P.Doc ann) where
      showb = showb . show
