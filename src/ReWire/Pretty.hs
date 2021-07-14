{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ReWire.Pretty (($+$), ($$), prettyPrint, text, int, empty, hang, P.Pretty) where

import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as P
import TextShow
import Data.Text (Text)

($$) :: P.Doc ann -> P.Doc ann -> P.Doc ann
a $$ b = P.vsep [a, b]

-- TODO(chathhorn): nesting
($+$) :: P.Doc ann -> P.Doc ann -> P.Doc ann
a $+$ b = P.vsep [a, P.nest 2 b]
infixl 5 $$, $+$

text :: Text -> P.Doc ann
text = P.pretty

int :: Int -> P.Doc ann
int = P.pretty

empty :: P.Doc ann
empty = P.emptyDoc

hang :: P.Doc ann -> Int -> P.Doc ann -> P.Doc ann
hang a n b = P.sep [a, P.nest n b]

prettyPrint :: P.Pretty a => a -> Text
prettyPrint = P.renderStrict . P.layoutPretty P.defaultLayoutOptions . P.pretty

-- TODO(chathhorn): orphan
instance TextShow (P.Doc ann) where
      showb = showb . show
