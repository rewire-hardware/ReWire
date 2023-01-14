{-# LANGUAGE Trustworthy #-}
module ReWire.Pretty
      ( ($$), text, int, empty, P.Pretty (pretty), P.Doc
      , prettyPrint, prettyPrint'
      , fastPrint, fastPrint'
      , TextShow (showt, showb, showbPrec), fromString, fromText
      , genericShowbPrec, P.space, P.angles, P.dot
      , (P.<>), (P.<+>), P.nest, P.defaultLayoutOptions, P.layoutSmart, P.renderStrict
      , P.vsep, P.hsep, P.parens, P.braces, P.punctuate, P.comma, P.dquotes, P.tupled
      , P.brackets, FromGeneric (..), P.semi, P.colon, P.hcat, P.align
      ) where

import Prelude hiding ((<>), lines, unlines)

import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as P
import Data.Text (Text)
import TextShow (TextShow (showt, showb, showbPrec), fromString, fromText)
import TextShow.Generic (genericShowbPrec, FromGeneric (..))

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
