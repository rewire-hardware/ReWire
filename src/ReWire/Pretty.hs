-- {-# LANGUAGE Safe #-}
module ReWire.Pretty (($+$), ($$), prettyPrint, text, int, empty, hang) where

import qualified Prettyprinter as P
import TextShow

($$) :: P.Doc ann -> P.Doc ann -> P.Doc ann
a $$ b = P.vcat [a, b]

-- TODO(chathhorn): nesting
($+$) :: P.Doc ann -> P.Doc ann -> P.Doc ann
a $+$ b = P.vcat [a, P.nest 2 b]
infixl 5 $$, $+$

text :: String -> P.Doc ann
text = P.pretty

int :: Int -> P.Doc ann
int = P.pretty

empty :: P.Doc ann
empty = P.emptyDoc

hang :: P.Doc ann -> Int -> P.Doc ann -> P.Doc ann
hang a n b = P.sep [a, P.nest n b]

prettyPrint :: P.Pretty a => a -> String
prettyPrint = show . P.pretty

instance TextShow (P.Doc a) where
      showb
