module ReWire.Pretty (Pretty(..), prettyPrint) where

import Text.PrettyPrint (Doc)

class Pretty a where
      pretty :: a -> Doc

prettyPrint :: Pretty a => a -> String
prettyPrint = show . pretty

