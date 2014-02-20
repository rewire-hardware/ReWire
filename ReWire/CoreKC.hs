module ReWire.CoreKC where

-- Kind checking for Core.

data Kind = Kvar (Name Kind) | Kstar | Kfun Kind Kind deriving (Eq,Show)

