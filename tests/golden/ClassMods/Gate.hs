module ClassMods.Gate where

class Gate a where
      inv :: a -> a
      par :: a -> a -> a

class Flip a where
      flick :: a -> a
