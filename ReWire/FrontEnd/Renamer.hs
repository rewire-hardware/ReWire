{-# LANGUAGE FlexibleInstances #-}
module ReWire.FrontEnd.Renamer
      ( Renamer
      , FQName(..)
      , rename
      , extend
      , exclude
      , finger
      , toFilePath
      ) where

import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import System.FilePath (joinPath, (<.>))

import Language.Haskell.Exts (Name(..), ModuleName(..), QName(..), prettyPrint)

type Renamer = Map.Map QName FQName

data FQName = FQName !ModuleName !Name
      deriving (Eq, Show)

class ToQName a where
      toQName :: a -> QName
instance ToQName QName where
      toQName = id
instance ToQName FQName where
      toQName (FQName m x) = Qual m x
instance ToQName Name where
      toQName = UnQual

class FromQName a where
      fromQName :: QName -> a
instance FromQName QName where
      fromQName = id
instance FromQName FQName where
      fromQName (Qual m x) = FQName m x
      fromQName _          = error "no qualifier."
instance FromQName Name where
      fromQName (Qual _ x) = x
      fromQName (UnQual x) = x
      fromQName _          = error "special name."
instance FromQName String where
      fromQName (Qual m x)          = qual $ FQName m x
      fromQName (UnQual (Ident x))  = x
      fromQName (UnQual (Symbol x)) = x
      fromQName x                   = prettyPrint x

qual :: FQName -> String
qual (FQName (ModuleName m) (Ident n))  = m ++ "." ++ n
qual (FQName (ModuleName m) (Symbol n)) = m ++ "." ++ n

rename :: (ToQName a, FromQName b) => Renamer -> a -> b
rename rn x = fromQName . maybe (toQName x) toQName $ Map.lookup (toQName x) rn

extend :: ToQName a => [(a, FQName)] -> Renamer -> Renamer
extend kvs = Map.union $ Map.fromList $ map ((toQName . fst) &&& snd) kvs

exclude :: ToQName a => [a] -> Renamer -> Renamer
exclude = foldr ((.) . Map.delete . toQName) id

-- | True iff an entry for the name exists in the renamer.
finger :: ToQName a => Renamer -> a -> Bool
finger rn = flip Map.member rn . toQName

toFilePath :: ModuleName -> FilePath
toFilePath (ModuleName n) = joinPath (splitOn "." n) <.> "hs"
