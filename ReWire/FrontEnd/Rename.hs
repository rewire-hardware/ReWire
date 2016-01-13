{-# LANGUAGE FlexibleInstances, TupleSections #-}
module ReWire.FrontEnd.Rename
      ( Renamer
      , FQName(..)
      , Namespace(..)
      , rename
      , extend
      , exclude
      , finger
      , toFilePath
      ) where

import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import Language.Haskell.Exts (Name(..), ModuleName(..), QName(..), prettyPrint)
import System.FilePath (joinPath, (<.>))

import qualified Data.Map.Strict as Map

type Renamer = Map.Map (Namespace, QName) FQName

data Namespace = TypeNS | ValueNS
      deriving (Ord, Eq, Show)

data FQName = FQName !ModuleName !Name
      deriving (Ord, Eq, Show)

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

rename :: (ToQName a, FromQName b) => Namespace -> Renamer -> a -> b
rename ns rn x = fromQName . maybe (toQName x) toQName $ Map.lookup (ns, toQName x) rn

extend :: ToQName a => Namespace -> [(a, FQName)] -> Renamer -> Renamer
extend ns kvs = Map.union $ Map.fromList $ map (((ns,) . toQName . fst) &&& snd) kvs

exclude :: ToQName a => Namespace -> [a] -> Renamer -> Renamer
exclude ns = foldr ((.) . Map.delete . (ns,) . toQName) id

-- | True iff an entry for the name exists in the renamer.
finger :: ToQName a => Namespace -> Renamer -> a -> Bool
finger ns rn = flip Map.member rn . (ns,) . toQName

toFilePath :: ModuleName -> FilePath
toFilePath (ModuleName n) = joinPath (splitOn "." n) <.> "hs"
