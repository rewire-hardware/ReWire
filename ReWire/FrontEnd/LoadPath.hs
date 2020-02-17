{-# LANGUAGE Safe #-}
module ReWire.FrontEnd.LoadPath (getSystemLoadPath) where

import Paths_ReWire

getSystemLoadPath :: IO [FilePath]
getSystemLoadPath = ("." :) . pure <$> getDataFileName "lib"
