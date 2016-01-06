module ReWire.FrontEnd.LoadPath (getSystemLoadPath) where

import Paths_ReWire

getSystemLoadPath :: IO [FilePath]
getSystemLoadPath = do
  libPath <- getDataFileName "lib"
  return [".",libPath]
