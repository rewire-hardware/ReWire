{-# LANGUAGE TemplateHaskell #-}
-- | The GHC package path captured when rwc was built, parallel to the
--   other build-time constants rwc bakes in ('GHC.Paths.libdir' and the
--   Paths_rewire data directory): the output of `stack path
--   --ghc-package-path`, run at compile time from the package directory.
--   'Nothing' if that failed (e.g., a build not driven by stack).
--
--   Consumers must validate that the databases still exist before use:
--   the checkout, the stack root, or the snapshot may have moved or been
--   garbage-collected since the build.
module ReWire.GHC.PackagePath (bakedPackagePath) where

import Control.Exception (SomeException, try)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Language.Haskell.TH.Syntax (lift, runIO)
import System.Process (readProcess)

-- | Colon-separated package databases, in GHC_PACKAGE_PATH syntax.
bakedPackagePath :: Maybe FilePath
bakedPackagePath = $( runIO ( either (const Nothing)
                                     ( (\ p -> if null p then Nothing else Just p)
                                     . dropWhileEnd isSpace . dropWhile isSpace )
                              <$> (try (readProcess "stack" ["path", "--ghc-package-path"] "")
                                    :: IO (Either SomeException String)) )
                      >>= lift )
