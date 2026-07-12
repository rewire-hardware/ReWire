{-# LANGUAGE OverloadedStrings #-}
-- | rwcry: the out-of-process Cryptol-to-Hyle translator behind rwc's
--   Cryptol foreign-function interface. Invoked by rwc (or by hand):
--
--   > rwcry <module.cry> <function> <cryptol-type> <entry-name>
--
--   On success, prints the translated definitions (a defns-only Hyle
--   fragment, re-parsed by rwc with parseHyleDefns) to stdout and any
--   compile-time warnings (each as a @warning: @ line) to stderr, exiting
--   zero; on failure, prints a diagnostic to stderr and exits nonzero.
module Main (main) where

import ReWire.Cryptol.Translate (translate)
import ReWire.Pretty (prettyPrint)

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetBuffering, stdout, stderr, BufferMode (LineBuffering))

import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
      hSetBuffering stdout LineBuffering
      hSetBuffering stderr LineBuffering
      getArgs >>= \ case
            [file, fn, ty, entry] -> translate file (T.pack fn) (T.pack ty) (T.pack entry) >>= \ case
                  Right (ds, ws) -> do
                        mapM_ (T.hPutStrLn stderr . ("warning: " <>)) ws
                        mapM_ (T.putStrLn . (<> "\n") . prettyPrint) ds
                  Left err -> T.hPutStrLn stderr err >> exitFailure
            _ -> T.hPutStrLn stderr "usage: rwcry <module.cry> <function> <cryptol-type> <entry-name>" >> exitFailure
