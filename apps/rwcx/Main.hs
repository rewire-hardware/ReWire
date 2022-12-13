{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import ReWire.HSE.GHCCoreToCrust (toCrust)
import ReWire.Pretty (prettyPrint)

import GHC.Paths (libdir)
import GHC.Plugins
      ( ppr, showSDoc
      , defaultFatalMessager
      , defaultFlushOut
      )
import GHC
      ( setSessionDynFlags, getSessionDynFlags
      , setTargets
      , runGhc
      , compileToCoreSimplified
      , defaultErrorHandler
      , CoreModule (..)
      , guessTarget, load, LoadHowMuch (..)
      )
import GHC.Driver.Monad (liftIO)
import qualified Data.Text.IO as T

main :: IO ()
main = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags
            -- setSessionDynFlags $ updOptLevel 2 $ dflags { hscTarget = HscNothing }
            setSessionDynFlags dflags
            setTargets []
            target <- guessTarget "test_main.hs" Nothing
            setTargets [target]
            _ <- load LoadAllTargets
            c <- compileToCoreSimplified "test_main.hs"
            liftIO $ putStrLn "************* GHC Core ***************"
            _ <- mapM (liftIO . putStrLn . showSDoc dflags . ppr) $ cm_binds c
            liftIO $ putStrLn "************* Crust ***************"
            (c', _) <- toCrust c
            liftIO $ T.putStrLn $ prettyPrint c'
--      where printBind :: MonadIO m => DynFlags -> CoreBind -> m ()
--            printBind dflags bndr@(NonRec b _) = do
--                  liftIO $ putStrLn $ "Non-recursive(??) binding named " <> showSDoc dflags (ppr b)
--            printBind dflags bndr@(Rec bs) = do
--                  liftIO $ putStrLn $ "Recursive binding, names: " <> concatMap ((<> " ") . showSDoc dflags . ppr . fst) bs
