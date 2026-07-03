{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | Verbose pass tracing for the HSE front end (the haskell-src-exts
--   halves of what ReWire.Pass used to provide; the HSE-free scaffolding
--   stays in rewire-frontend).
module ReWire.HSE.PassInfo
      ( printInfoTop
      , printInfoHSE
      ) where

import ReWire.HSE.Rename (Renamer, allExports)
import ReWire.Pass (printHeader)
import ReWire.Pretty (showt)

import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (Text)

import qualified Data.Text.IO                 as T
import qualified Language.Haskell.Exts.Pretty as P
import qualified Language.Haskell.Exts.Syntax as S (Module (..))

-- | Dump the header plus (when verbose) the renamer, exports, imports, and
--   the Show'd module. The imports and module are taken pre-rendered so
--   callers can choose the rendering.
printInfoTop :: MonadIO m => Text -> Renamer -> Text -> Bool -> Text -> m ()
printInfoTop hd rn imps verbose m = do
      printHeader hd
      when verbose $ liftIO $ T.putStrLn "\n-- ## Renamer:\n"
      when verbose $ liftIO $ T.putStrLn $ showt rn
      when verbose $ liftIO $ T.putStrLn "\n-- ## Exports:\n"
      when verbose $ liftIO $ T.putStrLn $ showt $ allExports rn
      when verbose $ liftIO $ T.putStrLn "\n-- ## Show imps:\n"
      when verbose $ liftIO $ T.putStrLn imps
      when verbose $ liftIO $ T.putStrLn "\n-- ## Show mod:\n"
      when verbose $ liftIO $ T.putStrLn m

printInfoHSE :: MonadIO m => Text -> Renamer -> Text -> Bool -> S.Module a -> m (S.Module a)
printInfoHSE hd rn imps verbose hse = do
      printInfoTop hd rn imps verbose $ showt $ void hse
      when verbose $ liftIO $ T.putStrLn "\n-- ## Pretty HSE mod:\n"
      liftIO $ putStrLn $ P.prettyPrint $ void hse
      pure hse
