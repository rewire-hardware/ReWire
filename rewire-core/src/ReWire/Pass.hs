{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | Scaffolding for running, tracing, and dumping pipeline passes, shared
--   between the compiler (ReWire.ModCache) and the embedder
--   (Embedder.ModCache).
module ReWire.Pass
      ( runPasses
      , printHeader
      , printInfoTop
      , printInfoHSE
      , verb'
      ) where

import ReWire.Config (Config, pDebug)
import ReWire.HSE.Rename (Renamer, allExports)
import ReWire.Pretty (TextShow, showt)

import Control.Monad ((>=>), void, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.Text.IO                 as T
import qualified Language.Haskell.Exts.Pretty as P
import qualified Language.Haskell.Exts.Syntax as S (Module (..))

-- | Run a sequence of named passes, numbered consecutively from the given
--   starting number. Before each pass, the announce action is called with the
--   pass number, name, and the pass's input (for verbose tracing and --dump);
--   after each pass, the hook runs on its output (e.g., re-typechecking with
--   --debug-typecheck).
runPasses :: Monad m
          => (Natural -> Text -> a -> m a) -- ^ Announce/dump, called before each pass.
          -> (a -> m a)                    -- ^ Hook run after each pass.
          -> Natural                       -- ^ Number of the first pass.
          -> [(Text, a -> m a)]            -- ^ The passes: name and transformation.
          -> a -> m a
runPasses announce after = go
      where go _ []               = pure
            go n ((name, f) : fs) = announce n name >=> f >=> after >=> go (n + 1) fs

printHeader :: MonadIO m => Text -> m ()
printHeader hd = do
      liftIO $ T.putStrLn   "-- # ==================================================="
      liftIO $ T.putStrLn $ "-- # " <> hd
      liftIO $ T.putStrLn   "-- # ===================================================\n"

-- | Dump the header plus (when verbose) the renamer, exports, imports, and
--   the Show'd module.
printInfoTop :: (MonadIO m, TextShow imps, TextShow mod) => Text -> Renamer -> imps -> Bool -> mod -> m ()
printInfoTop hd rn imps verbose m = do
      printHeader hd
      when verbose $ liftIO $ T.putStrLn "\n-- ## Renamer:\n"
      when verbose $ liftIO $ T.putStrLn $ showt rn
      when verbose $ liftIO $ T.putStrLn "\n-- ## Exports:\n"
      when verbose $ liftIO $ T.putStrLn $ showt $ allExports rn
      when verbose $ liftIO $ T.putStrLn "\n-- ## Show imps:\n"
      when verbose $ liftIO $ T.putStrLn $ showt imps
      when verbose $ liftIO $ T.putStrLn "\n-- ## Show mod:\n"
      when verbose $ liftIO $ T.putStrLn $ showt m

printInfoHSE :: (MonadIO m, TextShow imps) => Text -> Renamer -> imps -> Bool -> S.Module a -> m (S.Module a)
printInfoHSE hd rn imps verbose hse = do
      printInfoTop hd rn imps verbose $ void hse
      when verbose $ liftIO $ T.putStrLn "\n-- ## Pretty HSE mod:\n"
      liftIO $ putStrLn $ P.prettyPrint $ void hse
      pure hse

-- | Print a debug message (verbose only), passing the program through.
verb' :: MonadIO m => Config -> Text -> a -> m a
verb' conf s a = pDebug conf s >> pure a
