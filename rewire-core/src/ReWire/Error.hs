{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Error
      ( SyntaxErrorT, AstError, Warning (..)
      , MonadError
      , mark
      , failAt, failAt'
      , failNowhere
      , warnAt
      , filePath
      , runSyntaxError
      , PutMsg (..)
      ) where

import Prelude hiding ((<>), lines, unlines)

import ReWire.Annotation (Annotation (..), Annote (..), toSrcSpanInfo, noAnn)
import ReWire.Config (Config, noWarn, wError)
import ReWire.Pretty (($$), text, int, showt, Pretty (pretty), (<>), (<+>), nest, Doc, defaultLayoutOptions, layoutSmart, renderStrict)
import ReWire.Orphans ()

import Control.Lens ((^.))
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.Except (MonadError (..), ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (StateT (..), MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Text (Text, pack)
import Language.Haskell.Exts.Pretty (prettyPrim)
import Language.Haskell.Exts.SrcLoc (SrcLoc (..), SrcInfo (..), SrcSpanInfo, noLoc)
import Language.Haskell.Exts.Syntax (Annotated (..))
import System.IO (stderr)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data AstError = AstError !Annote !Text

class PutMsg a where
      putMsg :: Text -> a -> a

instance PutMsg AstError where
      putMsg msg (AstError an _) = AstError an msg

instance Pretty AstError where
      pretty (AstError an msg) = diagHdr "Error" an msg

-- | A non-fatal diagnostic: printed to stderr when emitted (see 'warnAt'),
--   or promoted to an 'AstError' under -Werror.
data Warning = Warning !Annote !Text
      deriving (Eq, Ord)

instance Pretty Warning where
      pretty (Warning an msg) = diagHdr "Warning" an msg

diagHdr :: Text -> Annote -> Text -> Doc ann
diagHdr lbl (AstAnnote a) msg = text $ trunc 50 (showt $ nest 4 $ text "...")
      $ doc2Text $ msgHdr lbl (ann a) msg
      $$ nest 4 (text "In the fragment:")
      $$ nest 6 (text $ pack $ show $ prettyPrim a) -- TODO(chathhorn): better way?
diagHdr lbl a@(MsgAnnote m) msg = msgHdr lbl (toSrcSpanInfo a) $ msg <> "\n" <> m
diagHdr lbl a msg               = msgHdr lbl (toSrcSpanInfo a) msg

-- | The point of the newtype and all the annoying boilerplate is to
--   redefine the "fail" method of the Monad and MonadFail typeclasses.
newtype SyntaxErrorT ex m a = SyntaxErrorT { unwrap :: StateT ex (ExceptT ex m) a }
      deriving (Functor, Applicative, MonadIO, MonadCatch, MonadThrow, MonadState ex)

instance MonadTrans (SyntaxErrorT ex) where
      lift = SyntaxErrorT . lift . lift

instance (PutMsg ex, Monad m) => MonadFail (SyntaxErrorT ex m) where
      fail = failNowhere . showt

instance Monad m => Monad (SyntaxErrorT ex m) where
      (SyntaxErrorT m) >>= f = SyntaxErrorT $ m >>= unwrap . f

instance Monad m => MonadError ex (SyntaxErrorT ex m) where
      throwError e = SyntaxErrorT (throwError e)
      catchError (SyntaxErrorT m) f = SyntaxErrorT (catchError m (unwrap . f))

msgHdr :: Text -> SrcSpanInfo -> Text -> Doc ann
msgHdr lbl l msg = if getPointLoc l == noLoc
      then text (lbl <> ":") <+> pretty msg
      else loc $$ nest 4 (text (lbl <> ":") <+> pretty msg)
      where loc :: Doc ann
            loc = text (T.pack file) <> num r <> num c <> text ":"
            num :: Int -> Doc ann
            num n = if n == -1 then mempty else text ":" <> int n
            SrcLoc file r c = getPointLoc l

trunc :: Int -> Text -> Text -> Text
trunc n t s
      | length (T.lines s) > n = T.unlines $ take n $ T.lines s <> [t]
      | otherwise                = s

failAt :: (MonadError AstError m, Annotation an) => an -> Text -> m a
failAt an msg = throwError $ AstError (toAnnote an) msg

-- | Emit a warning: printed to stderr immediately (so it isn't lost if a
--   later pass fails), suppressed by -w, or promoted to an error by -Werror
--   (which therefore fails on the *first* warning).
warnAt :: (MonadError AstError m, MonadIO m, Annotation an) => Config -> an -> Text -> m ()
warnAt conf an msg
      | conf^.wError = failAt an $ msg <> " [-Werror]"
      | conf^.noWarn = pure ()
      | otherwise    = liftIO $ T.hPutStrLn stderr $ doc2Text $ pretty $ Warning (toAnnote an) msg

-- | Like failAt, but include an extra bit of data on failure.
failAt' :: (MonadError (ex, AstError) m, Annotation an) => ex -> an -> Text -> m a
failAt' ex an msg = throwError (ex, AstError (toAnnote an) msg)

failNowhere :: (PutMsg ex, Monad m, MonadState ex m, MonadError ex m) => Text -> m a
failNowhere msg = get >>= throwError . putMsg msg

filePath :: FilePath -> SrcLoc
filePath fp = SrcLoc fp (-1) (-1)

runSyntaxError :: Monad m => SyntaxErrorT AstError m a -> m (Either AstError a)
runSyntaxError = runSyntaxError' $ AstError noAnn mempty

runSyntaxError' :: Monad m => ex -> SyntaxErrorT ex m a -> m (Either ex a)
runSyntaxError' ex0 = runExceptT . fmap fst . flip runStateT ex0 . unwrap

mark :: (MonadState AstError m, Annotation an) => an -> m ()
mark an = put $ AstError (toAnnote an) mempty

doc2Text :: Doc a -> Text
doc2Text = renderStrict . layoutSmart defaultLayoutOptions
