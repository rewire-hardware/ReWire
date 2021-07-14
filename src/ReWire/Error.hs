{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Error
      ( SyntaxErrorT, AstError
      , MonadError
      , mark
      , failAt
      , failNowhere
      , filePath
      , runSyntaxError
      ) where

import ReWire.Annotation (Annotation (..), Annote (..), toSrcSpanInfo, noAnn)

import Prelude hiding ((<>), lines, unlines)
import qualified Data.Text as T
import Control.Monad.Except (MonadError (..), ExceptT (..), runExceptT, throwError)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (StateT (..), MonadState (..))
import Data.Text (Text, pack)
import Language.Haskell.Exts.Syntax (Annotated (..))
import Language.Haskell.Exts.Pretty (prettyPrim)
import Language.Haskell.Exts.SrcLoc (SrcLoc (..), SrcInfo (..), SrcSpanInfo, noLoc)
import Prettyprinter (Pretty (..), (<>), (<+>), nest, Doc, defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.Text (renderStrict)
import ReWire.Pretty (($$), text, int)
import TextShow (TextShow (..))

data AstError = AstError !Annote !Text

-- | The point of the newtype and all the annoying boilerplate is to
--   redefine the "fail" method of the Monad and MonadFail typeclasses.
newtype SyntaxErrorT m a = SyntaxErrorT { unwrap :: StateT Annote (ExceptT AstError m) a }
      deriving (Functor, Applicative, MonadIO, MonadCatch, MonadThrow, MonadState Annote, MonadReader r)

instance MonadTrans SyntaxErrorT where
      lift = SyntaxErrorT . lift . lift

instance Pretty AstError where
      pretty (AstError (AstAnnote a) msg) = text (trunc 50 (showt $ nest 4 $ text "...")
            (doc2Text $ errorHdr (ann a) msg
            $$ nest 4 (text "In the fragment:")
            $$ nest 6 (text $ pack $ show $ prettyPrim a))) -- TODO(chathhorn): better way?
      pretty (AstError a@(MsgAnnote m) msg) = errorHdr (toSrcSpanInfo a) $ msg <> "\n" <> m
      pretty (AstError a msg)               = errorHdr (toSrcSpanInfo a) msg

instance Monad m => MonadFail (SyntaxErrorT m) where
      fail = failNowhere . showt

instance Monad m => Monad (SyntaxErrorT m) where
      return = SyntaxErrorT . return
      (SyntaxErrorT m) >>= f = SyntaxErrorT $ m >>= unwrap . f

instance Monad m => MonadError AstError (SyntaxErrorT m) where
      throwError e = SyntaxErrorT (throwError e)
      catchError (SyntaxErrorT m) f = SyntaxErrorT (catchError m (unwrap . f))

errorHdr :: SrcSpanInfo -> Text -> Doc ann
errorHdr l msg = if getPointLoc l == noLoc
      then text "Error:" <+> pretty msg
      else loc $$ nest 4 (text "Error:" <+> pretty msg)
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

failNowhere :: (Monad m, MonadState Annote m, MonadError AstError m) => Text -> m a
failNowhere msg = get >>= flip failAt msg

filePath :: FilePath -> SrcLoc
filePath fp = SrcLoc fp (-1) (-1)

runSyntaxError :: Monad m => SyntaxErrorT m a -> m (Either AstError a)
runSyntaxError = runExceptT . fmap fst . flip runStateT noAnn . unwrap

mark :: (MonadState Annote m, Annotation an) => an -> m ()
mark = put . toAnnote

doc2Text :: Doc a -> Text
doc2Text = renderStrict . layoutSmart defaultLayoutOptions
