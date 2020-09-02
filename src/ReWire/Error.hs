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

import Prelude hiding ((<>), length, lines, unlines, take)
import Data.Text
import Control.Monad.Except (MonadError (..), ExceptT (..), runExceptT, throwError)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (StateT (..), MonadState (..))
import Data.Text (Text)
import Language.Haskell.Exts.Syntax (Annotated (..))
import Language.Haskell.Exts.Pretty (prettyPrim)
import Language.Haskell.Exts.SrcLoc (SrcLoc (..), SrcInfo (..), SrcSpanInfo, noLoc)
import Prettyprinter (Pretty (..), (<>), (<+>), nest, Doc)
import ReWire.Pretty (($$), text, int)

data AstError = AstError !Annote !Text

-- | The point of the newtype and all the annoying boilerplate is to
--   redefine the "fail" method of the Monad and MonadFail typeclasses.
newtype SyntaxErrorT m a = SyntaxErrorT { unwrap :: StateT Annote (ExceptT AstError m) a }
      deriving (Functor, Applicative, MonadIO, MonadCatch, MonadThrow, MonadState Annote, MonadReader r)

instance MonadTrans SyntaxErrorT where
      lift = SyntaxErrorT . lift . lift

instance TextShow AstError where
      showb (AstError (AstAnnote a) msg) = trunc 50 (showb $ nest 4 $ text "...") $ show $
            errorHdr (ann a) msg
            $$ nest 4 (text "In the fragment:")
            $$ nest 6 (pretty $ show $ prettyPrim a) -- TODO(chathhorn): better way?
      showb (AstError a@(MsgAnnote m) msg) = showb $ errorHdr (toSrcSpanInfo a) $ msg <> "\n" <> m
      showb (AstError a msg)               = showb $ errorHdr (toSrcSpanInfo a) msg

instance Monad m => MonadFail (SyntaxErrorT m) where
      fail = failNowhere

instance Monad m => Monad (SyntaxErrorT m) where
      return = SyntaxErrorT . return
      (SyntaxErrorT m) >>= f = SyntaxErrorT $ m >>= unwrap . f

instance Monad m => MonadError AstError (SyntaxErrorT m) where
      throwError e = SyntaxErrorT (throwError e)
      catchError (SyntaxErrorT m) f = SyntaxErrorT (catchError m (unwrap . f))

errorHdr :: SrcSpanInfo -> Text -> Doc ann
errorHdr l msg = if getPointLoc l == noLoc
      then text "Error:" <+> text msg
      else loc $$ nest 4 (text "Error:" <+> text msg)
      where loc = text file <> num r <> num c <> text ":"
            num n = if n == -1 then empty else text ":" <> int n
            SrcLoc file r c = getPointLoc l

trunc :: Int -> Text -> Text -> Text
trunc n t s
      | length (lines s) > n = unlines $ take n $ lines s <> [t]
      | otherwise            = s

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
