{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Error
      ( SyntaxErrorT, AstError
      , MonadError
      , failAt
      , failNowhere
      , filePath
      , runSyntaxError
      ) where

import ReWire.Annotation (Annotation (..), Annote (..), toSrcSpanInfo)

import Prelude hiding ((<>))
import Control.Monad.Except (MonadError (..), ExceptT (..), runExceptT, throwError)
import Control.Monad.Fail (MonadFail (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Language.Haskell.Exts.Syntax (Annotated (..))
import Language.Haskell.Exts.Pretty (prettyPrim)
import Language.Haskell.Exts.SrcLoc (SrcLoc (..), SrcInfo (..), SrcSpanInfo, noLoc)
import Text.PrettyPrint (empty, nest, text, int, (<>), (<+>), ($$), Doc)

newtype SyntaxErrorT m a = SyntaxErrorT { toExcept :: ExceptT AstError m a }
      deriving (Functor, Applicative, MonadTrans, MonadIO, MonadCatch, MonadThrow, MonadReader r, MonadState s)

data AstError = AstError !Annote !String

instance Show AstError where
      show (AstError (AstAnnote a) msg) = trunc 50 (show $ nest 4 $ text "...") $ show $
            errorHdr (ann a) msg
            $$ nest 4 (text "In the fragment:")
            $$ nest 6 (prettyPrim a)
      show (AstError a@(MsgAnnote m) msg) = show $ errorHdr (toSrcSpanInfo a) $ msg ++ "\n" ++ m
      show (AstError a msg)               = show $ errorHdr (toSrcSpanInfo a) msg

instance Monad m => MonadFail (SyntaxErrorT m) where
      fail = failNowhere

instance Monad m => Monad (SyntaxErrorT m) where
      return = SyntaxErrorT . return
      (SyntaxErrorT m) >>= f = SyntaxErrorT (m >>= toExcept . f)
      fail = failNowhere

instance Monad m => MonadError AstError (SyntaxErrorT m) where
      throwError e = SyntaxErrorT (throwError e)
      catchError (SyntaxErrorT m) f = SyntaxErrorT (catchError m (toExcept . f))

errorHdr :: SrcSpanInfo -> String -> Doc
errorHdr l msg = if getPointLoc l == noLoc
      then text "Error:" <+> text msg
      else loc $$ nest 4 (text "Error:" <+> text msg)
      where loc = text file <> num r <> num c <> text ":"
            num n = if n == -1 then empty else text ":" <> int n
            SrcLoc file r c = getPointLoc l

trunc :: Int -> String -> String -> String
trunc n t s
      | length (lines s) > n = unlines $ take n $ lines s ++ [t]
      | otherwise            = s

failAt :: (MonadError AstError m, Annotation an) => an -> String -> m a
failAt an msg = throwError $ AstError (toAnnote an) msg

failNowhere :: MonadError AstError m => String -> m a
failNowhere = failAt noLoc

filePath :: FilePath -> SrcLoc
filePath fp = SrcLoc fp (-1) (-1)

runSyntaxError :: Monad m => SyntaxErrorT m a -> m (Either AstError a)
runSyntaxError = runExceptT . toExcept
