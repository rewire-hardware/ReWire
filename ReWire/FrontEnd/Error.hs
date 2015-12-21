{-# LANGUAGE LambdaCase #-}
module ReWire.FrontEnd.Error
      ( ParseError
      , pFailAt
      , pFail
      , runParseError
      ) where

import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)

import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts (SrcLoc, ParseResult(..))

type ParseError = ExceptT (SrcLoc, String)

pFailAt :: Monad m => SrcLoc -> String -> ParseError m a
pFailAt loc msg = throwE (loc, msg)

pFail :: Monad m => String -> ParseError m a
pFail = pFailAt noLoc

runParseError :: Monad m => ParseError m a -> m (ParseResult a)
runParseError e = runExceptT e >>= return . \case
      Left (loc, msg) -> ParseFailed loc msg
      Right a         -> return a
