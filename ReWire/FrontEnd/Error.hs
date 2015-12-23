{-# LANGUAGE LambdaCase #-}
module ReWire.FrontEnd.Error
      ( ParseError
      , pFailAt
      , pFail
      , runParseError
      , setLoc
      ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import Control.Monad.Trans.State (StateT(..), get, put)

import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts (SrcLoc, ParseResult(..))

type ParseError m = StateT SrcLoc (ExceptT (SrcLoc, String) m)

setLoc :: Monad m => SrcLoc -> ParseError m ()
setLoc = put

lastLoc :: Monad m => ParseError m SrcLoc
lastLoc = get

pFailAt :: Monad m => SrcLoc -> String -> ParseError m a
pFailAt loc msg = lift $ throwE (loc, msg)

pFail :: Monad m => String -> ParseError m a
pFail msg = do
      loc <- lastLoc
      pFailAt loc msg

runParseError :: Monad m => ParseError m a -> m (ParseResult a)
runParseError e = runExceptT (runStateT e noLoc) >>= return . \case
      Left (loc, msg) -> ParseFailed loc msg
      Right (a, _)    -> return a
