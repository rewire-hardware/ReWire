{-# LANGUAGE LambdaCase #-}
module ReWire.FrontEnd.Error
      ( ParseError
      , pFailAt
      , pFail
      , runParseError
      ) where

import ReWire.FrontEnd.Annotate

import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import Language.Haskell.Exts.Annotated.Syntax (Annotated(..))
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (noLoc, SrcInfo(..))
import Language.Haskell.Exts (SrcLoc, ParseResult(..))

type ParseError = ExceptT (SrcLoc, String)

pFailAt :: Monad m => Annote -> String -> ParseError m a
pFailAt (AnnoteLoc l) msg = throwE (getPointLoc l, msg)
pFailAt (Annote a)    msg = throwE (getPointLoc $ ann a, msg ++ ":\n" ++ unlines (take 5 $ lines $ prettyPrint a))

pFail :: Monad m => String -> ParseError m a
pFail msg = throwE (noLoc, msg)

runParseError :: Monad m => ParseError m a -> m (ParseResult a)
runParseError e = runExceptT e >>= return . \ case
      Left (loc, msg) -> ParseFailed loc msg
      Right a         -> return a
