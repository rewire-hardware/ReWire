{-# LANGUAGE FlexibleContexts #-}
module ReWire.Error
      ( SyntaxError, Error
      , failAt
      , failNowhere
      , runSyntaxError
      ) where

import ReWire.Core.Syntax

import Control.Monad.Except (MonadError(..), ExceptT(..), runExceptT, throwError)
import Language.Haskell.Exts.Annotated.Syntax (Annotated(..))
import Language.Haskell.Exts.Pretty (prettyPrim)
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcInfo(..), SrcSpanInfo)
import Text.PrettyPrint (nest, text, int, (<>), (<+>), ($$), Doc)

type SyntaxError = ExceptT Error

data Error = Error !Annote !String

instance Show Error where
      show (Error (AnnoteLoc l) msg) = show $ prettyHdr l msg
      show (Error (Annote a)    msg) = show $
            prettyHdr (ann a) msg
            $$ nest 4 (text "In the fragment:")
            $$ nest 6 (prettyPrim a)

prettyHdr :: SrcSpanInfo -> String -> Doc
prettyHdr l msg = loc $$ nest 4 (text "Error:" <+> text msg)
      where loc = text file <> text ":" <> int start <> text ":" <> int end <> text ":"
            SrcLoc file start end = getPointLoc l

failAt :: MonadError Error m => Annote -> String -> m a
failAt an msg = throwError $ Error an msg

failNowhere :: MonadError Error m => String -> m a
failNowhere msg = throwError $ Error noAnn msg

runSyntaxError :: Monad m => SyntaxError m a -> m (Either Error a)
runSyntaxError = runExceptT
