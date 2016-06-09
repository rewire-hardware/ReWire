{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE Safe #-}
module ReWire.Error
      ( SyntaxError, SyntaxErrorT, AstError
      , MonadError
      , failAt
      , failNowhere
      , filePath
      , runSyntaxError
      ) where

import ReWire.Annotation (Annotation (..), Annote (..), toSrcSpanInfo)

import Control.Monad.Except (MonadError (..), ExceptT (..), runExceptT, throwError)
import Language.Haskell.Exts.Annotated.Syntax (Annotated (..))
import Language.Haskell.Exts.Pretty (prettyPrim)
import Language.Haskell.Exts.SrcLoc (SrcLoc (..), SrcInfo (..), SrcSpanInfo, noLoc)
import Text.PrettyPrint (empty, nest, text, int, (<>), (<+>), ($$), Doc)

type SyntaxErrorT = ExceptT AstError

data AstError = AstError !Annote !String

-- UndecidableInstances required for this: basically just a typeclass synonym.
class MonadError AstError m => SyntaxError m
instance MonadError AstError m => SyntaxError m

instance Show AstError where
      show (AstError (AstAnnote a) msg) = trunc 50 (show $ nest 4 $ text "...") $ show $
            errorHdr (ann a) msg
            $$ nest 4 (text "In the fragment:")
            $$ nest 6 (prettyPrim a)
      show (AstError a@(MsgAnnote m) msg) = show $ errorHdr (toSrcSpanInfo a) $ msg ++ "\n" ++ m
      show (AstError a msg)               = show $ errorHdr (toSrcSpanInfo a) msg

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

failAt :: (SyntaxError m, Annotation an) => an -> String -> m a
failAt an msg = throwError $ AstError (toAnnote an) msg

failNowhere :: SyntaxError m => String -> m a
failNowhere = failAt noLoc

filePath :: FilePath -> SrcLoc
filePath fp = SrcLoc fp (-1) (-1)

runSyntaxError :: Monad m => SyntaxErrorT m a -> m (Either AstError a)
runSyntaxError = runExceptT
