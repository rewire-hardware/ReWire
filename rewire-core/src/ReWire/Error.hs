{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Error
      ( SyntaxErrorT, AstError, Warning (..), Label
      , MonadError
      , mark
      , failAt, failAt', failAtWith, failInternal
      , failNowhere
      , warnAt
      , filePath
      , printError
      , runSyntaxError
      , PutMsg (..)
      ) where

import Prelude hiding ((<>), lines, unlines)

import ReWire.Annotation (Annotation (..), Annote, Span (..), annContext, primSpan, toSrcSpanInfo, noAnn)
import ReWire.Config (Config, noWarn, wError, loadPath)
import ReWire.Pretty (($$), text, int, showt, Pretty (pretty), (<>), (<+>), nest, vsep, Doc, defaultLayoutOptions, layoutSmart, renderStrict)
import ReWire.Orphans ()

import Control.Lens ((^.))
import Data.Maybe (maybeToList)
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.Except (MonadError (..), ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (StateT (..), MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Text (Text)
import Language.Haskell.Exts.SrcLoc (SrcLoc (..), SrcInfo (..), SrcSpanInfo, noLoc)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (stderr)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | A secondary location with an explanatory note, attached to an error to
--   point at a related part of the source (e.g. "first defined here").
type Label = (Annote, Text)

-- | An error: a primary location and message, plus optional secondary
--   labelled locations and suggested-fix hints.
data AstError = AstError !Annote !Text ![Label] ![Text]

class PutMsg a where
      putMsg :: Text -> a -> a

instance PutMsg AstError where
      putMsg msg (AstError an _ ls hs) = AstError an msg ls hs

instance Pretty AstError where
      pretty (AstError an msg labels hints) =
            vsep $ diagHdr "Error" an msg : map (uncurry labelHdr) labels <> hintDocs hints

-- | A non-fatal diagnostic: printed to stderr when emitted (see 'warnAt'),
--   or promoted to an 'AstError' under -Werror.
data Warning = Warning !Annote !Text
      deriving (Eq, Ord)

instance Pretty Warning where
      pretty (Warning an msg) = diagHdr "Warning" an msg

diagHdr :: Text -> Annote -> Text -> Doc ann
diagHdr lbl an msg = msgHdr lbl (toSrcSpanInfo an) $ appendContext msg $ annContext an

-- | Append a node's context breadcrumbs below its message, one per line.
appendContext :: Text -> [Text] -> Text
appendContext msg = \ case
      []  -> msg
      cs  -> msg <> "\n" <> T.intercalate "\n" cs

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

failAt :: (MonadError AstError m, Annotation an) => an -> Text -> m a
failAt an msg = throwError $ AstError (toAnnote an) msg [] []

-- | Like 'failAt', but attach secondary labelled locations and/or hints.
failAtWith :: (MonadError AstError m, Annotation an) => an -> Text -> [Label] -> [Text] -> m a
failAtWith an msg labels hints = throwError $ AstError (toAnnote an) msg labels hints

-- | Report a violated internal invariant: a bug in rwc itself, not a problem
--   with the user's program. Rendered with a "please report it" hint so the
--   message can't be mistaken for a complaint about the user's code.
failInternal :: (MonadError AstError m, Annotation an) => an -> Text -> m a
failInternal an msg = failAtWith an ("internal error: " <> msg) []
      ["this is a bug in rwc, not a problem with your program; please report it at https://github.com/rewire-hardware/ReWire/issues"]

-- | Emit a warning: printed to stderr immediately (so it isn't lost if a
--   later pass fails), suppressed by -w, or promoted to an error by -Werror
--   (which therefore fails on the *first* warning).
warnAt :: (MonadError AstError m, MonadIO m, Annotation an) => Config -> an -> Text -> m ()
warnAt conf an msg
      | conf^.wError = failAt an $ msg <> " [-Werror]"
      | conf^.noWarn = pure ()
      | otherwise    = printDiag (conf ^. loadPath) "Warning" (toAnnote an) msg [] []

-- | Like failAt, but include an extra bit of data on failure.
failAt' :: (MonadError (ex, AstError) m, Annotation an) => ex -> an -> Text -> m a
failAt' ex an msg = throwError (ex, AstError (toAnnote an) msg [] [])

failNowhere :: (PutMsg ex, Monad m, MonadState ex m, MonadError ex m) => Text -> m a
failNowhere msg = get >>= throwError . putMsg msg

-- | Print an error to stderr, including a source snippet with a caret under
--   the offending region (and any secondary labels/hints) when the files can
--   be found on the load path.
printError :: MonadIO m => [FilePath] -> AstError -> m ()
printError dirs (AstError an msg labels hints) = printDiag dirs "Error" an msg labels hints

-- | Render a diagnostic header, source snippets for the primary and any
--   secondary locations, and any hints, to stderr. Source files are looked up
--   along the given search path.
printDiag :: MonadIO m => [FilePath] -> Text -> Annote -> Text -> [Label] -> [Text] -> m ()
printDiag dirs lbl an msg labels hints = liftIO $ do
      primSnip    <- snippetFor an
      -- Drop labels that resolve to the primary location: a "note" pointing at
      -- the same place as the error itself is just noise.
      labelBlocks <- mapM labelBlock $ filter ((primSpan an /=) . primSpan . fst) labels
      let body = (diagHdr lbl an msg : maybeToList primSnip) <> concat labelBlocks <> hintDocs hints
      T.hPutStrLn stderr $ doc2Text $ vsep body
      where snippetFor :: Annote -> IO (Maybe (Doc ann))
            snippetFor a = maybe (pure Nothing) (locateSnippet dirs) $ primSpan a
            labelBlock :: Label -> IO [Doc ann]
            labelBlock (a, note) = (labelHdr a note :) . maybeToList <$> snippetFor a

-- | A secondary "note:" header pointing at a related source location.
labelHdr :: Annote -> Text -> Doc ann
labelHdr a = msgHdr "note" (toSrcSpanInfo a)

-- | Suggested fixes, rendered below a diagnostic.
hintDocs :: [Text] -> [Doc ann]
hintDocs = map $ \ h -> nest 2 $ text "help:" <+> pretty h

-- | Find a span's source file along the search path and render its offending
--   line with an underline.
locateSnippet :: [FilePath] -> Span -> IO (Maybe (Doc ann))
locateSnippet dirs sp = (renderSnippet sp =<<) <$> locateSource dirs (spanFile sp)

-- | Read a source file, trying it directly then relative to each search dir.
locateSource :: [FilePath] -> FilePath -> IO (Maybe [Text])
locateSource dirs f = go $ f : map (</> f) dirs
      where go :: [FilePath] -> IO (Maybe [Text])
            go []       = pure Nothing
            go (c : cs) = do
                  ex <- doesFileExist c
                  if ex then Just . T.lines <$> T.readFile c else go cs

-- | The offending source line with a caret underline beneath it, e.g.:
--
-- >    |
-- >  7 |   foo bar baz
-- >    |       ^^^^^^^
--
--   A multi-line span underlines from its start column to the end of the start
--   line. Returns 'Nothing' if the start line is out of range.
renderSnippet :: Span -> [Text] -> Maybe (Doc ann)
renderSnippet (Span _ (sl, sc) (el, ec)) ls
      | sl >= 1, sl <= length ls = Just $ text bar $$ text srcLine $$ text caretLine
      | otherwise                = Nothing
      where src        = ls !! (sl - 1)
            gutter     = showt sl
            blankG     = T.replicate (T.length gutter) " "
            bar        = blankG <> " |"
            srcLine    = gutter <> " | " <> src
            endCol     = if el == sl then ec else T.length src + 1
            caretLine  = blankG <> " | " <> T.replicate (max 0 $ sc - 1) " " <> T.replicate (max 1 $ endCol - sc) "^"

filePath :: FilePath -> SrcLoc
filePath fp = SrcLoc fp (-1) (-1)

runSyntaxError :: Monad m => SyntaxErrorT AstError m a -> m (Either AstError a)
runSyntaxError = runSyntaxError' $ AstError noAnn mempty [] []

runSyntaxError' :: Monad m => ex -> SyntaxErrorT ex m a -> m (Either ex a)
runSyntaxError' ex0 = runExceptT . fmap fst . flip runStateT ex0 . unwrap

mark :: (MonadState AstError m, Annotation an) => an -> m ()
mark an = put $ AstError (toAnnote an) mempty [] []

doc2Text :: Doc a -> Text
doc2Text = renderStrict . layoutSmart defaultLayoutOptions
