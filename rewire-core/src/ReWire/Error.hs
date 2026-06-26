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
      , relocateErr, relocatingTo, relocatingNoLocTo
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
import ReWire.Pretty (($$), text, int, showt, Pretty (pretty), (<>), (<+>), vsep, Doc, defaultLayoutOptions, layoutSmart, renderStrict)
import ReWire.Orphans ()

import Control.Lens ((^.))
import Data.Maybe (maybeToList, catMaybes)
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.Except (MonadError (..), ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (StateT (..), MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Text (Text, pack)
import Language.Haskell.Exts.SrcLoc (SrcLoc (..), SrcInfo (..), noLoc)
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
      pretty (AstError an msg labels hints) = vsep $
            blockNoSnip "Error" an (appendContext msg $ annContext an)
                  : map (uncurry $ blockNoSnip "note") labels <> hintDocs hints

-- | A non-fatal diagnostic: printed to stderr when emitted (see 'warnAt'),
--   or promoted to an 'AstError' under -Werror.
data Warning = Warning !Annote !Text
      deriving (Eq, Ord)

instance Pretty Warning where
      pretty (Warning an msg) = blockNoSnip "Warning" an $ appendContext msg $ annContext an

-- | A diagnostic block without a source snippet (the location header, then the
--   labelled message below it), used where the source text isn't available.
blockNoSnip :: Text -> Annote -> Text -> Doc ann
blockNoSnip lbl an msg = vsep $ catMaybes [locLine an] <> [text (lbl <> ":") <+> pretty msg]

-- | The "file:line:col:" location header for an annotation, or Nothing if it
--   has no usable source position.
locLine :: Annote -> Maybe (Doc ann)
locLine an
      | getPointLoc l == noLoc = Nothing
      | otherwise              = Just $ text (T.pack file) <> num r <> num c <> text ":"
      where l = toSrcSpanInfo an
            num :: Int -> Doc ann
            num n = if n == -1 then mempty else text ":" <> int n
            SrcLoc file r c = getPointLoc l

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
      fail = failNowhere . pack

instance Monad m => Monad (SyntaxErrorT ex m) where
      (SyntaxErrorT m) >>= f = SyntaxErrorT $ m >>= unwrap . f

instance Monad m => MonadError ex (SyntaxErrorT ex m) where
      throwError e = SyntaxErrorT (throwError e)
      catchError (SyntaxErrorT m) f = SyntaxErrorT (catchError m (unwrap . f))

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

-- | Re-point a diagnostic at a fallback location when it has none, or when it
--   resolves to a different source file than the fallback — typically because
--   the error surfaced inside inlined library code while compiling a user
--   definition. The original location, when it had one, is kept as a secondary
--   note so the expansion site stays visible.
relocateErr :: Annotation an => an -> AstError -> AstError
relocateErr to e@(AstError from msg labels hints) = case primSpan $ toAnnote to of
      Nothing -> e
      Just toSpan -> case primSpan from of
            Just fromSpan | spanFile fromSpan == spanFile toSpan -> e
            Just _                                              -> relocated
            Nothing                                             -> AstError (toAnnote to) msg labels hints
      where relocated = AstError (toAnnote to) msg ((from, "in code expanded here") : labels) hints

-- | Run an action, relocating any error it raises to the given fallback
--   location (see 'relocateErr').
relocatingTo :: (MonadError AstError m, Annotation an) => an -> m a -> m a
relocatingTo to m = m `catchError` (throwError . relocateErr to)

-- | Give a fallback location only to diagnostics that have none, leaving
--   already-located errors untouched. Used at the top of the pipeline so that
--   a whole-program error (no single offending node) still names the file
--   being compiled.
relocateErrNoLoc :: Annotation an => an -> AstError -> AstError
relocateErrNoLoc to (AstError from msg labels hints)
      | Nothing <- primSpan from = AstError (toAnnote to) msg labels hints
relocateErrNoLoc _ e = e

relocatingNoLocTo :: (MonadError AstError m, Annotation an) => an -> m a -> m a
relocatingNoLocTo to m = m `catchError` (throwError . relocateErrNoLoc to)

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
      primary     <- renderBlock dirs lbl an $ appendContext msg $ annContext an
      -- Drop labels that resolve to the primary location: a "note" pointing at
      -- the same place as the error itself is just noise.
      labelBlocks <- mapM (uncurry $ renderBlock dirs "note") $ filter ((primSpan an /=) . primSpan . fst) labels
      T.hPutStrLn stderr $ doc2Text $ vsep $ primary : labelBlocks <> hintDocs hints

-- | One diagnostic block: the location header, the offending source line with a
--   caret, then the labelled message below it.
renderBlock :: [FilePath] -> Text -> Annote -> Text -> IO (Doc ann)
renderBlock dirs lbl an msg = do
      snip <- maybe (pure Nothing) (locateSnippet dirs) $ primSpan an
      pure $ vsep $ catMaybes [locLine an] <> maybeToList snip <> [text (lbl <> ":") <+> pretty msg]

-- | Notes (suggestions/hints) rendered below a diagnostic.
hintDocs :: [Text] -> [Doc ann]
hintDocs = map $ \ h -> text "note:" <+> pretty h

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
