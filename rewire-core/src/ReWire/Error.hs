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
import ReWire.Pretty (text, showt, Pretty (pretty), (<>), (<+>), vsep, Doc, defaultLayoutOptions, layoutSmart, renderStrict)
import ReWire.Orphans ()

import Control.Lens ((^.))
import Control.Monad (guard)
import Data.Char (toUpper, isUpper, isSpace)
import Data.Function (on)
import Data.List (nubBy)
import Data.Maybe (maybeToList)
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.Except (MonadError (..), ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (StateT (..), MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Text (Text, pack)
import Language.Haskell.Exts.SrcLoc (SrcLoc (..), SrcInfo (..), noLoc)
import Prettyprinter (annotate)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, bold)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (stderr, hIsTerminalDevice)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Prettyprinter.Render.Terminal as Term

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
            plainDiag SevError an (appendContext msg $ annContext an)
                  : map (uncurry $ plainDiag SevNote) labels <> map (plainDiag SevNote noAnn) hints

-- | A non-fatal diagnostic: printed to stderr when emitted (see 'warnAt'),
--   or promoted to an 'AstError' under -Werror.
data Warning = Warning !Annote !Text
      deriving (Eq, Ord)

instance Pretty Warning where
      pretty (Warning an msg) = plainDiag SevWarning an $ appendContext msg $ annContext an

-- | The severity of a diagnostic, controlling its label and colour.
data Severity = SevError | SevWarning | SevNote

sevLabel :: Severity -> Text
sevLabel = \ case SevError -> "error"; SevWarning -> "warning"; SevNote -> "note"

sevStyle :: Severity -> AnsiStyle
sevStyle = \ case
      SevError   -> color Red     <> bold
      SevWarning -> color Magenta <> bold
      SevNote    -> color Cyan    <> bold

-- | Style for the "file:line:col:" location header (bold, default foreground,
--   as GHC does).
locStyle :: AnsiStyle
locStyle = bold

-- | Style for the gutter: line numbers and the "|" separators.
gutterStyle :: AnsiStyle
gutterStyle = color Blue

-- | A coloured text fragment.
styled :: AnsiStyle -> Text -> Doc AnsiStyle
styled s = annotate s . text

-- | Present a diagnostic message as a sentence: capitalise the first letter and
--   end with terminal punctuation (adding a period when none is present).
normalizeMsg :: Text -> Text
normalizeMsg msg = case T.uncons trimmed of
      Nothing     -> trimmed
      Just (c, r) -> punct $ if capitalize then T.cons (toUpper c) r else trimmed
      where trimmed    = T.stripEnd msg
            -- Don't capitalise a message that opens with a code identifier
            -- (e.g. "rwPrimFinite: ...", "fromList: ..."): its first token
            -- already contains an upper-case letter.
            capitalize = not $ T.any isUpper $ T.takeWhile (not . isSpace) trimmed
            punct t | T.null t                             = t
                    | T.last t `elem` ['.', ':', '!', '?'] = t
                    | otherwise                            = t <> "."

-- | The "file:line:col:" header for an annotation, or Nothing if it has no
--   usable source position.
locHeaderText :: Annote -> Maybe Text
locHeaderText an
      | getPointLoc l == noLoc = Nothing
      | otherwise              = Just $ T.pack file <> num r <> num c <> ":"
      where l = toSrcSpanInfo an
            num n = if n == -1 then mempty else ":" <> showt n
            SrcLoc file r c = getPointLoc l

-- | A diagnostic block, GHC-style: "file:line:col: severity:" on the header
--   line, the message indented below it, then the source excerpt with a caret.
--   On a terminal the location is bold, the severity label and the underlined
--   source (and its caret) take the severity colour, and the gutter is blue.
--   With no source lines available the excerpt is omitted.
diagBlock :: Severity -> Annote -> Text -> Maybe [Text] -> Doc AnsiStyle
diagBlock sev an msg msrc = vsep $ header : msgLines <> maybeToList excerpt
      where loc      = primSpan an
            gutterW  = maybe 1 (\ (Span _ (l, _) _) -> length $ show l) loc
            indent   = T.replicate (gutterW + 1) " "
            sevDoc   = styled (sevStyle sev) $ sevLabel sev <> ":"
            header   = maybe sevDoc (\ t -> styled locStyle t <+> sevDoc) $ locHeaderText an
            msgLines = [ text indent <> styled bold l | l <- T.lines $ normalizeMsg msg ]
            excerpt  = do
                  Span _ (sl, sc) (el, ec) <- loc
                  ls <- msrc
                  guard $ sl >= 1 && sl <= length ls
                  let src    = ls !! (sl - 1)
                      gutter = showt sl
                      blankG = T.replicate (T.length gutter) " "
                      endCol = if el == sl then ec else T.length src + 1
                      n      = max 1 $ endCol - sc       -- width of the highlight
                      before = T.take (sc - 1) src
                      under  = T.take n $ T.drop (sc - 1) src
                      after  = T.drop (sc - 1 + n) src
                      caret  = T.replicate (max 0 $ sc - 1) " " <> T.replicate n "^"
                  pure $ vsep [ styled gutterStyle $ blankG <> " |"
                              , styled gutterStyle (gutter <> " | ") <> text before <> styled (sevStyle sev) under <> text after
                              , styled gutterStyle (blankG <> " | ") <> styled (sevStyle sev) caret ]

-- | Like 'diagBlock' but a plain, colour-free 'Doc' with no source excerpt,
--   for the pure 'Pretty' rendering (e.g. round-trip test failures).
plainDiag :: Severity -> Annote -> Text -> Doc ann
plainDiag sev an msg = vsep $ header : [ text "  " <> pretty l | l <- T.lines $ normalizeMsg msg ]
      where header = maybe sevDoc (\ t -> text t <+> sevDoc) $ locHeaderText an
            sevDoc = text $ sevLabel sev <> ":"

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
      | otherwise    = printDiag (conf ^. loadPath) SevWarning (toAnnote an) msg [] []

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
      where relocated = AstError (toAnnote to) msg ((from, "In code expanded here:") : labels) hints

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

-- | Print an error to stderr, GHC-style, with a source excerpt and a caret
--   under the offending region (plus any secondary "note" blocks) when the
--   files can be found on the load path.
printError :: MonadIO m => [FilePath] -> AstError -> m ()
printError dirs (AstError an msg labels hints) = printDiag dirs SevError an msg labels hints

-- | Render a diagnostic (its header, indented message, source excerpt, and any
--   secondary "note" blocks for labels and hints) to stderr, coloured when
--   stderr is a terminal. Source files are looked up along the search path.
printDiag :: MonadIO m => [FilePath] -> Severity -> Annote -> Text -> [Label] -> [Text] -> m ()
printDiag dirs sev an msg labels hints = liftIO $ do
      primary <- blockWithSource dirs sev an $ appendContext msg $ annContext an
      -- Drop labels that resolve to the primary location (a "note" pointing at
      -- the error itself is just noise), and collapse labels that share a
      -- location into one (e.g. the relocation note and an explicit "first used
      -- here" landing on the same spot).
      labelBs <- mapM (uncurry $ blockWithSource dirs SevNote)
                       $ nubBy ((==) `on` (primSpan . fst))
                       $ filter ((primSpan an /=) . primSpan . fst) labels
      let hintBs = map (\ h -> diagBlock SevNote noAnn h Nothing) hints
      colour  <- hIsTerminalDevice stderr
      T.hPutStr stderr $ (if colour then doc2Colour else doc2Text) (vsep $ primary : labelBs <> hintBs) <> "\n\n"

-- | A 'diagBlock' with the source lines read from the load path.
blockWithSource :: [FilePath] -> Severity -> Annote -> Text -> IO (Doc AnsiStyle)
blockWithSource dirs sev an msg = do
      msrc <- maybe (pure Nothing) (locateSource dirs . spanFile) $ primSpan an
      pure $ diagBlock sev an msg msrc

-- | Read a source file, trying it directly then relative to each search dir.
locateSource :: [FilePath] -> FilePath -> IO (Maybe [Text])
locateSource dirs f = go $ f : map (</> f) dirs
      where go :: [FilePath] -> IO (Maybe [Text])
            go []       = pure Nothing
            go (c : cs) = do
                  ex <- doesFileExist c
                  if ex then Just . T.lines <$> T.readFile c else go cs

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

-- | Render with ANSI colour escapes (for a terminal).
doc2Colour :: Doc AnsiStyle -> Text
doc2Colour = Term.renderStrict . layoutSmart defaultLayoutOptions
