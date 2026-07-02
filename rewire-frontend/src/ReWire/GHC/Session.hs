{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | The GHC-driven front end, part one: an in-process GHC session that
--   drives parse -> rename -> typecheck -> desugar over the whole home
--   module graph and hands back the (-O0, pre-simplifier) desugared Core,
--   one 'ModGuts' per home module in dependency order.
--
--   Wrinkles this module exists to handle:
--
--   * The desugarer runs GHC's simple optimizer unconditionally, and its
--     occurrence analysis drops top-level binds unreachable from an
--     /exported/ binder -- even at -O0. A file with no module header is
--     implicitly @module Main (main) where@, so everything but @main@
--     would vanish. 'ensureLiveExports' rewrites the parsed module before
--     typechecking: headerless modules become export-all.
--
--   * @mg_binds@ is strictly per-module: the target's guts contain no code
--     from imported modules (including all of rewire-user, compiled from
--     source via the loadpath). So every home module is desugared, not
--     just the target.
--
--   * GHC reports errors as 'SourceError' exceptions (from the per-module
--     phases) or through the log action (during 'load'); warnings only via
--     the log action. Both are re-routed through ReWire's 'AstError' /
--     'warnAt' machinery so @-w@/@-Werror@ and the usual error formatting
--     (filename included) keep working.
module ReWire.GHC.Session (loadCore, dumpCore) where

import ReWire.Annotation (Annote, noAnn, srcAnnote)
import ReWire.Config (Config, loadPath, start, verbose, pDebug)
import ReWire.Error (AstError, MonadError, failAt, warnAt)

import Control.Lens ((^.))
import Control.Monad (forM, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Containers.ListUtils (nubOrdOn)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Text (Text)
import System.FilePath (takeDirectory)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import GHC
      ( runGhc, getSessionDynFlags, setSessionDynFlags
      , guessTarget, setTargets, load, LoadHowMuch (..), SuccessFlag (..)
      , getModuleGraph
      , parseModule, typecheckModule, desugarModule, coreModule
      , ParsedModule (..), ModSummary (..)
      , DynFlags (..), GhcLink (..)
      , mgModSummaries
      , moduleName, moduleNameString, mkModuleName
      )
import GHC.Core (CoreBind, Bind (..))
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (unpackFS)
import GHC.Driver.Backend (noBackend)
import GHC.Driver.Monad (pushLogHookM)
import GHC.Driver.Session (updOptLevel)
import GHC.Hs (HsModule (..), GhcPs, IE (..), LIE, IEWrappedName (..), noExtField)
import GHC.Hs.ImpExp (ieNames)
import GHC.Parser.Annotation (noLocA)
import GHC.Paths (libdir)
import GHC.Types.Basic (mkIntWithInf)
import GHC.Types.Error (getMessages, errMsgSpan, Severity (..), MessageClass (..))
import GHC.Utils.Error (pprLocMsgEnvelopeDefault)
import GHC.Types.Name (nameOccName, occNameString, mkVarOcc)
import GHC.Types.Name.Reader (mkRdrUnqual, rdrNameOcc)
import GHC.Types.SourceError (SourceError, srcErrorMessages, handleSourceError)
import GHC.Types.SrcLoc (SrcSpan (..), GenLocated (..), srcSpanFile, srcSpanStartLine, srcSpanStartCol, srcSpanEndLine, srcSpanEndCol)
import GHC.Types.Var (Var, varName)
import GHC.Unit.Module.ModGuts (ModGuts (..))
import GHC.Utils.Logger (LogAction)
import GHC.Utils.Outputable (showSDocUnsafe, vcat, ppr)
import GHC.Utils.Panic (handleGhcException)

-- | A diagnostic collected from GHC's log action: is-error, location, rendered text.
type Diag = (Bool, Annote, Text)

-- | Load Core for the whole home module graph and dump it (a per-module
--   binder summary; the full Core under -v). For debugging the session.
dumpCore :: (MonadError AstError m, MonadIO m) => Config -> FilePath -> m ()
dumpCore conf fp = do
      gutss <- loadCore conf fp
      liftIO $ mapM_ pr gutss
      where pr :: ModGuts -> IO ()
            pr guts = do
                  T.putStrLn $ "-- ## GHC Core [" <> T.pack (moduleNameString $ moduleName $ mg_module guts) <> "]: "
                        <> T.intercalate ", " (map (T.pack . occNameString . nameOccName . varName) $ concatMap binders $ mg_binds guts)
                  when (conf^.verbose) $ T.putStrLn $ T.pack $ showSDocUnsafe $ ppr $ mg_binds guts

            binders :: CoreBind -> [Var]
            binders = \ case
                  NonRec b _ -> [b]
                  Rec bs     -> map fst bs

-- | Load @fp@ (and, recursively, its imports from the loadpath) through GHC,
--   returning the -O0 desugared Core of every home module. GHC errors are
--   re-raised as 'AstError's; GHC warnings are re-emitted through 'warnAt'.
loadCore :: (MonadError AstError m, MonadIO m) => Config -> FilePath -> m [ModGuts]
loadCore conf fp = do
      pDebug conf $ "GHC session: libdir: " <> T.pack libdir
      diags <- liftIO $ newIORef ([] :: [Diag])
      -- GhcExceptions (panics, usage errors like a missing plugin package
      -- when GHC_PACKAGE_PATH is unset) are IO exceptions, not SourceErrors.
      r     <- liftIO $ handleGhcException (pure . Left . (noAnn,) . ("ghc-frontend: " <>) . T.pack . show)
                  $ runGhc (Just libdir) $ handleSourceError (pure . Left . renderSourceError) $ do
            df0 <- getSessionDynFlags
            setSessionDynFlags $ configure conf fp df0
            pushLogHookM $ const $ logHook diags
            t   <- guessTarget fp Nothing Nothing
            setTargets [t]
            ok  <- load LoadAllTargets
            case ok of
                  Failed    -> pure $ Left (noAnn, "") -- Errors are in the log; see below.
                  Succeeded -> do
                        g     <- getModuleGraph
                        gutss <- forM (mgModSummaries g) $ \ ms -> do
                              pm  <- parseModule ms
                              tcm <- typecheckModule $ ensureLiveExports conf ms pm
                              dsm <- desugarModule tcm
                              pure $ coreModule dsm
                        pure $ Right gutss
      -- The graph is typechecked twice (once by load, once per-module for
      -- desugaring), so identical diagnostics arrive twice: dedupe (the
      -- rendered text embeds the location, so the key is location-aware).
      ds <- nubOrdOn (\ (e, _, m) -> (e, m)) . reverse <$> liftIO (readIORef diags)
      mapM_ (\ (_, an, m) -> warnAt conf an m) $ filter (not . isError) ds
      case r of
            Right gutss -> do
                  unless (any (definesStart conf) gutss) $ failAt noAnn
                        $ "ghc-frontend: no definition for the start symbol ("
                        <> conf^.start <> ") found in the compiled modules."
                  pure gutss
            Left (an, msg)
                  | not (T.null msg)                -> failAt an msg
                  | (e : _) <- filter isError ds    -> failAt (dAn e)
                        $ T.intercalate "\n" $ map dMsg $ filter isError ds
                  | otherwise                       -> failAt noAnn
                        "ghc-frontend: GHC failed to load the program (no diagnostics)."
      where isError :: Diag -> Bool
            isError (e, _, _) = e

            dAn :: Diag -> Annote
            dAn (_, an, _) = an

            dMsg :: Diag -> Text
            dMsg (_, _, m) = m

-- | DynFlags for the ReWire session: no code generation, no optimization
--   (-O0 keeps the reactive class selectors recognizable and the extern
--   descriptor literals local), loadpath as import paths, and the three
--   typelits typechecker plugins (which must be listed here: per-module
--   OPTIONS_GHC -fplugin pragmas are not honored on the API typecheckModule
--   path). The target's own directory heads the import paths, mirroring the
--   HSE loader's search order (Cache.hs: the importing module's directory
--   comes before the loadpath).
configure :: Config -> FilePath -> DynFlags -> DynFlags
configure conf fp df = (updOptLevel 0 df)
      { backend        = noBackend
      , ghcLink        = NoLink
      , importPaths    = takeDirectory fp : conf^.loadPath <> importPaths df
      , pluginModNames = map mkModuleName
            [ "GHC.TypeLits.Normalise"
            , "GHC.TypeLits.KnownNat.Solver"
            , "GHC.TypeLits.Extra.Solver"
            ]
      , reductionDepth = mkIntWithInf 1000
      }

-- | Rewrite the parsed module so the program survives the desugarer's
--   export-driven dead-code elimination (see the module comment):
--
--   * a headerless module (implicitly @module Main (main)@) is named from
--     its summary and made export-all -- both steps are required, because
--     GHC keys the implicit-@(main)@-export behavior on the header being
--     absent, so clearing the export list alone changes nothing;
--
--   * the start module with an explicit export list that omits the start
--     symbol gets an export for it spliced in (occurrence analysis then
--     keeps its whole reachable closure). Other explicit export lists are
--     left alone -- GHC's export diagnostics already fired during 'load',
--     which sees the original source.
ensureLiveExports :: Config -> ModSummary -> ParsedModule -> ParsedModule
ensureLiveExports conf ms pm = case (hsmodName m, hsmodExports m) of
      (Nothing, _)          -> pm { pm_parsed_source = L l m { hsmodName    = Just $ noLocA $ moduleName $ ms_mod ms
                                                             , hsmodExports = Nothing
                                                             } }
      (Just (L _ mn), Just (L le ies))
            | moduleNameString mn == startMod, not (any exportsStart ies)
                              -> pm { pm_parsed_source = L l m { hsmodExports = Just $ L le $ ies <> [startIE] } }
      _                     -> pm
      where L l m = pm_parsed_source pm

            startMod, startOcc :: String
            (startMod, startOcc) = splitStart $ conf^.start

            exportsStart :: LIE GhcPs -> Bool
            exportsStart (L _ ie) = case ie of
                  IEModuleContents _ (L _ mn') -> moduleNameString mn' == startMod
                  _                            -> any ((== mkVarOcc startOcc) . rdrNameOcc) $ ieNames ie

            startIE :: LIE GhcPs
            startIE = noLocA $ IEVar Nothing (noLocA $ IEName noExtField $ noLocA $ mkRdrUnqual $ mkVarOcc startOcc) Nothing

-- | Does any bind in the guts define the (unqualified) start symbol in the
--   start module?
definesStart :: Config -> ModGuts -> Bool
definesStart conf guts = moduleNameString (moduleName (mg_module guts)) == startMod
      && any ((== startOcc) . occNameString . nameOccName . varName) (concatMap binders (mg_binds guts))
      where startMod, startOcc :: String
            (startMod, startOcc) = splitStart $ conf^.start

            binders :: CoreBind -> [Var]
            binders = \ case
                  NonRec b _ -> [b]
                  Rec bs     -> map fst bs

-- | Split a qualified start symbol ("Main.start") into module and
--   occurrence parts.
splitStart :: Text -> (String, String)
splitStart s = case T.breakOnEnd "." s of
      ("", occ) -> ("Main", T.unpack occ)
      (m, occ)  -> (T.unpack $ T.dropEnd 1 m, T.unpack occ)

-- | Render a SourceError (thrown by the per-module parse/typecheck/desugar
--   phases) to an annotated message.
renderSourceError :: SourceError -> (Annote, Text)
renderSourceError e = (an, T.pack $ showSDocUnsafe $ vcat $ map pprLocMsgEnvelopeDefault envs)
      where envs = bagToList $ getMessages $ srcErrorMessages e
            an   = case envs of
                  (env : _) -> spanAnnote $ errMsgSpan env
                  _         -> noAnn

-- | The log hook: collect warnings and errors (errors reach the log action
--   during 'load'; they do not raise SourceError there).
logHook :: IORef [Diag] -> LogAction
logHook diags _lf mc loc doc = case mc of
      MCDiagnostic SevError _ _   -> modifyIORef' diags ((True, spanAnnote loc, msg) :)
      MCDiagnostic SevWarning _ _ -> modifyIORef' diags ((False, spanAnnote loc, msg) :)
      _                           -> pure ()
      where msg :: Text
            msg = T.pack (showSDocUnsafe (ppr loc)) <> ": " <> T.pack (showSDocUnsafe doc)

spanAnnote :: SrcSpan -> Annote
spanAnnote = \ case
      RealSrcSpan rs _ -> srcAnnote (unpackFS $ srcSpanFile rs)
                                    (srcSpanStartLine rs, srcSpanStartCol rs)
                                    (srcSpanEndLine rs, srcSpanEndCol rs)
      UnhelpfulSpan _  -> noAnn
