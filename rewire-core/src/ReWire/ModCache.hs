{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.ModCache
      ( runCache
      , getProgram
      , LoadPath
      , printInfo
      , printInfoHSE
      , printHeader
      ) where

import ReWire.Annotation (Annotation, SrcSpanInfo, unAnn)
import ReWire.Config (Config, verbose, dump, loadPath)
import ReWire.Crust.KindCheck (kindCheck)
import ReWire.Crust.PrimBasis (addPrims)
import ReWire.Crust.Purify (purify)
import ReWire.Crust.Syntax (FreeProgram, Defn (..), Module (Module), Exp, Ty, Kind, DataConId, TyConId, builtins, Program (Program), prettyFP)
import ReWire.Crust.ToCore (toCore)
import ReWire.Crust.Transform (removeMain, simplify, liftLambdas, purgeUnused, fullyApplyDefs, shiftLambdas, neuterExterns, expandTypeSynonyms, inline, prePurify)
import ReWire.Crust.TypeCheck (typeCheck, untype)
import ReWire.Error (failAt, AstError, MonadError, filePath)
import ReWire.HSE.Annotate (annotate)
import ReWire.HSE.Desugar (desugar, addMainModuleHead)
import ReWire.HSE.Parse (tryParseInDir)
import ReWire.HSE.Rename (Exports, Renamer, fromImps, allExports, toFilePath, fixFixity)
import ReWire.HSE.ToCrust (extendWithGlobs, toCrust, getImps)
import ReWire.Pretty (prettyPrint, prettyPrint', showt)
import ReWire.Unbound (fv, trec, runFreshMT, FreshMT, Name)

import Control.Lens ((^.))
import Control.Arrow ((***))
import Control.Monad ((>=>), msum, void, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State.Strict (runStateT, StateT, MonadState (..), modify, lift)
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text, pack)
import Language.Haskell.Exts.Syntax hiding (Annotation, Exp, Module (..), Namespace, Name, Kind)
import Numeric.Natural (Natural)
import System.FilePath ((</>), takeDirectory)

import qualified Data.HashMap.Strict          as Map
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Language.Haskell.Exts.Pretty as P
import qualified Language.Haskell.Exts.Syntax as S (Module (..))
import qualified ReWire.Core.Syntax           as Core
import qualified ReWire.Config                as C

type Cache m = StateT ModCache (FreshMT m)
type LoadPath = [FilePath]
type ModCache = HashMap FilePath (Module, Exports)

runCache :: (MonadIO m, MonadError AstError m) => Cache m a -> m a
runCache m = fst <$> runFreshMT (runStateT m mempty)

mkRenamer :: (MonadFail m, MonadIO m, MonadError AstError m, MonadState AstError m) => Config -> FilePath -> S.Module SrcSpanInfo -> Cache m Renamer
mkRenamer conf pwd' m = extendWithGlobs m . mconcat <$> mapM mkRenamer' (getImps m)
      where mkRenamer' :: (MonadFail m, MonadIO m, MonadError AstError m, MonadState AstError m) => ImportDecl SrcSpanInfo -> Cache m Renamer
            mkRenamer' (ImportDecl _ (void -> m) quald _ _ _ (fmap void -> as) specs) = do
                  (_, exps) <- getModule conf pwd' $ toFilePath m
                  fromImps m quald exps as specs

-- Pass 1    Parse.
-- Pass 2-4  Fixity fixing (uniquify + fix + deuniquify, because bug in applyFixities).
-- Pass 5    Annotate.
-- Pass 6-14 Desugar.
-- Pass 15   Translate to crust + rename globals.
-- Pass 16   Translate to core

getModule :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m) => Config -> FilePath -> FilePath -> Cache m (Module, Exports)
getModule conf pwd fp = pDebug conf ("Fetching module: " <> pack fp <> " (pwd: " <> pack pwd <> ")") >> Map.lookup fp <$> get >>= \ case
      Just p  -> pure p
      Nothing -> do
            modify $ Map.insert fp mempty

            let lp     = pwd : conf^.loadPath

            mmods      <- mapM (tryParseInDir fp) lp
            -- FIXME: The directory crawling could be more robust here. (Should
            -- use exception handling.)
            (pwd', m)  <- maybe
                              (failAt (filePath fp) "File not found in load-path")
                              (pure . (elideDot *** addMainModuleHead))
                        $ msum mmods

            rn         <- mkRenamer conf pwd' m
            imps       <- loadImports pwd' m

            -- Phase 1 (haskell-src-exts) transformations.
            (m', exps) <- pure
                      >=> pDebug' "Fixing fixity."
                      >=> lift . lift . fixFixity rn
                      >=> pDebug' "Annotating."
                      >=> pure . annotate
                      >=> pDebug' "[Pass 1] Pre-desugaring."
                      >=> whenDump 1 (printInfoHSE "[Pass 1] Haskell: Pre-desugaring" rn imps)
                      >=> pDebug' "Desugaring."
                      >=> desugar rn
                      >=> pDebug' "[Pass 2] Post-desugaring."
                      >=> whenDump 2 (printInfoHSE "[Pass 2] Haskell: Post-desugaring" rn imps)
                      >=> pDebug' "Translating to crust."
                      >=> toCrust rn
                      $ m

            let Module ts syns ds = m' <> imps
            _ <- whenDump 3 (printInfo $ "[Pass 3] Crust: Synthetic per-module: " <> pack fp) (ts, syns, ds)

            modify $ Map.insert fp (m' <> imps, exps)
            pure (m' <> imps, exps)

      where loadImports :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m, Annotation a) => FilePath -> S.Module a -> Cache m Module
            loadImports pwd' = fmap mconcat . mapM (fmap fst . getModule conf pwd' . toFilePath . void . importModule) . getImps

            whenDump :: Applicative m => Natural -> (Bool -> a -> m a) -> a -> m a
            whenDump n m = if (conf^.dump) n then m $ conf^.verbose else pure

            pDebug' :: MonadIO m => Text -> a -> m a
            pDebug' s a = pDebug conf (pack fp <> ": " <> s) >> pure a

            elideDot :: FilePath -> FilePath
            elideDot = \ case
                  "." -> takeDirectory fp
                  d   -> d </> takeDirectory fp

-- Phase 2 (pre-core) transformations.
getProgram :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m) => Config -> FilePath -> Cache m Core.Program
getProgram conf fp = do
      (Module ts syns ds,  _)  <- getModule conf "." fp

      p <- pure
       >=> pDebug' "[Pass 4] Adding primitives and inlining."
       >=> whenDump 4 (printInfo "[Pass 4] Crust: Post-desugaring")
       >=> pure . addPrims
       >=> pDebug' "Removing the Main.main definition (before attempting to typecheck it)."
       >=> pure . removeMain
       >=> inline
       >=> prePurify
       >=> pDebug' "Expanding type synonyms and simplifying."
       >=> expandTypeSynonyms
       >=> pDebug' "[Pass 5] Post-inlining, before typechecking."
       >=> whenDump 5 (printInfo "[Pass 5] Crust: Post-inlining")
       >=> pDebug' "Typechecking."
       >=> kindCheck >=> typeCheck start
       >=> pDebug' "[Pass 6] Post-typechecking."
       >=> whenDump 6 (printInfo "[Pass 6] Crust: Post-typechecking")
       >=> pDebug' "Simplifying and reducing."
       >=> pDebug' "Removing type annotations."
       >=> pDebug' "Removing Haskell definitions for externs."
       >=> pure . neuterExterns
       >=> pDebug' "Removing unused definitions."
       >=> pure . purgeUnused (start : (fst <$> builtins))
       >=> pDebug' "[Pass 7] Pre-simplification."
       >=> whenDump 7 (printInfo "[Pass 7] Crust: Pre-simplify")
       >=> pDebug' "Partially evaluating and reducing."
       >=> simplify conf
       >=> pDebug' "[Pass 8] Post-simplification."
       >=> whenDump 8 (printInfo "[Pass 8] Crust: Post-simplify")
       >=> pDebug' "Lifting lambdas (pre-purification)."
       >=> shiftLambdas >=> liftLambdas
       >=> pDebug' "Removing unused definitions (again)."
       >=> pure . purgeUnused (start : (fst <$> builtins))
       >=> pDebug' "[Pass 9] Pre-purification."
       >=> whenDump 9 (printInfo "[Pass 9] Crust: Pre-purification")
       >=> pDebug' "Purifying."
       >=> purify start
       >=> pDebug' "[Pass 10] Post-purification."
       >=> whenDump 10 (printInfo "[Pass 10] Crust: Post-purification")
       >=> pDebug' "Lifting lambdas (post-purification)."
       >=> liftLambdas
       >=> pDebug' "Fully apply global function definitions."
       >=> fullyApplyDefs
       >=> pDebug' "Removing unused definitions (again)."
       >=> pure . purgeUnused [start]
       >=> pDebug' "[Pass 11] Post-purification."
       >=> whenDump 11 (printInfo "[Pass 11] Crust: Post-second-lambda-lifting")
       -- >=> pDebug' "Substituting the unit/nil type for remaining free type variables."
       -- >=> pure . freeTyVarsToNil
       >=> pDebug' "Translating to core & HDL."
       >=> toCore conf start
       >=> pDebug' "[Pass 12] Core."
       $ (ts, syns, ds)

      when ((conf^.dump) 12) $ liftIO $ do
            printHeader "[Pass 12] Core"
            T.putStrLn $ prettyPrint p
            when (conf^.verbose) $ do
                  T.putStrLn "\n## Show core:\n"
                  T.putStrLn $ showt $ unAnn p

      pure p

      where whenDump :: Applicative m => Natural -> (Bool -> a -> m a) -> a -> m a
            whenDump n m = if (conf^.dump) n then m $ conf^.verbose else pure

            pDebug' :: MonadIO m => Text -> a -> m a
            pDebug' s a = pDebug conf s >> pure a

            start :: Text
            start = conf^.C.start

pDebug :: MonadIO m => Config -> Text -> m ()
pDebug conf s = when (conf^.verbose) $ liftIO $ T.putStrLn $ "Debug: " <> s

printHeader :: MonadIO m => Text -> m ()
printHeader hd = do
      liftIO $ T.putStrLn   "# ======================================="
      liftIO $ T.putStrLn $ "# " <> hd
      liftIO $ T.putStrLn   "# =======================================\n"

printInfo :: MonadIO m => Text -> Bool -> FreeProgram -> m FreeProgram
printInfo hd verbose fp = do
      let p = Program $ trec fp
      printHeader hd
      when verbose $ liftIO $ T.putStrLn "## Free kind vars:\n"
      when verbose $ liftIO $ T.putStrLn $ T.concat $ map (<> "\n") (nubOrd $ map prettyPrint (fv p :: [Name Kind]))
      when verbose $ liftIO $ T.putStrLn "## Free type vars:\n"
      when verbose $ liftIO $ T.putStrLn $ T.concat $ map (<> "\n") (nubOrd $ map prettyPrint (fv p :: [Name Ty]))
      when verbose $ liftIO $ T.putStrLn "## Free tycon vars:\n"
      when verbose $ liftIO $ T.putStrLn $ T.concat $ map (<> "\n") (nubOrd $ map prettyPrint (fv p :: [Name TyConId]))
      liftIO $ T.putStrLn "## Free con vars:\n"
      liftIO $ T.putStrLn $ T.concat $ map (<> "\n") (nubOrd $ map prettyPrint (fv p :: [Name DataConId]))
      liftIO $ T.putStrLn "## Free exp vars:\n"
      liftIO $ T.putStrLn $ T.concat $ map (<> "\n") (nubOrd $ map prettyPrint (fv p :: [Name Exp]))
      liftIO $ T.putStrLn "## Program:\n"
      liftIO $ T.putStrLn $ prettyPrint' $ prettyFP $ if verbose then fp else untype' fp
      when verbose $ liftIO $ T.putStrLn "\n## Program (show):\n"
      when verbose $ liftIO $ T.putStrLn $ showt $ unAnn fp
      pure fp

      where untype' :: FreeProgram -> FreeProgram
            untype' (ts, syns, vs) = (ts, syns, map untype'' vs)

            untype'' :: Defn -> Defn
            untype'' d = d { defnBody = untype $ defnBody d }

printInfoHSE :: MonadIO m => Text -> Renamer -> Module -> Bool -> S.Module a -> m (S.Module a)
printInfoHSE hd rn imps verbose hse = do
      printHeader hd
      when verbose $ liftIO $ T.putStrLn "\n## Renamer:\n"
      when verbose $ liftIO $ T.putStrLn $ showt rn
      when verbose $ liftIO $ T.putStrLn "\n## Exports:\n"
      when verbose $ liftIO $ T.putStrLn $ showt $ allExports rn
      when verbose $ liftIO $ T.putStrLn "\n## Show imps:\n"
      when verbose $ liftIO $ T.putStrLn $ showt imps
      when verbose $ liftIO $ T.putStrLn "\n## Show HSE mod:\n"
      when verbose $ liftIO $ T.putStrLn $ showt $ void hse
      when verbose $ liftIO $ T.putStrLn "\n## Pretty HSE mod:\n"
      liftIO $ putStrLn $ P.prettyPrint $ void hse
      pure hse
