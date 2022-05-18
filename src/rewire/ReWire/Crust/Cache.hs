{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Crust.Cache
      ( runCache
      , getProgram
      , LoadPath
      , printInfo
      , printInfoHSE
      , printHeader
      ) where

import ReWire.Annotation
import ReWire.Error
import ReWire.Crust.Annotate
import ReWire.Crust.Desugar
import ReWire.Crust.KindCheck
import ReWire.Crust.Parse
import ReWire.Crust.PrimBasis
import ReWire.Crust.Purify
import ReWire.Crust.Rename
import ReWire.Crust.Syntax
import ReWire.Crust.ToCore
import ReWire.Crust.ToCrust
import ReWire.Crust.Transform
import ReWire.Crust.TypeCheck (typeCheck, untype)
import ReWire.Unbound (runFreshMT, FreshMT (..))
import ReWire.Pretty
import ReWire.Flags (Flag (..))

import Control.Arrow ((***))
import Control.Monad ((>=>), msum, void, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (runReaderT, ReaderT, asks)
import Control.Monad.State.Strict (runStateT, StateT, MonadState (..), modify, lift)
import Data.Containers.ListUtils (nubOrd)
import Data.List.Split (splitOn)
import Data.Text (Text, pack)
import System.FilePath ((</>), takeDirectory)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import TextShow (showt)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict          as Map
import qualified Language.Haskell.Exts.Syntax as S (Module (..))
import qualified ReWire.Core.Syntax           as Core
import qualified Language.Haskell.Exts.Pretty as P

import Language.Haskell.Exts.Syntax hiding (Annotation, Exp, Module (..), Namespace, Name, Kind)

type Cache = ReaderT LoadPath (StateT ModCache (FreshMT (SyntaxErrorT AstError IO)))
type LoadPath = [FilePath]
type ModCache = HashMap FilePath (Module, Exports)

runCache :: Cache a -> LoadPath -> SyntaxErrorT AstError IO a
runCache m lp = fst <$> runFreshMT (runStateT (runReaderT m lp) mempty)

mkRenamer :: [Flag] -> FilePath -> S.Module SrcSpanInfo -> Cache Renamer
mkRenamer flags pwd' m = extendWithGlobs m . mconcat <$> mapM mkRenamer' (getImps m)
      where mkRenamer' :: ImportDecl SrcSpanInfo -> Cache Renamer
            mkRenamer' (ImportDecl _ (void -> m) quald _ _ _ (fmap void -> as) specs) = do
                  (_, exps) <- getModule flags pwd' $ toFilePath m
                  fromImps m quald exps as specs

-- Pass 1    Parse.
-- Pass 2-4  Fixity fixing (uniquify + fix + deuniquify, because bug in applyFixities).
-- Pass 5    Annotate.
-- Pass 6-14 Desugar.
-- Pass 15   Translate to mantle + rename globals.
-- Pass 16   Translate to core

getModule :: [Flag] -> FilePath -> FilePath -> Cache (Module, Exports)
getModule flags pwd fp = pDebug flags ("fetching module: " <> pack fp <> " (pwd: " <> pack pwd <> ")") >> Map.lookup fp <$> get >>= \ case
      Just p  -> pure p
      Nothing -> do
            modify $ Map.insert fp mempty

            lp         <- asks (pwd :)

            mmods      <- mapM (tryParseInDir fp) lp
            -- FIXME: The directory crawling could be more robust here. (Should
            -- use exception handling.)
            (pwd', m)  <- maybe
                              (failAt (filePath fp) "File not found in load-path")
                              (pure . (elideDot *** addMainModuleHead))
                        $ msum mmods

            rn         <- mkRenamer flags pwd' m
            imps       <- loadImports pwd' m

            -- Phase 1 (haskell-src-exts) transformations.
            (m', exps) <- pure
                      >=> pDebug' "Fixing fixity."
                      >=> lift . lift . fixFixity rn
                      >=> pDebug' "Annotating."
                      >=> annotate
                      >=> pDebug' "[Pass 1] Pre-desugaring."
                      >=> whenSet FlagDHask1 (printInfoHSE "Haskell 1: Pre-desugaring" rn imps)
                      >=> pDebug' "Desugaring."
                      >=> desugar rn
                      >=> pDebug' "[Pass 2] Post-desugaring."
                      >=> whenSet FlagDHask2 (printInfoHSE "Haskell 2: Post-desugaring" rn imps)
                      >=> pDebug' "Translating to crust."
                      >=> toCrust rn
                      $ m

            let Module ts syns ds = m' <> imps
            _ <- whenSet FlagDCrust0 (printInfo $ "Crust 0: Synthetic per-module: " <> pack fp) (ts, syns, ds)

            modify $ Map.insert fp (m' <> imps, exps)
            pure (m' <> imps, exps)

      where loadImports :: Annotation a => FilePath -> S.Module a -> Cache Module
            loadImports pwd' = fmap mconcat . mapM (fmap fst . getModule flags pwd' . toFilePath . void . importModule) . getImps

            whenSet :: Applicative m => Flag -> (Bool -> a -> m a) -> a -> m a
            whenSet f m = if f `elem` flags then m $ FlagV `elem` flags else pure

            pDebug' :: MonadIO m => Text -> a -> m a
            pDebug' s a = pDebug flags (pack fp <> ": " <> s) >> pure a

            elideDot :: FilePath -> FilePath
            elideDot = \ case
                  "." -> takeDirectory fp
                  d   -> d </> takeDirectory fp

-- Phase 2 (pre-core) transformations.
getProgram :: [Flag] -> FilePath -> Cache Core.Program
getProgram flags fp = do
      (Module ts syns ds,  _)  <- getModule flags "." fp

      p <- pure
       >=> pDebug' "[Pass 3] Adding primitives and inlining."
       >=> whenSet FlagDCrust1 (printInfo "Crust 1: Post-desugaring")
       >=> pure . addPrims
       >=> inline
       >=> prePurify
       >=> pDebug' "Expanding type synonyms and simplifying."
       >=> expandTypeSynonyms
       >=> pDebug' "[Pass 4] Post-inlining, before typechecking."
       >=> whenSet FlagDCrust2 (printInfo "Crust 2: Post-inlining")
       >=> pDebug' "Typechecking."
       >=> kindCheck >=> typeCheck
       >=> pDebug' "[Pass 4b] Post-typechecking."
       >=> whenSet FlagDCrust2b (printInfo "Crust 2b: Post-typechecking")
       >=> pDebug' "Simplifying and reducing."
       >=> pure . purgeUnused start
       >=> simplify
       >=> shiftLambdas
       >=> pDebug' "Lifting lambdas (pre-purification)."
       >=> liftLambdas
       >=> pDebug' "Removing unused definitions."
       >=> pure . purgeUnused start
       >=> pDebug' "[Pass 5] Pre-purification."
       >=> whenSet FlagDCrust3 (printInfo "Crust 3: Pre-purification")
       >=> pDebug' "Purifying."
       >=> purify start
       >=> pDebug' "[Pass 6] Post-purification."
       >=> whenSet FlagDCrust4 (printInfo "Crust 4: Post-purification")
       >=> pDebug' "Lifting lambdas (post-purification)."
       >=> liftLambdas
       >=> pDebug' "Fully apply global function definitions."
       >=> fullyApplyDefs
       >=> pDebug' "Removing unused definitions (again)."
       >=> pure . purgeUnused start
       >=> pDebug' "[Pass 7] Post-purification."
       >=> whenSet FlagDCrust5 (printInfo "Crust 5: Post-second-lambda-lifting")
       >=> pDebug' "Translating to core & HDL."
       >=> toCore start (concatMap getInputNames flags) (concatMap getOutputNames flags) (concatMap getStateNames flags)
       >=> pDebug' "[Pass 8] Core."
       $ (ts, syns, ds)

      when (FlagDCore1 `elem` flags) $ liftIO $ do
            printHeader "Core"
            T.putStrLn $ prettyPrint p
            when (FlagV `elem` flags) $ T.putStrLn "\n## Show core:\n"
            when (FlagV `elem` flags) $ T.putStrLn $ showt $ unAnn p

      pure p

      where whenSet :: Applicative m => Flag -> (Bool -> a -> m a) -> a -> m a
            whenSet f m = if f `elem` flags then m $ FlagV `elem` flags else pure

            pDebug' :: MonadIO m => Text -> a -> m a
            pDebug' s a = pDebug flags s >> pure a

            getInputNames :: Flag -> [Text]
            getInputNames = \ case
                  FlagInputNames [] -> []
                  FlagInputNames ns -> map pack $ splitOn "," ns
                  _                 -> []

            getOutputNames :: Flag -> [Text]
            getOutputNames = \ case
                  FlagOutputNames [] -> []
                  FlagOutputNames ns -> map pack $ splitOn "," ns
                  _                  -> []

            getStateNames :: Flag -> [Text]
            getStateNames = \ case
                  FlagStateNames []  -> []
                  FlagStateNames sts -> map pack $ splitOn "," sts
                  _                  -> []

            start :: Text
            start = case filter isFlagTopLevel flags of
                  FlagTopLevel s : _ -> pack s
                  _                  -> "Main.start"

            isFlagTopLevel :: Flag -> Bool
            isFlagTopLevel = \ case
                  FlagTopLevel _ -> True
                  _              -> False

pDebug :: MonadIO m => [Flag] -> Text -> m ()
pDebug flags s = when (FlagV `elem` flags) $ liftIO $ T.putStrLn $ "Debug: " <> s

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
