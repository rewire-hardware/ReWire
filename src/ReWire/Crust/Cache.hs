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
import ReWire.Crust.TypeCheck
import ReWire.Crust.TypeVerify
import ReWire.Unbound (runFreshMT, FreshMT (..))
import ReWire.Pretty
import ReWire.Flags (Flag (..))

import Control.Monad ((>=>), msum, void, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (runReaderT, ReaderT, MonadReader (..))
import Control.Monad.State.Strict (runStateT, StateT, MonadState (..), modify, lift)
import Data.Containers.ListUtils (nubOrd)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import TextShow (showt)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict          as Map
import qualified Language.Haskell.Exts.Syntax as S (Module (..))
import qualified ReWire.Core.Syntax           as Core
import qualified Language.Haskell.Exts.Pretty as P

import Language.Haskell.Exts.Syntax hiding (Annotation, Exp, Module (..), Namespace, Name, Kind)

type Cache = ReaderT LoadPath (StateT ModCache (FreshMT (SyntaxErrorT IO)))
type LoadPath = [FilePath]
type ModCache = HashMap FilePath (Module, (Module, Exports))

runCache :: Cache a -> LoadPath -> SyntaxErrorT IO a
runCache m lp = fst <$> runFreshMT (runStateT (runReaderT m lp) mempty)

mkRenamer :: [Flag] -> S.Module SrcSpanInfo -> Cache Renamer
mkRenamer flags m = extendWithGlobs m . mconcat <$> mapM mkRenamer' (getImps m)
      where mkRenamer' :: ImportDecl SrcSpanInfo -> Cache Renamer
            mkRenamer' (ImportDecl _ (void -> m) quald _ _ _ (fmap void -> as) specs) = do
                  (_, (_, exps)) <- getModule flags $ toFilePath m
                  fromImps m quald exps as specs

-- Pass 1    Parse.
-- Pass 2-4  Fixity fixing (uniquify + fix + deuniquify, because bug in applyFixities).
-- Pass 5    Annotate.
-- Pass 6-14 Desugar.
-- Pass 15   Translate to mantle + rename globals.
-- Pass 16   Translate to core

getModule :: [Flag] -> FilePath -> Cache (Module, (Module, Exports))
getModule flags fp = pDebug flags ("fetching module: " <> pack fp) >> Map.lookup fp <$> get >>= \ case
      Just p  -> pure p
      Nothing -> do
            modify $ Map.insert fp mempty

            lp         <- ask
            mmods      <- mapM (tryParseInDir fp) lp
            -- FIXME: The directory crawling could be more robust here. (Should
            -- use exception handling.)
            m          <- maybe
                              (failAt (filePath fp) "File not found in load-path")
                              (pure . addMainModuleHead)
                        $ msum mmods

            rn         <- mkRenamer flags m
            imps       <- loadImports m

            -- Phase 1 (haskell-src-exts) transformations.
            (m', exps) <- pure
                      >=> pDebug' "Fixing fixity."
                      >=> lift . lift . fixFixity rn
                      >=> pDebug' "Annotating."
                      >=> annotate
                      >=> whenSet FlagDHask1 (printInfoHSE "Haskell 1: Pre-desugaring" rn imps)
                      >=> pDebug' "Desugaring."
                      >=> desugar rn
                      >=> whenSet FlagDHask2 (printInfoHSE "Haskell 2: Post-desugaring" rn imps)
                      >=> pDebug' "Translating to crust."
                      >=> toCrust rn
                      $ m

            modify $ Map.insert fp (m', (imps, exps))
            pure (m', (imps, exps))

      where loadImports :: Annotation a => S.Module a -> Cache Module
            loadImports = fmap mconcat . mapM (fmap fst . getModule flags . toFilePath . void . importModule) . getImps

            whenSet :: Applicative m => Flag -> (Bool -> a -> m a) -> a -> m a
            whenSet f m = if f `elem` flags then m $ FlagV `elem` flags else pure

            pDebug' :: MonadIO m => Text -> a -> m a
            pDebug' s a = pDebug flags (pack fp <> ": " <> s) >> pure a

-- Phase 2 (pre-core) transformations.
getProgram :: [Flag] -> FilePath -> Cache Core.Program
getProgram flags fp = do
      (mod, (imps, _))  <- getModule flags fp
      let (Module ts ds) = mod <> imps

      p <- pure
       >=> pDebug' "Adding primitives and inlining."
       >=> whenSet FlagDCrust1 (printInfo "Crust 1: Post-desugaring")
       >=> pure . addPrims
       >=> inline
       >=> whenSet FlagDCrust2 (printInfo "Crust 2: Post-inlining")
       >=> pDebug' "Typechecking."
       >=> kindCheck >=> typeCheck
       >=> pDebug' "Simplifying and reducing."
       >=> neuterPrims
       >=> reduce
       >=> shiftLambdas
       >=> pDebug' "Lifting lambdas (pre-purification)."
       >=> liftLambdas
       >=> pDebug' "Removing unused definitions."
       >=> pure . purgeUnused
       >=> whenSet' FlagDTypes (pDebug' "Verifying types pre-purification." >=> typeVerify)
       >=> whenSet FlagDCrust3 (printInfo "Crust 3: Pre-purification")
       >=> pDebug' "Purifying."
       >=> purify
       >=> whenSet FlagDCrust4 (printInfo "Crust 4: Post-purification")
       >=> whenSet' FlagDTypes (pDebug' "Verifying types post-purification." >=> typeVerify)
       >=> pDebug' "Lifting lambdas (post-purification)."
       >=> liftLambdas
       >=> pDebug' "Fully apply global function definitions."
       >=> fullyApplyDefs
       >=> pDebug' "Removing unused definitions (again)."
       >=> pure . purgeUnused
       >=> whenSet FlagDCrust5 (printInfo "Crust 5: Post-second-lambda-lifting")
       >=> pDebug' "Translating to core & HDL."
       >=> toCore
       $ (ts, ds)

      when (FlagDCore1 `elem` flags) $ liftIO $ do
            printHeader "Core"
            T.putStrLn $ prettyPrint p
            when (FlagV `elem` flags) $ T.putStrLn "\n## Show core:\n"
            when (FlagV `elem` flags) $ T.putStrLn $ showt $ unAnn p

      pure p

      where whenSet :: Applicative m => Flag -> (Bool -> a -> m a) -> a -> m a
            whenSet f m = if f `elem` flags then m $ FlagV `elem` flags else pure

            whenSet' :: Applicative m => Flag -> (a -> m a) -> a -> m a
            whenSet' f m = whenSet f (\ _ -> m)

            pDebug' :: MonadIO m => Text -> a -> m a
            pDebug' s a = pDebug flags s >> pure a

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
      when verbose $ liftIO $ T.putStrLn $ T.concat $ map (<>"\n") (nubOrd $ map prettyPrint (fv p :: [Name Kind]))
      when verbose $ liftIO $ T.putStrLn "## Free type vars:\n"
      when verbose $ liftIO $ T.putStrLn $ T.concat $ map ((<>"\n")) (nubOrd $ map prettyPrint (fv p :: [Name Ty]))
      when verbose $ liftIO $ T.putStrLn "## Free tycon vars:\n"
      when verbose $ liftIO $ T.putStrLn $ T.concat $ map ((<>"\n")) (nubOrd $ map prettyPrint (fv p :: [Name TyConId]))
      liftIO $ T.putStrLn "## Free con vars:\n"
      liftIO $ T.putStrLn $ T.concat $ map ((<>"\n")) (nubOrd $ map prettyPrint (fv p :: [Name DataConId]))
      liftIO $ T.putStrLn "## Free exp vars:\n"
      liftIO $ T.putStrLn $ T.concat $ map ((<>"\n")) (nubOrd $ map prettyPrint (fv p :: [Name Exp]))
      liftIO $ T.putStrLn "## Program:\n"
      liftIO $ T.putStrLn $ prettyPrint p
      when verbose $ liftIO $ T.putStrLn "\n## Program (show):\n"
      when verbose $ liftIO $ T.putStrLn $ showt $ unAnn fp
      pure fp

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
