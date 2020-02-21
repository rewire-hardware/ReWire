{-# LANGUAGE LambdaCase, FlexibleInstances, ViewPatterns #-}
{-# LANGUAGE Safe #-}
module ReWire.Crust.Cache
      ( runCache
      , getProgram
      , LoadPath
      , printInfo
      , printInfoHSE
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

import qualified Data.Map.Strict              as Map
import qualified Language.Haskell.Exts.Syntax as S (Module (..))
import qualified ReWire.Core.Syntax           as Core
import qualified Language.Haskell.Exts.Pretty as P

import Language.Haskell.Exts.Syntax hiding (Annotation, Exp, Module (..), Namespace, Name, Kind)

type Cache = ReaderT LoadPath (StateT ModCache (FreshMT (SyntaxErrorT IO)))
type LoadPath = [FilePath]
type ModCache = Map.Map FilePath (Module, (Module, Exports))

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
getModule flags fp = Map.lookup fp <$> get >>= \ case
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
                      >=> lift . lift . fixFixity rn
                      >=> annotate
                      >=> whenSet FlagDHask1 (printInfoHSE "Haskell 1: Pre-desugaring" rn imps)
                      >=> desugar rn
                      >=> whenSet FlagDHask2 (printInfoHSE "Haskell 2: Post-desugaring" rn imps)
                      >=> toCrust rn
                      $ m

            modify $ Map.insert fp (m', (imps, exps))
            pure (m', (imps, exps))

      where loadImports :: Annotation a => S.Module a -> Cache Module
            loadImports = fmap mconcat . mapM (fmap fst . getModule flags . toFilePath . void . importModule) . getImps

            whenSet :: Applicative m => Flag -> (a -> m a) -> a -> m a
            whenSet f m = if f `elem` flags then m else pure

-- Phase 2 (pre-core) transformations.
getProgram :: [Flag] -> FilePath -> Cache Core.Program
getProgram flags fp = do
      (mod, (imps, _))  <- getModule flags fp
      let (Module ts ds) = mod <> imps

      p <- pure
       >=> whenSet FlagDCrust1 (printInfo (FlagV `elem` flags) "Crust 1: Post-desugaring")
       >=> pure . addPrims
       >=> pure . inline
       >=> kindCheck
       >=> typeCheck
       >=> neuterPrims
       >=> reduce
       >=> pure . shiftLambdas
       >=> liftLambdas
       >=> pure . purgeUnused
       >=> whenSet FlagDCrust2 (printInfo (FlagV `elem` flags) "Crust 2: Pre-purification")
       >=> purify -- TODO(chathhorn): move before purge? purge again after purify?
       >=> whenSet FlagDCrust3 (printInfo (FlagV `elem` flags) "Crust 3: Post-purification")
       >=> whenSet FlagDTypes typeVerify
       >=> liftLambdas
       >=> whenSet FlagDCrust4 (printInfo (FlagV `elem` flags) "Crust 4: Post-second-lambda-lifting")
       >=> toCore
       $ (ts, ds)

      when (FlagDCore `elem` flags) $ liftIO $ do
            printHeader "Core"
            putStrLn $ prettyPrint p
            when (FlagV `elem` flags) $ putStrLn "\n## Show core:\n"
            when (FlagV `elem` flags) $ print $ unAnn p

      pure p

      where whenSet :: Applicative m => Flag -> (a -> m a) -> a -> m a
            whenSet f m = if f `elem` flags then m else pure

printHeader :: MonadIO m => String -> m ()
printHeader hd = do
      liftIO $ putStrLn "# ======================================="
      liftIO $ putStrLn $ "# " ++ hd
      liftIO $ putStrLn "# =======================================\n"

printInfo :: MonadIO m => Bool -> String -> FreeProgram -> m FreeProgram
printInfo verbose hd fp = do
      let p = Program $ trec fp
      printHeader hd
      when verbose $ liftIO $ putStrLn "## Free kind vars:\n"
      when verbose $ liftIO $ putStrLn $ concatMap ((++"\n") . prettyPrint) (fv p :: [Name Kind])
      when verbose $ liftIO $ putStrLn "## Free type vars:\n"
      when verbose $ liftIO $ putStrLn $ concatMap ((++"\n") . prettyPrint) (fv p :: [Name Ty])
      when verbose $ liftIO $ putStrLn "## Free tycon vars:\n"
      when verbose $ liftIO $ putStrLn $ concatMap ((++"\n") . prettyPrint) (fv p :: [Name TyConId])
      liftIO $ putStrLn "## Free con vars:\n"
      liftIO $ putStrLn $ concatMap ((++"\n") . prettyPrint) (fv p :: [Name DataConId])
      liftIO $ putStrLn "## Free exp vars:\n"
      liftIO $ putStrLn $ concatMap ((++"\n") . prettyPrint) (fv p :: [Name Exp])
      liftIO $ putStrLn "## Program:\n"
      liftIO $ putStrLn $ prettyPrint p
      when verbose $ liftIO $ putStrLn "\n## Program (show):\n"
      when verbose $ liftIO $ print $ unAnn fp
      pure fp

printInfoHSE :: MonadIO m => String -> Renamer -> Module -> S.Module a -> m (S.Module a)
printInfoHSE hd rn imps hse = do
      printHeader hd
      liftIO $ putStrLn "\n## Renamer:\n"
      liftIO $ print rn
      liftIO $ putStrLn "\n## Exports:\n"
      liftIO $ print $ allExports rn
      liftIO $ putStrLn "\n## Show imps:\n"
      liftIO $ print imps
      liftIO $ putStrLn "\n## Show HSE mod:\n"
      liftIO $ print $ void hse
      liftIO $ putStrLn "\n## Pretty HSE mod:\n"
      liftIO $ putStrLn $ P.prettyPrint $ void hse
      pure hse
