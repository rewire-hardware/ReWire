{-# LANGUAGE LambdaCase, FlexibleInstances, TupleSections, NamedFieldPuns, ViewPatterns #-}
{-# LANGUAGE Safe #-}
module ReWire.FrontEnd.Cache
      ( runCache
      , getProgram
      , LoadPath
      , printInfo
      , printInfoHSE
      ) where

import ReWire.Annotation
import ReWire.Error
import ReWire.FrontEnd.Annotate
import ReWire.FrontEnd.Desugar
import ReWire.FrontEnd.KindCheck
import ReWire.FrontEnd.Parse
import ReWire.FrontEnd.PrimBasis
import ReWire.FrontEnd.Purify
import ReWire.FrontEnd.Rename
import ReWire.FrontEnd.Syntax
import ReWire.FrontEnd.ToCore
import ReWire.FrontEnd.ToMantle
import ReWire.FrontEnd.Transform
import ReWire.FrontEnd.TypeCheck
import ReWire.Pretty
import ReWire.FrontEnd.Unbound (runFreshMT, FreshMT (..))

import Control.Monad ((>=>), liftM, msum, void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (runReaderT, ReaderT, MonadReader (..))
import Control.Monad.State.Strict (runStateT, StateT, MonadState (..), modify)

import qualified Data.Map.Strict              as Map
import qualified Language.Haskell.Exts.Syntax as S (Module (..))
import qualified ReWire.Core.Syntax           as Core
import qualified Language.Haskell.Exts.Pretty as P

import Language.Haskell.Exts.Syntax hiding (Annotation, Exp, Module (..), Namespace, Name, Kind)

type Cache = ReaderT LoadPath (StateT ModCache (FreshMT (SyntaxErrorT IO)))
type LoadPath = [FilePath]
type ModCache = Map.Map FilePath (Module, Exports)

runCache :: Cache a -> LoadPath -> IO (Either AstError a)
runCache m lp = runSyntaxError $ fst <$> runFreshMT (runStateT (runReaderT m lp) mempty)

mkRenamer :: S.Module SrcSpanInfo -> Cache Renamer
mkRenamer m = extendWithGlobs m <$> mconcat <$> mapM mkRenamer' (getImps m)
      where mkRenamer' :: ImportDecl SrcSpanInfo -> Cache Renamer
            mkRenamer' (ImportDecl _ (void -> m) quald _ _ _ (fmap void -> as) specs) = do
                  (_, exps) <- getModule $ toFilePath m
                  fromImps m quald exps as specs

-- Pass 1    Parse.
-- Pass 2-4  Fixity fixing (uniquify + fix + deuniquify, because bug in applyFixities).
-- Pass 5    Annotate.
-- Pass 6-14 Desugar.
-- Pass 15   Translate to mantle + rename globals.
-- Pass 16   Translate to core

getModule :: FilePath -> Cache (Module, Exports)
getModule fp = Map.lookup fp <$> get >>= \ case
      Just p  -> return p
      Nothing -> do
            modify $ Map.insert fp mempty

            lp         <- ask
            mmods      <- mapM (tryParseInDir fp) lp
            -- FIXME: The directory crawling could be more robust here. (Should
            -- use exception handling.)
            m          <- maybe (failAt (filePath fp) "File not found in load-path") (return . addMainModuleHead) $ msum mmods

            rn         <- mkRenamer m
            imps       <- loadImports m

            -- Phase 1 (haskell-src-exts) transformations.
            (m', exps) <- return
                      >=> fixFixity rn
                      >=> annotate
                      >=> desugar rn
                      -- >=> printInfoHSE "__Post_Desugar__" rn
                      >=> toMantle rn
                      $ m

            modify $ Map.insert fp (m' <> imps, exps)
            return (m' <> imps, exps)

      where loadImports :: Annotation a => S.Module a -> Cache Module
            loadImports = liftM mconcat . mapM (liftM fst . getModule . toFilePath . void . importModule) . getImps

-- Phase 2 (pre-core) transformations.
getProgram :: FilePath -> Cache Core.Program
getProgram fp = do
      (Module ts ds, _) <- getModule fp

      p <- return
       -- >=> printInfo "__Desugared__"
       >=> addPrims
       >=> inline
       >=> kindCheck
       >=> typeCheck
       >=> neuterPrims
       >=> reduce
       >=> shiftLambdas
--       >=> printInfo "___Post_TC___"
       >=> liftLambdas
--       >=> typeCheck
--     >=> printInfo "___Post_LL___"
       >=> purge
       -- >=> typeCheck
--     >=> printInfo "___Post_Purge___"
       >=> purify
--       >=> typeCheck
--       >=> printInfo "___Post_Purify___"
       >=> liftLambdas
--       >=> printInfo "___Post_Second_LL___"
       >=> toCore
       $ (ts, ds)

      -- liftIO $ putStrLn "___Core___"
      -- liftIO $ putStrLn $ prettyPrint p

      return p

printInfo :: MonadIO m => String -> FreeProgram -> m FreeProgram
printInfo msg fp = do
      let p = Program $ trec fp
      liftIO $ putStrLn msg
      liftIO $ putStrLn "Free kind vars:\n"
      liftIO $ putStrLn $ concatMap ((++"\n") . prettyPrint) (fv p :: [Name Kind])
      liftIO $ putStrLn "Free type vars:\n"
      liftIO $ putStrLn $ concatMap ((++"\n") . prettyPrint) (fv p :: [Name Ty])
      liftIO $ putStrLn "Free tycon vars:\n"
      liftIO $ putStrLn $ concatMap ((++"\n") . prettyPrint) (fv p :: [Name TyConId])
      liftIO $ putStrLn "Free con vars:\n"
      liftIO $ putStrLn $ concatMap ((++"\n") . prettyPrint) (fv p :: [Name DataConId])
      liftIO $ putStrLn "Free exp vars:\n"
      liftIO $ putStrLn $ concatMap ((++"\n") . prettyPrint) (fv p :: [Name Exp])
      liftIO $ putStrLn "Program:\n"
      liftIO $ putStrLn $ prettyPrint p
      return fp

printInfoHSE :: MonadIO m => String -> Renamer -> S.Module a -> m (S.Module a)
printInfoHSE msg rn hse = do
      liftIO $ putStrLn msg
      liftIO $ putStrLn "\nRenamer:\n"
      liftIO $ putStrLn $ show rn
      liftIO $ putStrLn "\nExports:\n"
      liftIO $ putStrLn $ show $ allExports rn
      liftIO $ putStrLn "\nShow HSE:\n"
      liftIO $ putStrLn $ show $ void hse
      liftIO $ putStrLn "\nPretty HSE:\n"
      liftIO $ putStrLn $ P.prettyPrint $ void hse
      return hse

