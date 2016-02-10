{-# LANGUAGE LambdaCase, FlexibleInstances, TupleSections, NamedFieldPuns, ViewPatterns #-}
module ReWire.FrontEnd.Cache
      ( runCache
      , getProgram
      , LoadPath
      ) where

import ReWire.Core.Syntax
import ReWire.Error
import ReWire.FrontEnd.Annotate
import ReWire.FrontEnd.Desugar
import ReWire.FrontEnd.Rename
import ReWire.FrontEnd.Translate

import Control.Monad ((<=<), liftM, msum)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, ReaderT, MonadReader (..))
import Control.Monad.State.Strict (runStateT, StateT, MonadState (..), modify)
import Data.Functor ((<$>))
import Data.Monoid ((<>), mempty, mconcat)
import Language.Haskell.Exts.Annotated (parseFileWithMode, ParseResult (..), defaultParseMode, ParseMode (..))
import Language.Haskell.Exts.Annotated.Simplify (sModuleName)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import System.Directory (getCurrentDirectory, setCurrentDirectory, doesFileExist, doesDirectoryExist)

import qualified Data.Map.Strict              as Map
import qualified Language.Haskell.Exts.Syntax as S

import Language.Haskell.Exts.Annotated.Syntax hiding (Annotation, Namespace)

type Cache = ReaderT LoadPath (StateT ModCache (SyntaxErrorT IO))
type LoadPath = [FilePath]
type ModCache = Map.Map FilePath (RWCProgram, Exports)

runCache :: Cache a -> LoadPath -> IO (Either AstError a)
runCache m lp = runSyntaxError $ fst <$> runStateT (runReaderT m lp) mempty

mkRenamer :: Annotation a => Module a -> Cache Renamer
mkRenamer (Module _ _ _ imps _) = mconcat <$> mapM mkRenamer' imps
      where mkRenamer' :: Annotation a => ImportDecl a -> Cache Renamer
            mkRenamer' (ImportDecl _ (sModuleName -> m) quald _ _ _ (fmap sModuleName -> as) specs) = do
                  (_, exps) <- getProgram $ toFilePath m
                  fromImps m quald exps as specs
mkRenamer m = failAt (ann m) "Unsupported module syntax"

-- Pass 1    Parse.
-- Pass 2-4  Fixity fixing (uniquify + fix + deuniquify, because bug in applyFixities).
-- Pass 5    Annotate.
-- Pass 6-12 Desugar.
-- Pass 13   Translate + rename globals.

getProgram :: FilePath -> Cache (RWCProgram, Exports)
getProgram fp = Map.lookup fp <$> get >>= \ case
      Just p  -> return p
      Nothing -> do
            modify $ Map.insert fp mempty

            lp         <- ask
            mmods      <- mapM tryParseInDir lp
            -- FIXME: The directory crawling could be more robust here. (Should
            -- use exception handling.)
            m          <- maybe (failAt (filePath fp) "File not found in loadpath") return $ msum mmods

            rn         <- mkRenamer m
            imps       <- loadImports m

            (m', exps) <- translate rn <=< desugar <=< annotate <=< fixFixity rn $ m

            modify $ Map.insert fp (m' <> imps, exps)
            return (m' <> imps, exps)

      where tryParseInDir :: FilePath -> Cache (Maybe (Module SrcSpanInfo))
            tryParseInDir dp = do
                  dExists <- liftIO $ doesDirectoryExist dp
                  if not dExists then return Nothing else do
                        oldCwd <- liftIO getCurrentDirectory
                        liftIO $ setCurrentDirectory dp
                        exists <- liftIO $ doesFileExist fp
                        result <- if not exists then return Nothing else do
                              pr <- liftIO parse
                              Just <$> pr2Cache pr
                        liftIO $ setCurrentDirectory oldCwd
                        return result

            parse :: IO (ParseResult (Module SrcSpanInfo))
            parse = parseFileWithMode defaultParseMode { parseFilename = fp, fixities = Nothing } fp

            pr2Cache :: ParseResult a -> Cache a
            pr2Cache = \ case
                  ParseOk p                         -> return p
                  ParseFailed (S.SrcLoc "" r c) msg -> failAt (S.SrcLoc fp r c) msg
                  ParseFailed l msg                 -> failAt l msg

            loadImports :: Module a -> Cache RWCProgram
            loadImports = liftM mconcat . mapM (liftM fst . getProgram . toFilePath) . getImps

            getImps :: Module a -> [S.ModuleName]
            getImps (Module _ _ _ imps _)            = map (sModuleName . importModule) imps
            getImps XmlPage {}                       = []
            getImps (XmlHybrid _ _ _ imps _ _ _ _ _) = map (sModuleName . importModule) imps
