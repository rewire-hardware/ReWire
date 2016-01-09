{-# LANGUAGE LambdaCase, FlexibleInstances, TupleSections, NamedFieldPuns, ViewPatterns #-}
module ReWire.FrontEnd.Cache
      ( runCache
      , getProgram
      , LoadPath
      ) where

import ReWire.Core.Syntax
import ReWire.FrontEnd.Annotate
import ReWire.FrontEnd.Desugar
import ReWire.FrontEnd.Error
import ReWire.FrontEnd.Rename
import ReWire.FrontEnd.Translate

import Control.Monad (foldM, when, msum)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (runReaderT, ReaderT, ask)
import Control.Monad.Trans.State (runStateT, StateT, get, modify)
import Data.Functor ((<$>))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..),(<>))
import Language.Haskell.Exts.Annotated (parseFile, ParseResult(..))
import Language.Haskell.Exts.Annotated.Simplify (sName, sModuleName)
import Language.Haskell.Exts.SrcLoc (SrcInfo(..), SrcSpanInfo, noLoc, noInfoSpan, mkSrcSpan)
import System.Directory (getCurrentDirectory, setCurrentDirectory, doesFileExist, doesDirectoryExist)

import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as Set
import qualified Language.Haskell.Exts.Syntax as S

import Language.Haskell.Exts.Annotated.Syntax hiding (Namespace)

data Exports = Exports !(Set.Set FQName) !(Set.Set FQName) !(Map.Map S.Name (Set.Set FQName))
      deriving Show

instance Monoid Exports where
      mempty = Exports mempty mempty mempty
      mappend (Exports a b c) (Exports a' b' c') = Exports (a <> a') (b <> b') (Map.unionWith mappend c c')

expValue :: FQName -> Exports -> Exports
expValue x (Exports vs ts cs) = Exports (Set.insert x vs) ts cs

expType :: FQName -> Set.Set FQName -> Exports -> Exports
expType x cs' (Exports vs ts cs) = Exports (cs' <> vs) (Set.insert x ts) $ Map.insert (unqual x) cs' cs

checkImpValue :: Name Annote -> Exports -> Cache ()
checkImpValue x (Exports vs _ _) = when (Set.null $ Set.filter ((== sName x) . unqual) vs)
      $ lift $ lift $ pFailAt (ann x) "attempting to import an unexported symbol"

checkImpType :: Name Annote -> Exports -> Cache ()
checkImpType x (Exports _ ts _) = when (Set.null $ Set.filter ((== sName x) . unqual) ts)
      $ lift $ lift $ pFailAt (ann x) "attempting to import an unexported symbol"

getCtors :: S.Name -> Exports -> Set.Set FQName
getCtors x (Exports _ _ cs) = fromMaybe mempty $ Map.lookup x cs

unqual :: FQName -> S.Name
unqual (FQName _ x) = x

type Cache = ReaderT LoadPath (StateT Caches (ParseError IO))

type LoadPath = [FilePath]

data Caches = Caches
      { desugared :: Map.Map FilePath (Module Annote)
      , renamer   :: Map.Map FilePath Renamer
      , exports   :: Map.Map FilePath Exports
      , program   :: Map.Map FilePath RWCProgram
      }

runCache :: Cache a -> LoadPath -> IO (ParseResult a)
runCache m lp = runParseError
                  (fst <$> runStateT (runReaderT m lp) (Caches Map.empty Map.empty Map.empty Map.empty))

getDesugared :: FilePath -> Cache (Module Annote)
getDesugared fp = do
      cached <- desugared <$> lift get
      case Map.lookup fp cached of
            Just m  -> return m
            Nothing -> do
                  m <- justParse fp
                  m' <- lift $ lift $ desugar $ annotate m
                  lift $ modify $ \ c@Caches { desugared } -> c { desugared = Map.insert fp m' desugared }
                  getDesugared fp
      where justParse :: FilePath -> Cache (Module SrcSpanInfo)
            justParse fp = do lp    <- ask
                              mmods <- mapM (tryParseInDir fp) lp
                              case msum mmods of
                                Nothing  -> lift $ lift $ pFail $ "file not found in loadpath: " ++ fp
                                Just mod -> return mod
            -- FIXME: The directory crawling could be more robust here. (Should
            -- use exception handling.)
            tryParseInDir :: FilePath -> FilePath -> Cache (Maybe (Module SrcSpanInfo))
            tryParseInDir fp dp = do
              dExists <- liftIO $ doesDirectoryExist dp
              if not dExists
               then return Nothing
               else do
                oldCwd <- liftIO getCurrentDirectory
                liftIO $ setCurrentDirectory dp
                exists <- liftIO $ doesFileExist fp
                result <- if exists then do pr <- liftIO $ parseFile fp
                                            Just <$> pr2Cache pr
                                    else return Nothing
                liftIO $ setCurrentDirectory oldCwd
                return result
            pr2Cache :: ParseResult a -> Cache a
            pr2Cache = \ case
                  ParseOk p           -> return p
                  ParseFailed l msg -> lift $ lift $ pFailAt (fromSrcInfo $ noInfoSpan $ mkSrcSpan l l) msg

getExports :: FilePath -> Cache Exports
getExports fp = do
      cached <- exports <$> lift get
      case Map.lookup fp cached of
            Just exps -> return exps
            Nothing   -> do
                  m <- getDesugared fp
                  rn <- getRenamer fp
                  exps <- lift $ lift $ translateExps rn m

                  lift $ modify $ \ c@Caches { exports } -> c { exports = Map.insert fp mempty exports }

                  allExps <- mapM (getExports . toFilePath) $ getImps m
                  exps' <- foldM (transExport $ mconcat allExps) mempty exps

                  lift $ modify $ \ c@Caches { exports } -> c { exports = Map.insert fp exps' exports }
                  return exps'
      where transExport :: Exports -> Exports -> Export -> Cache Exports
            transExport iexps exps = \ case
                  Export x        -> return $ expValue x exps
                  ExportAll x     -> return $ expType x (getCtors (unqual x) iexps) exps
                  ExportWith x cs -> return $ expType x (Set.fromList cs) exps
                  ExportMod m     -> do
                        exps' <- getExports (toFilePath m)
                        return $ exps <> exps'

getRenamer :: FilePath -> Cache Renamer
getRenamer fp = do
      cached <- renamer <$> lift get
      case Map.lookup fp cached of
            Just rn -> return rn
            Nothing -> do
                  m <- getDesugared fp

                  lift $ modify $ \ c@Caches { renamer } -> c { renamer = Map.insert fp mempty renamer }

                  rn <- mkRenamer m

                  lift $ modify $ \ c@Caches { renamer } -> c { renamer = Map.insert fp rn renamer }
                  return rn
      where mkRenamer :: Module Annote -> Cache Renamer
            mkRenamer (Module _ _ _ imps _) = do
                  rns <- mapM mkRenamer' imps
                  return $ mconcat rns
            mkRenamer m = lift $ lift $ pFailAt (ann m) "unsupported module syntax"

            mkRenamer' :: ImportDecl Annote -> Cache Renamer
            mkRenamer' (ImportDecl _ (sModuleName -> m) quald _ _ _ as specs) = do
                  exps <- getExports $ toFilePath m
                  mkTable exps as specs
                  where mkTable :: Exports -> Maybe (ModuleName Annote) -> Maybe (ImportSpecList Annote) -> Cache Renamer
                        mkTable exps Nothing   Nothing          = mkTable' m Nothing exps
                        mkTable exps (Just m') Nothing          = mkTable' (sModuleName m') Nothing exps
                        mkTable exps (Just m') (Just (ImportSpecList _ h imps)) = do
                              imps' <- foldM (getImp exps) [] imps
                              mkTable' (sModuleName m') (Just (h, imps')) exps
                        mkTable exps Nothing   (Just (ImportSpecList _ h imps))   = do
                              imps' <- foldM (getImp exps) [] imps
                              mkTable' m (Just (h, imps')) exps

                        getImp :: Exports -> [(Namespace, Name Annote)] -> ImportSpec Annote -> Cache [(Namespace, Name Annote)]
                        getImp exps imps = \ case
                              IVar _ x          -> do
                                    checkImpValue x exps
                                    return $ (ValueNS, x) : imps
                              IAbs _ _ x        -> do
                                    checkImpType x exps
                                    return $ (TypeNS, x) : imps
                              IThingAll _ x     -> do
                                    checkImpType x exps
                                    return $ (TypeNS, x) : (map ((ValueNS,) . giveAnnote . unqual) (Set.toList $ getCtors (sName x) exps) ++ imps)
                              IThingWith _ x cs -> do
                                    checkImpType x exps
                                    mapM_ ((`checkImpValue` exps) . toName) cs
                                    return $ (TypeNS, x):(map ((ValueNS,) . toName) cs ++ imps)
                              where toName :: CName Annote -> Name Annote
                                    toName (VarName _ x) = x
                                    toName (ConName _ x) = x

                        mkTable' :: S.ModuleName -> Maybe (Bool, [(Namespace, Name Annote)]) -> Exports -> Cache Renamer
                        -- No list of imports -- so import everything.
                        mkTable' m' Nothing exps = mkTable' m' (Just (False, toImports exps)) exps
                              where toImports :: Exports -> [(Namespace, Name Annote)]
                                    toImports (Exports vs ts _) = map ((ValueNS,) . giveAnnote . unqual) (Set.toList vs)
                                                               ++ map ((TypeNS,) . giveAnnote . unqual) (Set.toList ts)

                        -- List of imports, no "hiding".
                        mkTable' m' (Just (False, imps)) exps = foldM ins Map.empty imps
                              where ins :: Renamer -> (Namespace, Name Annote) -> Cache Renamer
                                    ins table (ns, x) = do
                                          e <- lookupExp ns x exps
                                          return $ let tab' = Map.insert (ns, S.Qual m' $ sName x) e table
                                                in if quald then tab' else Map.insert (ns, S.UnQual $ sName x) e tab'
                        -- List of imports with "hiding" -- import everything, then delete
                        -- the items from the list.
                        mkTable' m' (Just (True, imps)) exps = do
                              tab <- mkTable' m' Nothing exps
                              foldM del tab imps
                              where del table (ns, x) = return
                                                $ Map.delete (ns, S.Qual m' $ sName x)
                                                $ Map.delete (ns, S.UnQual $ sName x) table

-- | Fills in the provenance with dummy values for exports.
giveAnnote :: S.Name -> Name Annote
giveAnnote = \ case
      S.Ident x -> Ident an x
      S.Symbol x -> Symbol an x
      where an :: Annote
            an = fromSrcInfo $ noInfoSpan $ mkSrcSpan noLoc noLoc

lookupExp :: Namespace -> Name Annote -> Exports -> Cache FQName
lookupExp ns x (Exports vs ts _) = case ns of
      ValueNS -> lkup vs
      TypeNS -> lkup ts
      where lkup :: Set.Set FQName -> Cache FQName
            lkup xs = case find cmp (Set.toList xs) of
                  Just x' -> return x'
                  Nothing -> lift $ lift $ pFailAt (ann x) "attempting to import an unexported symbol"
            cmp :: FQName -> Bool
            cmp (FQName _ x') = sName x == x'

getProgram :: FilePath -> Cache RWCProgram
getProgram fp = do
      cached <- program <$> lift get
      case Map.lookup fp cached of
            Just m  -> return m
            Nothing -> do
                  m <- getDesugared fp
                  rn <- getRenamer fp

                  lift $ modify $ \ c@Caches { program } -> c
                        { program = Map.insert fp (RWCProgram [] []) program }

                  rwcm <- lift $ lift $ translate rn m
                  allMods <- mapM (getProgram . toFilePath) $ getImps m
                  let merged = mconcat $ rwcm : allMods

                  lift $ modify $ \ c@Caches { desugared, renamer, exports, program } -> c
                        { desugared = Map.delete fp desugared
                        , renamer   = Map.delete fp renamer
                        , exports   = Map.delete fp exports
                        , program   = Map.insert fp merged program
                        }
                  return merged

getImps :: Module Annote -> [S.ModuleName]
getImps (Module _ _ _ imps _) = map (sModuleName . importModule) imps
getImps _                     = error "impossible"
