{-# LANGUAGE LambdaCase, FlexibleInstances, TupleSections, NamedFieldPuns #-}
module ReWire.FrontEnd.Cache
      ( runCache
      , getProgram
      ) where

import ReWire.Core.Syntax
import ReWire.FrontEnd.Desugar
import ReWire.FrontEnd.Translate
import ReWire.FrontEnd.Error
import ReWire.FrontEnd.Renamer

import Control.Monad (foldM, (>=>), when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (runStateT, StateT, get, modify)
--import Data.Functor ((<$>))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Language.Haskell.Exts as Haskell (parseFile)
import           Language.Haskell.Exts hiding (parseFile, loc, name, binds, op, Namespace(..))

data Exports = Exports !(Set.Set FQName) !(Set.Set FQName) !(Map.Map Name (Set.Set FQName))
      deriving Show

instance Monoid Exports where
      mempty = Exports mempty mempty mempty
      mappend (Exports a b c) (Exports a' b' c') = Exports (a <> a') (b <> b') (Map.unionWith mappend c c')

expValue :: FQName -> Exports -> Exports
expValue x (Exports vs ts cs) = Exports (Set.insert x vs) ts cs

expType :: FQName -> Set.Set FQName -> Exports -> Exports
expType x cs' (Exports vs ts cs) = Exports (cs' <> vs) (Set.insert x ts) $ Map.insert (unqual x) cs' cs

checkImpValue :: Name -> Exports -> Cache ()
checkImpValue x (Exports vs _ _) = when (Set.null $ Set.filter ((==x) . unqual) vs)
      $ lift $ pFail $ "attempting to import an unexported symbol: " ++ prettyPrint x

checkImpType :: Name -> Exports -> Cache ()
checkImpType x (Exports _ ts _) = when (Set.null $ Set.filter ((==x) . unqual) ts)
      $ lift $ pFail $ "attempting to import an unexported symbol: " ++ prettyPrint x

getCtors :: Name -> Exports -> Set.Set FQName
getCtors x (Exports _ _ cs) = fromMaybe mempty $ Map.lookup x cs

lookupExp :: Namespace -> Name -> Exports -> Cache FQName
lookupExp ns x (Exports vs ts _) = case ns of
      ValueNS -> lkup vs
      TypeNS -> lkup ts
      where lkup :: Set.Set FQName -> Cache FQName
            lkup xs = case find (cmp x) (Set.toList xs) of
                  Just x' -> return x'
                  Nothing -> lift $ pFail $ "attempting to import an unexported symbol: " ++ prettyPrint x
            cmp :: Name -> FQName -> Bool
            cmp x (FQName _ x') = x == x'

toImports :: Exports -> [(Namespace, Name)]
toImports (Exports vs ts _) = map ((ValueNS,) . unqual) (Set.toList vs)
                           ++ map ((TypeNS,) . unqual) (Set.toList ts)

unqual :: FQName -> Name
unqual (FQName _ x) = x

type Cache = StateT Caches (ParseError IO)

data Caches = Caches
      { desugared :: Map.Map FilePath Module
      , renamer   :: Map.Map FilePath Renamer
      , exports   :: Map.Map FilePath Exports
      , program   :: Map.Map FilePath RWCProgram
      }

runCache :: Cache a -> IO (ParseResult a)
runCache = runParseError . (flip runStateT (Caches Map.empty Map.empty Map.empty Map.empty)
                              >=> return . fst)

getDesugared :: FilePath -> Cache Module
getDesugared fp = do
      cached <- desugared <$> get
      case Map.lookup fp cached of
            Just m  -> return m
            Nothing -> do
                  m <- justParse fp
                  m' <- lift $ desugar m
                  modify $ \c@Caches { desugared } -> c { desugared = Map.insert fp m' desugared }
                  getDesugared fp
      where justParse :: FilePath -> Cache Module
            justParse = (liftIO . Haskell.parseFile) >=> pr2Cache
            pr2Cache :: ParseResult a -> Cache a
            pr2Cache = \case
                  ParseOk p           -> return p
                  ParseFailed loc msg -> lift $ pFailAt loc msg

getExports :: FilePath -> Cache Exports
getExports fp = do
      cached <- exports <$> get
      case Map.lookup fp cached of
            Just exps -> return exps
            Nothing   -> do
                  m <- getDesugared fp
                  rn <- getRenamer fp
                  exps <- lift $ translateExps rn m

                  modify $ \c@Caches { exports } -> c { exports = Map.insert fp mempty exports }

                  allExps <- mapM (getExports . toFilePath) $ getImps m
                  exps' <- foldM (transExport $ mconcat allExps) mempty exps

                  modify $ \c@Caches { exports } -> c { exports = Map.insert fp exps' exports }
                  return exps'
      where transExport :: Exports -> Exports -> Export -> Cache Exports
            transExport iexps exps = \case
                  Export x        -> return $ expValue x exps
                  ExportAll x     -> return $ expType x (getCtors (unqual x) iexps) exps
                  ExportWith x cs -> return $ expType x (Set.fromList cs) exps
                  ExportMod m     -> do
                        exps' <- getExports (toFilePath m)
                        return $ exps <> exps'

getRenamer :: FilePath -> Cache Renamer
getRenamer fp = do
      cached <- renamer <$> get
      case Map.lookup fp cached of
            Just rn -> return rn
            Nothing -> do
                  m <- getDesugared fp

                  modify $ \c@Caches { renamer } -> c { renamer = Map.insert fp mempty renamer }

                  rn <- mkRenamer m

                  modify $ \c@Caches { renamer } -> c { renamer = Map.insert fp rn renamer }
                  return rn
      where mkRenamer :: Module -> Cache Renamer
            mkRenamer (Module loc _ _ _ _ imps _) = do
                  lift $ setLoc loc
                  rns <- mapM mkRenamer' imps
                  return $ mconcat rns

            mkRenamer' :: ImportDecl -> Cache Renamer
            mkRenamer' (ImportDecl loc m quald _ _ _ as specs) = do
                  lift $ setLoc loc
                  exps <- getExports $ toFilePath m
                  mkTable exps as specs
                  where mkTable :: Exports -> Maybe ModuleName -> Maybe (Bool, [ImportSpec]) -> Cache Renamer
                        mkTable exps Nothing   Nothing          = mkTable' m Nothing exps
                        mkTable exps (Just m') Nothing          = mkTable' m' Nothing exps
                        mkTable exps (Just m') (Just (h, imps)) = do
                              imps' <- foldM (getImp exps) [] imps
                              mkTable' m' (Just (h, imps')) exps
                        mkTable exps Nothing   (Just (h, imps)) = do
                              imps' <- foldM (getImp exps) [] imps
                              mkTable' m (Just (h, imps')) exps

                        getImp :: Exports -> [(Namespace, Name)] -> ImportSpec -> Cache [(Namespace, Name)]
                        getImp exps imps = \case
                              IVar x              -> do
                                    checkImpValue x exps
                                    return $ (ValueNS, x) : imps
                              IAbs _ x            -> do
                                    checkImpType x exps
                                    return $ (TypeNS, x) : imps
                              IThingAll x         -> do
                                    checkImpType x exps
                                    return $ (TypeNS, x) : (map ((ValueNS,) . unqual) (Set.toList $ getCtors x exps) ++ imps)
                              IThingWith x cs     -> do
                                    checkImpType x exps
                                    mapM_ ((`checkImpValue` exps) . toName) cs
                                    return $ (TypeNS, x):(map ((ValueNS,) . toName) cs ++ imps)
                              where toName :: CName -> Name
                                    toName (VarName x) = x
                                    toName (ConName x) = x

                        -- TODO(chathhorn): clean this stuff up.
                        mkTable' :: ModuleName -> Maybe (Bool, [(Namespace, Name)]) -> Exports -> Cache Renamer
                        -- No list of imports -- so import everything.
                        mkTable' m' Nothing exps = mkTable' m' (Just (False, toImports exps)) exps
                        -- List of imports, no "hiding".
                        mkTable' m' (Just (False, imps)) exps = foldM ins Map.empty imps
                              where ins :: Renamer -> (Namespace, Name) -> Cache Renamer
                                    ins table (ns, x) = do
                                          e <- lookupExp ns x exps
                                          return $ let tab' = Map.insert (ns, Qual m' x) e table
                                                in if quald then tab' else Map.insert (ns, UnQual x) e tab'
                        -- List of imports with "hiding" -- import everything, then delete
                        -- the items from the list.
                        mkTable' m' (Just (True, imps)) exps = do
                              tab <- mkTable' m' Nothing exps
                              foldM del tab imps
                              where del table (ns, x) = return
                                                $ Map.delete (ns, Qual m' x)
                                                $ Map.delete (ns, UnQual x) table


getProgram :: FilePath -> Cache RWCProgram
getProgram fp = do
      cached <- program <$> get
      case Map.lookup fp cached of
            Just m  -> return m
            Nothing -> do
                  m <- getDesugared fp
                  rn <- getRenamer fp

                  modify $ \c@Caches { program } -> c
                        { program   = Map.insert fp (RWCProgram [] []) program
                        }

                  rwcm <- lift $ translate rn m
                  allMods <- mapM (getProgram . toFilePath) $ getImps m
                  let merged = mconcat $ rwcm : allMods

                  modify $ \c@Caches { desugared, renamer, exports, program } -> c
                        { desugared = Map.delete fp desugared
                        , renamer   = Map.delete fp renamer
                        , exports   = Map.delete fp exports
                        , program   = Map.insert fp merged program
                        }
                  return merged

getImps :: Module -> [ModuleName]
getImps (Module _ _ _ _ _ imps _) = map importModule imps

