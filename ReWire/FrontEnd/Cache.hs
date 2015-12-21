{-# LANGUAGE LambdaCase #-}
module ReWire.FrontEnd.Cache
      ( runCache
      , getModule
      , getExports
      , cache
      ) where

import ReWire.Core.Syntax
import ReWire.FrontEnd.Desugar (desugar)
import ReWire.FrontEnd.Translate (translate, Export(..), deSigil)
import ReWire.FrontEnd.Error (ParseError, pFailAt, runParseError)
import ReWire.FrontEnd.Renamer (Renamer, FQName(..), toFilePath)

import Control.Monad (foldM, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (runStateT, StateT, get, modify)
import Data.List (find, nub, foldl')
import Data.Monoid (mconcat)
import qualified Data.Map.Strict as Map

import qualified Language.Haskell.Exts as Haskell (parseFile)
import           Language.Haskell.Exts hiding (parseFile, loc, name, binds, op)

import ReWire.Core.PrettyPrintHaskell

data ModMeta = ModMeta !RWCModule ![NExport]
      deriving Show

type Cache = StateT (Map.Map FilePath ModMeta) (ParseError IO)

_ppRWC :: ModMeta -> Cache ()
_ppRWC (ModMeta m exps) = do
      liftIO $ putStrLn "\nEXPORTS:"
      liftIO $ print exps
      liftIO $ putStrLn "\nMOD:"
      liftIO $ print $ ppHaskell m

_pp :: Module -> Cache ()
_pp = liftIO . putStrLn . prettyPrint

runCache :: Cache a -> IO (ParseResult a)
runCache = runParseError . (flip runStateT Map.empty >=> return . fst)

getModule :: FilePath -> Cache RWCModule
getModule fp = do
      ModMeta m  _ <- getCached fp
      return m

getExports :: FilePath -> Cache [NExport]
getExports fp = do
      ModMeta _  exps <- getCached fp
      return exps

getCached :: FilePath -> Cache ModMeta
getCached fp = do
      cached <- get
      -- This is where everything happens...
      case Map.lookup fp cached of
            Just mm  -> return mm
            Nothing -> do
                  m <- justParse fp
                  m' <- lift $ desugar m
                  rn <- mkRenamer m'
                  (rwcm, imps, exps) <- lift $ translate rn m'
                  mods <- mapM (getModule . toFilePath) imps

                  allExps <- mapM (getExports . toFilePath) imps
                  exps' <- foldM (transExport $ nub $ concat allExps) [] exps

                  cache fp $ ModMeta (mergeMods $ rwcm : mods) exps'
                  -- _ppRWC $ ModMeta (mergeMods $ rwcm : mods) exps'
                  getCached fp

      where justParse :: FilePath -> Cache Module
            justParse = (liftIO . Haskell.parseFile) >=> pr2Cache
            pr2Cache :: ParseResult a -> Cache a
            pr2Cache = \case
                  ParseOk p           -> return p
                  ParseFailed loc msg -> lift $ pFailAt loc msg
            mergeMods :: [RWCModule] -> RWCModule
            mergeMods = foldl' mergeMods' (RWCModule [] [])
                  where mergeMods' (RWCModule ts fs) (RWCModule ts' fs') = RWCModule (ts ++ ts') (fs ++ fs')
            transExport :: [NExport] -> [NExport] -> Export -> Cache [NExport]
            transExport iexps exps = \case
                  Export x        -> return $ insExport (NExport x) exps
                  ExportAll x     -> return $ insExport (NExportWith x (getCtors x iexps)) exps
                  ExportWith x cs -> return $ insExport (NExportWith x cs) exps
                  ExportMod m     -> do
                        exps' <- getExports (toFilePath m)
                        return $ foldl1 (.) (id:map insExport exps') exps

            getCtors :: FQName -> [NExport] -> [FQName]
            getCtors x = foldr getCtors' []
                  where getCtors' :: NExport -> [FQName] -> [FQName]
                        getCtors' (NExportWith x' cs)
                              | x == x'   = (cs ++)
                              | otherwise = id
                        getCtors' _       = id

            insExport :: NExport -> [NExport] -> [NExport]
            insExport (NExportWith x cs) exps = case find (cmpEq x) exps of
                  Just (NExportWith _ cs') -> NExportWith x (nub $ cs ++ cs') : filter (not . cmpEq x) exps
                  _                        -> NExportWith x cs : exps
                  where cmpEq x (NExportWith x' _) = x == x'
                        cmpEq _ _                  = False
            insExport x exps = nub $ x : exps

data NExport = NExport FQName | NExportWith FQName [FQName]
      deriving (Eq, Show)

cache :: FilePath -> ModMeta -> Cache ()
cache fp mm = modify $ Map.insert fp mm

mkRenamer :: Module -> Cache Renamer
mkRenamer (Module _ _ _ _ _ imps _) = do
      rns <- mapM toRenamer imps
      return $ mconcat rns

toRenamer :: ImportDecl -> Cache Renamer
toRenamer (ImportDecl loc m quald _ _ _ as specs) = do
      exps <- getExports $ toFilePath m
      callMrt exps as specs
      where callMrt exps Nothing   Nothing          = mkTable m Nothing (foldr unExp [] exps)
            callMrt exps (Just m') Nothing          = mkTable m' Nothing (foldr unExp [] exps)
            callMrt exps (Just m') (Just (h, imps)) = do
                  imps' <- foldM (getImp exps) [] imps
                  mkTable m' (Just (h, imps')) (foldr unExp [] exps)
            callMrt exps Nothing   (Just (h, imps)) = do
                  imps' <- foldM (getImp exps) [] imps
                  mkTable m (Just (h, imps')) (foldr unExp [] exps)

            unExp :: NExport -> [FQName] -> [FQName]
            unExp = \case
                  NExport x        -> (x :)
                  NExportWith x xs -> ((x : xs) ++)

            unqual :: FQName -> Name
            unqual (FQName _ x) = x

            getImp :: [NExport] -> [Name] -> ImportSpec -> Cache [Name]
            getImp exps imps = \case
                  IVar x              -> return $ x : imps
                  IAbs _ x            -> return $ x : imps
                  IThingAll x         -> return $ getCtors x exps ++ imps
                  IThingWith x cs     -> return $ x : map toName cs ++ imps
                  where toName :: CName -> Name
                        toName (VarName x) = x
                        toName (ConName x) = x

            -- TODO(chathhorn) duplication here...
            getCtors :: Name -> [NExport] -> [Name]
            getCtors x = foldr getCtors' []
                  where getCtors' :: NExport -> [Name] -> [Name]
                        getCtors' (NExportWith (FQName _ x') cs)
                              | x == x'   = (map unqual cs ++)
                              | otherwise = id
                        getCtors' _       = id

            mkTable :: ModuleName -> Maybe (Bool, [Name]) -> [FQName] -> Cache Renamer
            -- No list of imports -- so import everything.
            mkTable m' Nothing exps = mkTable m' (Just (False, map unqual exps)) exps
            -- List of imports, no "hiding".
            mkTable m' (Just (False, imps)) exps = foldM ins Map.empty imps
                  where ins table imp = case find (cmp imp) exps of
                              Just e -> return $
                                    let tab' = Map.insert (Qual m' imp) (deSigil e) table
                                    in if quald then tab' else Map.insert (UnQual imp) (deSigil e) tab'
                              Nothing  -> lift $ pFailAt loc $ "importing an unexported symbol from " ++ prettyPrint m
            -- List of imports with "hiding" -- import everything, then delete
            -- the items from the list.
            mkTable m' (Just (True, imps)) exps = do
                  tab <- mkTable m' Nothing exps
                  foldM del tab imps
                  where del table imp = case find (cmp imp) exps of
                              Just _ -> return
                                    $ Map.delete (Qual m' imp)
                                    $ Map.delete (UnQual imp) table
                              Nothing  -> lift $ pFailAt loc $ "importing an unexported symbol from " ++ prettyPrint m

            cmp :: Name -> FQName -> Bool
            cmp x (FQName _ x') = x == x'


