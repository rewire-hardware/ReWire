{-# LANGUAGE Rank2Types, FlexibleInstances, TupleSections, LambdaCase, ViewPatterns, NamedFieldPuns #-}
module ReWire.FrontEnd.Rename
      ( Renamer, fixFixity, getExports, allExports
      , exclude, extend, finger, rename
      , FQName (mod, name), qnamish
      , Namespace (..)
      , Exports, expValue, expType, expFixity, getCtors
      , toFilePath
      , fromImps
      ) where

import ReWire.Core.Syntax (Annotation)
import ReWire.Error
import ReWire.FrontEnd.Fixity

import Control.Arrow ((&&&))
import Control.Monad (liftM, foldM)
import Data.Functor ((<$>))
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid (..), (<>))
import Language.Haskell.Exts.Annotated.Fixity (Fixity (..), AppFixity (..))
import Language.Haskell.Exts.Annotated.Simplify (sName, sQName)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import System.FilePath (joinPath, (<.>))

import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as Set
import qualified Language.Haskell.Exts.Syntax as S

import Language.Haskell.Exts.Annotated.Syntax hiding (Namespace, Annotation)

-- Note that GHC (although we might not catch this) disallows the same symbol
-- appearing twice in an export list (e.g., with different qualifiers, from
-- different modules), but clearly you can import the same symbol (defined in
-- the same or different modules) twice with different qualifiers.
data Exports = Exports
      (Set.Set FQName)                   -- Values
      (Set.Set FQName)                   -- Types
      (Set.Set Fixity)                   -- Fixities
      (Map.Map S.Name (Set.Set FQName))  -- Type |-> Ctors (where Type and Ctors are also in Types and Values, respectively)
      deriving Show

expValue :: FQName -> Exports -> Exports
expValue x (Exports vs ts fs cs) = Exports (Set.insert x vs) ts fs cs

expType :: FQName -> Set.Set FQName -> Exports -> Exports
expType x cs' (Exports vs ts fs cs) = Exports (cs' <> vs) (Set.insert x ts) fs (Map.insert (qnamish x) cs' cs)

expFixity :: S.Assoc -> Int -> S.Name -> Exports -> Exports
expFixity asc lvl x (Exports vs ts fs cs) = Exports vs ts (Set.insert (Fixity asc lvl $ S.UnQual x) fs) cs

getCtors :: S.Name -> Exports -> Set.Set FQName
getCtors x (Exports _ _ _ cs) = fromMaybe mempty $ Map.lookup x cs

fixities :: Exports -> [S.Name] -> [Fixity]
fixities (Exports _ _ fs _) ns = Set.toList $ Set.filter (\ (Fixity _ _ n') -> n' `elem` map S.UnQual ns) fs

instance Monoid Exports where
      mempty = Exports mempty mempty mempty mempty
      mappend (Exports a b c d) (Exports a' b' c' d') = Exports (a <> a') (b <> b') (c <> c') (Map.unionWith mappend d d')

data Namespace = Type | Value
      deriving (Ord, Eq, Show)

data FQName = FQName { mod :: !S.ModuleName, name :: !S.Name }
      deriving (Ord, Eq, Show)

-- | Sometimes-partial conversion between name-like things.
class QNamish a where
      toQNamish :: a -> S.QName
      fromQNamish :: S.QName -> a

qnamish :: (QNamish a, QNamish b) => a -> b
qnamish = fromQNamish . toQNamish

instance QNamish S.QName where
      toQNamish = id
      fromQNamish = id
instance QNamish (QName a) where
      toQNamish = sQName
      fromQNamish (S.Qual (S.ModuleName m) n) = Qual undefined (ModuleName undefined m) $ fakeAnnote n
      fromQNamish (S.UnQual n)                = UnQual undefined $ fakeAnnote n
      fromQNamish n                           = UnQual undefined $ fakeAnnote $ S.Ident $ prettyPrint n
instance QNamish S.Name where
      toQNamish = S.UnQual
      fromQNamish (S.Qual _ n) = n
      fromQNamish (S.UnQual n) = n
      fromQNamish n            = S.Ident $ prettyPrint n
instance QNamish (Name a) where
      toQNamish = toQNamish . sName
      fromQNamish (S.Qual _ n) = fakeAnnote n
      fromQNamish (S.UnQual n) = fakeAnnote n
      fromQNamish n            = fakeAnnote $ S.Ident $ prettyPrint n
instance QNamish FQName where
      toQNamish (FQName m n) = S.Qual m n
      fromQNamish (S.Qual m n) = FQName m n
      fromQNamish (S.UnQual n) = FQName (S.ModuleName "") n
      -- ^ This definition kinda defeats the purpose of FQName.
      fromQNamish n            = FQName (S.ModuleName "") $ S.Ident $ prettyPrint n
instance QNamish String where
      toQNamish = S.UnQual . S.Ident
      fromQNamish (S.Qual (S.ModuleName "") (S.Ident x))  = x
      fromQNamish (S.Qual (S.ModuleName "") (S.Symbol x)) = x
      fromQNamish (S.Qual (S.ModuleName m) (S.Ident x))   = m ++ "." ++ x
      fromQNamish (S.Qual (S.ModuleName m) (S.Symbol x))  = m ++ "." ++ x
      fromQNamish (S.UnQual (S.Ident x))                  = x
      fromQNamish (S.UnQual (S.Symbol x))                 = x
      fromQNamish n                                       = prettyPrint n

fakeAnnote :: S.Name -> Name a
fakeAnnote (S.Ident x)  = Ident undefined x
fakeAnnote (S.Symbol x) = Symbol undefined x

data Renamer = Renamer
      { rnNames    :: Map.Map (Namespace, S.QName) FQName
      , rnExports  :: Map.Map S.ModuleName Exports
      , rnFixities :: Set.Set Fixity
      }

instance Monoid Renamer where
      mempty = Renamer mempty mempty mempty
      mappend (Renamer a b c) (Renamer a' b' c') = Renamer (a <> a') (b <> b') (c <> c')

rename :: (QNamish a, QNamish b) => Namespace -> Renamer -> a -> b
rename ns Renamer { rnNames } x = fromQNamish . maybe (toQNamish x) toQNamish $ Map.lookup (ns, toQNamish x) rnNames

extend :: QNamish a => Namespace -> [(a, FQName)] -> Renamer -> Renamer
extend ns kvs rn@Renamer { rnNames } = rn { rnNames = Map.fromList (map (((ns,) . toQNamish . fst) &&& snd) kvs) `Map.union` rnNames }

exclude :: QNamish a => Namespace -> [a] -> Renamer -> Renamer
exclude ns ks rn@Renamer { rnNames } = rn { rnNames = foldr (Map.delete . (ns,) . toQNamish) rnNames ks }

fixFixity :: Monad m => Renamer -> Module SrcSpanInfo -> m (Module SrcSpanInfo)
fixFixity Renamer { rnFixities } = liftM deuniquifyLocalOps . applyFixities (Set.toList rnFixities) . fixLocalOps

addFixities :: [Fixity] -> Renamer -> Renamer
addFixities fixities' rn@Renamer { rnFixities } = rn { rnFixities = rnFixities `Set.union` Set.fromList fixities' }

addExports :: S.ModuleName -> Exports -> Renamer -> Renamer
addExports m exps rn@Renamer { rnExports } = rn { rnExports = Map.insert m exps rnExports }

getExports :: S.ModuleName -> Renamer -> Exports
getExports m Renamer { rnExports } = fromMaybe mempty $ Map.lookup m rnExports

allExports :: Renamer -> Exports
allExports Renamer { rnExports } = mconcat $ Map.elems rnExports

requalFixity :: S.ModuleName -> Fixity -> Fixity
requalFixity m (Fixity asc lvl n) = Fixity asc lvl $ case n of
      S.Qual _ n' -> S.Qual m n'
      S.UnQual n' -> S.Qual m n'
      n'          -> S.Qual m $ qnamish n'

filterFixities :: (S.Name -> Bool) -> [Fixity] -> [Fixity]
filterFixities p = filter $ \ (Fixity _ _ n) -> p $ qnamish n

-- | True iff an entry for the name exists in the renamer.
finger :: QNamish a => Namespace -> Renamer -> a -> Bool
finger ns Renamer { rnNames } = flip Map.member rnNames . (ns,) . toQNamish

toFilePath :: S.ModuleName -> FilePath
toFilePath (S.ModuleName n) = joinPath (splitOn "." n) <.> "hs"

lookupExp :: (Annotation a, SyntaxError m) => Namespace -> Name a -> Exports -> m FQName
lookupExp ns x (Exports vs ts _ _) = case ns of
      Value -> lkup vs
      Type  -> lkup ts
      where lkup :: SyntaxError m => Set.Set FQName -> m FQName
            lkup xs = case find cmp (Set.toList xs) of
                  Just x' -> return x'
                  Nothing -> failAt (ann x) $ "Attempting to import an unexported symbol: " ++ prettyPrint x

            cmp :: FQName -> Bool
            cmp (FQName _ x') = sName x == x'

noExps :: Maybe (Bool, Imports SrcSpanInfo)
noExps = Nothing

type Imports a = ([(Namespace, Name a)], [Fixity])

-- | Build renamer from a single import line. Should work on either pre- or
-- post-desugared import lists. Params: module being imported, "qualified",
-- exports (of this import), "... as <qualifier>", imports.
fromImps :: (Annotation a, Functor m, SyntaxError m) => S.ModuleName -> Bool -> Exports -> Maybe S.ModuleName -> Maybe (ImportSpecList a) -> m Renamer
fromImps m quald exps Nothing   Nothing                          = addExports m exps <$> fromImps' m quald noExps exps
fromImps m quald exps (Just m') Nothing                          = addExports m exps <$> fromImps' m' quald noExps exps
fromImps m quald exps (Just m') (Just (ImportSpecList _ h imps)) = do
      imps' <- foldM (getImp exps) mempty imps
      addExports m exps <$> fromImps' m' quald (Just (h, imps')) exps
fromImps m quald exps Nothing (Just (ImportSpecList _ h imps))   = do
      imps' <- foldM (getImp exps) mempty imps
      addExports m exps <$> fromImps' m quald (Just (h, imps')) exps

fromImps' :: (Annotation a, SyntaxError m) => S.ModuleName -> Bool -> Maybe (Bool, Imports a) -> Exports -> m Renamer
-- No list of imports -- so import everything.
fromImps' m' quald Nothing exps = fromImps' m' quald (Just (False, toImports exps)) exps
      where toImports :: Exports -> Imports SrcSpanInfo
            toImports (Exports vs ts fs _) =
                  ( map ((Value,) . qnamish) (Set.toList vs) ++ map ((Type,) . qnamish) (Set.toList ts)
                  , Set.toList $ fs <> Set.map (requalFixity m') fs
                  )
-- List of imports, no "hiding".
fromImps' m' quald (Just (False, (imps, fs))) exps = foldM ins mempty imps
      where ins :: (Annotation a, SyntaxError m) => Renamer -> (Namespace, Name a) -> m Renamer
            ins tab (ns, x) = do
                  e <- lookupExp ns x exps
                  let xs' = (S.Qual m' $ sName x, e)                                : if quald then [] else [(S.UnQual $ sName x, e)]
                      fs' = map (requalFixity m') (filterFixities (== sName x) fs) ++ if quald then [] else filterFixities (== sName x) fs
                  return $ extend ns xs' $ addFixities fs' tab
-- List of imports with "hiding" -- import everything, then delete the items
-- from the list.
fromImps' m' quald (Just (True, (imps, fs))) exps = do
      tab <- fromImps' m' quald noExps exps
      foldM del tab imps
      where del :: (Annotation a, SyntaxError m) => Renamer -> (Namespace, Name a) -> m Renamer
            del tab (ns, x) = return
                  $ exclude ns [S.Qual m'$ sName x, S.UnQual $ sName x]
                  $ addFixities (filterFixities (/= sName x) fs) tab

getImp :: (Annotation a, SyntaxError m) => Exports -> Imports a -> ImportSpec a -> m (Imports a)
getImp exps (imps, fs) = \ case
      IVar _ x          -> do
            _ <- lookupExp Value x exps
            return ((Value, x) : imps, fixities exps [sName x] ++ fs)
      IAbs _ _ x        -> do
            _ <- lookupExp Type x exps
            return ((Type, x) : imps, fs)
      IThingAll _ x     -> do
            _ <- lookupExp Type x exps
            return ( (Type, x) : (map ((Value,) . qnamish) (Set.toList $ getCtors (sName x) exps) ++ imps)
                   , fixities exps (map name (Set.toList $ getCtors (sName x) exps)) ++ fs
                   )
      IThingWith _ x cs -> do
            _ <- lookupExp Type x exps
            mapM_ (flip (lookupExp Value) exps . toName) cs
            return ( (Type, x) : (map ((Value,) . toName) cs ++ imps)
                   , fixities exps (map (sName . toName) cs) ++ fs
                   )
      where toName :: CName a -> Name a
            toName (VarName _ x) = x
            toName (ConName _ x) = x
