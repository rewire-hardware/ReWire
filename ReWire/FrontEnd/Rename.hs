{-# LANGUAGE Rank2Types, FlexibleInstances, FlexibleContexts, TupleSections, LambdaCase, ViewPatterns, NamedFieldPuns #-}
{-# LANGUAGE Safe #-}
module ReWire.FrontEnd.Rename
      ( Renamer, fixFixity, getExports, allExports
      , exclude, extend, finger, rename
      , FQName (mod, name), qnamish
      , Namespace (..), Module (..)
      , Exports, expValue, expType, expFixity, getCtors
      , toFilePath
      , fromImps
      ) where

import ReWire.Annotation (Annotation, Annote, noAnn)
import ReWire.Error
import ReWire.FrontEnd.Fixity
import ReWire.FrontEnd.Syntax (DataDefn, Defn)

import Control.Arrow ((&&&))
import Control.Monad (liftM, foldM, void)
import Control.Monad.Fail (MonadFail)
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Language.Haskell.Exts.Fixity (Fixity (..), AppFixity (..))
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo, noSrcSpan)
import System.FilePath (joinPath, (<.>))

import qualified Data.Map.Strict                        as Map
import qualified Data.Set                               as Set
import qualified Language.Haskell.Exts.Syntax           as S

import Language.Haskell.Exts.Syntax hiding (Namespace, Annotation, Module)

-- Note that GHC (although we might not catch this) disallows the same symbol
-- appearing twice in an export list (e.g., with different qualifiers, from
-- different modules), but clearly you can import the same symbol (defined in
-- the same or different modules) twice with different qualifiers.
data Exports = Exports
      (Set.Set FQName)                     -- Values
      (Set.Set FQName)                     -- Types
      (Set.Set Fixity)                     -- Fixities
      (Map.Map (Name ()) (Set.Set FQName)) -- Type |-> Ctors (where Type and Ctors are also in Types and Values, respectively)
      deriving Show

data Module = Module [DataDefn] [Defn]

instance Semigroup Module where
      (Module a b) <> (Module a' b') = Module (a ++ a') (b ++ b')

instance Monoid Module where
      mempty = Module [] []

---

expValue :: FQName -> Exports -> Exports
expValue x (Exports vs ts fs cs) = Exports (Set.insert x vs) ts fs cs

expType :: FQName -> Set.Set FQName -> Exports -> Exports
expType x cs' (Exports vs ts fs cs) = Exports (cs' <> vs) (Set.insert x ts) fs (Map.insert (qnamish x) cs' cs)

expFixity :: Assoc () -> Int -> Name () -> Exports -> Exports
expFixity asc lvl x (Exports vs ts fs cs) = Exports vs ts (Set.insert (Fixity asc lvl $ UnQual () x) fs) cs

getCtors :: Name () -> Exports -> Set.Set FQName
getCtors x (Exports _ _ _ cs) = fromMaybe mempty $ Map.lookup x cs

fixities :: Exports -> [Name ()] -> [Fixity]
fixities (Exports _ _ fs _) ns = Set.toList $ Set.filter (\ (Fixity _ _ n') -> n' `elem` map (UnQual ()) ns) fs

instance Semigroup Exports where
      (Exports a b c d) <> (Exports a' b' c' d') = Exports (a <> a') (b <> b') (c <> c') (Map.unionWith mappend d d')

instance Monoid Exports where
      mempty = Exports mempty mempty mempty mempty

data Namespace = Type | Value
      deriving (Ord, Eq, Show)

data FQName = FQName { mod :: !(ModuleName ()), name :: !(Name ())}
      deriving (Ord, Eq, Show)

-- | Sometimes-partial conversion between name-like things.
class QNamish a where
      toQNamish :: a -> QName ()
      fromQNamish :: QName l -> a

qnamish :: (QNamish a, QNamish b) => a -> b
qnamish = fromQNamish . toQNamish

instance QNamish (QName a) where
      toQNamish = void
      fromQNamish (Qual _ (ModuleName _ m) n) = Qual undefined (ModuleName undefined m) $ undefined <$ n
      fromQNamish (UnQual _ n)                = UnQual undefined $ undefined <$ n
      fromQNamish n                           = UnQual undefined $ (undefined <$) $ Ident undefined $ prettyPrint n
instance QNamish (Name ()) where
      toQNamish = UnQual () . void
      fromQNamish (Qual _ _ n) = () <$ n
      fromQNamish (UnQual _ n) = () <$ n
      fromQNamish n            = (() <$) $ Ident () $ prettyPrint n
instance QNamish (Name SrcSpanInfo) where
      toQNamish = UnQual () . void
      fromQNamish (Qual _ _ n) = noSrcSpan <$ n
      fromQNamish (UnQual _ n) = noSrcSpan <$ n
      fromQNamish n            = (noSrcSpan <$) $ Ident noSrcSpan $ prettyPrint n
instance QNamish (Name Annote) where
      toQNamish = UnQual () . void
      fromQNamish (Qual _ _ n) = noAnn <$ n
      fromQNamish (UnQual _ n) = noAnn <$ n
      fromQNamish n            = (noAnn <$) $ Ident noAnn $ prettyPrint n
instance QNamish FQName where
      toQNamish (FQName m n) = Qual () m n
      fromQNamish (Qual _ m n) = FQName (void m) (void n)
      fromQNamish (UnQual _ n) = FQName (ModuleName () "") $ void n
      -- ^ This definition kinda defeats the purpose of FQName.
      fromQNamish n            = FQName (ModuleName () "") $ Ident () $ prettyPrint n
instance QNamish String where
      toQNamish = UnQual () . Ident ()
      fromQNamish (Qual _ (ModuleName _ "") (Ident _ x))  = x
      fromQNamish (Qual _ (ModuleName _ "") (Symbol _ x)) = x
      fromQNamish (Qual _ (ModuleName _ m) (Ident _ x))   = m ++ "." ++ x
      fromQNamish (Qual _ (ModuleName _ m) (Symbol _ x))  = m ++ "." ++ x
      fromQNamish (UnQual _ (Ident _ x))                  = x
      fromQNamish (UnQual _ (Symbol _ x))                 = x
      fromQNamish n                                       = prettyPrint n

data Renamer = Renamer
      { rnNames    :: Map.Map (Namespace, QName ()) FQName
      , rnExports  :: Map.Map (ModuleName ()) Exports
      , rnFixities :: Set.Set Fixity
      }

instance Semigroup Renamer where
      (Renamer a b c) <> (Renamer a' b' c') = Renamer (a <> a') (b <> b') (c <> c')

instance Monoid Renamer where
      mempty = Renamer mempty mempty mempty

rename :: (QNamish a, QNamish b) => Namespace -> Renamer -> a -> b
rename ns Renamer { rnNames } x = fromQNamish . maybe (toQNamish x) toQNamish $ Map.lookup (ns, toQNamish x) rnNames

extend :: QNamish a => Namespace -> [(a, FQName)] -> Renamer -> Renamer
extend ns kvs rn@Renamer { rnNames } = rn { rnNames = Map.fromList (map (((ns,) . toQNamish . fst) &&& snd) kvs) `Map.union` rnNames }

exclude :: QNamish a => Namespace -> [a] -> Renamer -> Renamer
exclude ns ks rn@Renamer { rnNames } = rn { rnNames = foldr (Map.delete . (ns,) . toQNamish) rnNames ks }

fixFixity :: MonadFail m => Renamer -> S.Module SrcSpanInfo -> m (S.Module SrcSpanInfo)
fixFixity Renamer { rnFixities } = liftM deuniquifyLocalOps . applyFixities (Set.toList rnFixities) . fixLocalOps

addFixities :: [Fixity] -> Renamer -> Renamer
addFixities fixities' rn@Renamer { rnFixities } = rn { rnFixities = rnFixities `Set.union` Set.fromList fixities' }

addExports :: ModuleName () -> Exports -> Renamer -> Renamer
addExports m exps rn@Renamer { rnExports } = rn { rnExports = Map.insert m exps rnExports }

getExports :: ModuleName () -> Renamer -> Exports
getExports m Renamer { rnExports } = fromMaybe mempty $ Map.lookup m rnExports

allExports :: Renamer -> Exports
allExports Renamer { rnExports } = mconcat $ Map.elems rnExports

requalFixity :: ModuleName () -> Fixity -> Fixity
requalFixity m (Fixity asc lvl n) = Fixity asc lvl $ case n of
      Qual _ _ n' -> Qual () m n'
      UnQual _ n' -> Qual () m n'
      n'          -> Qual () m $ qnamish n'

filterFixities :: (Name () -> Bool) -> [Fixity] -> [Fixity]
filterFixities p = filter $ \ (Fixity _ _ n) -> p $ qnamish n

-- | True iff an entry for the name exists in the renamer.
finger :: Namespace -> Renamer -> QName () -> Bool
finger ns Renamer { rnNames } = flip Map.member rnNames . (ns,) . toQNamish

toFilePath :: ModuleName a -> FilePath
toFilePath (ModuleName _ n) = joinPath (splitOn "." n) <.> "hs"

lookupExp :: (Annotation a, MonadError AstError m) => Namespace -> Name a -> Exports -> m FQName
lookupExp ns x (Exports vs ts _ _) = case ns of
      Value -> lkup vs
      Type  -> lkup ts
      where lkup :: MonadError AstError m => Set.Set FQName -> m FQName
            lkup xs = case find cmp (Set.toList xs) of
                  Just x' -> return x'
                  Nothing -> failAt (ann x) $ "Attempting to import an unexported symbol: " ++ prettyPrint x

            cmp :: FQName -> Bool
            cmp (FQName _ x') = void x == x'

noExps :: Maybe (Bool, Imports SrcSpanInfo)
noExps = Nothing

type Imports a = ([(Namespace, Name a)], [Fixity])

-- | Build renamer from a single import line. Should work on either pre- or
-- post-desugared import lists. Params: module being imported, "qualified",
-- exports (of this import), "... as <qualifier>", imports.
fromImps :: (Functor m, MonadError AstError m) => ModuleName () -> Bool -> Exports -> Maybe (ModuleName ()) -> Maybe (ImportSpecList SrcSpanInfo) -> m Renamer
fromImps m quald exps Nothing   Nothing                          = addExports m exps <$> fromImps' m quald noExps exps
fromImps m quald exps (Just m') Nothing                          = addExports m exps <$> fromImps' m' quald noExps exps
fromImps m quald exps (Just m') (Just (ImportSpecList _ h imps)) = do
      imps' <- foldM (getImp exps) mempty imps
      addExports m exps <$> fromImps' m' quald (Just (h, imps')) exps
fromImps m quald exps Nothing (Just (ImportSpecList _ h imps))   = do
      imps' <- foldM (getImp exps) mempty imps
      addExports m exps <$> fromImps' m quald (Just (h, imps')) exps

fromImps' :: (Annotation a, MonadError AstError m) => ModuleName () -> Bool -> Maybe (Bool, Imports a) -> Exports -> m Renamer
-- No list of imports -- so import everything.
fromImps' m' quald Nothing exps = fromImps' m' quald (Just (False, toImports exps)) exps
      where toImports :: Exports -> Imports SrcSpanInfo
            toImports (Exports vs ts fs _) =
                  ( map ((Value,) . qnamish) (Set.toList vs) ++ map ((Type,) . qnamish) (Set.toList ts)
                  , Set.toList $ fs <> Set.map (requalFixity m') fs
                  )
-- List of imports, no "hiding".
fromImps' m' quald (Just (False, (imps, fs))) exps = foldM ins mempty imps
      where ins :: (Annotation a, MonadError AstError m) => Renamer -> (Namespace, Name a) -> m Renamer
            ins tab (ns, x) = do
                  e <- lookupExp ns x exps
                  let xs' = (Qual () m' $ void x, e)                                : if quald then [] else [(UnQual () $ void x, e)]
                      fs' = map (requalFixity m') (filterFixities (== void x) fs) ++ if quald then [] else filterFixities (== void x) fs
                  return $ extend ns xs' $ addFixities fs' tab
-- List of imports with "hiding" -- import everything, then delete the items
-- from the list.
fromImps' m' quald (Just (True, (imps, fs))) exps = do
      tab <- fromImps' m' quald noExps exps
      foldM del tab imps
      where del :: (Annotation a, MonadError AstError m) => Renamer -> (Namespace, Name a) -> m Renamer
            del tab (ns, x) = return
                  $ exclude ns [Qual () m' $ void x, UnQual () $ void x]
                  $ addFixities (filterFixities (/= void x) fs) tab

getImp :: (MonadError AstError m) => Exports -> Imports SrcSpanInfo -> ImportSpec SrcSpanInfo -> m (Imports SrcSpanInfo)
getImp exps (imps, fs) = \ case
      IVar _ x          -> do
            _ <- lookupExp Value x exps
            return ((Value, x) : imps, fixities exps [void x] ++ fs)
      IAbs _ _ x        -> do
            _ <- lookupExp Type x exps
            return ((Type, x) : imps, fs)
      IThingAll _ x     -> do
            _ <- lookupExp Type x exps
            return ( (Type, x) : (map ((Value,) . qnamish) (Set.toList $ getCtors (void x) exps) ++ imps)
                   , fixities exps (map name (Set.toList $ getCtors (void x) exps)) ++ fs
                   )
      IThingWith _ x cs -> do
            _ <- lookupExp Type x exps
            mapM_ (flip (lookupExp Value) exps . toName) cs
            return ( (Type, x) : (map ((Value,) . toName) cs ++ imps)
                   , fixities exps (map (void . toName) cs) ++ fs
                   )
      where toName :: CName a -> Name a
            toName (VarName _ x) = x
            toName (ConName _ x) = x
