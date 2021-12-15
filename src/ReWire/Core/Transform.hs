{-# LANGUAGE FlexibleContexts #-}
module ReWire.Core.Transform ( mergeSlices , purgeUnused ) where

import ReWire.Core.Syntax
import ReWire.Annotation (unAnn)
import Control.Arrow ((&&&))
import Control.Monad.Reader (runReaderT, MonadReader (..), asks)
import Data.Containers.ListUtils (nubOrd)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

type DefnMap = HashMap GId [Exp]

-- | Removes all zero-length arguments and parameters.
mergeSlices :: MonadFail m => Program -> m Program
mergeSlices (Program start ds) = do
      Program start <$> runReaderT (mapM reDefn $ filter ((> 0) . sizeOf) ds) defnMap
      where defnMap :: DefnMap
            defnMap = Map.fromList $ map (defnName &&& defnBody) ds

reDefn :: (MonadReader DefnMap m, MonadFail m) => Defn -> m Defn
reDefn d = do
      body' <- reExps (renumLVars $ defnSig d) $ defnBody d
      pure d
            { defnSig  = reSig $ defnSig d
            , defnBody = body'
            }

renumLVars :: Sig -> LId -> Maybe (LId, Int)
renumLVars (Sig _ szVec _) x = if x >= 0 && x < length szVec && szVec !! x > 0
      then Just (x - preZeros, szVec !! x)
      else Nothing
      where preZeros :: Int
            preZeros = preZeros' szVec 0 x

            preZeros' :: [Int] -> Int -> Int -> Int
            preZeros' _ n 0  = n
            preZeros' [] n _ = n
            preZeros' (0 : szs) n m = preZeros' szs (n + 1) (m - 1)
            preZeros' (_ : szs) n m = preZeros' szs n (m - 1)

reExp :: (MonadReader DefnMap m, MonadFail m) => (LId -> Maybe (LId, Int)) -> Exp -> m [Exp]
reExp rn = \ case
      LVar a s l                                                  -> pure $ [LVar a s $ maybe l fst $ rn l]
      Lit a s v                                                   -> pure $ [Lit a s v]
      Call _ _ (Global g) es _ _  | sum (map sizeOf es) == 0      -> do
            Just body <- asks (Map.lookup g)
            reExps rn body
      Call _ _ (Global g) _ ps [] | length (filter isVar ps) == 0 -> do
            Just body <- asks (Map.lookup g)
            reExps rn body
      Call a s (Global g) es ps els                               -> do
            Just body <- asks (Map.lookup g)
            es'       <- reExps rn es
            els'      <- reExps rn els
            let g'  | isId ps body = Id
                    | isConst body = Const $ toConst body
                    | otherwise    = Global g
                ps' | isConst body = mergePats $ map varToWild ps
                    | otherwise    = mergePats ps
            pure [Call a s g' es' ps' (if alwaysMatches ps' then [] else els')]
      Call a s g es ps els                                        -> do
            es'       <- reExps rn es
            els'      <- reExps rn els
            let ps'    = mergePats ps
            pure [Call a s g es' ps' (if alwaysMatches ps' then [] else els')]
      where isVar :: Pat -> Bool
            isVar = \ case
                  PatVar {} -> True
                  _         -> False
            isWild :: Pat -> Bool
            isWild = \ case
                  PatWildCard {} -> True
                  _              -> False

            varToWild :: Pat -> Pat
            varToWild = \ case
                  PatVar an sz -> PatWildCard an sz
                  p            -> p

            isId :: [Pat] -> [Exp] -> Bool
            isId p body = unAnn (patToExp $ filter isVar p) == unAnn body

            isConst :: [Exp] -> Bool
            isConst = \ case
                  [Lit {}] -> True
                  _        -> False

            toConst :: [Exp] -> Int
            toConst = \ case
                  [Lit _ _ v] -> v
                  _           -> (-1)

            patToExp :: [Pat] -> [Exp]
            patToExp = zipWith patToExp' [0::Int ..]
                  where patToExp' :: Int -> Pat -> Exp
                        patToExp' i = \ case
                              PatVar      an sz   -> LVar an sz i
                              PatLit      an sz v -> Lit  an sz v
                              PatWildCard an sz   -> Lit  an sz 0


            alwaysMatches :: [Pat] -> Bool
            alwaysMatches = all (\ p -> isWild p || isVar p)

reExps :: (MonadReader DefnMap m, MonadFail m) => (LId -> Maybe (LId, Int)) -> [Exp] -> m [Exp]
reExps rn es = mergeLits <$> (concat <$> mapM (reExp rn) (filter ((> 0) . sizeOf) es))

mergeLits :: [Exp] -> [Exp]
mergeLits = \ case
      Lit a s 0 : (Lit _ s' 0) : es -> mergeLits $ Lit a (s + s') 0 : es
      e : es                        -> e : mergeLits es
      []                            -> []

reSig :: Sig -> Sig
reSig (Sig an ps r) = Sig an (filter (> 0) ps) r

mergePats :: [Pat] -> [Pat]
mergePats = \ case
      e : es | sizeOf e == 0                    -> mergePats es
      PatLit a s 0 : (PatLit _ s' 0) : es       -> mergePats $ PatLit a (s + s') 0 : es
      PatWildCard a s : (PatWildCard _ s') : es -> mergePats $ PatWildCard a (s + s') : es
      [PatWildCard _ _]                         -> [] -- TODO(chathhorn): not sure about this -- removes trailing WCs.
      e : es                                    -> e : mergePats es
      []                                        -> []

-- | Remove unused definitions.
purgeUnused :: MonadFail m => Program -> m Program
purgeUnused (Program start@(StartDefn _ _ _ (loop, _) (state0, _)) ds) = do
      pure $ Program start $ filter ((`elem` uses) . defnName) ds
      where uses :: [GId]
            uses = uses' [loop, state0]

            uses' :: [GId] -> [GId]
            uses' u = let u' = nubOrd (concatMap defnUses u) in
                  if length u' == length u then u else uses' u'

            defnUses :: GId -> [GId]
            defnUses d = maybe [d] ((<>[d]) . getUses) (Map.lookup d defnMap)

            getUses :: [Exp] -> [GId]
            getUses = concatMap $ \ case
                  Call _ _ (Global g) es _ es' -> [g] <> getUses es <> getUses es'
                  Call _ _ _          es _ es' -> getUses es <> getUses es'
                  _                            -> []

            defnMap :: DefnMap
            defnMap = Map.fromList $ map (defnName &&& defnBody) ds

