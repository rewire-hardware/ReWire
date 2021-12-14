{-# LANGUAGE FlexibleContexts #-}
module ReWire.Core.Transform ( mergeSlices ) where

import ReWire.Core.Syntax
import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))
import Control.Monad.Reader (runReader, MonadReader (..), asks)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

type DefnMap = HashMap GId [Exp]

-- | Removes all zero-length arguments and parameters.
mergeSlices :: Monad m => Program -> m Program
mergeSlices (Program start@(StartDefn _ _ _ (loop, _) (state0, _)) ds) = do
      pure $ Program start $ filter ((`elem` uses) . defnName) ds'
      where uses :: [GId]
            uses = [loop, state0] <> concatMap (getUses . defnBody) ds'

            getUses :: [Exp] -> [GId]
            getUses = concatMap getUses'

            getUses' :: Exp -> [GId]
            getUses' = \ case
                  Match _ _ g es _ es' -> [g] <> getUses es <> getUses es'
                  _                    -> []

            defnMap = Map.fromList $ map (defnName &&& defnBody) ds
            ds'     = runReader (mapM reDefn $ filter ((> 0) . sizeOf) ds) defnMap

reDefn :: MonadReader DefnMap m => Defn -> m Defn
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

reExp :: MonadReader DefnMap m => (LId -> Maybe (LId, Int)) -> Exp -> m [Exp]
reExp rn = \ case
      LVar a s l                                              -> pure $ [LVar a s $ maybe l fst $ rn l]
      Lit a s v                                               -> pure $ [Lit a s v]
      m@(Match _ _ g es _ _)  | sum (map sizeOf es) == 0      -> (fromMaybe [m] <$> asks (Map.lookup g)) >>= reExps rn
      m@(Match _ _ g _ ps []) | length (filter isVar ps) == 0 -> (fromMaybe [m] <$> asks (Map.lookup g)) >>= reExps rn
      Match a s g es ps es'                                   -> pure <$> (Match a s g <$> reExps rn es <*> pure (rePat ps) <*> reExps rn es')
      Extern a s txt args                                     -> pure <$> (Extern a s txt <$> reExps rn args)
      where isVar :: Pat -> Bool
            isVar = \ case
                  PatVar {} -> True
                  _         -> False

reExps :: MonadReader DefnMap m => (LId -> Maybe (LId, Int)) -> [Exp] -> m [Exp]
reExps rn es = mergeLits <$> (concat <$> mapM (reExp rn) (filter ((> 0) . sizeOf) es))

mergeLits :: [Exp] -> [Exp]
mergeLits = \ case
      Lit a s 0 : (Lit _ s' 0) : es -> mergeLits $ Lit a (s + s') 0 : es
      e : es                        -> e : mergeLits es
      []                            -> []

reSig :: Sig -> Sig
reSig (Sig an ps r) = Sig an (filter (> 0) ps) r

rePat :: [Pat] -> [Pat]
rePat = filter ((> 0) . sizeOf)
