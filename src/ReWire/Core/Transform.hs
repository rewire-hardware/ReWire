{-# LANGUAGE FlexibleContexts #-}
module ReWire.Core.Transform ( mergeSlices , purgeUnused, partialEval ) where

import ReWire.Core.Syntax
import ReWire.Core.Interp (interpDefn, interpExps, patMatches, patApply, DefnMap)
import ReWire.Annotation (Annote, unAnn, ann)
import Control.Arrow ((&&&))
import Control.Monad.Reader (runReaderT, MonadReader (..), asks)
import Data.Containers.ListUtils (nubOrd)
import Data.List (genericLength, genericIndex)
import Data.BitVector (BV, bitVec, int, width, zeros)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

type DefnBodyMap = HashMap GId [Exp]

-- | Removes all zero-length arguments and parameters.
mergeSlices :: MonadFail m => Program -> m Program
mergeSlices (Program start ds) = do
      Program start <$> runReaderT (mapM reDefn $ filter ((> 0) . sizeOf) ds) defnMap
      where defnMap :: DefnBodyMap
            defnMap = Map.fromList $ map (defnName &&& defnBody) ds

reDefn :: (MonadReader DefnBodyMap m, MonadFail m) => Defn -> m Defn
reDefn d = do
      body' <- reExps (renumLVars $ defnSig d) $ defnBody d
      pure d
            { defnSig  = reSig $ defnSig d
            , defnBody = body'
            }

renumLVars :: Sig -> LId -> Maybe (LId, Size)
renumLVars (Sig _ szVec _) x = if x >= 0 && x < genericLength szVec && genericIndex szVec x > 0
      then Just (x - preZeros, genericIndex szVec x)
      else Nothing
      where preZeros :: LId
            preZeros = preZeros' szVec 0 x

            preZeros' :: [Size] -> LId -> LId -> LId
            preZeros' _         n 0 = n
            preZeros' []        n _ = n
            preZeros' (0 : szs) n m = preZeros' szs (n + 1) (m - 1)
            preZeros' (_ : szs) n m = preZeros' szs n       (m - 1)

reExp :: (MonadReader DefnBodyMap m, MonadFail m) => (LId -> Maybe (LId, Size)) -> Exp -> m [Exp]
reExp rn = \ case
      LVar a s l                                   -> pure $ [LVar a s $ maybe l fst $ rn l]
      Lit a bv                                     -> pure $ [Lit a bv]
      Call _ _ (Global g) es _ _
            | sum (map sizeOf es) == 0             -> do
            Just body <- asks (Map.lookup g)
            reExps rn body
      Call _ _ (Global g) _ ps []
            | length (filter isVar ps) == 0 -> do
            Just body <- asks (Map.lookup g)
            reExps rn body
      Call a s (Global g) es ps els                -> do
            Just body <- asks (Map.lookup g)
            es'       <- reExps rn es
            els'      <- reExps rn els
            let g'  | isId ps body = Id
                    | isConst body = Const $ toConst body
                    | otherwise    = Global g
                ps' | isConst body = mergePats $ map varToWild ps
                    | otherwise    = mergePats ps
            pure [Call a s g' es' ps' (if alwaysMatches ps' then [] else els')]
      Call a s g es ps els                         -> do
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

            toConst :: [Exp] -> Value
            toConst = \ case
                  [Lit _ bv] -> int bv
                  _          -> (-1)

            patToExp :: [Pat] -> [Exp]
            patToExp = zipWith patToExp' [0::LId ..]
                  where patToExp' :: LId -> Pat -> Exp
                        patToExp' i = \ case
                              PatVar      an sz -> LVar an sz i
                              PatLit      an bv -> Lit  an bv
                              PatWildCard an sz -> Lit  an (zeros $ fromIntegral sz)


            alwaysMatches :: [Pat] -> Bool
            alwaysMatches = all (\ p -> isWild p || isVar p)

reExps :: (MonadReader DefnBodyMap m, MonadFail m) => (LId -> Maybe (LId, Size)) -> [Exp] -> m [Exp]
reExps rn es = mergeLits <$> (concat <$> mapM (reExp rn) (filter ((> 0) . sizeOf) es))

mergeLits :: [Exp] -> [Exp]
mergeLits = \ case
      Lit a bv : (Lit _ bv') : es -> mergeLits $ Lit a (bv <> bv') : es
      e : es                      -> e : mergeLits es
      []                          -> []

reSig :: Sig -> Sig
reSig (Sig an ps r) = Sig an (filter (> 0) ps) r

mergePats :: [Pat] -> [Pat]
mergePats = \ case
      p : ps | sizeOf p == 0                    -> mergePats ps
      PatLit a bv : (PatLit _ bv') : ps         -> mergePats $ PatLit a (bv <> bv') : ps
      PatWildCard a s : (PatWildCard _ s') : es -> mergePats $ PatWildCard a (s + s') : es
      [PatWildCard _ _]                         -> [] -- TODO(chathhorn): not sure about this -- removes trailing WCs.
      p : ps                                    -> p : mergePats ps
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

            defnMap :: DefnBodyMap
            defnMap = Map.fromList $ map (defnName &&& defnBody) ds

partialEval :: MonadFail m => Program -> m Program
partialEval (Program start ds) = pure $ Program start $ map (evalDefn defnMap) ds
      where defnMap :: DefnMap
            defnMap = Map.fromList $ map (defnName &&& id) ds

evalDefn :: DefnMap -> Defn -> Defn
evalDefn defns (Defn an n sig body) = Defn an n sig $ evalExps defns body

evalExps :: DefnMap -> [Exp] -> [Exp]
evalExps _     []         = []
evalExps defns es@(e : _) = if all isValue es' then [Lit (ann e) $ interpExps defns es' []] else es'
      where es' :: [Exp]
            es' = map (evalExp defns) es

isValue :: Exp -> Bool
isValue = \ case
      Lit {} -> True
      _      -> False

isValue' :: [Exp] -> Bool
isValue' = \ case
      [e] -> isValue e
      _   -> False

toValue :: Exp -> Maybe BV
toValue = \ case
      Lit _ bv -> Just bv
      _        -> Nothing

toValue' :: [Exp] -> Maybe BV
toValue' = \ case
      [e] -> toValue e
      _   -> Nothing

evalExp :: DefnMap -> Exp -> Exp
evalExp defns =  \ case
      Call an sz (Global (lkupDefn -> Just g)) (evalExps' -> Just v) ps els -> if patMatches v ps
            then Lit an $ interpDefn defns g $ patApply v id ps
            else deplural an $ evalExps defns els
      Call an sz t es ps els       -> Call an sz t (evalExps defns es) ps (evalExps defns els)
      e                            -> e
      where lkupDefn :: Name -> Maybe Defn
            lkupDefn = flip Map.lookup defns

            evalExps' :: [Exp] -> Maybe BV
            evalExps' = toValue' . evalExps defns

            deplural :: Annote -> [Exp] -> Exp
            deplural an = \ case
                  [e] -> e
                  es  -> let sz = sum $ map sizeOf es in Call an sz Id es [PatVar an sz] []

