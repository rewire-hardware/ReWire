{-# LANGUAGE FlexibleContexts #-}
module ReWire.Core.Transform (mergeSlices, purgeUnused, partialEval, dedupe) where

import ReWire.Annotation (unAnn, ann)
import ReWire.Core.Interp (interpExp, DefnMap)
import ReWire.Core.Syntax

import Control.Arrow ((&&&))
import Control.Monad.Reader (runReaderT, MonadReader (..), asks)
import Data.BitVector (BV, zeros)
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import Data.List (genericLength, genericIndex)
import qualified Data.BitVector as BV
import qualified Data.HashMap.Strict as Map

type DefnBodyMap = HashMap GId Exp

-- | Removes all zero-length arguments and parameters.
mergeSlices :: MonadFail m => Program -> m Program
mergeSlices (Program start ds) = Program start <$> runReaderT (mapM reDefn $ filter ((> 0) . sizeOf) ds) defns
      where defns :: DefnMap
            defns = Map.fromList $ map (defnName &&& id) ds

            reDefn :: (MonadReader DefnMap m, MonadFail m) => Defn -> m Defn
            reDefn (Defn an n sig body) = Defn an n (reSig sig) <$> reExp (renumLVars sig) body

            reSig :: Sig -> Sig
            reSig (Sig an ps r) = Sig an (filter (> 0) ps) r

            renumLVars :: Sig -> LId -> Maybe (LId, Size)
            renumLVars (Sig _ szVec _) x | x >= 0, x < genericLength szVec, genericIndex szVec x > 0 = Just (x - preZeros, genericIndex szVec x)
                                         | otherwise                                                 = Nothing
                  where preZeros :: LId
                        preZeros = preZeros' szVec 0 x

                        preZeros' :: [Size] -> LId -> LId -> LId
                        preZeros' _         n 0 = n
                        preZeros' []        n _ = n
                        preZeros' (0 : szs) n m = preZeros' szs (n + 1) (m - 1)
                        preZeros' (_ : szs) n m = preZeros' szs n       (m - 1)

            reExp :: MonadReader DefnMap m => (LId -> Maybe (LId, Size)) -> Exp -> m Exp
            reExp rn = \ case
                  LVar a s l                                                         -> pure $ LVar a s $ maybe l fst $ rn l
                  Lit a bv                                                           -> pure $ Lit a bv
                  Concat _ e1 e2                                                     -> packExps <$> mapM (reExp rn) (gather e1 <> gather e2)
                  c@(Call _ _ (Global g) e _ _)    | isNil e                         -> getBody g >>= maybe (pure c) (reExp rn)
                  c@(Call _ _ (Global g) _ ps els) | null (getPVars ps) && isNil els -> getBody g >>= maybe (pure c) (reExp rn)
                  c@(Call a s (Global g) e ps els)                                   -> getBody g >>= maybe (pure c) (\ body ->
                        case inline ps body of
                              Just g' -> reExp rn $ Call a s g' e ps els
                              Nothing -> do
                                    e'   <- reExp rn e
                                    els' <- reExp rn els
                                    let g'  | isId ps body = Id -- TODO(chathhorn): refactor this.
                                            | isConst body = Const $ toConst body
                                            | otherwise    = Global g
                                        ps' | isConst body = mergePats $ map varToWild ps
                                            | otherwise    = mergePats ps
                                    pure $ Call a s g' e' ps' $ if alwaysMatches ps' then nil else els')
                  Call a s g e ps els                              -> do
                        e'   <- reExp rn e
                        els' <- reExp rn els
                        let ps'    = mergePats ps
                        pure $ Call a s g e' ps' $ if alwaysMatches ps' then nil else els'
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

                        isId :: [Pat] -> Exp -> Bool
                        isId (getPVars -> [PatVar _ 1]) (Call _ _ (Const bv1) (LVar _ _ 0) [PatLit _ bv1'] (Lit _ bv0))
                              | bv1 == bvTrue  && bv1' == bvTrue  && bv0 == bvFalse = True
                              | bv1 == bvFalse && bv1' == bvFalse && bv0 == bvTrue = True
                        isId (getPVars -> p) body = unAnn (patToExp p) == unAnn body

                        getPVars :: [Pat] -> [Pat]
                        getPVars = filter isVar

                        isConst :: Exp -> Bool
                        isConst = \ case
                              Lit {} -> True
                              _      -> False

                        toConst :: Exp -> BV
                        toConst = \ case
                              Lit _ bv -> bv
                              _        -> BV.nil

                        patToExp :: [Pat] -> Exp
                        patToExp = packExps . zipWith patToExp' [0::LId ..]
                              where patToExp' :: LId -> Pat -> Exp
                                    patToExp' i = \ case
                                          PatVar      an sz -> LVar an sz i
                                          PatLit      an bv -> Lit  an bv
                                          PatWildCard an sz -> Lit  an (zeros $ fromIntegral sz)

                        alwaysMatches :: [Pat] -> Bool
                        alwaysMatches = all $ \ p -> isWild p || isVar p

                        inline :: [Pat] -> Exp -> Maybe Target
                        inline ar (Call _ _ g' e ps _)
                              | unAnn (patToExp ar) == unAnn e && unAnn e == unAnn (patToExp ps) = Just g'
                        inline _ _                                                               = Nothing

                        packExps :: [Exp] -> Exp
                        packExps = cat . mergeLits . filter (not . isNil)

                        mergeLits :: [Exp] -> [Exp]
                        mergeLits = \ case
                              Lit a bv : (Lit _ bv') : es -> mergeLits $ Lit a (bv <> bv') : es
                              e : es                      -> e : mergeLits es
                              []                          -> []

                        mergePats :: [Pat] -> [Pat]
                        mergePats = \ case
                              p : ps | isNilPat p                       -> mergePats ps
                              PatLit a bv : (PatLit _ bv') : ps         -> mergePats $ PatLit a (bv <> bv') : ps
                              PatWildCard a s : (PatWildCard _ s') : es -> mergePats $ PatWildCard a (s + s') : es
                              p : ps                                    -> p : mergePats ps
                              []                                        -> []

                        getBody :: MonadReader DefnMap m => GId -> m (Maybe Exp)
                        getBody g = asks (Map.lookup g) >>= \ case
                              Just (Defn _ _ _ body) -> pure $ Just body
                              _                      -> pure Nothing


-- | Remove unused definitions.
purgeUnused :: MonadFail m => Program -> m Program
purgeUnused (Program start@(StartDefn _ _ loop state0) ds) = do
      pure $ Program start $ filter ((`elem` uses) . defnName) ds
      where uses :: [GId]
            uses = uses' [loop, state0]

            uses' :: [GId] -> [GId]
            uses' u = let u' = nubOrd (concatMap defnUses u) in
                  if length u' == length u then u else uses' u'

            defnUses :: GId -> [GId]
            defnUses d = maybe [d] ((<>[d]) . getUses) (Map.lookup d defns)

            getUses :: Exp -> [GId]
            getUses = \ case
                  Concat _ e1 e2              -> getUses e1 <> getUses e2
                  Call _ _ (Global g) e _ els -> [g] <> getUses e <> getUses els
                  Call _ _ _          e _ els ->        getUses e <> getUses els
                  _                           -> []

            defns :: DefnBodyMap
            defns = Map.fromList $ map (defnName &&& defnBody) ds

-- | Substitute all calls to functions with duplicate bodies to the same
--   function. Should follow with `purgeUnused`.
dedupe :: Applicative m => Program -> m Program
dedupe p = pure $ p { defns = map ddDefn $ defns p }
      where ddDefn :: Defn -> Defn
            ddDefn d = d { defnBody = ddExp $ defnBody d }

            ddExp :: Exp -> Exp
            ddExp = \ case
                  Call an sz t e ps e' -> Call an sz (ddTarget t) (ddExp e) ps $ ddExp e'
                  Concat an e e'       -> Concat an (ddExp e) $ ddExp e'
                  e                    -> e

            ddTarget :: Target -> Target
            ddTarget = \ case
                  Global g | Just g' <- Map.lookup g ddMap -> Global g'
                  t                                        -> t

            ddMap :: HashMap GId GId
            ddMap = foldr (\ (Defn _ g sig body) -> maybe id (Map.insert g) $ Map.lookup (unAnn sig, unAnn body) bodies) mempty $ defns p
                  where bodies :: HashMap (Sig, Exp) GId
                        bodies = foldr (\ (Defn _ g sig body) -> Map.insert (unAnn sig, unAnn body) g) mempty $ defns p

-- | Attempt to reduce definition bodies to a constant literal value.
partialEval :: MonadFail m => Program -> m Program
partialEval (Program start ds) = pure $ Program start $ map evalDefn ds
      where defns :: DefnMap
            defns = Map.fromList $ map (defnName &&& id) ds

            evalDefn :: Defn -> Defn
            evalDefn d = d { defnBody = evalExp $ defnBody d }

            evalExp :: Exp -> Exp
            evalExp e = either fst (Lit $ ann e) $ interpExp defns [] e
