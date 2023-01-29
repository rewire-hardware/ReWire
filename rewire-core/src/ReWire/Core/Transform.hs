{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Safe #-}
module ReWire.Core.Transform (mergeSlices, purgeUnused, partialEval, dedupe) where

import ReWire.Annotation (unAnn, ann)
import ReWire.BitVector (BV, zeros)
import ReWire.Core.Interp (interpExp, DefnMap)
import ReWire.Core.Syntax
import qualified ReWire.BitVector as BV

import Control.Arrow ((&&&))
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import Data.List (genericLength, genericIndex)
import qualified Data.HashMap.Strict as Map

type DefnBodyMap = HashMap GId Exp

-- | Removes all zero-length arguments and parameters.
mergeSlices :: Program -> Program
mergeSlices p@Program {loop, state0, defns}  = p { loop = reDefn loop, state0 = reDefn state0, defns = reDefn <$> filter ((> 0) . sizeOf) defns }
      where defns' :: DefnMap
            defns' = Map.fromList $ map (defnName &&& id) allDefns

            allDefns :: [Defn]
            allDefns = loop : state0 : defns

            reDefn :: Defn -> Defn
            reDefn (Defn an n sig body) = Defn an n (reSig sig) $ reExp (renumLVars sig) body

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

            reExp :: (LId -> Maybe (LId, Size)) -> Exp -> Exp
            reExp rn = \ case
                  LVar a s l                                                      -> LVar a s $ maybe l fst $ rn l
                  Lit a bv                                                        -> Lit a bv
                  Concat _ e1 e2                                                  -> packExps $ map (reExp rn) $ gather e1 <> gather e2
                  c@(Call _ _ (Global g) _ _ _) | Nothing <- getBody g            -> c
                  Call _ _ (Global g) e _ _     | Just body <- getBody g, isNil e -> reExp rn body
                  Call _ _ (Global g) _ ps els  | Just body <- getBody g
                                                , null (getPVars ps), isNil els   -> reExp rn body
                  Call a s (Global g) e ps els  | Just body  <- getBody g
                                                , Just body' <- inline ps body    -> reExp rn $ Call a s body' e ps els
                                                | Just body  <- getBody g         ->
                        let e'                  = reExp rn e
                            els'                = reExp rn els
                            g'   | isId ps body = Prim Id -- TODO(chathhorn): refactor this.
                                 | isConst body = Const $ toConst body
                                 | otherwise    = Global g
                            ps'  | isConst body = mergePats $ map varToWild ps
                                 | otherwise    = mergePats ps
                        in Call a s g' e' ps' $ if alwaysMatches ps' then nil else els'
                  Call a s g e ps els                                             ->
                        let e'   = reExp rn e
                            els' = reExp rn els
                            ps'  = mergePats ps
                        in Call a s g e' ps' $ if alwaysMatches ps' then nil else els'
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

                        getBody :: GId -> Maybe Exp
                        getBody g = case Map.lookup g defns' of
                              Just (Defn _ _ _ body) -> Just body
                              _                      -> Nothing


-- | Remove unused definitions.
purgeUnused :: Program -> Program
purgeUnused p@Program { loop, state0, defns } = p { defns = filter ((`elem` uses) . defnName) defns }
      where uses :: [GId]
            uses = uses' [defnName loop, defnName state0]

            uses' :: [GId] -> [GId]
            uses' u = let u' = nubOrd (concatMap defnUses u) in
                  if length u' == length u then u else uses' u'

            defnUses :: GId -> [GId]
            defnUses d = maybe [d] ((<> [d]) . getUses) $ Map.lookup d defns'

            getUses :: Exp -> [GId]
            getUses = \ case
                  Concat _ e1 e2              -> getUses e1 <> getUses e2
                  Call _ _ (Global g) e _ els -> [g] <> getUses e <> getUses els
                  Call _ _ _          e _ els ->        getUses e <> getUses els
                  _                           -> []

            defns' :: DefnBodyMap
            defns' = Map.fromList $ (defnName &&& defnBody) <$> allDefns

            allDefns :: [Defn]
            allDefns = loop : state0 : defns

-- | Substitute all calls to functions with duplicate bodies to the same
--   function. Should follow with `purgeUnused`.
dedupe :: Program -> Program
dedupe p@Program { loop, state0, defns } = p { loop = ddDefn loop, state0 = ddDefn state0, defns = ddDefn <$> defns }
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
            ddMap = foldr (\ (Defn _ g sig body) -> maybe id (Map.insert g) $ Map.lookup (unAnn sig, unAnn body) bodies) mempty allDefns
                  where bodies :: HashMap (Sig, Exp) GId
                        bodies = foldr (\ (Defn _ g sig body) -> Map.insert (unAnn sig, unAnn body) g) mempty allDefns

            allDefns :: [Defn]
            allDefns = loop : state0 : defns

-- | Attempt to reduce definition bodies to a constant literal value.
partialEval :: Program -> Program
partialEval p@Program { loop, state0, defns } = p { loop = evalDefn loop, state0 = evalDefn state0, defns = evalDefn <$> defns }
      where defns' :: DefnMap
            defns' = Map.fromList $ map (defnName &&& id) allDefns

            evalDefn :: Defn -> Defn
            evalDefn d = d { defnBody = evalExp $ defnBody d }

            evalExp :: Exp -> Exp
            evalExp e = either fst (Lit $ ann e) $ interpExp defns' [] e

            allDefns :: [Defn]
            allDefns = loop : state0 : defns
