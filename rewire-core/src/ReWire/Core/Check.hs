{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
module ReWire.Core.Check (check) where

import ReWire.Annotation (ann, noAnn)
import ReWire.BitVector (BV, width)
import ReWire.Core.Syntax (isNil, sizeOf, LId, GId, Size, ExternSig (..), Device (..), Defn (..), Pat (..), Exp (..), Sig (..), Target (..), Prim (..))
import ReWire.Error (MonadError, AstError, failAt)
import ReWire.Pretty (showt, prettyPrint)

import Control.Arrow ((&&&))
import Control.Monad (foldM_)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set

type LIds = HashMap LId Size
type DefnSigs = HashMap GId (Sig, Exp)

check :: MonadError AstError m => Device -> m Device
check p = do
      checkLoop
      checkState0
      mapM_ (checkDefn defnSigs) $ defns p
      checkRecursion defnSigs $ loop p : state0 p : defns p
      pure p
      where defnSigs :: DefnSigs
            defnSigs = Map.fromList $ map (defnName &&& (defnSig &&& defnBody)) $ loop p : state0 p : defns p

            checkLoop :: MonadError AstError m => m ()
            checkLoop = checkDefn defnSigs $ loop p

            checkState0 :: MonadError AstError m => m ()
            checkState0
                  | not (null state0Args) = failAt (ann $ state0 p) "core check: invalid state0 (non-constant)"
                  | state0Sz /= loopSz    = failAt (ann $ state0 p) $ "core check: " <> prettyPrint (defnName $ loop p)
                                                                          <> " and " <> prettyPrint (defnName $ state0 p) <> " have unequal result sizes"
                  | otherwise          = checkDefn defnSigs $ state0 p

            loopSz :: Size
            loopSz | Sig _ _ sz <- defnSig $ loop p = sz

            state0Sz :: Size
            state0Sz | Sig _ _ sz <- defnSig $ state0 p = sz

            state0Args :: [Size]
            state0Args | Sig _ args _ <- defnSig $ state0 p = args

checkDefn :: MonadError AstError m => DefnSigs -> Defn -> m ()
checkDefn dsigs (Defn an n (Sig _ args res) body)
      | sizeOf body /= res = failAt an $ "core check: " <> n <> ": function body size mismatch: expected " <> showt res <> ", got " <> showt (sizeOf body) <> "."
      | otherwise          = checkExp dsigs (Map.fromList $ zip [0..] args) body

checkExp :: MonadError AstError m => DefnSigs -> LIds -> Exp -> m ()
checkExp dsigs args = \ case
      Lit _ _                                                                  -> pure ()
      LVar an sz x | Just sz' <- Map.lookup x args, sz == sz'                  -> pure ()
                   | otherwise                                                 -> failAt an "core check: LVar"
      Concat _ e1 e2                                                           -> checkExp dsigs args e1 >> checkExp dsigs args e2
      Call an sz _ _ _ e                 | not (isNil e), sz /= sizeOf e       -> failAt an "core check: call: else size mismatch"
      Call an _ _ disc ps _              | sum (sizeOf <$> ps) /= sizeOf disc  -> failAt an "core check: call: size mismatch between discriminator and pattern."
      Call an _ (Global g) _ _ _         | Nothing <- Map.lookup g dsigs       -> failAt an $ "core check: call: unknown global: " <> g
      Call an sz (Global g) _ ps _       | Just (sig, _) <- Map.lookup g dsigs
                                         , mkSig ps sz `neq` sig               -> failAt an $ "core check: call: global sig mismatch: " <> g
      Call an sz (Extern sig _ _ _) _ ps _ | mkSig ps sz `neq` toSig sig       -> failAt an "core check: call: extern sig mismatch"
      Call an _ (Extern _ ex _ (Just g)) _ _ _
                                         | Nothing <- Map.lookup g dsigs       -> failAt an $ "core check: call: unknown model defn " <> g <> " for extern " <> ex
      Call an sz (Extern _ ex _ (Just g)) _ ps _
                                         | Just (sig, _) <- Map.lookup g dsigs
                                         , mkSig ps sz `neq` sig               -> failAt an $ "core check: call: sig mismatch between extern " <> ex <> " and its model defn " <> g
      Call an sz (Prim pr) _ ps _        | not (primCompat (mkSig ps sz) pr)   -> failAt an $ "core check: call: prim sig mismatch: " <> showt pr <> " failed to match pattern type " <> showt (mkSig ps sz)
      Call an sz (Const bv) _ ps _       | mkSig ps sz `neq` constSig bv       -> failAt an "core check: call: const sig mismatch"
      Call _ _ _ disc _ e                                                      -> checkExp dsigs args disc >> checkExp dsigs args e

-- | Check that the call graph is acyclic: a depth-first search over global
--   calls, reporting a cycle at the call site that closes it. Defns already
--   checked (done) are skipped, so each defn body is walked at most once --
--   re-walking callee bodies at every call site, as previously, could blow
--   up combinatorially on deep call graphs.
checkRecursion :: forall m. MonadError AstError m => DefnSigs -> [Defn] -> m ()
checkRecursion dsigs = foldM_ visitDefn mempty
      where visitDefn :: HashSet GId -> Defn -> m (HashSet GId)
            visitDefn done (Defn _ n _ body)
                  | n `Set.member` done = pure done
                  | otherwise           = Set.insert n <$> visitExp (Set.singleton n) done body

            visitExp :: HashSet GId -> HashSet GId -> Exp -> m (HashSet GId)
            visitExp stack done = \ case
                  Concat _ e1 e2                                       -> visitExp stack done e1 >>= flip (visitExp stack) e2
                  Call an _ (callee -> Just g) _ _ _
                                             | g `Set.member` stack    -> failAt an $ "core check: unsupported use of recursion (core id: " <> g <> ")."
                  Call _ _ (callee -> Just g) e1 _ e2                  -> do
                        done' <- visitExp stack done e1 >>= flip (visitExp stack) e2
                        visitCallee stack done' g
                  Call _ _ _ e1 _ e2                                   -> visitExp stack done e1 >>= flip (visitExp stack) e2
                  _                                                    -> pure done

            -- | An extern's model defn is followed like a call edge (the
            --   interpreter calls it).
            callee :: Target -> Maybe GId
            callee = \ case
                  Global g             -> Just g
                  Extern _ _ _ (Just g) -> Just g
                  _                    -> Nothing

            visitCallee :: HashSet GId -> HashSet GId -> GId -> m (HashSet GId)
            visitCallee stack done g
                  | g `Set.member` done                  = pure done
                  | Just (_, body) <- Map.lookup g dsigs = Set.insert g <$> visitExp (Set.insert g stack) done body
                  | otherwise                            = pure done

mkSig :: [Pat] -> Size -> Sig
mkSig ps = Sig noAnn (sizeOf <$> filter isVar ps)

toSig :: ExternSig -> Sig
toSig (ExternSig an _ _ _ is os) = Sig an (snd <$> is) $ sum $ snd <$> os

primCompat :: Sig -> Prim -> Bool
primCompat (Sig _ args res) p = case (p, args) of
      (Add, [a, b])         -> a == b && a == res
      (Sub, [a, b])         -> a == b && a == res
      (Mul, [a, b])         -> a == b && a == res
      (Div, [a, b])         -> a == b && a == res
      (Mod, [a, b])         -> a == b && a == res
      (Pow, [a, b])         -> a == b && a == res
      (LAnd, [a, b])        -> a == b && 1 == res
      (LOr, [a, b])         -> a == b && 1 == res
      (And, [a, b])         -> a == b && a == res
      (Or, [a, b])          -> a == b && a == res
      (XOr, [a, b])         -> a == b && a == res
      (XNor, [a, b])        -> a == b && a == res
      (LShift, [a, b])      -> a == b && a == res
      (RShift, [a, b])      -> a == b && a == res
      (RShiftArith, [a, b]) -> a == b && a == res
      (Eq, [a, b])          -> a == b && 1 == res
      (Gt, [a, b])          -> a == b && 1 == res
      (GtEq, [a, b])        -> a == b && 1 == res
      (Lt, [a, b])          -> a == b && 1 == res
      (LtEq, [a, b])        -> a == b && 1 == res
      (Replicate n, [a])    -> a * fromIntegral n == res
      (LNot, [_])           -> 1 == res
      (Not, [a])            -> a == res
      (RAnd, [_])           -> 1 == res
      (RNAnd, [_])          -> 1 == res
      (ROr, [_])            -> 1 == res
      (RNor, [_])           -> 1 == res
      (RXOr, [_])           -> 1 == res
      (RXNor, [_])          -> 1 == res
      (MSBit, [_])          -> 1 == res
      (Resize, [_])         -> True
      (Reverse, xs@(a:as))  -> all (== a) as && sum xs == res
      (Id, xs)              -> sum xs == res
      _                     -> False

constSig :: BV -> Sig
constSig bv = Sig noAnn [] $ fromIntegral $ width bv

neq :: Sig -> Sig -> Bool
neq (Sig _ args res) (Sig _ args' res') = not (args == args' && res == res')

isVar :: Pat -> Bool
isVar = \ case
      PatVar {} -> True
      _         -> False

