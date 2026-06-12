{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}

module Embedder.Atmo.FlattenMonadTrans where

import safe Embedder.Atmo.Syntax as A
    ( Ty(..), TyBuiltin (..),)
import ReWire.Orphans ()
import ReWire.Annotation (Annote)

import Embedder.Atmo.Types (nilTy, pairTy)

-- transMonadT
-- Convert `StateT s (StateT t (StateT u Identity)) a`
--     to  `State (s,(t,(u,()))) a`
-- Convert `ReacT i o m a`
--     to  `Re i o s a`
--     where s = stateOf (transStateT m)
transMonadT :: Ty -> Ty
transMonadT = \ case
      TyApp an (TyBuiltin _ TyReacT) [i,o,m@(TyVar {}),a] -> 
            TyApp an (TyBuiltin an TyRe) [i,o,m,a]
      TyApp an (TyBuiltin _ TyReacT) [i,o,m,a] -> 
            let s = mkState an (getStates m)
            in TyApp an (TyBuiltin an TyRe) [i,o,s,a]
      TyApp an (TyBuiltin _ TyStateT) [s,TyVar {},a] ->
            TyApp an (TyBuiltin an TyState) [TyTuple an [s,TyTuple an []],a]
      TyApp an (TyBuiltin _ TyStateT) [s,m,a] -> 
            let s' = pairTy an s (mkState an (getStates m))
            in TyApp an (TyBuiltin an TyState) [s',a]
      TyApp an (TyBuiltin _ TyIdentity) [a] ->
            TyApp an (TyBuiltin an TyState) [TyTuple an [],a]
      TyApp an (TyBuiltin _ TyStateDev) [i,o,m@(TyVar {})] ->
            TyApp an (TyBuiltin an TyStateDev) [i,o,m]
      TyApp an (TyBuiltin _ TyStateDev) [i,o,m] ->
            let s = mkState an (getStates m)
            in TyApp an (TyBuiltin an TyStateDev) [i,o,s]
      t -> t

getStates :: Ty -> [Ty]
getStates = \ case
      TyApp _ (TyBuiltin _ TyStateT) [s,m] -> s : getStates m
      (TyBuiltin _ TyIdentity) -> []
      _ -> []

mkState :: Annote -> [Ty] -> Ty
mkState _a [] = nilTy
mkState a (t:ts) = pairTy a t $ mkState a ts


-- We also need to diambiguate lift
-- lift of return type ReacT is liftR
-- lift of return type StateT is liftS
