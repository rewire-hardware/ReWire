
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

module Embedder.Atmo.FlattenMonadTrans where

import safe Embedder.Atmo.Syntax as A
    ( Ty(..), TyBuiltin (..),)
import ReWire.Orphans ()
import ReWire.Annotation (Annote)

import Debug.Trace (trace)
import ReWire.Pretty (pretty)
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
            -- let _ = trace "transMonadT : Found applied ReacT" ()
            TyApp an (TyBuiltin an TyRe) [i,o,m,a]
      TyApp an (TyBuiltin _ TyReacT) [i,o,m,a] -> 
            -- let _ = trace "transMonadT : Found applied ReacT" ()
            let s = mkState an (getStates m)
            in TyApp an (TyBuiltin an TyRe) [i,o,s,a]
      TyApp an (TyBuiltin _ TyStateT) [s,TyVar {},a] ->
            -- trace "transMonadT: Interior monad variable won't work" $ 
            TyApp an (TyBuiltin an TyState) [TyTuple an [s,TyTuple an []],a]
      TyApp an (TyBuiltin _ TyStateT) [s,m,a] -> 
            -- let _ = trace "transMonadT : Found applied StateT" ()
            let s' = pairTy an s (mkState an (getStates m))
            in TyApp an (TyBuiltin an TyState) [s',a]
      TyApp an (TyBuiltin _ TyIdentity) [a] ->
            -- let _ = trace "transMonadT : Found applied Identity" ()
            TyApp an (TyBuiltin an TyState) [TyTuple an [],a]
      -- t0@(TyApp _an (TyBuiltin _ b) ts) ->
            -- trace ("transMonadT: Skipped builtin: " <> unpack (tb2s b) <> " with " <> show (hsep (map pretty ts))) t0
      TyApp an (TyBuiltin _ TyStateDev) [i,o,m@(TyVar {})] ->
            TyApp an (TyBuiltin an TyStateDev) [i,o,m]
      TyApp an (TyBuiltin _ TyStateDev) [i,o,m] ->
            let s = mkState an (getStates m)
            in TyApp an (TyBuiltin an TyStateDev) [i,o,s]
      t -> t -- trace ("transMonadT: Not a MonadT: " <> show (pretty t)) 

getStates :: Ty -> [Ty]
getStates = \ case
      TyApp _ (TyBuiltin _ TyStateT) [s,m] -> s : getStates m
      (TyBuiltin _ TyIdentity) -> []
      _ -> -- trace ("getStates: unidentified monad: " <> show (pretty t))
            []

mkState :: Annote -> [Ty] -> Ty
mkState _a [] = nilTy
mkState a (t:ts) = pairTy a t $ mkState a ts


-- We also need to diambiguate lift
-- lift of return type ReacT is liftR
-- lift of return type StateT is liftS



