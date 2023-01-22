{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.Core.Check (check) where

import ReWire.Annotation (ann, noAnn)
import ReWire.BitVector (BV, width)
import ReWire.Core.Syntax (isNil, sizeOf, LId, GId, Size, ExternSig (..), Program (..), Defn (..), Pat (..), Exp (..), Sig (..), Target (..), Prim (..))
import ReWire.Error (MonadError, AstError, failAt)
import ReWire.Pretty (showt)

import Control.Arrow ((&&&))

type LIds = [(LId, Size)]
type DefnSigs = [(GId, Sig)]

check :: MonadError AstError m => Program -> m Program
check p = do
      checkLoop defnSigs (loop p)
      checkState0 defnSigs (state0 p)
      mapM_ (checkDefn defnSigs) (defns p)
      pure p
      where defnSigs :: DefnSigs
            defnSigs = (defnName &&& defnSig) <$> defns p

checkLoop :: MonadError AstError m => DefnSigs -> Defn -> m ()
checkLoop dsigs d@Defn { defnSig = Sig _ args _ }
      | null args = failAt (ann d) "core check: invalid dispatch (no arguments)"
      | otherwise = checkDefn dsigs d

checkState0 :: MonadError AstError m => DefnSigs -> Defn -> m ()
checkState0 dsigs d@Defn { defnSig = Sig _ args _ }
      | not (null args) = failAt (ann d) "core check: invalid state0 (non-constant)"
      | otherwise       = checkDefn dsigs d

checkDefn :: MonadError AstError m => DefnSigs -> Defn -> m ()
checkDefn dsigs (Defn an n (Sig _ args res) body)
      | sizeOf body /= res = failAt an $ "core check: " <> n <> ": function body size mismatch: expected " <> showt res <> ", got " <> showt (sizeOf body) <> "."
      | otherwise          = checkExp dsigs (zip [0..] args) body

checkExp :: MonadError AstError m => DefnSigs -> LIds -> Exp -> m ()
checkExp dsigs args = \ case
      Lit _ _                                                                   -> pure ()
      LVar an sz x | Just sz' <- lookup x args, sz == sz'                       -> pure ()
                   | otherwise                                                  -> failAt an "core check: LVar"
      Concat _ e1 e2                                                            -> checkExp dsigs args e1 >> checkExp dsigs args e2
      Call an sz _ _ _ e                 | not (isNil e), sz /= sizeOf e        -> failAt an "core check: call: else size mismatch"
      Call an _ _ disc ps _              | (sum $ sizeOf <$> ps) /= sizeOf disc -> failAt an "core check: call: size mismatch between discriminator and pattern."
      Call an _ (Global g) _ _ _         | Nothing <- lookup g dsigs            -> failAt an "core check: call: unknown global"
      Call an sz (Global g) _ ps _       | Just sig <- lookup g dsigs
                                         , mkSig ps sz `neq` sig                -> failAt an "core check: call: global sig mismatch"
      Call an sz (Extern sig _ _) _ ps _ | mkSig ps sz `neq` toSig sig          -> failAt an "core check: call: extern sig mismatch"
      Call an sz (Prim pr) _ ps _        | not (primCompat (mkSig ps sz) pr)    -> failAt an $ "core check: call: prim sig mismatch: " <> showt pr
      Call an sz (Const bv) _ ps _       | mkSig ps sz `neq` constSig bv        -> failAt an "core check: call: const sig mismatch"
      Call _ _ _ disc _ e                                                       -> checkExp dsigs args disc >> checkExp dsigs args e

mkSig :: [Pat] -> Size -> Sig
mkSig ps sz = Sig noAnn (sizeOf <$> filter isVar ps) sz

toSig :: ExternSig -> Sig
toSig (ExternSig an _ _ is os) = Sig an (snd <$> is) $ sum $ snd <$> os

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
      (Reverse, [a])        -> a == res
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

