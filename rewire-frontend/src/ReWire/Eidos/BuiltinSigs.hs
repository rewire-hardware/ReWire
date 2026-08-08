{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | The Eidos-level builtin signature table (doc/eidos.md §7.6): the
--   signature scheme every 'Prim' occurrence must instantiate, and the
--   one-way matcher the linter checks occurrences with.
--
--   Matching is first-order and unification-free: scheme variables bind
--   to the occurrence type's subterms, bindings must agree (up to
--   'natNorm'), and everything else compares structurally. Type-level
--   arithmetic on the scheme side (@Vec ((i + n) + m) a@ and the like)
--   cannot be inverted by matching, so arithmetic subterms become
--   deferred equations, checked only when substitution makes both sides
--   nat-closed and skipped otherwise — the check is deliberately partial
--   there (sound: it never rejects a correct instance).
--
--   A builtin with no recorded signature ('Nothing') has its occurrence
--   types trusted, as all builtins were before this table existed:
--   currently only 'Extern' (its parameter-list type is legacy-shaped).
module ReWire.Eidos.BuiltinSigs (builtinSig, matchesSig) where

import ReWire.Annotation (Annote (MsgAnnote))
import ReWire.Builtins (Builtin (..))
import ReWire.Eidos.Syntax
import ReWire.Eidos.Types (natNorm, evalNat, substTv, flattenTyApp)

import Data.HashMap.Strict (HashMap)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set

-- | Does the occurrence type instantiate the signature scheme?
matchesSig :: Sig -> Ty -> Bool
matchesSig (Sig tvs sigT) t = case go sigT (natNorm t) (Map.empty, []) of
      Nothing           -> False
      Just (bnds, defs) -> all (checkDeferred bnds) defs
      where vs = Set.fromList tvs

            go :: Ty -> Ty -> (HashMap TyVar Ty, [(Ty, Ty)]) -> Maybe (HashMap TyVar Ty, [(Ty, Ty)])
            go s tgt (bnds, defs) = case s of
                  TyVarT _ v | v `Set.member` vs -> case Map.lookup v bnds of
                        Nothing -> Just (Map.insert v tgt bnds, defs)
                        Just t0 -> if tyEq t0 tgt then Just (bnds, defs) else Nothing
                  _ | isNatArith s -> Just (bnds, (s, tgt) : defs)
                  Arrow _ s1 s2 | Arrow _ t1 t2 <- tgt -> go s1 t1 (bnds, defs) >>= go s2 t2
                  TyApp _ s1 s2 | TyApp _ t1 t2 <- tgt -> go s1 t1 (bnds, defs) >>= go s2 t2
                  TyCon _ c     | TyCon _ c'    <- tgt, c == c' -> Just (bnds, defs)
                  TyNat _ n     | TyNat _ n'    <- tgt, n == n' -> Just (bnds, defs)
                  _ -> Nothing

            -- An application of the built-in nat arithmetic constructors
            -- (doc/eidos.md §3.1) — deferred rather than decomposed.
            isNatArith :: Ty -> Bool
            isNatArith ty = case flattenTyApp ty of
                  (TyCon _ c, _ : _) -> c `elem` (["+", "-", "*"] :: [TyConId])
                  _                  -> False

            checkDeferred :: HashMap TyVar Ty -> (Ty, Ty) -> Bool
            checkDeferred bnds (s, tgt) = case (evalNat $ substTv bnds s, evalNat tgt) of
                  (Just n, Just n') -> n == n'
                  _                 -> True -- open on either side: unchecked.

            tyEq :: Ty -> Ty -> Bool
            tyEq t1 t2 = natNorm t1 == natNorm t2

-- | The signature scheme of each builtin (doc/eidos.md §7.6).
builtinSig :: Builtin -> Maybe Sig
builtinSig = \ case
      Error           -> Just $ Sig [aS] $ string --> va
      Extern          -> Nothing
      Cryptol         -> Just $ Sig [aS] $ string --> string --> va --> va
      Bind            -> Just $ Sig [mM, aS, bS] $ TyApp an vm va --> (va --> TyApp an vm vb) --> TyApp an vm vb
      Return          -> Just $ Sig [mM, aS] $ va --> TyApp an vm va
      Put             -> Just $ Sig [sS, mM] $ vs --> stateT vs vm unit
      Get             -> Just $ Sig [sS, mM] $ stateT vs vm vs
      Signal          -> Just $ Sig [oS, iS, mM] $ vo --> reacT vi vo vm vi
      Lift            -> Just $ Sig [tT, mM, aS] $ TyApp an vm va --> TyApp an (TyApp an vt vm) va
      Extrude         -> Just $ Sig [iS, oS, sS, mM, aS] $ reacT vi vo (TyApp an (TyApp an (TyCon an "StateT") vs) vm) va --> vs --> reacT vi vo vm va
      VecFromList     -> Just $ Sig [nN, aS] $ list va --> vec vn va
      VecReplicate    -> Just $ Sig [nN, aS] $ va --> vec vn va
      VecReverse      -> Just $ Sig [nN, aS] $ vec vn va --> vec vn va
      VecSlice        -> Just $ Sig [iN, nN, mN, aS] $ proxy vi' --> vec (plus (plus vi' vn) vm') va --> vec vn va
      VecRSlice       -> Just $ Sig [iN, nN, mN, aS] $ proxy vi' --> vec (plus (plus vi' vn) vm') va --> vec vn va
      VecIndex        -> Just $ Sig [nN, aS] $ vec vn va --> finite vn --> va
      VecIndexProxy   -> Just $ Sig [nN, mN, aS] $ vec (plus (plus vn vm') (TyNat an 1)) va --> proxy vn --> va
      VecConcat       -> Just $ Sig [nN, mN, aS] $ vec vn va --> vec vm' va --> vec (plus vn vm') va
      VecMap          -> Just $ Sig [nN, aS, bS] $ (va --> vb) --> vec vn va --> vec vn vb
      VecGenerate     -> Just $ Sig [nN, aS] $ (finite vn --> va) --> vec vn va
      Finite          -> Just $ Sig [nN] $ integer --> finite vn
      FiniteMinBound  -> Just $ Sig [nN] $ finite vn
      FiniteMaxBound  -> Just $ Sig [nN] $ finite vn
      ToFinite        -> Just $ Sig [mN, nN] $ bVec vm' --> finite vn
      ToFiniteMod     -> Just $ Sig [mN, nN] $ bVec vm' --> finite vn
      FromFinite      -> Just $ Sig [nN, mN] $ finite vn --> bVec vm'
      NatVal          -> Just $ Sig [nN] $ proxy vn --> integer
      Bits            -> Just $ Sig [] $ integer --> bVec (TyNat an 128)
      Resize          -> Just $ Sig [mN, nN] $ bVec vn --> bVec vm'
      BitSlice        -> Just $ Sig [mN, nN] $ bVec vn --> finite vn --> finite vn --> bVec vm'
      BitIndex        -> Just $ Sig [nN] $ bVec vn --> finite vn --> bool
      Add             -> binOp
      Sub             -> binOp
      Mul             -> binOp
      Div             -> binOp
      Mod             -> binOp
      Pow             -> binOp
      LAnd            -> cmpOp
      LOr             -> cmpOp
      And             -> binOp
      Or              -> binOp
      XOr             -> binOp
      XNor            -> binOp
      LShift          -> binOp
      RShift          -> binOp
      RShiftArith     -> binOp
      Eq              -> cmpOp
      Gt              -> cmpOp
      GtEq            -> cmpOp
      Lt              -> cmpOp
      LtEq            -> cmpOp
      LNot            -> Just $ Sig [nN] $ bVec vn --> bool
      Not             -> Just $ Sig [nN] $ bVec vn --> bVec vn
      RAnd            -> Just $ Sig [nN] $ bVec vn --> bool
      ROr             -> Just $ Sig [nN] $ bVec vn --> bool
      RNAnd           -> redOp
      RNor            -> redOp
      RXOr            -> redOp
      RXNor           -> redOp
      MSBit           -> redOp
      where an :: Annote
            an = MsgAnnote "builtin signature"

            -- Table type variables: negative uniques, per the primitive
            -- basis convention (these signatures are only matched
            -- against, never inserted into programs).
            kmonad = KStar `KFun` KStar

            nN  = TyVar "n" (-9001) KNat
            mN  = TyVar "m" (-9002) KNat
            iN  = TyVar "i" (-9003) KNat
            aS  = TyVar "a" (-9004) KStar
            bS  = TyVar "b" (-9005) KStar
            sS  = TyVar "s" (-9006) KStar
            oS  = TyVar "o" (-9007) KStar
            iS  = TyVar "i" (-9008) KStar
            mM  = TyVar "m" (-9009) kmonad
            tT  = TyVar "t" (-9010) $ kmonad `KFun` kmonad

            vn  = TyVarT an nN
            vm' = TyVarT an mN
            vi' = TyVarT an iN
            va  = TyVarT an aS
            vb  = TyVarT an bS
            vs  = TyVarT an sS
            vo  = TyVarT an oS
            vi  = TyVarT an iS
            vm  = TyVarT an mM
            vt  = TyVarT an tT

            infixr 1 -->
            (-->) = Arrow an

            bool    = TyCon an "Bool"
            unit    = TyCon an "()"
            string  = TyCon an "String"
            integer = TyCon an "Integer"

            vec n t    = TyApp an (TyApp an (TyCon an "Vec") n) t
            bVec n     = vec n bool
            list       = TyApp an $ TyCon an "[_]"
            proxy      = TyApp an $ TyCon an "Proxy"
            finite     = TyApp an $ TyCon an "Finite"
            plus t1 t2 = TyApp an (TyApp an (TyCon an "+") t1) t2

            stateT s m t    = TyApp an (TyApp an (TyApp an (TyCon an "StateT") s) m) t
            reacT i o m t   = TyApp an (TyApp an (TyApp an (TyApp an (TyCon an "ReacT") i) o) m) t

            -- Vec n Bool -> Vec n Bool -> Vec n Bool
            binOp = Just $ Sig [nN] $ bVec vn --> bVec vn --> bVec vn
            -- Vec n Bool -> Vec n Bool -> Bool
            cmpOp = Just $ Sig [nN] $ bVec vn --> bVec vn --> bool
            -- Vec (1 + n) Bool -> Bool
            redOp = Just $ Sig [nN] $ bVec (plus (TyNat an 1) vn) --> bool
