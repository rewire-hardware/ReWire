{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | The primitive datatype basis for Eidos programs: the built-in type
--   constructors (with their data constructors, where they have any) that
--   bridged programs may reference without declaring — the unit and tuple
--   families, Bool, Maybe/Either (base names, as the bridge emits them),
--   the abstract width-bearing types (Vec, Finite, Proxy), and the reactive
--   stack types (which exist at the P level only; the M level deletes
--   them). The bridge prepends these to every program so the linter can
--   resolve constructor occurrences.
--
--   The basis' type-variable uniques are NEGATIVE: the bridge mints
--   non-negative uniques, so basis binders can never collide with program
--   binders (the uniqueness lint checks both together).
module ReWire.Eidos.PrimBasis (primDatas, addPrims) where

import ReWire.Annotation (Annote (MsgAnnote))
import ReWire.Eidos.Syntax

import Data.Text (Text)

import qualified Data.Text as T

-- | Prepend the primitive basis (dropping any duplicate declarations, which
--   the bridge does not produce but hand-written .eir might).
addPrims :: Program -> Program
addPrims p = p { progDatas = primDatas <> filter ((`notElem` map dataName primDatas) . dataName) (progDatas p) }

primDatas :: [DataDefn]
primDatas =
      [ mkData "()"       KStar                                                [nullCtor "()" "()"]
      -- (The type-level arithmetic constructors +, -, * are recognized by
      -- name in evalNat/natNorm and need no declaration; their kinds
      -- construct Nat, not *, so they are not datatypes.)
      , mkData "Bool"     KStar                                                [nullCtor "False" "Bool", nullCtor "True" "Bool"]
      , mkData "ExtDev"   (KStar `KFun` (KStar `KFun` KStar))                  []
      , mkData "Finite"   (KNat `KFun` KStar)                                  []
      , mkData "Identity" kmonad                                               []
      , mkData "Integer"  KStar                                                []
      , mkData "Proxy"    (KNat `KFun` KStar)                                  [proxyCtor]
      , mkData "ReacT"    (KStar `KFun` (KStar `KFun` (kmonad `KFun` kmonad))) []
      , mkData "StateT"   (KStar `KFun` (kmonad `KFun` kmonad))                []
      , mkData "String"   KStar                                                []
      , mkData "Vec"      (KNat `KFun` (KStar `KFun` KStar))                   []
      , mkData "[_]"      (KStar `KFun` KStar)                                 []
      , mkData "[]"       (KStar `KFun` KStar)                                 []
      , maybeData
      , eitherData
      ]
      <> map mkTuple [2 .. 62]
      where kmonad :: Kind
            kmonad = KStar `KFun` KStar

an :: Text -> Annote
an n = MsgAnnote $ "Prim: " <> n

mkData :: TyConId -> Kind -> [DataCon] -> DataDefn
mkData n = DataDefn (an n) n

nullCtor :: DataConId -> TyConId -> DataCon
nullCtor c t = DataCon (an c) c $ monoSig $ TyCon (an t) t

-- Basis type variables: negative uniques, disjoint per declaration (each
-- declaration gets a distinct hundred).
basisTv :: Int -> Int -> Text -> Kind -> TyVar
basisTv decl i x = TyVar x (negate $ decl * 100 + i + 1)

proxyCtor :: DataCon
proxyCtor = DataCon (an "Proxy") "Proxy" $ Sig [n] $ TyApp a (TyCon a "Proxy") $ TyVarT a n
      where a = an "Proxy"
            n = basisTv 1 0 "n" KNat

maybeData :: DataDefn
maybeData = DataDefn a "GHC.Internal.Maybe.Maybe" (KStar `KFun` KStar)
      [ DataCon a "GHC.Internal.Maybe.Nothing" $ Sig [tv] mt
      , DataCon a "GHC.Internal.Maybe.Just"    $ Sig [tv] $ Arrow a (TyVarT a tv) mt
      ]
      where a  = an "Maybe"
            tv = basisTv 2 0 "a" KStar
            mt = TyApp a (TyCon a "GHC.Internal.Maybe.Maybe") $ TyVarT a tv

eitherData :: DataDefn
eitherData = DataDefn a "GHC.Internal.Data.Either.Either" (KStar `KFun` (KStar `KFun` KStar))
      [ DataCon a "GHC.Internal.Data.Either.Left"  $ Sig [tva, tvb] $ Arrow a (TyVarT a tva) et
      , DataCon a "GHC.Internal.Data.Either.Right" $ Sig [tva, tvb] $ Arrow a (TyVarT a tvb) et
      ]
      where a   = an "Either"
            tva = basisTv 3 0 "a" KStar
            tvb = basisTv 3 1 "b" KStar
            et  = TyApp a (TyApp a (TyCon a "GHC.Internal.Data.Either.Either") $ TyVarT a tva) $ TyVarT a tvb

mkTuple :: Int -> DataDefn
mkTuple n = DataDefn a name k [DataCon a name $ Sig tvs $ foldr (Arrow a . TyVarT a) rt tvs]
      where name = "(" <> T.replicate (n - 1) "," <> ")"
            a    = an "tuple"
            tvs  = [ basisTv (10 + n) i (T.pack $ tvName i) KStar | i <- [0 .. n - 1] ]
            k    = foldr KFun KStar $ replicate n KStar
            rt   = foldl (TyApp a) (TyCon a name) $ map (TyVarT a) tvs

            tvName :: Int -> String
            tvName i | i < 26    = [toEnum $ fromEnum 'a' + i]
                     | otherwise = 't' : show i
