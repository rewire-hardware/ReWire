{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.Crust.PrimBasis (addPrims, primDatas) where

import ReWire.Annotation (Annote (MsgAnnote))
import ReWire.Crust.Syntax (Kind (..), Ty (..), DataCon (..), DataDefn (..), FreeProgram)
import ReWire.Crust.Types ((|->), arr, kmonad, pairTy)

import Data.Text (Text)
import qualified Data.Text as T
import ReWire.Unbound (s2n)

-- The "primitive basis" has some magical voodoo that cannot be expressed in
-- the concrete syntax... so here we are.

-- TODO: raise an error for mis-typed included primitives (don't silently drop them).
addPrims :: FreeProgram -> FreeProgram
addPrims (ts, syns, vs) = (filter ((`notElem` (dataName <$> primDatas)) . dataName) ts <> primDatas, syns, vs)

primDatas :: [DataDefn]
primDatas = map mkData
      [ ("()",       KStar,                                              [nullDataCon "()" "()"])
      , ("+",        KNat `KFun` KNat  `KFun` KNat,                      [])
      , ("->",       KStar `KFun` (KStar `KFun` KStar),                  [])
      , ("A_",       KStar,                                              [])
      , ("Bool",     KStar,                                              [nullDataCon "False" "Bool", nullDataCon "True" "Bool"])
      , ("ExtDev",   KStar `KFun` (KStar `KFun` KStar),                  [])
      , ("Finite",   KNat `KFun` KStar,                                  [])
      , ("Identity", kmonad,                                             [])
      , ("Integer",  KStar,                                              [])
      , ("Proxy",    KNat `KFun` KStar,                                  [proxyCtor])
      , ("PuRe",     KStar `KFun` (KStar `KFun` KStar),                  pureCtors)
      , ("R_",       KStar,                                              [])
      , ("ReacT",    KStar `KFun` (KStar `KFun` (kmonad `KFun` kmonad)), [])
      , ("StateT",   KStar `KFun` (kmonad `KFun` kmonad),                [])
      , ("String",   KStar,                                              [])
      , ("Vec",      KNat `KFun` KStar `KFun` KStar,                     [])
      , ("[_]",      KStar `KFun` KStar,                                 [])
      -- These are intended for internal typechecking use only.
      , ("-",        KNat `KFun` KNat,                                   [])
      , ("*",        KNat `KFun` KNat  `KFun` KNat,                      [])
      ] <> map mkTuple' [2..128]

msg :: Text -> Annote
msg = MsgAnnote

mkData :: (Text, Kind, [DataCon]) -> DataDefn
mkData (n, k, cs) = DataDefn (msg $ "Prim: " <> n) (s2n n) k cs

nullDataCon :: Text -> Text -> DataCon
nullDataCon c t = DataCon (MsgAnnote $ "Prim: " <> c <> " data constructor") (s2n c) ([] |-> TyCon (MsgAnnote $ "Prim: " <> t <> " type constructor") (s2n t))

pureCtors :: [DataCon]
pureCtors = [doneCtor, pauseCtor]
      where doneCtor :: DataCon
            doneCtor = DataCon an (s2n "Done") ([s2n "s", s2n "o"] |-> tup tA tvS `arr` tPure)

            pauseCtor :: DataCon
            pauseCtor = DataCon an (s2n "Pause") ([s2n "s", s2n "o"] |-> tup tvO (tup tR tvS) `arr` tPure)

            tPure :: Ty
            tPure = TyApp an (TyApp an (TyCon an $ s2n "PuRe") tvS) tvO

            tup :: Ty -> Ty -> Ty
            tup = pairTy an

            tA :: Ty
            tA = TyCon an $ s2n "A_"

            tR :: Ty
            tR = TyCon an $ s2n "R_"

            tvS :: Ty
            tvS = TyVar an KStar $ s2n "s"

            tvO :: Ty
            tvO = TyVar an KStar $ s2n "o"

            an :: Annote
            an = MsgAnnote "Prim: PuRe data/type constructor"

proxyCtor :: DataCon
proxyCtor = DataCon an (s2n "Proxy") ([s2n "n"] |-> TyApp an (TyCon an $ s2n "Proxy") (TyVar an KNat $ s2n "n"))
      where an :: Annote
            an = MsgAnnote "Prim: Proxy data/type constructor"

mkTuple' :: Int -> DataDefn
mkTuple' n = DataDefn (msg "Prim: tuple") (s2n i) k [ctor]
      where i    = "(" <> T.replicate (n-1) "," <> ")"
            tvs  = map (s2n . T.pack) $ take n $ [[c] | c <- ['a'..'z']] <> map (('t':) . show) [0::Integer ..]
            tvs' = map (TyVar (MsgAnnote "Prim: tuple type variable") KStar) tvs
            k    = foldr KFun KStar $ replicate n KStar
            rt   = foldl' (TyApp (MsgAnnote "Prim: tuple type ctor app")) (TyCon (MsgAnnote "Prim: tuple type ctor") $ s2n i) tvs'
            ctor = DataCon (MsgAnnote "Prim: tuple data ctor") (s2n i) $ tvs |-> foldr arr rt tvs'
