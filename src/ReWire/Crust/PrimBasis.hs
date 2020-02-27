{-# LANGUAGE Safe #-}
module ReWire.Crust.PrimBasis (addPrims) where

import ReWire.Annotation
import ReWire.Crust.Syntax

import Data.List (foldl')
import ReWire.Unbound (s2n)

-- The "primitive basis" has some magical voodoo that cannot be expressed in
-- the concrete syntax... so here we are.

addPrims :: FreeProgram -> FreeProgram
addPrims (ts, vs) = (ts ++ primDatas, vs)

primDatas :: [DataDefn]
primDatas = map mkData
      [ ("->",  KStar `KFun` (KStar `KFun` KStar),                  [])
      , ("ReT", KStar `KFun` (KStar `KFun` (kmonad `KFun` kmonad)), [])
      , ("StT", KStar `KFun` (kmonad `KFun` kmonad),                [])
      , ("I",   kmonad,                                             [])
      , ("()",  KStar,                                              [DataCon (MsgAnnote "Prim: () data ctor") (s2n "()") ([] |-> TyCon (MsgAnnote "Prim: () type ctor") (s2n "()"))])
      ] ++ map mkTuple [2..62] -- why 62? 'cause that's what ghc does!

msg :: String -> Annote
msg = MsgAnnote

mkData :: (String, Kind, [DataCon]) -> DataDefn
mkData (n, k, cs) = DataDefn (msg $ "Prim: " ++ n) (s2n n) k cs

mkTuple :: Int -> DataDefn
mkTuple n = DataDefn (msg "Prim: tuple") (s2n i) k [ctor]
      where i    = "(" ++ replicate (n-1) ',' ++ ")"
            tvs  = map s2n $ take n $ [[c] | c <- ['a'..'z']] ++ map (('t':) . show) [0::Integer ..]
            tvs' = map (TyVar (MsgAnnote "Prim: tuple type variable") KStar) tvs
            k    = foldr KFun KStar $ replicate n KStar
            rt   = foldl' (TyApp (MsgAnnote "Prim: tuple type ctor app")) (TyCon (MsgAnnote "Prim: tuple type ctor") $ s2n i) tvs'
            ctor = DataCon (MsgAnnote "Prim: tuple data ctor") (s2n i) $ tvs |-> foldr arr rt tvs'
