{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.Crust.PrimBasis (addPrims) where

import ReWire.Annotation
import ReWire.Crust.Syntax

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import ReWire.Unbound (s2n)

-- The "primitive basis" has some magical voodoo that cannot be expressed in
-- the concrete syntax... so here we are.

addPrims :: FreeProgram -> FreeProgram
addPrims (ts, syns, vs) = (ts <> primDatas, syns, vs)

primDatas :: [DataDefn]
primDatas = map mkData
      [ ("->",      KStar `KFun` (KStar `KFun` KStar),                  [])
      , ("ReT",     KStar `KFun` (KStar `KFun` (kmonad `KFun` kmonad)), [])
      , ("StT",     KStar `KFun` (kmonad `KFun` kmonad),                [])
      , ("I",       kmonad,                                             [])
      , ("Integer", KStar,                                              [])
      , ("Bit",     KStar,                                              [nullDataCon "C" "Bit", nullDataCon "S" "Bit"])
      , ("Ref",     KStar `KFun` KStar,                                 [refCtor])
      , ("String",  KStar,                                              [])
      , ("[_]",     KStar `KFun` KStar,                                 [])
      , ("()",      KStar,                                              [nullDataCon "()" "()"])
      ] <> map mkTuple' [2..62] -- why 62? 'cause that's what ghc does!

msg :: Text -> Annote
msg = MsgAnnote

mkData :: (Text, Kind, [DataCon]) -> DataDefn
mkData (n, k, cs) = DataDefn (msg $ "Prim: " <> n) (s2n n) k cs

nullDataCon :: Text -> Text -> DataCon
nullDataCon c t = DataCon (MsgAnnote $ "Prim: " <> c <> " data ctor") (s2n c) ([] |-> TyCon (MsgAnnote $ "Prim: " <> t <> " type ctor") (s2n t))

refCtor :: DataCon
refCtor = DataCon an (s2n "Ref") $ [s2n "a"] |-> (strTy an `arr` refTy an (TyVar an KStar $ s2n "a"))
      where an :: Annote
            an = MsgAnnote "Prim: Ref data/type ctor"

mkTuple' :: Int -> DataDefn
mkTuple' n = DataDefn (msg "Prim: tuple") (s2n i) k [ctor]
      where i    = "(" <> T.replicate (n-1) "," <> ")"
            tvs  = map (s2n . T.pack) $ take n $ [[c] | c <- ['a'..'z']] <> map (('t':) . show) [0::Integer ..]
            tvs' = map (TyVar (MsgAnnote "Prim: tuple type variable") KStar) tvs
            k    = foldr KFun KStar $ replicate n KStar
            rt   = foldl' (TyApp (MsgAnnote "Prim: tuple type ctor app")) (TyCon (MsgAnnote "Prim: tuple type ctor") $ s2n i) tvs'
            ctor = DataCon (MsgAnnote "Prim: tuple data ctor") (s2n i) $ tvs |-> foldr arr rt tvs'
