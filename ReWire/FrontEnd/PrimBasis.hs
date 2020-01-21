{-# LANGUAGE Safe #-}
module ReWire.FrontEnd.PrimBasis (addPrims) where

import ReWire.Annotation
import ReWire.FrontEnd.Syntax

import Data.List (foldl')
import Unbound.Generics.LocallyNameless.Name (string2Name)

-- The "primitive basis" has some magical voodoo that cannot be expressed in
-- the concrete syntax... so here we are.

addPrims :: Monad m => FreeProgram -> m FreeProgram
addPrims (ts, vs) = return (ts ++ primDatas, vs)

primDatas :: [DataDefn]
primDatas = map mkData
      [ ("->",  KStar `KFun` (KStar `KFun` KStar),                  [])
      , ("ReT", KStar `KFun` (KStar `KFun` (kmonad `KFun` kmonad)), [])
      , ("StT", KStar `KFun` (kmonad `KFun` kmonad),                [])
      , ("I",   kmonad,                                             [])
      , ("()",  KStar,                                              [DataCon noAnn (mkId "()") ([] |-> TyCon noAnn (mkId "()"))])
      ] ++ map mkTuple [2..62] -- why 62? 'cause that's what ghc does!

mkId :: String -> Name b
mkId = string2Name

msg :: String -> Annote
msg m = MsgAnnote m

mkData :: (String, Kind, [DataCon]) -> DataDefn
mkData (n, k, cs) = DataDefn (msg $ "Primitive: " ++ n) (mkId n) k cs

mkTuple :: Int -> DataDefn
mkTuple n = DataDefn (msg "Primitive: tuple") (mkId i) k [ctor]
      where i    = "(" ++ replicate (n-1) ',' ++ ")"
            tvs  = map mkId $ take n $ [[c] | c <- ['a'..'z']] ++ map (('t':) . show) [0::Integer ..]
            tvs' = map (TyVar noAnn KStar) tvs
            k    = foldr KFun KStar $ replicate n KStar
            rt   = foldl' (TyApp noAnn) (TyCon noAnn $ mkId i) tvs'
            ctor = DataCon noAnn (mkId i) $ tvs |-> foldr arr0 rt tvs'
