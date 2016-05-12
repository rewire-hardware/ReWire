module ReWire.FrontEnd.PrimBasis (addPrims) where

import ReWire.Annotation
import ReWire.FrontEnd.Syntax

import Data.List (foldl')
import Unbound.Generics.LocallyNameless (string2Name)

-- The "primitive basis" has some magical voodoo that cannot be expressed in
-- the concrete syntax... so here we are.

addPrims :: ([RWMData], [RWMDefn]) -> RWMProgram
addPrims (ts, vs) = RWMProgram $ trec (ts ++ primDatas, vs ++ primDefns)

primDatas :: [RWMData]
primDatas = map mkData
      [ ("->",  KStar `KFun` (KStar `KFun` KStar),                  [])
      , ("ReT", KStar `KFun` (KStar `KFun` (KMonad `KFun` KMonad)), [])
      , ("StT", KStar `KFun` (KMonad `KFun` KMonad),                [])
      , ("I",   KMonad,                                             [])
      , ("()",  KStar,                                              [RWMDataCon noAnn (mkId "()") ([] |-> RWMTyCon noAnn (mkId "()"))])
      ] ++ map mkTuple [2..62] -- why 62? 'cause that's what ghc does!

primDefns :: [RWMDefn]
primDefns = map mkDefn
      [ ( "return"
        , [mkId "a", mkId "m"]
        |-> v "a" `arr0` RWMTyComp noAnn (mv "m") (v "a")
        )
      , ( ">>="
        , [mkId "m", mkId "a", mkId "b"]
        |-> RWMTyComp noAnn (mv "m") (v "a")
            `arr0` (v "a" `arr0` RWMTyComp noAnn (mv "m") (v "b"))
            `arr0` RWMTyComp noAnn (mv "m") (v "b")
        )
      , ( "get"
        , [mkId "s", mkId "m"]
        |-> RWMTyComp noAnn (stT (v "s") (mv "m")) (v "s")
        )
      , ( "put"
        , [mkId "s", mkId "m"]
        |-> v "s" `arr0` RWMTyComp noAnn (stT (v "s") (mv "m")) (c "()")
        )
      , ( "signal"
        , [mkId "o", mkId "i", mkId "m"]
        |-> v "o" `arr0` RWMTyComp noAnn (reT (v "i") (v "o") (mv "m")) (v "i")
        )
      , ( "lift"
        , [mkId "m", mkId "a", mkId "t"]
        |-> RWMTyComp noAnn (mv "m") (v "a") `arr0` RWMTyComp noAnn (tv "t" `tyApp` mv "m") (v "a")
        )
      , ( "extrude"
        , [mkId "i", mkId "o", mkId "s", mkId "m", mkId "a"]
        |-> RWMTyComp noAnn (reT (v "i") (v "o") (stT (v "s") (mv "m"))) (v "a")
            `arr0` v "s"
            `arr0` RWMTyComp noAnn (reT (v "i") (v "o") (mv "m")) (c "(,)" `tyApp` v "a" `tyApp` v "s")
        )
      ]

mkId :: String -> Name b
mkId = string2Name

msg :: String -> Annote
msg m = MsgAnnote m

mkData :: (String, Kind, [RWMDataCon]) -> RWMData
mkData (n, k, cs) = RWMData (msg $ "Builtin: " ++ n) (mkId n) k cs

mkTuple :: Int -> RWMData
mkTuple n = RWMData (msg "Builtin: tuple") (mkId i) k [ctor]
      where i    = "(" ++ replicate (n-1) ',' ++ ")"
            tvs  = map mkId $ take n $ [[c] | c <- ['a'..'z']] ++ map (('t':) . show) [0::Integer ..]
            tvs' = map (RWMTyVar noAnn KStar) tvs
            k    = foldr KFun KStar $ replicate n KStar
            rt   = foldl' (RWMTyApp noAnn) (RWMTyCon noAnn $ mkId i) tvs'
            ctor = RWMDataCon noAnn (mkId i) $ tvs |-> foldr arr0 rt tvs'

tv :: String -> RWMTy
tv = RWMTyVar noAnn (KMonad `KFun` KMonad) . mkId

mv :: String -> RWMTy
mv = RWMTyVar noAnn KMonad . mkId

v :: String -> RWMTy
v = RWMTyVar noAnn KStar . mkId

c :: String -> RWMTy
c = RWMTyCon noAnn . mkId

tyApp :: RWMTy -> RWMTy -> RWMTy
tyApp = RWMTyApp noAnn

reT :: RWMTy -> RWMTy -> RWMTy -> RWMTy
reT i o m = c "ReT" `tyApp` i `tyApp` o `tyApp` m

stT :: RWMTy -> RWMTy -> RWMTy
stT s m   = c "StT" `tyApp` s `tyApp` m

mkDefn :: (String, Embed Poly) -> RWMDefn
mkDefn (n, t) = RWMDefn (msg $ "Builtin: " ++ n) (mkId n) t False $ Embed $ bind [] $ RWMVar noAnn tblank $ mkId n
