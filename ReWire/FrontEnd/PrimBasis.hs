{-# LANGUAGE Safe #-}
module ReWire.FrontEnd.PrimBasis (addPrims) where

import ReWire.Annotation
import ReWire.FrontEnd.Syntax

import Data.List (foldl')
import Unbound.Generics.LocallyNameless.Name (string2Name)

-- The "primitive basis" has some magical voodoo that cannot be expressed in
-- the concrete syntax... so here we are.

addPrims :: ([DataDefn], [Defn]) -> Program
addPrims (ts, vs) = Program $ trec (ts ++ primDatas, vs ++ primDefns)

primDatas :: [DataDefn]
primDatas = map mkData
      [ ("->",  KStar `KFun` (KStar `KFun` KStar),                  [])
      , ("ReT", KStar `KFun` (KStar `KFun` (KMonad `KFun` KMonad)), [])
      , ("StT", KStar `KFun` (KMonad `KFun` KMonad),                [])
      , ("I",   KMonad,                                             [])
      , ("()",  KStar,                                              [DataCon noAnn (mkId "()") ([] |-> TyCon noAnn (mkId "()"))])
      ] ++ map mkTuple [2..62] -- why 62? 'cause that's what ghc does!

primDefns :: [Defn]
primDefns = map mkDefn
      [ ( "return"
        , [mkId "a", mkId "m"]
        |-> v "a" `arr0` TyComp noAnn (mv "m") (v "a")
        )
      , ( ">>="
        , [mkId "m", mkId "a", mkId "b"]
        |-> TyComp noAnn (mv "m") (v "a")
            `arr0` (v "a" `arr0` TyComp noAnn (mv "m") (v "b"))
            `arr0` TyComp noAnn (mv "m") (v "b")
        )
      , ( "get"
        , [mkId "s", mkId "m"]
        |-> TyComp noAnn (stT (v "s") (mv "m")) (v "s")
        )
      , ( "put"
        , [mkId "s", mkId "m"]
        |-> v "s" `arr0` TyComp noAnn (stT (v "s") (mv "m")) (c "()")
        )
      , ( "signal"
        , [mkId "o", mkId "i", mkId "m"]
        |-> v "o" `arr0` TyComp noAnn (reT (v "i") (v "o") (mv "m")) (v "i")
        )
      , ( "lift"
        , [mkId "m", mkId "a", mkId "t"]
        |-> TyComp noAnn (mv "m") (v "a") `arr0` TyComp noAnn (tv "t" `tyApp` mv "m") (v "a")
        )
      , ( "extrude"
        , [mkId "i", mkId "o", mkId "s", mkId "m", mkId "a"]
        |-> TyComp noAnn (reT (v "i") (v "o") (stT (v "s") (mv "m"))) (v "a")
            `arr0` v "s"
            `arr0` TyComp noAnn (reT (v "i") (v "o") (mv "m")) (c "(,)" `tyApp` v "a" `tyApp` v "s")
        )
      -- FIXME(adam): probably should not be making reference into the Prelude here
      , ( "unfold"
        , [mkId "b",mkId "i",mkId "a",mkId "o"]
        |-> (v "b"
                `arr0` v "i"
                `arr0` tyApp (tyApp (c "Prelude.Either")
                                       (v "a"))
                                       (tyApp (tyApp (c "(,)") (v "o")) (v "b")))
            `arr0` tyApp (tyApp (c "Prelude.Either")
                                   (v "a"))
                                   (tyApp (tyApp (c "(,)") (v "o")) (v "b"))
            `arr0` TyComp noAnn (reT (v "i") (v "o") (c "I")) (v "a")
       )
      ]

mkId :: String -> Name b
mkId = string2Name

msg :: String -> Annote
msg m = MsgAnnote m

mkData :: (String, Kind, [DataCon]) -> DataDefn
mkData (n, k, cs) = DataDefn (msg $ "Builtin: " ++ n) (mkId n) k cs

mkTuple :: Int -> DataDefn
mkTuple n = DataDefn (msg "Builtin: tuple") (mkId i) k [ctor]
      where i    = "(" ++ replicate (n-1) ',' ++ ")"
            tvs  = map mkId $ take n $ [[c] | c <- ['a'..'z']] ++ map (('t':) . show) [0::Integer ..]
            tvs' = map (TyVar noAnn KStar) tvs
            k    = foldr KFun KStar $ replicate n KStar
            rt   = foldl' (TyApp noAnn) (TyCon noAnn $ mkId i) tvs'
            ctor = DataCon noAnn (mkId i) $ tvs |-> foldr arr0 rt tvs'

tv :: String -> Ty
tv = TyVar noAnn (KMonad `KFun` KMonad) . mkId

mv :: String -> Ty
mv = TyVar noAnn KMonad . mkId

v :: String -> Ty
v = TyVar noAnn KStar . mkId

c :: String -> Ty
c = TyCon noAnn . mkId

tyApp :: Ty -> Ty -> Ty
tyApp = TyApp noAnn

reT :: Ty -> Ty -> Ty -> Ty
reT i o m = c "ReT" `tyApp` i `tyApp` o `tyApp` m

stT :: Ty -> Ty -> Ty
stT s m   = c "StT" `tyApp` s `tyApp` m

mkDefn :: (String, Embed Poly) -> Defn
mkDefn (n, t) = Defn (msg $ "Builtin: " ++ n) (mkId n) t False $ Embed $ bind [] $ Var noAnn tblank $ mkId n
