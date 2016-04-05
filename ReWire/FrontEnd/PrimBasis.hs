module ReWire.FrontEnd.PrimBasis (primBasis) where

import ReWire.Annotation
import ReWire.FrontEnd.Syntax

import Unbound.Generics.LocallyNameless (string2Name)

--
-- The "primitive basis" has some magical voodoo that cannot be expressed in
-- the concrete syntax... so here we are.
--

mkId :: String -> Name b
mkId = string2Name

primBasis :: RWMProgram
primBasis = RWMProgram (
            [ RWMData noAnn (TyConId "->")  [mkId "a", mkId "b"]           (KStar `KFun` (KStar `KFun` KStar))                  []
            , RWMData noAnn (TyConId "ReT") [mkId "i", mkId "o", mkId "m"] (KStar `KFun` (KStar `KFun` (KMonad `KFun` KMonad))) []
            , RWMData noAnn (TyConId "StT") [mkId "s", mkId "m"]           (KStar `KFun` (KMonad `KFun` KMonad))                []
            , RWMData noAnn (TyConId "I")   []                             KMonad                                               []
            , RWMData noAnn (TyConId "()")  []                             KStar                                                [RWCDataCon noAnn (DataConId "()") []]
            ] ++ map mkTuple [2..62] )  -- why 62? 'cause that's what ghc does!
            $ trec defns
      where defns = map mkPrimDefn prims

mkTuple :: Int -> RWMData
mkTuple n = RWMData noAnn (TyConId i) tvs k [ctor]
      where i    = "(" ++ replicate (n-1) ',' ++ ")"
            tvs  = map mkId (take n ([[c] | c <- ['a'..'z']] ++ map (('t':) . show) [0::Integer ..]))
            k    = foldr KFun KStar $ replicate n KStar
            ctor = RWCDataCon noAnn (DataConId i) $ map (RWCTyVar noAnn) tvs

v :: String -> RWCTy
v = RWCTyVar noAnn . mkId

c :: String -> RWCTy
c = RWCTyCon noAnn . TyConId

tyApp :: RWCTy -> RWCTy -> RWCTy
tyApp = RWCTyApp noAnn

reT :: RWCTy -> RWCTy -> RWCTy -> RWCTy
reT i o m = c "ReT" `tyApp` i `tyApp` o `tyApp` m

stT :: RWCTy -> RWCTy -> RWCTy
stT s m   = c "StT" `tyApp` s `tyApp` m

mkPrimDefn :: (Name RWMExp, [Name RWCTy], RWCTy) -> RWMDefn
mkPrimDefn (n, vs, t) = RWMDefn noAnn n (vs |-> t) False $ Embed $ bind [] $ RWMVar noAnn t n

prims :: [(Name RWMExp, [Name RWCTy], RWCTy)]
prims =
      [ ( mkId "return"
        , [mkId "a", mkId "m"]
        , v "a" `mkArrow` RWCTyComp noAnn (v "m") (v "a")
        )
      , ( mkId ">>="
        , [mkId "m", mkId "a", mkId "b"]
        , RWCTyComp noAnn (v "m") (v "a")
            `mkArrow` (v "a" `mkArrow` RWCTyComp noAnn (v "m") (v "b"))
            `mkArrow` RWCTyComp noAnn (v "m") (v "b")
        )
      , ( mkId "get"
        , [mkId "s", mkId "m"]
        , RWCTyComp noAnn (stT (v "s") (v "m")) (v "s")
        )
      , ( mkId "put"
        , [mkId "s", mkId "m"]
        , v "s" `mkArrow` RWCTyComp noAnn (stT (v "s") (v "m")) (c "()")
        )
      , ( mkId "signal"
        , [mkId "o", mkId "i", mkId "m"]
        , v "o" `mkArrow` RWCTyComp noAnn (reT (v "i") (v "o") (v "m")) (v "i")
        )
      , ( mkId "lift"
        , [mkId "m", mkId "a", mkId "t"]
        , RWCTyComp noAnn (v "m") (v "a") `mkArrow` RWCTyComp noAnn (v "t" `tyApp` v "m") (v "a")
        )
      , ( mkId "extrude"
        , [mkId "i", mkId "o", mkId "s", mkId "m", mkId "a"]
        , RWCTyComp noAnn (reT (v "i") (v "o") (stT (v "s") (v "m"))) (v "a")
            `mkArrow` v "s"
            `mkArrow` RWCTyComp noAnn (reT (v "i") (v "o") (v "m")) (c "(,)" `tyApp` v "a" `tyApp` v "s")
        )
      ]
