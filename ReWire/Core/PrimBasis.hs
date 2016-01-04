module ReWire.Core.PrimBasis where

import ReWire.Scoping
import ReWire.Core.Kinds
import ReWire.Core.Syntax

--
-- The "primitive basis" has some magical voodoo that cannot be expressed in
-- the concrete syntax... so here we are.
--

primBasis :: RWCProgram
primBasis = RWCProgram
               ([ RWCData (TyConId "->")  [mkId "a",mkId "b"]          (Kstar `Kfun` (Kstar `Kfun` Kstar))                  []
                , RWCData (TyConId "ReT") [mkId "i",mkId "o",mkId "m"] (Kstar `Kfun` (Kstar `Kfun` (Kmonad `Kfun` Kmonad))) []
                , RWCData (TyConId "StT") [mkId "s",mkId "m"]          (Kstar `Kfun` (Kmonad `Kfun` Kmonad))                []
                , RWCData (TyConId "I")   []                           Kmonad                                               []

                , RWCData (TyConId "()") [] Kstar
                    [RWCDataCon (DataConId "()") []]
                ]
                ++ map mkTuple [2..62]   -- why 62? 'cause that's what ghc does!
               )
               (map mkPrimDefn prims)
   where mkTuple n = RWCData (TyConId i) tvs k [ctor]
             where i    = "(" ++ replicate (n-1) ',' ++ ")"
                   tvs  = map mkId (take n ([[c] | c <- ['a'..'z']]++map (('t':).show) ([0::Integer ..])))
                   k    = foldr Kfun Kstar (replicate n Kstar)
                   ctor = RWCDataCon (DataConId i) (map RWCTyVar tvs)

         v :: String -> RWCTy
         v = RWCTyVar . mkId

         c :: String -> RWCTy
         c = RWCTyCon . TyConId

         reT i o m = c "ReT" `RWCTyApp` i `RWCTyApp` o `RWCTyApp` m
         stT s m   = c "StT" `RWCTyApp` s `RWCTyApp` m

         mkPrimDefn :: (Id RWCExp,Poly RWCTy) -> RWCDefn
         mkPrimDefn (n,vs :-> t) = RWCDefn n (vs :-> t) False (RWCVar n t)

         prims :: [(Id RWCExp,Poly RWCTy)]
         prims = [(mkId "return", [mkId "a",mkId "m"]          :-> v "a" `mkArrow` RWCTyComp (v "m") (v "a")),
                  (mkId ">>=",    [mkId "m",mkId "a",mkId "b"] :-> RWCTyComp (v "m") (v "a") `mkArrow`
                                                                      (v "a" `mkArrow` RWCTyComp (v "m") (v "b")) `mkArrow`
                                                                      RWCTyComp (v "m") (v "b")),
                  (mkId "get",    [mkId "s",mkId "m"]          :-> RWCTyComp (stT (v "s") (v "m")) (v "s")),
                  (mkId "put",    [mkId "s",mkId "m"]          :-> v "s" `mkArrow`
                                                                      RWCTyComp (stT (v "s") (v "m")) (c "()")),
                  (mkId "signal", [mkId "o",mkId "i",mkId "m"] :-> v "o" `mkArrow`
                                                                      RWCTyComp (reT (v "i") (v "o") (v "m")) (v "i")),
                  (mkId "lift",   [mkId "m",mkId "a",mkId "t"] :-> RWCTyComp (v "m") (v "a") `mkArrow`
                                                                      RWCTyComp (v "t" `RWCTyApp` v "m") (v "a")),
                  (mkId "extrude",[mkId "i",mkId "o",mkId "s",mkId "m",mkId "a"]
                                                               :-> RWCTyComp (reT (v "i") (v "o") (stT (v "s") (v "m"))) (v "a") `mkArrow`
                                                                      v "s" `mkArrow`
                                                                      RWCTyComp (reT (v "i") (v "o") (v "m"))
                                                                              (c "(,)" `RWCTyApp` (v "a") `RWCTyApp` (v "s")))]
