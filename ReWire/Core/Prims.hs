module ReWire.Core.Prims where

import ReWire.Core.Syntax
import ReWire.Scoping

v :: String -> RWCTy
v = RWCTyVar . mkId

c :: String -> RWCTy
c = RWCTyCon . TyConId

reT i o m = c "ReT" `RWCTyApp` i `RWCTyApp` o `RWCTyApp` m
stT s m   = c "StT" `RWCTyApp` s `RWCTyApp` m

prims :: [(Id RWCExp,Poly RWCTy)]
prims = [(mkId "return",[mkId "a",mkId "m"]          :-> v "a" `mkArrow` RWCTyComp (v "m") (v "a")),
         (mkId "bind",  [mkId "m",mkId "a",mkId "b"] :-> RWCTyComp (v "m") (v "a") `mkArrow`
                                                           (v "a" `mkArrow` RWCTyComp (v "m") (v "b")) `mkArrow`
                                                           RWCTyComp (v "m") (v "b")),
         (mkId "get",   [mkId "s",mkId "m"]          :-> RWCTyComp (stT (v "s") (v "m")) (v "s")),
         (mkId "put",   [mkId "s",mkId "m"]          :-> v "s" `mkArrow`
                                                           RWCTyComp (stT (v "s") (v "m")) (c "Unit")),
         (mkId "signal",[mkId "o",mkId "i",mkId "m"] :-> v "o" `mkArrow`
                                                           RWCTyComp (reT (v "i") (v "o") (v "m")) (v "i")),
         (mkId "lift",  [mkId "m",mkId "a",mkId "t"] :-> RWCTyComp (v "m") (v "a") `mkArrow`
                                                           RWCTyComp (v "t" `RWCTyApp` v "m") (v "a")),
         (mkId "extrude",[mkId "i",mkId "o",mkId "s",mkId "m",mkId "a"]
                                                     :-> RWCTyComp (reT (v "i") (v "o") (stT (v "s") (v "m"))) (v "a") `mkArrow`
                                                           v "s" `mkArrow`
                                                           RWCTyComp (reT (v "i") (v "o") (v "m"))
                                                                     (c "Tuple2" `RWCTyApp` (v "a") `RWCTyApp` (v "s"))),
         --par :: ReacT i1 o1 m a -> ReacT i2 o2 m a -> ReacT (i1,i2) (o1,o2) m a
         (mkId "par", [mkId "i",mkId "o",mkId "m", mkId "a", mkId "j", mkId "p",mkId "s", mkId "t"] :-> RWCTyComp (reT (v "i") (v "o") (stT (v "s") (v "m"))) (v "a") `mkArrow`
                                                                                                        RWCTyComp (reT (v "j") (v "p") (stT (v "t") (v "m"))) (v "a") `mkArrow`
                                                                                                        RWCTyComp (reT (c "Tuple2" `RWCTyApp` (v "i") `RWCTyApp` (v "j"))
                                                                                                                       (c "Tuple2" `RWCTyApp` (v "o") `RWCTyApp` (v "p"))
                                                                                                                       (stT (c "Tuple2" `RWCTyApp` (v "s") `RWCTyApp` (v "t")) (v "m")))
                                                                                                                       (v "a")),
         --refold :: (Monad m) => (o1 -> o2) -> (o1 -> i2 -> i1) -> ReacT i1 o1 m a -> ReacT i2 o2 m a
         (mkId "refold", [mkId "o",mkId "p", mkId "i", mkId "j", mkId "m", mkId "a"] :-> ((v "o") `mkArrow` (v "p")) `mkArrow`
                                                                                         ((v "o") `mkArrow` (v "j") `mkArrow` (v "i")) `mkArrow`
                                                                                         RWCTyComp (reT (v "i") (v "o") (v "m")) (v "a") `mkArrow`
                                                                                         RWCTyComp (reT (v "j") (v "p") (v "m")) (v "a"))]

