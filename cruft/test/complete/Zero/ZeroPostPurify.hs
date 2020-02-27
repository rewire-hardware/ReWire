{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Zero where

---------------------------------------------
--- Start: ReWire Fig Leaf
---------------------------------------------

import Data.Bits
import Data.Word
import Data.Char
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Resumption.Reactive

type ReT = ReacT
type StT = StateT
type I = Identity
{-
--Default typing of unfold from PrimBasis.hs:

(v "b"
                `arr0` v "i"
                `arr0` tyApp (tyApp (c "Prelude.Either")
                                       (v "a"))
                                       (tyApp (tyApp (c "(,)") (v "o")) (v "b")))
            `arr0` tyApp (tyApp (c "Prelude.Either")
                                   (v "a"))
                                   (tyApp (tyApp (c "(,)") (v "o")) (v "b"))
            `arr0` TyComp noAnn (reT (v "i") (v "o") (c "I")) (v "a")

-- Haskell definition of unfold:

unfold :: Monad m => (b -> i -> Either a (o,b)) -> Either a (o,b) -> ReacT i o m a
unfold f = \ case
  Left a      -> ReacT $ return (Left a)
  Right (o,b) -> ReacT $ return (Right (o, unfold f . f b))
-}

unfold :: (b -> i -> Either a (o,b)) -> Either a (o,b) -> ReT i o I a
unfold f (Left a)      = ReacT $ return (Left a)
unfold f (Right (o,b)) = ReacT $ return (Right (o, unfold f . f b))
    
-- Need to add this too because of the way dispatch function is generated now (will change).
data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit

---------------------------------------------
--- End: ReWire Fig Leaf
---------------------------------------------


data Bit :: * where
     Zero :: Bit

data R :: * where
    R_ll_lambda :: R

--start3 :: ReT Bit Bit I ()
start3 = unfold dispatch2 start

-- Known error with purification.txt: \/ should be Bit (or output type generally).
dispatch2 :: R -> Bit -> Either () (Bit , R)
dispatch2 x_dsc_1 x_i_ =
    case x_dsc_1 of
         R_ll_lambda -> ll_lambda1 x_i_

--ll_lambda1 :: Bit -> Either () (Bit , R)
ll_lambda1 d = start

--start :: Either () (Bit , R)
start = Right (Zero , R_ll_lambda)
