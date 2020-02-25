{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module OneState where

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
type I   = Identity

unfold :: (b -> i -> Either a (o,b)) -> Either a (o,b) -> ReT i o I a
unfold f (Left a)      = ReacT $ return (Left a)
unfold f (Right (o,b)) = ReacT $ return (Right (o, unfold f . f b))

extrude :: Monad m => ReT i o (StT s m) a -> s -> ReT i o m (a,s)
extrude = undefined
    
---------------------------------------------
--- End: ReWire Fig Leaf
---------------------------------------------

data Bit :: * where
    Zero :: Bit

data W8 :: * where
    W8 :: Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> W8

data R :: * where
    R_lambda :: R

{-
--start8 :: ReT Bit W8 I ((() , W8))
start8 =
    unfold dispatch7 start

-}


--dispatch7 :: R -> Bit -> Either ((() , W8)) ((W8 , R))
dispatch7 :: R -> Bit -> W8 -> (Either () (W8, R), W8)
dispatch7 x_dsc_1 x_i_ =
    (case x_dsc_1 of
         {(R_lambda) -> (lambda4 x_i_)})

grunt6 :: W8 -> (() , W8)
grunt6 x_sigma0_ =
    (case (x_sigma0_ , x_sigma0_) of
         {(x_st0_1 , x_st1_) -> ((lambda5 x_st0_1) x_st1_)})

lambda5 :: W8 -> W8 -> (() , W8)
lambda5 x1 x_sigma0_ =
    (() , x1)

lambda4 :: Bit -> W8 -> (Either () ((W8 , R)) , W8)
lambda4 d x_sto0_ = incr1 x_sto0_

lambda3 :: W8 -> W8 -> (Either () ((W8 , R)) , W8)
lambda3 x1 x_sto0_ =
    ((Right (x1 , R_lambda)) , x_sto0_)

lambda2 :: () -> W8 -> (Either () ((W8 , R)) , W8)
lambda2 d x_sto0_ =
    (case (case (x_sto0_ , x_sto0_) of
               {(x_var_1 , x_State0_) -> ((Left
                                                 x_var_1) , x_State0_)}) of
--                                                 x_var_1) , x_sto0_)}) of
         {((Left x_v_1) , x_state0_) -> lambda3  x_v_1 x_state0_ })
--         {((Left x_v_1) , x_state0_) -> lambda3 x_sto0_ x_v_1 x_state0_ })    
    
--incr1 :: W8 -> (Either () ((W8 , R)) , W8)
incr1 x_sto0_ =
    (case (case (grunt6 x_sto0_) of
               {(x_var_1 , x_State0_) -> ((Left
                                                 x_var_1) , x_State0_)}) of
--                                                 x_var_1) , x_sto0_)}) of
         {((Left x_v_1) , x_state0_) -> lambda2 x_v_1 x_state0_ })
--         {((Left x_v_1) , x_state0_) -> (((lambda2 x_sto0_) x_v_1) x_state0_)})

--start :: Either ((() , W8)) ((W8 , R))
start :: (Either () (W8, R), W8)
start =
    (incr1
         ((((((((W8 Zero) Zero) Zero) Zero)
                 Zero)
                Zero)
               Zero)
              Zero))


----
---- Stuff produced by hand
----

type Range a o k s = Either (a,s) (o,(k,s))

--start1 :: ReT Bit W8 I ()
start1 :: ReT Bit W8 I ((), W8)
start1 = unfold dispatch _start

_start :: Range () W8 R W8 -- (Either () (W8, R), W8)
_start =
    (_incr
         ((((((((W8 Zero) Zero) Zero) Zero)
                 Zero)
                Zero)
               Zero)
              Zero))


dispatch :: (R,W8) -> Bit -> Range () W8 R W8
dispatch (x_dsc_1,stoW8) x_i_ =
    (case x_dsc_1 of
         {(R_lambda) -> (_lambda4 x_i_ stoW8)})

_incr :: W8 -> Range () W8 R W8 -- (Either () ((W8 , R)) , W8)
_incr x_sto0_ =
    (case (case (grunt6 x_sto0_) of
               {(x_var_1 , x_State0_) -> ((Left
                                                 x_var_1) , x_State0_)}) of
--                                                 x_var_1) , x_sto0_)}) of
         {((Left x_v_1) , x_state0_) -> _lambda2 x_v_1 x_state0_ })
--         {((Left x_v_1) , x_state0_) -> (((lambda2 x_sto0_) x_v_1) x_state0_)})


_lambda4 :: Bit -> W8 -> Range () W8 R W8 -- (Either () ((W8 , R)) , W8)
_lambda4 d x_sto0_ = _incr x_sto0_

_lambda3 :: W8 -> W8 -> Range () W8 R W8  -- (Either () ((W8 , R)) , W8)
_lambda3 x1 x_sto0_ =  Right (x1, (R_lambda, x_sto0_))
-- _lambda3 x1 x_sto0_ = ((Right (x1 , R_lambda)) , x_sto0_)

_lambda2 :: () -> W8 -> Range () W8 R W8  -- (Either () ((W8 , R)) , W8)
_lambda2 d x_sto0_ =
    (case (case (x_sto0_ , x_sto0_) of
               {(x_var_1 , x_State0_) -> ((Left
                                                 x_var_1) , x_State0_)}) of
--                                                 x_var_1) , x_sto0_)}) of
         {((Left x_v_1) , x_state0_) -> _lambda3  x_v_1 x_state0_ })
--         {((Left x_v_1) , x_state0_) -> lambda3 x_sto0_ x_v_1 x_state0_ })    

----
---- End of stuff produced by hand
----
