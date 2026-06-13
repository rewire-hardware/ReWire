module IdentityMonad where

import Control.Monad.Identity -- this is new.

data Exp = Const Int | Neg Exp | Add Exp Exp

instance Show Exp where
  show (Const i) = show i
  show (Neg e)   = "-" ++ show e
  show (Add e1 e2) = show e1 ++ " + " ++ show e2

eval1 :: Exp -> Identity Int
eval1 (Const i)   = return i
eval1 (Neg e)     = eval1 e >>= \ v -> return (- v)
eval1 (Add e1 e2) = eval1 e1 >>= \ v1 -> eval1 e2 >>= \ v2 -> return (v1 + v2)

c = Const 99
n = Neg c
a = Add c n

-- | eval0 :: Exp -> Int
-- | eval1 :: Exp -> Identity Int
-- | eval2 :: Exp -> Maybe Int
-- | eval3 :: Exp -> StateT Int Identity Int
-- | eval4 :: Exp -> StateT Int Maybe Int
