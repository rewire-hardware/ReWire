module Arith where

data Exp = Const Int | Neg Exp | Add Exp Exp

instance Show Exp where
  show (Const i) = show i
  show (Neg e)   = "-" ++ show e
  show (Add e1 e2) = show e1 ++ " + " ++ show e2

eval0 :: Exp -> Int
eval0 (Const i)   = i
eval0 (Neg e)     = - (eval0 e)
eval0 (Add e1 e2) = eval0 e1 + eval0 e2

c = Const 99
n = Neg c
a = Add c n

-- | eval0 :: Exp -> Int
-- | eval1 :: Exp -> Identity Int
-- | eval2 :: Exp -> Maybe Int
-- | eval3 :: Exp -> StateT Int Identity Int
-- | eval4 :: Exp -> StateT Int Maybe Int
