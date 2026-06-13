module Errors where

data Exp = Const Int | Neg Exp | Add Exp Exp
         | Div Exp Exp                      -- new

instance Show Exp where
  show (Const i) = show i
  show (Neg e)   = "-" ++ show e
  show (Add e1 e2) = show e1 ++ " + " ++ show e2
  show (Div e1 e2) = show e1 ++ " / " ++ show e2

eval0 :: Exp -> Int
eval0 (Const i)   = i
eval0 (Neg e)     = - (eval0 e)
eval0 (Add e1 e2) = eval0 e1 + eval0 e2
eval0 (Div e1 e2) = eval0 e1 `div` eval0 e2 -- new

a = Add c (Neg c)
  where
    c = Const 99
uhoh = Div (Const 1) (Const 0)

eval2 :: Exp -> Maybe Int
eval2 (Const i)   = return i
eval2 (Neg e)     = do
                      v <- eval2 e
                      return (- v)
eval2 (Add e1 e2) = do
                      v1 <- eval2 e1
                      v2 <- eval2 e2
                      return (v1 + v2)
eval2 (Div e1 e2) = do
                      v1 <- eval2 e1
                      v2 <- eval2 e2
                      if v2==0 then Nothing else return (v1 `div` v2)

-- | eval0 :: Exp -> Int
-- | eval1 :: Exp -> Identity Int
-- | eval2 :: Exp -> Maybe Int
-- | eval3 :: Exp -> StateT Int Identity Int
-- | eval4 :: Exp -> StateT Int Maybe Int
