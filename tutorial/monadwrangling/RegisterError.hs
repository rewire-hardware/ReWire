module Register where

import Control.Monad.State

data Exp = Const Int | Neg Exp | Add Exp Exp
         | Div Exp Exp
         | X  -- new register X

instance Show Exp where
  show (Const i) = show i
  show (Neg e)   = "-" ++ show e
  show (Add e1 e2) = show e1 ++ " + " ++ show e2
  show (Div e1 e2) = show e1 ++ " / " ++ show e2
  show X           = "X"

----------------------------------------------------------
----------------------------------------------------------
----------------------------------------------------------

readX :: StateT Int Maybe Int
readX = get

eval4 :: Exp -> StateT Int Maybe Int
eval4 (Const i)   = return i
eval4 (Neg e)     = do
                      v <- eval4 e
                      return (- v)
eval4 (Add e1 e2) = do
                      v1 <- eval4 e1
                      v2 <- eval4 e2
                      return (v1 + v2)
eval4 (Div e1 e2) = do
                      v1 <- eval4 e1
                      v2 <- eval4 e2
                      if v2==0 then lift Nothing else return (v1 `div` v2)

eval4 X           = readX

-- | eval0 :: Exp -> Int
-- | eval1 :: Exp -> Identity Int
-- | eval2 :: Exp -> Maybe Int
-- | eval3 :: Exp -> StateT Int Identity Int
-- | eval4 :: Exp -> StateT Int Maybe Int
