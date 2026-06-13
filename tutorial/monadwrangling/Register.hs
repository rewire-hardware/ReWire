module Register where

import Control.Monad.Identity
import Control.Monad.State

data Exp = Const Int | Neg Exp | Add Exp Exp
         | X  -- new register X

instance Show Exp where
  show (Const i) = show i
  show (Neg e)   = "-" ++ show e
  show (Add e1 e2) = show e1 ++ " + " ++ show e2
  show X           = "X"

-- | Just a copy 
eval1 :: Exp -> Identity Int
eval1 (Const i)   = return i
eval1 (Neg e)     = do
                      v <- eval1 e
                      return (- v)
eval1 (Add e1 e2) = do
                      v1 <- eval1 e1
                      v2 <- eval1 e2
                      return (v1 + v2)

eval1 X           = error "Where's the current value of X?"
----------------------------------------------------------
----------------------------------------------------------
----------------------------------------------------------

readX :: StateT Int Identity Int -- Oh! Here it is.
readX = get

eval3 :: Exp -> StateT Int Identity Int
eval3 (Const i)   = return i
eval3 (Neg e)     = do
                      v <- eval3 e
                      return (- v)
eval3 (Add e1 e2) = do
                      v1 <- eval3 e1
                      v2 <- eval3 e2
                      return (v1 + v2)

eval3 X           = readX

-- | eval0 :: Exp -> Int
-- | eval1 :: Exp -> Identity Int
-- | eval2 :: Exp -> Maybe Int
-- | eval3 :: Exp -> StateT Int Identity Int
-- | eval4 :: Exp -> StateT Int Maybe Int
