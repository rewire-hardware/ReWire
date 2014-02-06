module ReWireLib.Examples.Fibonacci where

--import ReWireLib.Prelude
import Control.Monad.Resumption.Reactive
import Data.Word
import Control.Monad.Identity
--import Prelude ()

--Shims
type W8 = Word8
type Bit = Bool

type InnerMonad = Identity

type Re = ReacT Bit W8 InnerMonad 

fib, trfib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)
trfib n = fibacc (0,1) n
  where
    fibacc (a,b) 0 = a
    fibacc (a,b) n  = fibacc (b,a+b) (n-1) 

machine_main :: Re ()
machine_main = stepReT 0 (\ _ -> return (0,1) :: InnerMonad (W8,W8)) >>= machine_fib
  
machine_fib :: (W8,W8) -> Re ()
machine_fib (r1,r2) = stepReT r1 next >>= machine_fib
   where 
         next :: Bit -> InnerMonad (W8,W8)
         next sw = case sw of
                        True -> return (r2,r1+r2)
                        False -> return (r1,r2)

stepReT :: Monad m => out -> (inp -> m a) -> ReacT inp out m a
stepReT out proc = ReacT $ return $ (Right (out,\input -> ReacT $ do
                                                                    res <- proc input
                                                                    return $ Left res))
                                                           
