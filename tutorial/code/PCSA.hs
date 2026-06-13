{-# LANGUAGE DataKinds #-}
import Prelude hiding ((^))
import ReWire
import ReWire.Bits

-- | This file is for interactive exploration with GHCi only: rwc can't
-- | compile it (it imports ReWire.Interactive and declares a class instance).
import ReWire.Interactive 

type W8 = W 8

f :: W8 -> W8 -> W8 -> (W8, W8)
f a b c = ( ((a .&. b) .|. (a .&. c) .|. (b .&. c) ) <<. lit 1 , (a ^ b) ^ c )

-- |
-- | Example 3. Pipelined CSA
-- |

data Ans a = DC | Val a

pcsa :: Re W8 () (Ans (W8, W8)) ()
pcsa = do
          a <- signal DC
          b <- signal DC
          c <- signal DC
          signal (Val (f a b c))
          pcsa
  
start :: Re W8 () (Ans (W8 , W8)) ()
start = pcsa 

snapshot0 :: (W8 , () , Ans (W8 , W8))
snapshot0 = ( lit 0 , () , DC )
      
inputs :: [W8]
inputs = lit 40 : lit 25 : lit 20 : lit 0 : []

instance Pretty a => Pretty (Ans a) where
  pp DC      = "DC"
  pp (Val a) = "Val " Prelude.++ pp a

