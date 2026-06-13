{-# LANGUAGE DataKinds #-}
import Prelude hiding ((+))
import ReWire
import ReWire.Bits
-- import ReWire.Interactive

start :: ReacT Bit (W 8) Identity ()
start = fibgen (lit 0) (lit 1)

fibgen :: W 8 -> W 8 -> ReacT Bit (W 8) Identity ()
fibgen n m = do b <- signal n
                if b then fibgen n m else fibgen m (n + m)

{-
-- | Compare with
-- fibgen :: Int -> Int -> [Int]
-- fibgen n m = n : fibgen m (n + m)

is1 , is2 :: [Bit]
is1 = False : False : False : False : False : False : False : False : False : False : []
is2 = True : False : True : False : True : False : True : False : True : False : []
-}
