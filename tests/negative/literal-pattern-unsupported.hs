-- EXPECT-ERROR: Unsupported syntax in a pattern
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Monad (iter, Dev)
g :: Int -> Bool
g x = case x of
      0 -> True
      _ -> False
start :: Dev Bool Bool
start = iter (\ b -> g 0) False
main = undefined
