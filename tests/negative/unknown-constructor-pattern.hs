-- EXPECT-ERROR: Unknown constructor: Mystery
import ReWire
import ReWire.Monad (iter, Dev)
f :: Bool -> Bool
f x = case x of
      Mystery -> False
      _       -> True
start :: Dev Bool Bool
start = iter f False
main = undefined
