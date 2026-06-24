-- EXPECT-ERROR: Pattern is not applied to enough arguments
import ReWire
import ReWire.Monad (iter, Dev)
data T = MkT Bool Bool
f :: T -> Bool
f x = case x of
      MkT a -> a
start :: Dev T T
start = iter (\ x -> x) (MkT False False)
main = undefined
