-- EXPECT-ERROR: unsupported expression syntax
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Monad (iter, Dev)
g :: Bool -> Bool
g x = case 'a' of
      _ -> x
start :: Dev Bool Bool
start = iter g False
main = undefined
