-- EXPECT-ERROR: list literal has 2 elements
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits
import ReWire.Monad (Dev, iter)

-- The list literal has two elements, but the result type expects three.
f :: W 8 -> W 3
f _ = fromList [True, False]

start :: Dev (W 8) (W 3)
start = iter f (lit 0)

main :: IO ()
main = undefined
