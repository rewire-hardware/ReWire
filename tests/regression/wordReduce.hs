{-# LANGUAGE DataKinds #-}
import ReWire ( W, fromList )
import ReWire.Bits (lit, rAnd, rNAnd, rOr, rNor, rXOr, rXNor)
import ReWire.Monad (iter, Dev)

-- | Bits: reduction operators
-- (RAnd, "rAnd"), (RNAnd, "rNAnd"), (ROr, "rOr"), (RNor, "rNor"),
-- (RXOr, "rXOr"), (RXNor, "rXNor")
compute :: W 8 -> W 6
compute w = fromList [rAnd w, rNAnd w, rOr w, rNor w, rXOr w, rXNor w]

start :: Dev (W 8) (W 6)
start = iter compute (lit 0)

main = undefined
