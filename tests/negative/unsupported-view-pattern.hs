-- EXPECT-ERROR: Unsupported pattern
{-# LANGUAGE ViewPatterns #-}
import ReWire
import ReWire.Bits
idf :: Bit -> Bit
idf b = b
foo :: Bit -> Bit
foo n@(idf -> y) = n
start :: ReacT Bit Bit Identity ()
start = do { signal (foo zero); start }
main = undefined
