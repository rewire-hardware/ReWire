-- EXPECT-ERROR: Cannot evaluate the initial state
{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits (lit, (+))
ext :: W 8
ext = extern "seed" ext
loop :: ReacT (W 8) (W 8) (StateT (W 8) Identity) ()
loop = do
      s <- lift get
      i <- signal s
      lift (put (s + i))
      loop
start :: ReacT (W 8) (W 8) Identity ()
start = extrude loop ext
main :: IO ()
main = undefined
