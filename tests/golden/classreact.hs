{-# LANGUAGE DataKinds #-}
-- Class methods with reactive result types: dictionaries are eliminated
-- before purification, and the selected methods' signal loops purify into
-- a multi-phase machine (distinct states must survive in the device).
import ReWire
import ReWire.Bits (lit, (+), (-), (==))
import ReWire.Monad (Dev)
import Prelude hiding ((+), (-), (==))

data Mode = Up | Down

class Runner a where
      run  :: a -> W 8 -> Dev (W 8) (W 8)
      seed :: a -> W 8

instance Runner Mode where
      run Up   = countUp
      run Down = countDown
      seed Up   = lit 0
      seed Down = lit 255

countUp :: W 8 -> Dev (W 8) (W 8)
countUp n = do
      i <- signal n
      if i == lit 0 then return () else countUp (n + i)

countDown :: W 8 -> Dev (W 8) (W 8)
countDown n = do
      i <- signal n
      if i == lit 0 then return () else countDown (n - i)

start :: Dev (W 8) (W 8)
start = run Up (seed Up) >> run Down (seed Down) >> start

main :: IO ()
main = undefined
