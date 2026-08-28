{-# LANGUAGE DataKinds #-}
-- Single-method classes have newtype dictionaries; this exercises every
-- shape whose class type would leak without the bridge's unwrap: a direct
-- instance, a single-method superclass held in a data dictionary's theta
-- field, a default method with an empty instance, a methodless subclass
-- (whose dictionary IS the superclass dictionary), and two single-method
-- classes sharing a method type (they unwrap to the same Eidos type).
import ReWire
import ReWire.Bits (lit, (^), (.&.), (.|.))
import ReWire.Monad (iter, Dev)
import Prelude hiding ((^))

class Scramble a where
      scramble :: a -> a

class Twiddle a where
      twiddle :: a -> a

class Scramble a => Mix a where
      mixup   :: a -> a
      mixdown :: a -> a

class Nudge a where
      nudge :: a -> a
      nudge x = x

class Scramble a => Tagged a

instance Scramble (W 8) where
      scramble w = w ^ lit 0x5a

instance Twiddle (W 8) where
      twiddle w = w ^ lit 0xa5

instance Mix (W 8) where
      mixup w   = scramble w .|. lit 0x0f
      mixdown w = scramble w .&. lit 0xf0

instance Nudge (W 8)

instance Tagged (W 8)

viaSuper :: Tagged a => a -> a
viaSuper = scramble

step :: W 8 -> W 8
step w = mixup (twiddle w) ^ mixdown (nudge (viaSuper w))

start :: Dev (W 8) (W 8)
start = iter step (lit 0)

main :: IO ()
main = undefined
