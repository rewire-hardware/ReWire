{-# LANGUAGE DataKinds #-}
import ReWire

-- Current State | Input | Output | Next State
-- --------------------------------------------
--    si             0       0          s0
--    si             1       0          s1
--    s0             0       0          s0
--    s0             1       1          s1
--    s1             0       1          s0
--    s1             1       0          s1

data Alphabet = Zero | One

si , s0 , s1 :: Alphabet -> ReacT Alphabet Alphabet Identity ()

si Zero = signal Zero >>= s0
si One  = signal Zero >>= s1

s0 Zero = signal Zero >>= s0
s0 One  = signal One  >>= s1

s1 Zero = signal One  >>= s0
s1 One  = signal Zero >>= s1

start :: ReacT Alphabet Alphabet Identity ()
start = signal Zero >>= s0

