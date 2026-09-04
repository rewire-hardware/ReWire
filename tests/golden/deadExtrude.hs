{-# LANGUAGE DataKinds #-}
import ReWire
import ReWire.Bits

-- A stateless device with a dead branch that carries an inline extrude:
-- the partial evaluator's last round leaves the orphaned definition in
-- the program, and its StateT layer must not mint a state cell (the
-- device has no register beyond its resumption tag).
dead :: ReacT Bit Bit Identity ()
dead = extrude (signal True >>= \ i -> lift (put i) >>= \ _ -> lift get >>= \ s -> return ()) False >>= \ _ -> dead

live :: ReacT Bit Bit Identity ()
live = signal True >>= \ i -> signal i >>= \ _ -> live

go :: Bool -> ReacT Bit Bit Identity ()
go b = if b then dead else live

start :: ReacT Bit Bit Identity ()
start = go False

main = undefined
