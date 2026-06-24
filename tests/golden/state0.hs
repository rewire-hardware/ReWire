{-# LANGUAGE DataKinds #-}
import ReWire ( W, signal, Identity, ReacT )
import ReWire.Bits (lit)

type Dev i o = ReacT i o Identity ()

dev :: W 3 -> Dev (W 3) (Maybe (W 3))
dev i = signal Nothing >> signal Nothing >> signal (Just i) >>= dev

start :: Dev (W 3) (Maybe (W 3))
start = dev (lit 0)

main = undefined
