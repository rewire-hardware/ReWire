import ReWire

type M   = StateT Bit Identity

-- | Reads a register's current value
{-# INLINE reg #-}
reg :: Monad m => (s -> a) -> StateT s m a
reg f = get >>= return . f

{-# INLINE (<??>) #-}
(<??>) :: (Bit -> Bit) -> M () -> M ()
reg <??> x = do
               rf <- get
               if reg rf then x else return ()

go :: ReacT Bit Bit M Bit
go = do
      lift (id <??> return ())
      x <- signal False
      go

start :: ReacT Bit Bit Identity Bit
start = extrude go True

main = undefined
