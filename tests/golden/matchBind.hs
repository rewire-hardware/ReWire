{-# LANGUAGE DataKinds #-}
import Prelude hiding ((^), (+), (==), (&&) , even)
import ReWire.Bits
import ReWire

type Byte = W 8

type S   = StateT Byte Identity
type Dev = ReacT Byte Byte S

tick :: Dev ()
tick     = do o <- lift get
              i <- signal o
              lift (put i)

reset :: Dev ()
reset = do
           lift $ put (lit 0)
           tick
           loop

incr :: Dev ()
incr = do _ <- signal (lit 2)
          return ()


loop :: Dev ()
loop = do i <- lift get
          if even i then incr else incr
          loop


start :: ReacT Byte Byte Identity ()
start = extrude reset (lit 0)

main = undefined
