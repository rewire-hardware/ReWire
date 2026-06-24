import ReWire

data Ip = Ip
data Op = Op
data So

incr :: ReacT Ip Op (StateT () Identity) ()
incr = do
      i <- signal Op
      case i of
            Ip -> incr

start :: ReacT Ip Op Identity ()
start = extrude incr ()

main = undefined
