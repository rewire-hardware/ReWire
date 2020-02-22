data Ip = Ip
data Op = Op
data So

incr :: ReT Ip Op (StT () I) ()
incr = do
      i <- signal Op
      case i of
            Ip -> incr

start :: ReT Ip Op I ()
start = extrude incr ()
