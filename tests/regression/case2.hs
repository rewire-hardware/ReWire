import ReWire

data A = A
data B = B

data IP = IP
data OP = OP

main = undefined

start :: ReacT IP OP Identity ()
start = extrude (extrude (extrude loop A) B) A

intr :: StateT A (StateT B (StateT A Identity)) ()
intr = do
      A <- get
      lift (lift (put A))

loop :: ReacT IP OP (StateT A (StateT B (StateT A Identity))) ()
loop = do
      signal OP
      lift intr
      loop
