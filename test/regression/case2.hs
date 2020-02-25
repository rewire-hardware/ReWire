data A = A
data B = B

data IP = IP
data OP = OP

start :: ReT IP OP I ()
start = extrude (extrude (extrude loop A) B) A

intr :: StT A (StT B (StT A I)) ()
intr = do
      A <- get
      lift (lift (put A))

loop :: ReT IP OP (StT A (StT B (StT A I))) ()
loop = do
      signal OP
      lift intr
      loop
