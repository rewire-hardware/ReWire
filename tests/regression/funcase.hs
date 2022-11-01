import ReWire

data In = IA | IB | IC | ID
data Out = Out1 | Out2

proc :: In -> Out
proc i = case i of
      IA -> Out1
      IB -> Out1
      IC -> Out1
      _ -> Out2

startp :: In -> ReT In Out I ()
startp i = signal (proc i) >>= \ x -> startp x

starti :: ReT In Out I ()
starti =  startp IA

start :: ReT In Out I ()
start =  starti

main = undefined
