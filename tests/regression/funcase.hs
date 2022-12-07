import ReWire

data In = IA | IB | IC | ID
data Out = Out1 | Out2

proc :: In -> Out
proc i = case i of
      IA -> Out1
      IB -> Out1
      IC -> Out1
      _ -> Out2

startp :: In -> ReacT In Out Identity ()
startp i = signal (proc i) >>= \ x -> startp x

starti :: ReacT In Out Identity ()
starti =  startp IA

start :: ReacT In Out Identity ()
start =  starti

main = undefined
