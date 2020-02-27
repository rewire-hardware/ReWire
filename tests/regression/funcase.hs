import ReWire

data In = A | B | C | D
data Out = Out1 | Out2

proc :: In -> Out
proc i = case i of
      A -> Out1
      B -> Out1
      C -> Out1
      _ -> Out2

startp :: In -> ReT In Out I ()
startp i = signal (proc i) >>= \ x -> startp x

starti :: ReT In Out I ()
starti =  startp A

start :: ReT In Out I ()
start =  starti

main = undefined
