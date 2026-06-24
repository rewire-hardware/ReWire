-- Regression for a purify "might pause" false positive. A bind whose
-- left-hand side pauses (here `lift (...) >>= emit`, where `emit` signals),
-- sitting directly in a `case` arm and referencing a `where`/`let`-bound
-- monadic action (`toggle`), was fused into the match discriminant by
-- liftLambdas (its operands hidden behind MatchPatVars) and so went
-- unrecognized by normalizeBind's `dstBind`. The pausing LHS then survived to
-- purify, which rejected it with "the left-hand side of this bind (RVar) might
-- pause". liftLambdas now leaves such binds syntactic so normalizeBind can
-- normalize them.
import ReWire hiding (Bit)

data Bit = C | S
data P = P Bit

notb :: Bit -> Bit
notb C = S
notb S = C

emit :: Bit -> ReacT P Bit (StateT Bit Identity) P
emit x = do
      lift (put x)
      signal x

go :: P -> ReacT P Bit (StateT Bit Identity) ()
go (P g) = case g of
      S -> (lift (toggle >> get) >>= emit) >>= go
      C -> (lift get >>= emit) >>= go
   where toggle = modify notb

start :: ReacT P Bit Identity ()
start = extrude (go (P C)) C

main = undefined
