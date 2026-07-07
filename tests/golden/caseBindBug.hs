-- Regression for a retired-pipeline "might pause" false positive: a bind
-- whose left-hand side pauses (here `lift (...) >>= emit`, where `emit`
-- signals), sitting directly in a `case` arm and referencing a
-- `where`/`let`-bound monadic action (`toggle`), was mis-fused during
-- lambda lifting and rejected by purification with "the left-hand side of
-- this bind (RVar) might pause". Kept as a shape test for case-headed
-- reactive binds (a pausing bind LHS now compiles by splicing the callee's
-- block graph; see Eidos.Procify).
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
