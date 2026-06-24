{-# LANGUAGE DataKinds #-}
import Prelude hiding (not, return, (>>=), (>>), init, last, head, replicate)
import ReWire ( extrude, W, Vec)
import ReWire.Bits ( lit)
import ReWire.Monad (Dev, iterSt)
import ReWire.Vectors (cons, init, last, head, replicate)

type MyBool = Bool

data MyState = MyState MyBool (Maybe (W 4)) (Either (W 4) (W 8))

type PipelineState = Vec 2 MyState

data Input = InputA (W 4) | InputB (W 8) | InputC MyBool

data Output = Output MyBool MyState

start :: Dev Input Output
start = extrude (iterSt loop initInput) emptyPipeline

incrPipeline :: (MyState, PipelineState) -> (PipelineState, MyState)
incrPipeline (s,ps) = (cons s (init ps :: Vec 1 MyState), last ps)

inputToMyState :: Input -> MyState -> MyState
inputToMyState (InputA w4) (MyState b _ _) = MyState b (Just w4) (Left w4)
inputToMyState (InputB w8) (MyState b w4 _) = MyState b w4 (Right w8)
inputToMyState (InputC True) (MyState _ (Just w4) _) = MyState True Nothing (Left w4)
inputToMyState (InputC True) (MyState _ Nothing w8) = MyState True Nothing w8
inputToMyState (InputC False) (MyState _ _ w8) = MyState False Nothing w8

myStateToOutput :: MyState -> Output
myStateToOutput (MyState True mw4 (Left w4)) = Output True (MyState True mw4 (Left w4))
myStateToOutput (MyState True mw4 (Right w8)) = Output False (MyState True mw4 (Right w8))
myStateToOutput (MyState False _ w4w8) = Output False (MyState False Nothing w4w8)

loop :: Input -> PipelineState -> (Output,PipelineState)
loop i s = let (s',out) = incrPipeline (inputToMyState i (head s),s)
           in (myStateToOutput out,s')

emptyMyState :: MyState
emptyMyState = MyState False Nothing (Left (lit 0))

emptyPipeline :: PipelineState
emptyPipeline = replicate emptyMyState

initInput :: Input
initInput = InputC False

main = undefined
