## Carry Save Addition

There are three carry-save adders in the tutorial, [CSA.hs](code/CSA.hs), [SCSA.hs](code/SCSA.hs), and [PCSA.hs](code/PCSA.hs), and the first of these is explained in detail below.

Carry save addition (<https://en.wikipedia.org/wiki/Carry-save_adder>) is defined as function `f`:
```haskell
f :: W 8 -> W 8 -> W 8 -> (W 8, W 8)
f a b c = ( ((a .&. b) .|. (a .&. c) .|. (b .&. c) ) <<. lit 1 , (a ^ b) ^ c )
```
Here, I define `f` using ReWire's built-in word constructor, picking `W 8` for the sake of concreteness.
I'll define a few constants for convenience in a running example.
```haskell
_40 , _25 , _20 , _41 , _0 :: W 8
_40 = lit 40
_25 = lit 25
_20 = lit 20
_41 = lit 41
_0  = lit 0
```

Using GHCi, we can test it out, like any Haskell function:
```haskell
λ> :t f
f :: W 8 -> W 8 -> W 8 -> (W 8, W 8)
λ> f _40 _25 _20
(Vector [False,False,True,True,False,False,False,False],Vector [False,False,True,False,False,True,False,True])
```
What's this mess? `W 8` values are represented internally using Haskell's `Data.Vector` library and, well, it ain't pretty. There is a ReWire library you can import to make all this more palatable called `ReWire.Interactive`:
```haskell
λ> pretty (f _40 _25 _20)
(48,37)
λ> pretty (f _41 _25 _20)
(50,36)
λ> 
```

### Making a basic carry save adder

```haskell
-- |
-- | Example 1. CSA
-- |
-- | The only thing this does is take its inputs i, computes csa on them, and
-- | output the results every clock cycle.

csa :: (W 8, W 8, W 8) -> ReacT (W 8, W 8, W 8) (W 8, W 8) Identity ()
csa (a, b, c) = do
                   abc' <- signal (f a b c)
                   csa abc'

start :: ReacT (W 8, W 8, W 8) (W 8, W 8) Identity ()
start = csa (_0, _0, _0)
```

First, `csa` consumes its three inputs `a`, `b`, and `c` as a tuple. Then, it computes the carry save addition on these and puts the result on the output port, `signal (f a b c)`. Finally, it obtains the next inputs, `abc'` and continues.

*What does the type of `csa` mean?* It's worth contemplating the type of `csa`'s codomain, which is `ReacT (W 8, W 8, W 8) (W 8, W 8) Identity ()`.
- The input type is `(W 8, W 8, W 8)`, meaning that every it takes three `W 8`s each clock cycle;
- The output type is `(W 8, W 8)`, meaning that every it produces two `W 8`s each clock cycle; and
- It does not use any internal storage or registers, hence the `Identity` monad is used rather than a state monad.


#### Running it in GHCi
You can run this using `pretty` and `runP` from `ReWire.Interactive`. First, define some inputs that look familiar:
```haskell
inputs :: [(W 8 , W 8 , W 8)]
inputs = (_40 , _25 , _20)
       : (_41 , _25 , _20)
       : (_40 , _25 , _20)  : []
```
```haskell
λ> :t pretty
pretty :: Pretty a => a -> IO ()
λ> pretty $ runP start ((_0  , _0  , _0) , (_0  , _0 )) inputs
((0,0,0),(0,0)) :> ((40,25,20),(0,0)) :> ((41,25,20),(48,37)) :> ((40,25,20),(50,36)) :+> Nothing
```
(**WARNING:** `ReWire.Interactive` is currently in *super-king-kong-major-hacky* form right now.)

#### Compiling it with RWC

First, here's the entire file as it stands:
```haskell
{-# LANGUAGE DataKinds #-}
import Prelude hiding ((^))
import ReWire
import ReWire.Bits

-- | ReWire compiler will complain if this is imported
import ReWire.Interactive

f :: W 8 -> W 8 -> W 8 -> (W 8, W 8)
f a b c = ( ((a .&. b) .|. (a .&. c) .|. (b .&. c) ) <<. lit 1 , (a ^ b) ^ c )

-- Constants for a running example.
_40 , _25 , _20 , _41 , _0 :: W 8
_40 = lit 40
_25 = lit 25
_20 = lit 20
_41 = lit 41
_0  = lit 0

-- |
-- | Example 1. CSA
-- |
-- | The only thing this does is take its inputs i, computes csa on them, and
-- | output the results every clock cycle.

csa :: (W 8, W 8, W 8) -> ReacT (W 8, W 8, W 8) (W 8, W 8) Identity ()
csa (a, b, c) = do
                   abc' <- signal (f a b c)
                   csa abc'

start :: ReacT (W 8, W 8, W 8) (W 8, W 8) Identity ()
start = csa (_0, _0, _0)

-- | ReWire compiler will complain if this is here (i.e., comment it before compiling):
inputs :: [(W 8 , W 8 , W 8)]
inputs = (_40 , _25 , _20)
       : (_41 , _25 , _20)
       : (_40 , _25 , _20)  : []
```

*Pro-tip.* Because ReWire doesn't know about things likes lists, `ReWire.Interactive` and the definition of `inputs` need to be commented out *before* compiling with `rwc`. Otherwise, you will receive a non-informative error message like this:
```haskell
$ rwc CSA.hs --verilog
Control/Monad/Identity.hs:
Error: File not found in load-path
$ 
```

Assuming these are now commented out, you can proceed to compile CSA.hs with:
```haskell
$ ls -l CSA.* 
-rw-r--r--  1 william.harrison  staff  1039 Jun 13 09:02 CSA.hs
$ rwc CSA.hs --verilog
$ ls -l CSA.*         
-rw-r--r--  1 william.harrison  staff  1039 Jun 13 09:02 CSA.hs
-rw-r--r--  1 william.harrison  staff  2159 Jun 13 09:04 CSA.sv
$ 
```
