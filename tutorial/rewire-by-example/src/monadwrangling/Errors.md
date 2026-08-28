# 2nd Interpreter: Errors and Maybe

The code for this section is found in [Errors.hs](Errors.hs). This new interpreter adds a new arithmetic operation `Div`. I pasted in the `eval0` with a new case for `Div`.
```haskell
module Errors where

data Exp = Const Int | Neg Exp | Add Exp Exp
         | Div Exp Exp                      -- new

instance Show Exp where
  show (Const i) = show i
  show (Neg e)   = "-" ++ show e
  show (Add e1 e2) = show e1 ++ " + " ++ show e2
  show (Div e1 e2) = show e1 ++ " / " ++ show e2

-- | Same as before, but with a new case
eval0 :: Exp -> Int
eval0 (Const i)   = i
eval0 (Neg e)     = - (eval0 e)
eval0 (Add e1 e2) = eval0 e1 + eval0 e2
eval0 (Div e1 e2) = eval0 e1 `div` eval0 e2 -- new

a    = Add c (Neg c)
        where
          c = Const 99
uhoh = Div (Const 1) (Const 0)              -- new
```

Note that, when you run the `Div`-extended version of `eval0`, things don't always end well:
```haskell
λ> uhoh
1 / 0
λ> eval0 uhoh
*** Exception: divide by zero
λ> 
```

### Why can't we just check for 0? 

Think about it this way, what should I replace `????` with below? There's no way of handling that exceptional case and it crashes the program. 
```haskell
eval0 (Div e1 e2) = if v2 == 0 then ???? else eval0 e1 `div` v2 
   where
      v2 = eval0 e2
```

But with the `Maybe` monad, we can use its `Nothing` constructor for this erroneous case; recall the definition of the `Maybe` data type:
```haskell
data Maybe a = Nothing | Just a
```

Here's the definition of `eval2` which is typed in the `Maybe` monad:
```haskell
eval2 :: Exp -> Maybe Int     -- N.b., the new type
eval2 (Const i)   = return i
eval2 (Neg e)     = do
                      v <- eval2 e
                      return (- v)
eval2 (Add e1 e2) = do
                      v1 <- eval2 e1
                      v2 <- eval2 e2
                      return (v1 + v2)
eval2 (Div e1 e2) = do
                      v1 <- eval2 e1
                      v2 <- eval2 e2
                      if v2==0 then Nothing else return (v1 `div` v2) -- fill in ???? with Nothing
```

```haskell
λ> uhoh
1 / 0
λ> eval2 uhoh
Nothing
```

### Maybe Under the Hood

Below is the definition of the `Maybe` monad. The way to think of a computation `x >>= f` is that, if `x` returns some value (i.e., it's `Just v`), then just proceed normally. If an exception is thrown by computing `x` (i.e., it's `Nothing`), then the whole computation `x >>= f` is `Nothing`.
```haskell
data Maybe a = Nothing | Just a

return :: a -> Maybe a
return v = Just v

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b 
(Just v) >>= f = f v
Nothing >>= f  = Nothing
```
