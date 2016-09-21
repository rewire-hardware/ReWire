module Prelude where

-- These are the Prelude functions we can support.
--
-- See: https://www.haskell.org/onlinereport/haskell2010/haskellch9.html

-- Function type

-- identity function
{-# INLINE id #-}
id :: a -> a
id x = x

-- constant function
{-# INLINE const #-}
const            :: a -> b -> a
const x _        =  x

-- function composition
{-# INLINE (.) #-}
(.)              :: (b -> c) -> (a -> b) -> a -> c
f . g            =  \ x -> f (g x)

{-# INLINE flip #-}
flip                    :: (a -> b -> c) -> b -> a -> c
flip f x y              =  f y x

{-# INLINE ($) #-}
($)                     :: (a -> b) -> a -> b
f $ x                   =  f x

-- Boolean type

data Bool = False | True

(&&),(||) :: Bool -> Bool -> Bool
True && b  = b
False && _ = False
True || _  = True
False || b = b

not :: Bool -> Bool
not True  = False
not False = True

otherwise :: Bool
otherwise = True

-- Maybe type

data Maybe a = Nothing | Just a

{-# INLINE maybe #-}
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing  = n
maybe n f (Just x) = f x

-- Either type

data Either a b = Left a | Right b

{-# INLINE either #-}
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x)  = f x
either f g (Right y) = g y

-- Tuples

-- (Data types for tuples are defined in PrimBasis)
-- data  (a,b)   =  (a,b)
-- data  (a,b,c) =  (a,b,c)
{-# INLINE fst #-}
fst :: (a,b) -> a
fst (x,y) = x

{-# INLINE snd #-}
snd :: (a,b) -> b
snd (x,y) = y

-- curry converts an uncurried function to a curried function;
-- uncurry converts a curried function to a function on pairs.
{-# INLINE curry #-}
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

{-# INLINE uncurry #-}
uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p = f (fst p) (snd p)

undefined :: a
undefined = primError "Prelude.undefined"

infixr 9  .
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $
