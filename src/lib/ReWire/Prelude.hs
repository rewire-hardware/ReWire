module ReWire.Prelude where

import ReWire
import Prelude () -- Imports of "Prelude" are always ignored by rwc.
import qualified Prelude as GHC

data Bool = False | True
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b

-- | Identity function.
{-# INLINE id #-}
id :: a -> a
id x = x

-- | Constant function.
{-# INLINE const #-}
const            :: a -> b -> a
const x _        =  x

-- | Function composition.
{-# INLINE (.) #-}
(.)              :: (b -> c) -> (a -> b) -> a -> c
f . g            =  \ x -> f (g x)

{-# INLINE flip #-}
flip                    :: (a -> b -> c) -> b -> a -> c
flip f x y              =  f y x

{-# INLINE ($) #-}
($)                     :: (a -> b) -> a -> b
f $ x                   =  f x

-- | Boolean type

(&&) :: Bool -> Bool -> Bool
True  && b = b
False && _ = False

(||) :: Bool -> Bool -> Bool
True  || _ = True
False || b = b

not :: Bool -> Bool
not True  = False
not False = True

otherwise :: Bool
otherwise = True

-- | Maybe type

{-# INLINE maybe #-}
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing  = n
maybe _ f (Just x) = f x

-- | Either type

{-# INLINE either #-}
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x)  = f x
either _ g (Right y) = g y

{-# INLINE fst #-}
fst :: (a, b) -> a
fst (x, _) = x

{-# INLINE snd #-}
snd :: (a, b) -> b
snd (_, y) = y

-- | Converts an uncurried function to a curried function;
{-# INLINE curry #-}
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

-- | Converts a curried function to a function on pairs.
{-# INLINE uncurry #-}
uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p = f (fst p) (snd p)

{-# INLINE undefined #-}
undefined :: a
undefined = error "undefined"

{-# INLINE return #-}
return :: GHC.Monad m => a -> m a
return = rwReturn

{-# INLINE (>>=) #-}
(>>=) :: GHC.Monad m => m a -> (a -> m b) -> m b
(>>=) = rwBind

{-# INLINE (=<<) #-}
(=<<) :: GHC.Monad m => (a -> m b) -> m a -> m b
(=<<) = flip rwBind

{-# INLINE (>>) #-}
(>>) :: GHC.Monad m => m a -> m b -> m b
ma >> mb = ma >>= (\ _ -> mb)

infixr 9  .
infixr 3 &&
infixr 2 ||
infixr 1  =<<
infixr 0  $

infixl 1  >>, >>=
