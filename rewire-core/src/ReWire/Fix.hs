{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.Fix (fix, fix', fixOn, boundedFixOn, fixOn', fixUntil, boundedFix, fixPure) where

import ReWire.Error (MonadError, AstError, failAt)
import ReWire.Annotation (noAnn)

import Control.Monad.Identity (Identity (runIdentity))
import Data.Hashable (Hashable (hash))
import Data.Text (Text)
import Numeric.Natural (Natural)

-- | Note: direct equality rather than comparing hashes: equality can stop at
--   the first difference, while hashing always traverses both terms fully.
fixPure :: Eq a => Natural -> (a -> a) -> a -> a
fixPure n f = runIdentity . boundedFix (==) n (pure . f)

fix :: (MonadError AstError m, Hashable a) => Text -> Natural -> (a -> m a) -> a -> m a
fix = fixOn hash

fixUntil :: MonadError AstError m => (a -> Bool) -> Text -> Natural -> (a -> m a) -> a -> m a
fixUntil h = boundedFixOn (const h)

fixOn :: (MonadError AstError m, Eq b) => (a -> b) -> Text -> Natural -> (a -> m a) -> a -> m a
fixOn h = boundedFixOn (\ a' a -> h a' == h a)

-- | Note: evaluates `(f a)` at least once if the bound is not reached.
boundedFixOn :: MonadError AstError m => (a -> a -> Bool) -> Text -> Natural -> (a -> m a) -> a -> m a
boundedFixOn _ m 0 _ _ = failAt noAnn $ m <> " not terminating (mutually recursive definitions?)."
boundedFixOn h m n f a = f a >>= \ a' -> if h a' a then pure a' else boundedFixOn h m (n - 1) f a'

fix' :: Hashable a => (a -> a) -> a -> a
fix' = fixOn' hash

fixOn' :: Eq b => (a -> b) -> (a -> a) -> a -> a
fixOn' h f a | h (f a) == h a = a
             | otherwise      = fixOn' h f (f a)

boundedFix :: Monad m => (a -> a -> Bool) -> Natural -> (a -> m a) -> a -> m a
boundedFix _ 0 _ a = pure a
boundedFix h n f a = f a >>= \ a' -> if h a' a then pure a' else boundedFix h (n - 1) f a'
