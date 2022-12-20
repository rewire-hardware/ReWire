{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module ReWire.Fix (fix, fix', fixOn, boundedFixOn, fixOn', fixUntil, boundedFix) where

import ReWire.Error (MonadError, AstError, failAt)
import ReWire.Annotation (noAnn)

import Data.Hashable (Hashable (hash))
import Data.Text (Text)
import Numeric.Natural (Natural)

fix :: (MonadError AstError m, Hashable a) => Text -> Natural -> (a -> m a) -> a -> m a
fix = fixOn hash

fixUntil :: MonadError AstError m => (a -> Bool) -> Text -> Natural -> (a -> m a) -> a -> m a
fixUntil h = boundedFixOn (const h)

fixOn :: (MonadError AstError m, Eq b) => (a -> b) -> Text -> Natural -> (a -> m a) -> a -> m a
fixOn h = boundedFixOn (\ a' a -> h a' == h a)

boundedFixOn :: MonadError AstError m => (a -> a -> Bool) -> Text -> Natural -> (a -> m a) -> a -> m a
boundedFixOn _ m 0 _ _ = failAt noAnn $ m <> " not terminating (mutually recursive definitions?)."
boundedFixOn h m n f a = f a >>= \ a' -> if h a' a then pure a else boundedFixOn h m (n - 1) f a'

fix' :: Hashable a => (a -> a) -> a -> a
fix' = fixOn' hash

fixOn' :: Eq b => (a -> b) -> (a -> a) -> a -> a
fixOn' h f a | h (f a) == h a = a
             | otherwise      = fixOn' h f (f a)

boundedFix :: Monad m => (a -> a -> Bool) -> Natural -> (a -> m a) -> a -> m a
boundedFix _ 0 _ a = pure a
boundedFix h n f a = f a >>= \ a' -> if h a' a then pure a else boundedFix h (n - 1) f a'
