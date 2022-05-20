{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module ReWire.Fix (fix, fix', fixOn, fixUntil) where

import ReWire.Error (MonadError, AstError, failAt)
import ReWire.Annotation (noAnn)

import Data.Hashable (Hashable (hash))
import Data.Text (Text)

fix :: (MonadError AstError m, Hashable a) => Text -> Int -> (a -> m a) -> a -> m a
fix = fixOn hash

fixUntil :: MonadError AstError m => (a -> Bool) -> Text -> Int -> (a -> m a) -> a -> m a
fixUntil h = fixOn' (const h)

fixOn :: (MonadError AstError m, Eq b) => (a -> b) -> Text -> Int -> (a -> m a) -> a -> m a
fixOn h = fixOn' (\ a' a -> h a' == h a)

fixOn' :: MonadError AstError m => (a -> a -> Bool) -> Text -> Int -> (a -> m a) -> a -> m a
fixOn' _ m 0 _ _ = failAt noAnn $ m <> " not terminating (mutually recursive definitions?)."
fixOn' h m n f a = f a >>= \ a' -> if h a' a then pure a else fixOn' h m (n - 1) f a'

fix' :: Hashable a => (a -> a) -> a -> a
fix' f a = if hash (f a) == hash a then a else fix' f (f a)
