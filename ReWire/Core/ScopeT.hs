{-# LANGUAGE GeneralizedNewtypeDeriving,MultiParamTypeClasses,FlexibleInstances,TupleSections,FunctionalDependencies #-}

module AssumeT where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Map.Strict (Map,insert,delete)
import qualified Data.Map.Strict as Map
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Maybe (fromJust)

class (Ord v,Monad m) => MonadAssume v t m | m -> v, m -> t where
  assume :: v -> t -> m a -> m a
  forget :: v -> m a -> m a
  query  :: v -> m (Maybe t)

instance (Ord v,Monad m) => MonadAssume v t (AssumeT v t m) where
  assume v t m = AssumeT $ local (insert v t) (deAssumeT m)
  forget v m   = AssumeT $ local (delete v) (deAssumeT m)
  query v      = AssumeT $ do { m <- ask ; return (Map.lookup v m) }

assumptions :: (Foldable f,MonadAssume v t m) => f (v,t) -> m a -> m a
assumptions as m = Foldable.foldr (uncurry assume) m as

forgets :: (Foldable f,MonadAssume v t m) => f v -> m a -> m a
forgets as m = Foldable.foldr forget m as

unsafeQuery :: MonadAssume v t m => v -> m t
unsafeQuery = liftM fromJust . query

addscope v = assume v ()
inscope v  = do mt <- query v
                case mt of
                  Just _  -> return True
                  Nothing -> return False

newtype AssumeT v t m a = AssumeT { deAssumeT :: ReaderT (Map v t) m a }
                           deriving (Monad,MonadTrans,MonadPlus)
type Assume v t = AssumeT v t Identity
type ScopeT v = AssumeT v ()
type Scope v = Assume v ()

runAssumeTWith :: (Foldable f,Ord v) => f (v,t) -> AssumeT v t m a -> m a
runAssumeTWith as m = runReaderT (deAssumeT m) (Foldable.foldr (uncurry insert) Map.empty as)

runAssumeT :: Ord v => AssumeT v t m a -> m a
runAssumeT = runAssumeTWith []

runAssumeWith :: (Foldable f,Ord v) => f (v,t) -> Assume v t a -> a
runAssumeWith as = runIdentity . runAssumeTWith as

runAssume :: Ord v => Assume v t a -> a
runAssume = runAssumeWith []

runScopeTWith :: (Foldable f,Ord v) => f v -> ScopeT v m a -> m a
runScopeTWith as m = runReaderT (deAssumeT m) (Foldable.foldr (uncurry insert . (,())) Map.empty as)

runScopeT :: Ord v => ScopeT v m a -> m a
runScopeT = runScopeTWith []

runScopeWith :: (Foldable f,Ord v) => f v -> Scope v a -> a
runScopeWith as = runIdentity . runScopeTWith as

runScope :: Ord v => Scope v  a -> a
runScope = runScopeWith []

data Lam = Lam String Lam | Var String | App Lam Lam deriving Show

refresh :: String -> (String -> Assume String Lam a) -> Assume String Lam a
refresh x k = do undefined
  where ys = x : map (++"'") ys

subst :: Lam -> Assume String Lam Lam
subst (Var x)     = do ml <- query x
                       case ml of
                         Just l  -> return l
                         Nothing -> return (Var x)
subst (App e1 e2) = do e1' <- subst e1
                       e2' <- subst e2
                       return (App e1' e2')
subst (Lam x e)   = refresh x $ \ x' -> do
                       e' <- subst e
                       return (Lam x' e')
