module ReWire.Core.Transformations.ConnectLogic where

import Prelude hiding (mapM)
import Data.Functor
import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid
import ReWire.PreHDL.Syntax
import ReWire.Scoping
import Control.Monad.State hiding (mapM)

type Fun = () --Id FunDefn --FIXME

data CLTree a  = Par (CLTree a) (CLTree a)
                 | ReFold Fun Fun (CLTree a)
                 | Leaf a 
                  deriving (Show)


instance Functor CLTree where
  fmap f (Par l r)        = Par (fmap f l) (fmap f r)
  fmap f (ReFold f1 f2 r) = ReFold f1 f2 (fmap f r) 
  fmap f (Leaf a)         = Leaf (f a)

instance Foldable CLTree where
  foldMap f (Par l r) = (foldMap f l) `mappend` (foldMap f r)
  foldMap f (ReFold _ _ r) = foldMap f r
  foldMap f (Leaf a) = f a

instance Traversable CLTree where
  traverse f (Leaf a)          = Leaf <$> f a 
  traverse f (ReFold f1 f2 re) = ReFold f1 f2 <$> traverse f re
  traverse f (Par l r)         = (liftA2 Par) (traverse f l) (traverse f r)



test :: CLTree Int
test = Par (Leaf 0) (Leaf 0)

test2 :: CLTree Int
test2 = Par 
          (ReFold () () (Leaf 0)) 
          (Par (Leaf 0) (Leaf 0))

type TestM = State Int 

addInc :: (Int -> TestM Int)
addInc x = do
              y <- get
              modify (+1)
              return (x+y)

