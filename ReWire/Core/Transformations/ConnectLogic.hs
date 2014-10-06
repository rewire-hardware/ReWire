module ReWire.Core.Transformations.ConnectLogic where

import Prelude hiding (mapM)
import Data.Functor
import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.Monad.Trans.State hiding (mapM)
import Control.Monad.Trans.Writer 
import Control.Monad.Trans.Class
import Data.Functor.Identity

import ReWire.PreHDL.Syntax
import ReWire.Scoping
import ReWire.Core.Syntax

type Fun = RWCExp --FIXME: Not sure what makes sense here

data CLTree a  = Par [CLTree a]
                 | ReFold Fun Fun (CLTree a)
                 | Leaf a 
                  deriving (Show)
                  
type CLExp = CLTree RWCExp
type CLNamed = CLTree String

type NRe = (String, RWCExp) --Named Re computations that do not contain connect logic 
type NCL = (String, CLNamed)

type CLM = WriterT [NCL] (WriterT [NRe] (StateT Int Identity))

writeCL :: NCL -> CLM ()
writeCL cl = do
               tell [cl]

writeRe :: NRe -> CLM ()
writeRe re = do
                lift $ tell [re]

nextLabel :: CLM Int
nextLabel = do
              r <- lift $ lift $ get
              lift $ lift $ put (r+1) 
              return r

instance Functor CLTree where
  fmap f (Par ls)        = Par $ fmap (fmap f) ls 
  fmap f (ReFold f1 f2 r) = ReFold f1 f2 (fmap f r) 
  fmap f (Leaf a)         = Leaf (f a)

instance Foldable CLTree where
  foldMap f (Par ls) = mconcat $ map (foldMap f) ls --) `mappend` (foldMap f r)
  foldMap f (ReFold _ _ r) = foldMap f r
  foldMap f (Leaf a) = f a

instance Traversable CLTree where
  traverse f (Leaf a)          = Leaf <$> f a 
  traverse f (ReFold f1 f2 re) = ReFold f1 f2 <$> traverse f re
  traverse f (Par ls)          = Par <$> (traverse (traverse f) ls) --(traverse f r)


clExpr :: RWCExp -> CLExp 
clExpr e = case ef of
                RWCVar x _ | x == mkId "par"    -> case args of
                                                      (a1:a2:[]) -> Par [clExpr $ a1,clExpr $ a2] 
                                                      _ -> error "clExpr: Par wrong # args."
                RWCVar x _ | x == mkId "refold" -> case args of
                                                      (f1:f2:re:[]) -> ReFold f1 f2 (clExpr re)
                                                      _ -> error "clExpr: ReFold wrong # args."
                                                       
                _ -> Leaf e
  where
    (ef:args) = flattenApp e
