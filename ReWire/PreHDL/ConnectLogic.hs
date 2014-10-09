{-#LANGUAGE RankNTypes #-}
module ReWire.PreHDL.ConnectLogic(CLTree(..),
                                                CLExp,
                                                CLNamed,
                                                CLFNamed,
                                                NRe,
                                                NCL,
                                                NCLF,
                                                flattenCLExp
                                               )where

import Prelude 
import Data.Functor
import Control.Applicative
import Data.Foldable
import Data.Traversable hiding (mapM)
import Data.Monoid
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer 
import Control.Monad.Trans.Class
import Data.Functor.Identity

import ReWire.Scoping
import ReWire.Core.Syntax
import ReWire.PreHDL.Syntax (FunDefn)

type Fun = RWCExp --FIXME: Not sure what makes sense here

data CLTree f a  = Par [CLTree f a]
                 | ReFold f f (CLTree f a)
                 | Leaf a 
                  deriving (Show)
                  
type CLExp = CLTree Fun RWCExp
type CLNamed = CLTree Fun String
type CLFNamed = CLTree FunDefn String

type NRe = (String, RWCExp) --Named Re computations that do not contain connect logic 
type NCL = (String, CLNamed)
type NCLF = (String, CLFNamed)

type CLM = WriterT [NCL] (WriterT [NRe] (StateT Int Identity))

writeCL :: NCL -> CLM ()
writeCL cl = do
               tell [cl]

writeRe :: NRe -> CLM ()
writeRe re = do
                lift $ tell [re]

nextLabel :: CLM String 
nextLabel = do
              r <- lift $ lift $ get
              lift $ lift $ put (r+1) 
              return ("rwcomp" ++ show r)

instance forall f . Functor (CLTree f) where
  fmap f (Par ls)        = Par $ fmap (fmap f) ls 
  fmap f (ReFold f1 f2 r) = ReFold f1 f2 (fmap f r) 
  fmap f (Leaf a)         = Leaf (f a)

instance forall f . Foldable (CLTree f) where
  foldMap f (Par ls) = mconcat $ map (foldMap f) ls --) `mappend` (foldMap f r)
  foldMap f (ReFold _ _ r) = foldMap f r
  foldMap f (Leaf a) = f a

instance forall f . Traversable (CLTree f) where
  traverse f (Leaf a)          = Leaf <$> f a 
  traverse f (ReFold f1 f2 re) = ReFold f1 f2 <$> traverse f re
  traverse f (Par ls)          = Par <$> (traverse (traverse f) ls) --(traverse f r)


clExpr :: RWCExp -> CLExp 
clExpr e = case ef of
                RWCVar x _ | x == mkId "par"    -> case args of
                                                      (a1:a2:[]) -> Par $ flattenPars [clExpr $ a1,clExpr $ a2] 
                                                      _ -> error "clExpr: Par wrong # args."
                RWCVar x _ | x == mkId "refold" -> case args of
                                                      (f1:f2:re:[]) -> ReFold f1 f2 (clExpr re)
                                                      _ -> error "clExpr: ReFold wrong # args."
                                                       
                _ -> Leaf e
  where
    (ef:args) = flattenApp e

rnCLExp :: CLExp -> CLM CLNamed
rnCLExp (Leaf re) = do
                      lbl <- nextLabel
                      writeRe (lbl,re)
                      return $ Leaf lbl
rnCLExp (Par ls) = do
                      ls' <- mapM rnCLExp ls
                      lbl <- nextLabel
                      writeCL (lbl,Par ls') 
                      return $ Leaf lbl 
rnCLExp (ReFold f1 f2 r) = do
                             r' <- rnCLExp r
                             lbl <- nextLabel
                             writeCL $ (lbl,ReFold f1 f2 r')
                             return $ Leaf lbl

flattenPars :: [CLExp] -> [CLExp]
flattenPars [] = []
flattenPars ((Par ls):xs) = (flattenPars ls) ++ (flattenPars xs)
flattenPars (x:xs) = [x] ++ (flattenPars xs)

convCLExp :: CLExp -> (CLNamed, [NCL], [NRe])
convCLExp cl = let (((main,labeled),exprs), _) = (runIdentity . 
                                                  (flip runStateT 0) . 
                                                  runWriterT . 
                                                  runWriterT) (rnCLExp cl)
                in (main,labeled,exprs)


flattenCLExp :: RWCExp -> (CLNamed, [NCL], [NRe])
flattenCLExp = convCLExp . clExpr

