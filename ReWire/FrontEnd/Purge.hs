{-# LANGUAGE LambdaCase #-}
module ReWire.FrontEnd.Purge (purge) where

import ReWire.FrontEnd.Syntax
import ReWire.FrontEnd.Monad
import ReWire.Scoping

import Control.Monad.State (StateT (..), put, get)
import Control.Monad.Trans (lift)
import Control.Monad (unless)
import Data.List (find)
import Data.Set (Set, insert, member)

type IM m = StateT (Set (Id RWMExp)) (RWT m)

inuseExp :: Monad m => RWMExp -> IM m ()
inuseExp = \ case
      RWMApp _ e1 e2      -> inuseExp e1 >> inuseExp e2
      RWMLam _ _ _ e      -> inuseExp e
      RWMVar _ n t        -> do
            inuse <- get
            unless (n `member` inuse) $ do
                  put $ insert n inuse
                  me <- lift $ askVar t n
                  case me of
                        Just e  -> inuseExp e
                        Nothing -> return ()
      RWMCon {}           -> return ()
      RWMCase _ e _ e1 e2 -> inuseExp e >> inuseExp e1 >> inuseExp e2
      RWMNativeVHDL {}    -> return ()   -- FIXME(?!): special case here
      RWMError {}         -> return ()

inuseProgram :: Monad m => Id RWMExp -> RWMProgram -> IM m RWMProgram
inuseProgram n m = do
      let md = find (\ d -> defnName d == n) $ defns m
      case md of
            Nothing                    -> return mempty
            Just (RWMDefn _ _ _ _ _ e) -> do
                  inuse <- get
                  put $ insert n inuse
                  inuseExp e
                  inuse <- get
                  let ds' = filter (\ d -> defnName d `member` inuse) (defns m)
                  return m { defns = ds' }

purge :: Monad m => Id RWMExp -> RWMProgram -> RWT m RWMProgram
purge n m = fst <$> runStateT (inuseProgram n m) mempty

