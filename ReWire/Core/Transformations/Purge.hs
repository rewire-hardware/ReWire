{-# LANGUAGE FlexibleContexts #-}

module ReWire.Core.Transformations.Purge where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.Monad
import Unbound.LocallyNameless hiding (empty)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Data.Set hiding (map,filter,null)
import Data.List hiding (insert)
import Debug.Trace (trace)

inuseAlt :: (MonadReWire m,MonadState (Set (Name RWCExp)) m) => RWCAlt -> m ()
inuseAlt (RWCAlt b) = lunbind b (\(p,e) -> inuseExp e)

inuseExp :: (MonadReWire m,MonadState (Set (Name RWCExp)) m) => RWCExp -> m ()
inuseExp (RWCApp _ e1 e2)   = inuseExp e1 >> inuseExp e2
inuseExp (RWCLam _ b)       = lunbind b (\(n,e) -> inuseExp e)
inuseExp (RWCVar _ v)       = do md    <- askDefn v
                                 case md of
                                   Just d  -> inuseDefn d
                                   Nothing -> return ()
inuseExp (RWCCon _ _)       = return ()
inuseExp (RWCLiteral _ _)   = return ()
inuseExp (RWCCase _ e alts) = inuseExp e >> mapM_ inuseAlt alts

inuseDefn :: (MonadReWire m,MonadState (Set (Name RWCExp)) m) => RWCDefn -> m ()
inuseDefn (RWCDefn n (Embed b)) = do inuse <- get
                                     if n `member` inuse
                                        then return ()
                                        else do put (insert n inuse)
                                                lunbind b (\(tvs,(t,e)) -> inuseExp e)

purge :: Name RWCExp -> RWCProg -> Maybe RWCProg
purge n p = fst $
             runRW p $
             runStateT (do put empty
                           md <- askDefn n
                           case md of
                             Just d  -> do inuseDefn d
                                           ds    <- askDefns
                                           inuse <- get
                                           let ds' =  filter (\ (RWCDefn n _) -> n `member` inuse) ds
                                           return (Just $ p { defns = trec ds' })
                             Nothing -> return Nothing) empty
   where defnName (RWCDefn n _) = AnyName n

cmdPurge :: TransCommand
cmdPurge arg_ p = (purge (s2n arg) p,Nothing)
  where arg = if null arg_ then "start" else arg_