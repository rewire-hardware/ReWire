{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.Purge where

import ReWire.Core.Syntax
import ReWire.Scoping
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Data.Set hiding (map,filter,null)
import Data.List hiding (insert)
import Debug.Trace (trace)
import ReWire.Core.Transformations.Uniquify (uniquify)
import ReWire.Core.Transformations.DeUniquify (deUniquify)

type IM = StateT (Set (Id RWCExp)) RW

inuseAlt :: RWCAlt -> IM ()
inuseAlt (RWCAlt _ e) = inuseExp e

inuseExp :: RWCExp -> IM ()
inuseExp (RWCApp e1 e2)     = inuseExp e1 >> inuseExp e2
inuseExp (RWCLam _ _ e)     = inuseExp e
inuseExp (RWCLet _ el eb)   = inuseExp el >> inuseExp eb
inuseExp (RWCVar n t)       = do inuse <- get
                                 if n `member` inuse
                                    then return ()
                                    else do
                                      put (insert n inuse)
                                      me  <- lift $ askVar t n
                                      case me of
                                        Just e  -> inuseExp e
                                        Nothing -> return ()
inuseExp (RWCCon _ _)       = return ()
inuseExp (RWCLiteral _)     = return ()
inuseExp (RWCCase e alts)   = inuseExp e >> mapM_ inuseAlt alts

inuseProg :: Id RWCExp -> RWCProg -> IM (Maybe RWCProg)
inuseProg n p = do let md = find (\ d -> defnName d == n) (defns p)
                   case md of
                     Nothing              -> return Nothing
                     Just (RWCDefn _ _ e) -> do inuse <- get
                                                put (insert n inuse)
                                                inuseExp e
                                                inuse <- get
                                                let ds' = filter (\ d -> defnName d `member` inuse) (defns p)
                                                return (Just (p { defns = ds' }))

occursProg :: Id RWCExp -> RWCProg -> IM [Id RWCExp]
occursProg n p = do let md = find (\ d -> defnName d == n) (defns p)
                    case md of
                      Nothing              -> return []
                      Just (RWCDefn _ _ e) -> do inuse <- get
                                                 put (insert n inuse)
                                                 inuseExp e
                                                 inuse <- get
                                                 return (toList inuse)

purge :: Id RWCExp -> RWCProg -> Maybe RWCProg
purge n p_ = liftM deUniquify $ fst $ runRW ctr p (runStateT (inuseProg n p) empty)
  where (p,ctr) = uniquify 0 p_

occurs :: Id RWCExp -> RWCProg -> [Id RWCExp]
occurs n p_ = filter (not . ('@' `elem`) . show) $ fst $ runRW ctr p (runStateT (occursProg n p) empty)
  where (p,ctr) = uniquify 0 p_

cmdPurge :: TransCommand
cmdPurge arg_ p = (purge (mkId arg) p,Nothing)
  where arg = if null arg_ then "start" else arg_

cmdOccurs :: TransCommand
cmdOccurs arg_ p = (Nothing, Just $ show $ occurs (mkId arg) p)
  where arg = if null arg_ then "start" else arg_
