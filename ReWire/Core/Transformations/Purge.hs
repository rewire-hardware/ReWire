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
import ReWire.Core.Transformations.Uniquify (uniquify)
import ReWire.Core.Transformations.DeUniquify (deUniquify)

type IM = StateT (Set (Id RWCExp)) RW

inuseAlt :: RWCAlt -> IM ()
inuseAlt (RWCAlt _ e) = inuseExp e

inuseExp :: RWCExp -> IM ()
inuseExp (RWCApp e1 e2)      = inuseExp e1 >> inuseExp e2
inuseExp (RWCLam _ _ e)      = inuseExp e
inuseExp (RWCVar n t)        = do inuse <- get
                                  if n `member` inuse
                                     then return ()
                                     else do
                                       put (insert n inuse)
                                       me  <- lift $ askVar t n
                                       case me of
                                         Just e  -> inuseExp e
                                         Nothing -> return ()
inuseExp (RWCCon _ _)        = return ()
inuseExp (RWCLiteral _)      = return ()
inuseExp (RWCCase e alts)    = inuseExp e >> mapM_ inuseAlt alts
inuseExp (RWCNativeVHDL n _) = return ()   -- FIXME(?!): special case here

inuseModule :: Id RWCExp -> RWCModule -> IM (Maybe RWCModule)
inuseModule n m = do let md = find (\ d -> defnName d == n) (defns m)
                     case md of
                       Nothing              -> return Nothing
                       Just (RWCDefn _ _ _ e) -> do inuse <- get
                                                    put (insert n inuse)
                                                    inuseExp e
                                                    inuse <- get
                                                    let ds' = filter (\ d -> defnName d `member` inuse) (defns m)
                                                    return (Just (m { defns = ds' }))

occursModule :: Id RWCExp -> RWCModule -> IM [Id RWCExp]
occursModule n m = do let md = find (\ d -> defnName d == n) (defns m)
                      case md of
                        Nothing              -> return []
                        Just (RWCDefn _ _ _ e) -> do inuse <- get
                                                     put (insert n inuse)
                                                     inuseExp e
                                                     inuse <- get
                                                     return (toList inuse)

purge :: Id RWCExp -> RWCModule -> Maybe RWCModule
purge n m_ = liftM deUniquify $ fst $ runRW ctr m (runStateT (inuseModule n m) empty)
  where (m,ctr) = uniquify 0 m_

-- FIXME: Not sure why the filter is here...
occurs :: Id RWCExp -> RWCModule -> [Id RWCExp]
occurs n m_ = filter (not . ('@' `elem`) . show) $ fst $ runRW ctr m (runStateT (occursModule n m) empty)
  where (m,ctr) = uniquify 0 m_

cmdPurge :: TransCommand
cmdPurge arg_ m = (purge (mkId arg) m,Nothing)
  where arg = if null arg_ then "Main.start" else arg_

cmdOccurs :: TransCommand
cmdOccurs arg_ m = (Nothing, Just $ show $ occurs (mkId arg) m)
  where arg = if null arg_ then "Main.start" else arg_
