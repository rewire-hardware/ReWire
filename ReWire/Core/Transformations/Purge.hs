{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.Purge where

import ReWire.Core.Syntax
import ReWire.Scoping
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.Monad
import Control.Monad.Reader
import Control.Monad.State
--import Control.Monad.Identity
import Data.Set hiding (map,filter,null)
import Data.List hiding (insert)
import ReWire.Core.Transformations.Uniquify (uniquify)
import ReWire.Core.Transformations.DeUniquify (deUniquify)

type IM = StateT (Set (Id RWCExp)) RW

inuseExp :: RWCExp -> IM ()
inuseExp (RWCApp _ e1 e2)      = inuseExp e1 >> inuseExp e2
inuseExp (RWCLam _ _ _ e)      = inuseExp e
inuseExp (RWCVar _ n t)        = do inuse <- get
                                    unless (n `member` inuse) $ do
                                      put (insert n inuse)
                                      me <- lift $ askVar t n
                                      case me of
                                        Just e  -> inuseExp e
                                        Nothing -> return ()
inuseExp RWCCon {}             = return ()
inuseExp RWCLiteral {}         = return ()
inuseExp (RWCCase _ e _ e1 e2) = inuseExp e >> inuseExp e1 >> inuseExp e2
inuseExp RWCNativeVHDL {}      = return ()   -- FIXME(?!): special case here
inuseExp RWCError {}           = return ()

inuseModule :: Id RWCExp -> RWCProgram -> IM (Maybe RWCProgram)
inuseModule n m = do let md = find (\ d -> defnName d == n) (defns m)
                     case md of
                       Nothing                  -> return Nothing
                       Just (RWCDefn _ _ _ _ e) -> do inuse <- get
                                                      put (insert n inuse)
                                                      inuseExp e
                                                      inuse <- get
                                                      let ds' = filter (\ d -> defnName d `member` inuse) (defns m)
                                                      return (Just (m { defns = ds' }))

occursModule :: Id RWCExp -> RWCProgram -> IM [Id RWCExp]
occursModule n m = do let md = find (\ d -> defnName d == n) (defns m)
                      case md of
                        Nothing                  -> return []
                        Just (RWCDefn _ _ _ _ e) -> do inuse <- get
                                                       put (insert n inuse)
                                                       inuseExp e
                                                       inuse <- get
                                                       return (toList inuse)

purge :: Id RWCExp -> RWCProgram -> Maybe RWCProgram
purge n m_ = liftM deUniquify $ fst $ runRW ctr m (runStateT (inuseModule n m) empty)
  where (m,ctr) = uniquify 0 m_

-- FIXME: Not sure why the filter is here...
occurs :: Id RWCExp -> RWCProgram -> [Id RWCExp]
occurs n m_ = filter (not . ('@' `elem`) . show) $ fst $ runRW ctr m (runStateT (occursModule n m) empty)
  where (m,ctr) = uniquify 0 m_

cmdPurge :: TransCommand
cmdPurge arg_ m = (purge (mkId arg) m,Nothing)
  where arg = if null arg_ then "Main.start" else arg_

cmdOccurs :: TransCommand
cmdOccurs arg_ m = (Nothing, Just $ show $ occurs (mkId arg) m)
  where arg = if null arg_ then "Main.start" else arg_
