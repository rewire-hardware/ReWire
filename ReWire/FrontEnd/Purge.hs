module ReWire.FrontEnd.Purge
  ( inuseExp
  , inuseModule
  , occursModule
  , purge,occurs
  ) where

import ReWire.FrontEnd.Syntax
import ReWire.FrontEnd.Monad
import ReWire.Scoping

import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative ((<$>))
import Data.Set hiding (map,filter,null)
import Data.List hiding (insert)
import Data.Monoid (mempty)

type IM = StateT (Set (Id RWMExp)) RW

inuseExp :: RWMExp -> IM ()
inuseExp (RWMApp _ e1 e2)      = inuseExp e1 >> inuseExp e2
inuseExp (RWMLam _ _ _ e)      = inuseExp e
inuseExp (RWMVar _ n t)        = do inuse <- get
                                    unless (n `member` inuse) $ do
                                      put (insert n inuse)
                                      me <- lift $ askVar t n
                                      case me of
                                        Just e  -> inuseExp e
                                        Nothing -> return ()
inuseExp RWMCon {}             = return ()
inuseExp RWMLiteral {}         = return ()
inuseExp (RWMCase _ e _ e1 e2) = inuseExp e >> inuseExp e1 >> inuseExp e2
inuseExp RWMNativeVHDL {}      = return ()   -- FIXME(?!): special case here
inuseExp RWMError {}           = return ()

inuseModule :: Id RWMExp -> RWMProgram -> IM RWMProgram
inuseModule n m = do let md = find (\ d -> defnName d == n) (defns m)
                     case md of
                       Nothing                  -> return mempty
                       Just (RWMDefn _ _ _ _ e) -> do inuse <- get
                                                      put (insert n inuse)
                                                      inuseExp e
                                                      inuse <- get
                                                      let ds' = filter (\ d -> defnName d `member` inuse) (defns m)
                                                      return m { defns = ds' }

occursModule :: Id RWMExp -> RWMProgram -> IM [Id RWMExp]
occursModule n m = do let md = find (\ d -> defnName d == n) (defns m)
                      case md of
                        Nothing                  -> return []
                        Just (RWMDefn _ _ _ _ e) -> do inuse <- get
                                                       put (insert n inuse)
                                                       inuseExp e
                                                       inuse <- get
                                                       return (toList inuse)

purge :: Id RWMExp -> RWMProgram -> RW RWMProgram
purge n m = fst <$> runStateT (inuseModule n m) empty

-- FIXME: Not sure why the filter is here...
occurs :: Id RWMExp -> RWMProgram -> RW [Id RWMExp]
occurs n m = do
      m' <- fst <$> runStateT (occursModule n m) empty
      return $ filter (not . ('@' `elem`) . show) m'
