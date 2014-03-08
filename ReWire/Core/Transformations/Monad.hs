{-# LANGUAGE FlexibleInstances,UndecidableInstances,MultiParamTypeClasses #-}

--
-- This is a handy monad class/transformer with lots of morphisms for stuff
-- you often want to do in ReWire transformations.
--
module ReWire.Core.Transformations.Monad where

import Unbound.LocallyNameless
import Unbound.LocallyNameless.Fresh
import Control.Monad.Reader
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.Identity
import ReWire.Core.Syntax
import Control.Monad.State.Class
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Control.Monad.Error
import Data.List (find)

data RWTEnv = RWTEnv { envDefns     :: [RWCDefn],
                       envDataDecls :: [RWCData] }

newtype RWT m a = RWT { deRWT :: ReaderT RWTEnv (LFreshMT m) a }

class (Monad m,LFresh m) => MonadReWire m where
  askDefns       :: m [RWCDefn]
  askDataDecls   :: m [RWCData]
  localDefns     :: ([RWCDefn] -> [RWCDefn]) -> m a -> m a
  localDataDecls :: ([RWCData] -> [RWCData]) -> m a -> m a

instance MonadTrans RWT where
  lift = RWT . lift . lift

instance MonadReWire m => MonadReWire (Lazy.StateT s m) where
  askDefns       = lift askDefns
  askDataDecls   = lift askDataDecls
  localDefns     = Lazy.mapStateT . localDefns
  localDataDecls = Lazy.mapStateT . localDataDecls

instance MonadReWire m => MonadReWire (Strict.StateT s m) where
  askDefns       = lift askDefns
  askDataDecls   = lift askDataDecls
  localDefns     = Strict.mapStateT . localDefns
  localDataDecls = Strict.mapStateT . localDataDecls

instance MonadState s m => MonadState s (RWT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance (Error e, MonadReWire m) => MonadReWire (ErrorT e m) where
    askDefns = lift askDefns
    localDefns = mapErrorT . localDefns
    askDataDecls = lift askDataDecls
    localDataDecls = mapErrorT . localDataDecls
    
instance MonadError e m => MonadError e (RWT m) where
    throwError = lift . throwError
    catchError m h = RWT $ catchError (deRWT m) (deRWT . h)

instance Monad m => Monad (RWT m) where
  return x = RWT (return x)
  m >>= f  = RWT (deRWT m >>= deRWT . f)

instance Monad m => MonadReWire (RWT m) where
  askDefns             = RWT (ask >>= return . envDefns)
  askDataDecls         = RWT (ask >>= return . envDataDecls)
  localDefns f phi     = RWT (local (\ env -> env { envDefns = f (envDefns env) }) (deRWT phi))
  localDataDecls f phi = RWT (local (\ env -> env { envDataDecls = f (envDataDecls env) }) (deRWT phi))

instance Monad m => LFresh (RWT m) where
  lfresh n     = RWT (lfresh n)
  avoid ns phi = RWT (avoid ns (deRWT phi))
  getAvoids    = RWT getAvoids

runRWT :: Monad m => RWCProg -> RWT m a -> m a
runRWT p phi = runLFreshMT $ runReaderT (deRWT $ do ds <- luntrec (defns p)
                                                    avoid (map defnName ds) $
                                                     localDefns (const ds) $
                                                       localDataDecls (const (dataDecls p)) $ 
                                                       phi) undefEnv
  where defnName (RWCDefn n _) = AnyName n
        undefEnv = RWTEnv { envDefns = error "RWT: no definitions",
                            envDataDecls = error "RWT: no data declarations" }

type RW = RWT Identity

runRW :: RWCProg -> RW a -> a
runRW p = runIdentity . runRWT p

askvar :: MonadReWire m => RWCTy -> Name RWCExp -> m RWCExp
askvar t n = do ds <- askDefns
                case find (\ (RWCDefn n' _) -> n == n') ds of
                  Just (RWCDefn _ (Embed b)) -> lunbind b (\(tvs,(t',e)) ->
                                                 do sub <- matchty [] t' t
                                                    return (substs sub e))
                  _                          -> return (RWCVar t n)

askDefn :: MonadReWire m => Name RWCExp -> m (Maybe RWCDefn)
askDefn n = do defns <- askDefns
               return $ find (\(RWCDefn n' _) -> n==n') defns

-- FIXME: begin stuff that should maybe be moved to a separate module
mergesubs :: Monad m => [(Name RWCTy,RWCTy)] -> [(Name RWCTy,RWCTy)] -> m [(Name RWCTy,RWCTy)]
mergesubs ((n,t):sub) sub' = case lookup n sub' of
                               Just t' -> if t `aeq` t' then mergesubs sub sub'
                                                        else fail "mergesubs failed"
                               Nothing -> do sub'' <- mergesubs sub sub'
                                             return ((n,t):sub'')
mergesubs [] sub'          = return sub'

matchty :: Monad m => [(Name RWCTy,RWCTy)] -> RWCTy -> RWCTy -> m [(Name RWCTy,RWCTy)]
matchty sub (RWCTyVar n) t                         = case lookup n sub of
                                                       Nothing -> return ((n,t):sub)
                                                       Just t' -> if t `aeq` t' then return sub
                                                                                else fail "matchty failed (variable inconsistency)"
matchty sub (RWCTyCon i1) (RWCTyCon i2) | i1 == i2 = return sub
matchty sub (RWCTyApp t1 t2) (RWCTyApp t1' t2')    = do sub1 <- matchty [] t1 t1'
                                                        sub2 <- matchty [] t2 t2'
                                                        mergesubs sub1 sub2
matchty _ t1 t2                                    = fail $ "matchty failed (constructor head): " ++ show t1 ++ ", " ++ show t2
-- FIXME: end stuff that should maybe be moved to a separate module