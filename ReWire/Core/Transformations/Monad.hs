--
-- This is a handy monad class/ transformer with lots of morphisms for stuff
-- you often want to do in ReWire transformations.
--
module ReWire.Core.Transformations.Monad where

import Unbound.LocallyNameless
import Control.Monad.Reader
import Control.Monad.Identity
import ReWire.Core.Syntax

data RWTEnv = RWTEnv { envDefns     :: [RWCDefn],
                       envDataDecls :: [RWCData] }

newtype RWT m a = RWT { deRWT :: ReaderT RWTEnv (LFreshMT m) a }

class Monad m => MonadReWire m where
  askDefns       :: m [RWCDefn]
  askDataDecls   :: m [RWCData]
  localDefns     :: ([RWCDefn] -> [RWCDefn]) -> m a -> m a
  localDataDecls :: ([RWCData] -> [RWCData]) -> m a -> m a

instance MonadTrans RWT where
  lift = RWT . lift . lift

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