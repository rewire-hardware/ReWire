module ReWire.Core.Transformations.Purge where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Types
import Unbound.LocallyNameless hiding (empty)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Data.Set hiding (map,filter,null)
import Data.List hiding (insert)

type M = StateT (Set (Name RWCExp)) (ReaderT [RWCDefn] (LFreshMT Identity))

askDefn :: Name RWCExp -> M (Maybe RWCDefn)
askDefn n = do defns  <- ask
               return $ find (\(RWCDefn n' _) -> n==n') defns

inuseAlt :: RWCAlt -> M ()
inuseAlt (RWCAlt b) = lunbind b (\(p,e) -> inuseExp e)

inuseExp :: RWCExp -> M ()
inuseExp (RWCApp _ e1 e2)   = inuseExp e1 >> inuseExp e2
inuseExp (RWCLam _ b)       = lunbind b (\(n,e) -> inuseExp e)
inuseExp (RWCVar _ v)       = do inuse <- get
                                 put (insert v inuse)
                                 md    <- askDefn v
                                 case md of
                                   Just d  -> inuseDefn d
                                   Nothing -> return ()
inuseExp (RWCCon _ _)       = return ()
inuseExp (RWCLiteral _ _)   = return ()
inuseExp (RWCCase _ e alts) = inuseExp e >> mapM_ inuseAlt alts

inuseDefn :: RWCDefn -> M ()
inuseDefn (RWCDefn n (Embed b)) = do inuse <- get
                                     if n `member` inuse
                                        then return ()
                                        else do put (insert n inuse)
                                                lunbind b (\(tvs,(t,e)) -> inuseExp e)

purge :: Name RWCExp -> RWCProg -> M (Maybe RWCProg)
purge n p = do put empty
               ds <- luntrec (defns p)
               avoid (map defnName ds) $ local (const ds) $ do
                 md      <- askDefn n
                 case md of
                   Just d  -> do inuseDefn d
                                 inuse   <- get
                                 let ds' =  filter (\ (RWCDefn n _) -> n `member` inuse) ds
                                 return (Just $ p { defns = trec ds' })
                   Nothing -> return Nothing
   where defnName (RWCDefn n _) = AnyName n

cmdPurge :: TransCommand
cmdPurge arg_ p = (fst $ runIdentity (runLFreshMT (runReaderT (runStateT (purge (s2n arg) p) empty) [])),Nothing)
  where arg = if null arg_ then "main" else arg_