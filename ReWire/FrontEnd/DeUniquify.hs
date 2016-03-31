module ReWire.FrontEnd.DeUniquify (deUniquify) where

import ReWire.FrontEnd.Syntax
import ReWire.Scoping

import Control.Monad.Identity
import Control.Monad.Reader
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.Set (Set)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as BS

data DQEnv = DQEnv { inScopeE   :: Map (Id RWMExp) (Id RWMExp),
                     allocatedE :: Set (Id RWMExp),
                     inScopeT   :: Map (Id RWCTy) (Id RWCTy),
                     allocatedT :: Set (Id RWCTy) }

type DQM = ReaderT DQEnv Identity

askInScopeE :: DQM (Map (Id RWMExp) (Id RWMExp))
askInScopeE = return . inScopeE =<< ask

askAllocatedE :: DQM (Set (Id RWMExp))
askAllocatedE = return . allocatedE =<< ask

askInScopeT :: DQM (Map (Id RWCTy) (Id RWCTy))
askInScopeT = return . inScopeT =<< ask

askAllocatedT :: DQM (Set (Id RWCTy))
askAllocatedT = return . allocatedT =<< ask

localInScopeE :: (Map (Id RWMExp) (Id RWMExp) -> Map (Id RWMExp) (Id RWMExp)) -> DQM a -> DQM a
localInScopeE f = local (\ e -> e { inScopeE = f (inScopeE e) })

localAllocatedE :: (Set (Id RWMExp) -> Set (Id RWMExp)) -> DQM a -> DQM a
localAllocatedE f = local (\ e -> e { allocatedE = f (allocatedE e) })

localInScopeT :: (Map (Id RWCTy) (Id RWCTy) -> Map (Id RWCTy) (Id RWCTy)) -> DQM a -> DQM a
localInScopeT f = local (\ e -> e { inScopeT = f (inScopeT e) })

localAllocatedT :: (Set (Id RWCTy) -> Set (Id RWCTy)) -> DQM a -> DQM a
localAllocatedT f = local (\ e -> e { allocatedT = f (allocatedT e) })

variants :: String -> [String]
variants s = s : (s ++ "'") : (s ++ "''") : [s ++ show n | n <- [0::Integer ..]]

pretty :: String -> String
pretty = takeWhile (/='@')

dqvT :: Id RWCTy -> DQM (Id RWCTy)
dqvT (Id s x) = do alloced <- askAllocatedT
                   let xs  =  map (Id s . BS.pack) $ variants (pretty (BS.unpack x))
                       x'  =  fromJust $ find (not . (`Set.member` alloced)) xs
                   return x'

dqvE :: Id RWMExp -> DQM (Id RWMExp)
dqvE (Id s x) = do alloced <- askAllocatedE
                   let xs  =  map (Id s . BS.pack) $ variants (pretty (BS.unpack x))
                       x'  =  fromJust $ find (not . (`Set.member` alloced)) xs
                   return x'

dqingT :: [Id RWCTy] -> DQM a -> DQM ([Id RWCTy],a)
dqingT ns m = do ns'    <- mapM dqvT ns
                 let bs =  Map.fromList (zip ns ns')
                 v      <- localAllocatedT (Set.fromList ns' `Set.union`) $
                            localInScopeT (bs `Map.union`) m
                 return (ns',v)

dqingE :: [Id RWMExp] -> DQM a -> DQM ([Id RWMExp],a)
dqingE ns m = do ns'    <- mapM dqvE ns
                 let bs =  Map.fromList (zip ns ns')
                 v      <- localAllocatedE (Set.fromList ns' `Set.union`) $
                            localInScopeE (bs `Map.union`) m
                 return (ns',v)

dqTy :: RWCTy -> DQM RWCTy
dqTy (RWCTyApp an t1 t2)  = do t1' <- dqTy t1
                               t2' <- dqTy t2
                               return (RWCTyApp an t1' t2')
dqTy (RWCTyCon an tci)    = return (RWCTyCon an tci)
dqTy (RWCTyVar an n)      = do ism <- askInScopeT
                               case Map.lookup n ism of
                                 Just n' -> return (RWCTyVar an n')
                                 Nothing -> return (RWCTyVar an n)
dqTy (RWCTyComp an t1 t2) = do t1' <- dqTy t1
                               t2' <- dqTy t2
                               return (RWCTyComp an t1' t2')

pvs :: RWMPat -> [Id RWMExp]
pvs (RWMPatCon _ _ ps)  = concatMap pvs ps
pvs (RWMPatLiteral _ _) = []
pvs (RWMPatVar _ x _)   = [x]

dqPat :: RWMPat -> DQM RWMPat
dqPat (RWMPatCon an dci ps) = do ps' <- mapM dqPat ps
                                 return (RWMPatCon an dci ps')
dqPat (RWMPatLiteral an l)  = return (RWMPatLiteral an l)
dqPat (RWMPatVar an n t)    = do t' <- dqTy t
                                 ism <- askInScopeE
                                 case Map.lookup n ism of
                                   Just n' -> return (RWMPatVar an n' t')
                                   Nothing -> return (RWMPatVar an n t')

dqExpr :: RWMExp -> DQM RWMExp
dqExpr (RWMApp an e1 e2)      = do e1' <- dqExpr e1
                                   e2' <- dqExpr e2
                                   return (RWMApp an e1' e2')
dqExpr (RWMLam an n t e)      = do t' <- dqTy t
                                   ([n'],e') <- dqingE [n] (dqExpr e)
                                   return (RWMLam an n' t' e')
dqExpr (RWMVar an n t)        = do t' <- dqTy t
                                   ism <- askInScopeE
                                   case Map.lookup n ism of
                                     Just n' -> return (RWMVar an n' t')
                                     Nothing -> return (RWMVar an n t')
dqExpr (RWMCon an dci t)      = do t' <- dqTy t
                                   return (RWMCon an dci t')
dqExpr (RWMLiteral an l)      = return (RWMLiteral an l)
dqExpr (RWMCase an e p e1 e2) = do e'    <- dqExpr e
                                   let vs = pvs p
                                   (_,(p',e1')) <- dqingE vs (do
                                     p'  <- dqPat p
                                     e1' <- dqExpr e1
                                     return (p',e1'))
                                   e2' <- dqExpr e2
                                   return (RWMCase an e' p' e1' e2')

dqExpr (RWMNativeVHDL an n e) = do e' <- dqExpr e
                                   return (RWMNativeVHDL an n e')
dqExpr (RWMError an m t)      = do t' <- dqTy t
                                   return (RWMError an m t')

dqDataCon :: RWCDataCon -> DQM RWCDataCon
dqDataCon (RWCDataCon an dci ts) = do ts' <- mapM dqTy ts
                                      return (RWCDataCon an dci ts')

dqDataDecl :: RWMData -> DQM RWMData
dqDataDecl (RWMData an n tvs k dcs) = do (tvs',dcs') <- dqingT tvs (mapM dqDataCon dcs)
                                         return (RWMData an n tvs' k dcs')

dqDefn :: RWMDefn -> DQM RWMDefn
dqDefn (RWMDefn an n (tvs :-> t) b e) = do (tvs',(t',e')) <- dqingT tvs (do
                                             t' <- dqTy t
                                             e' <- dqExpr e
                                             return (t',e'))
                                           return (RWMDefn an n (tvs' :-> t') b e')

dqModule :: RWMProgram -> DQM RWMProgram
dqModule (RWMProgram dds ds) = do dds' <- mapM dqDataDecl dds
                                  ds'  <- mapM dqDefn ds
                                  return (RWMProgram dds' ds')

deUniquify :: RWMProgram -> RWMProgram
deUniquify m = runIdentity (runReaderT (dqModule m) (DQEnv Map.empty Set.empty Map.empty Set.empty))
