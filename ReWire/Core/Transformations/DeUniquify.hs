{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.DeUniquify where

import ReWire.Core.Syntax
import ReWire.Scoping
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as BS
import Data.List (find)
import Data.Maybe (fromJust)
import ReWire.Core.Transformations.Types

data DQEnv = DQEnv { inScopeE   :: Map (Id RWCExp) (Id RWCExp),
                     allocatedE :: Set (Id RWCExp),
                     inScopeT   :: Map (Id RWCTy) (Id RWCTy),
                     allocatedT :: Set (Id RWCTy) }

type DQM = ReaderT DQEnv Identity

askInScopeE :: DQM (Map (Id RWCExp) (Id RWCExp))
askInScopeE = return . inScopeE =<< ask

askAllocatedE :: DQM (Set (Id RWCExp))
askAllocatedE = return . allocatedE =<< ask

askInScopeT :: DQM (Map (Id RWCTy) (Id RWCTy))
askInScopeT = return . inScopeT =<< ask

askAllocatedT :: DQM (Set (Id RWCTy))
askAllocatedT = return . allocatedT =<< ask

localInScopeE :: (Map (Id RWCExp) (Id RWCExp) -> Map (Id RWCExp) (Id RWCExp)) -> DQM a -> DQM a
localInScopeE f = local (\ e -> e { inScopeE = f (inScopeE e) })

localAllocatedE :: (Set (Id RWCExp) -> Set (Id RWCExp)) -> DQM a -> DQM a
localAllocatedE f = local (\ e -> e { allocatedE = f (allocatedE e) })

localInScopeT :: (Map (Id RWCTy) (Id RWCTy) -> Map (Id RWCTy) (Id RWCTy)) -> DQM a -> DQM a
localInScopeT f = local (\ e -> e { inScopeT = f (inScopeT e) })

localAllocatedT :: (Set (Id RWCTy) -> Set (Id RWCTy)) -> DQM a -> DQM a
localAllocatedT f = local (\ e -> e { allocatedT = f (allocatedT e) })

variants :: String -> [String]
variants s = s : (s ++ "'") : (s ++ "''") : [s ++ show n | n <- [0..]]

pretty :: String -> String
pretty = takeWhile (/='@')

dqvT :: Id RWCTy -> DQM (Id RWCTy)
dqvT (Id s x) = do alloced <- askAllocatedT
                   let xs  =  map (Id s . BS.pack) $ variants (pretty (BS.unpack x))
                       x'  =  fromJust $ find (not . (`Set.member` alloced)) xs
                   return x'
                   
dqvE :: Id RWCExp -> DQM (Id RWCExp)
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

dqingE :: [Id RWCExp] -> DQM a -> DQM ([Id RWCExp],a)
dqingE ns m = do ns'    <- mapM dqvE ns
                 let bs =  Map.fromList (zip ns ns')
                 v      <- localAllocatedE (Set.fromList ns' `Set.union`) $
                            localInScopeE (bs `Map.union`) m
                 return (ns',v)

dqTy (RWCTyApp t1 t2)  = do t1' <- dqTy t1
                            t2' <- dqTy t2
                            return (RWCTyApp t1' t2')
dqTy (RWCTyCon tci)    = return (RWCTyCon tci)
dqTy (RWCTyVar n)      = do ism <- askInScopeT
                            case Map.lookup n ism of
                              Just n' -> return (RWCTyVar n')
                              Nothing -> return (RWCTyVar n)
dqTy (RWCTyComp t1 t2) = do t1' <- dqTy t1
                            t2' <- dqTy t2
                            return (RWCTyComp t1' t2')

pvs (RWCPatCon dci ps) = concatMap pvs ps
pvs (RWCPatLiteral l)  = []
pvs (RWCPatVar x t)    = [x]
pvs RWCPatWild         = []

dqPat (RWCPatCon dci ps) = do ps' <- mapM dqPat ps
                              return (RWCPatCon dci ps')
dqPat (RWCPatLiteral l)  = return (RWCPatLiteral l)
dqPat (RWCPatVar n t)    = do t' <- dqTy t
                              ism <- askInScopeE
                              case Map.lookup n ism of
                                Just n' -> return (RWCPatVar n' t')
                                Nothing -> return (RWCPatVar n t')
dqPat RWCPatWild         = return RWCPatWild

dqAlt (RWCAlt p e) = do let vs      =  pvs p
                        (_,(p',e')) <- dqingE vs (do
                          p' <- dqPat p
                          e' <- dqExpr e
                          return (p',e'))
                        return (RWCAlt p' e')
                           
dqExpr (RWCApp e1 e2) = do e1' <- dqExpr e1
                           e2' <- dqExpr e2
                           return (RWCApp e1' e2')
dqExpr (RWCLam n t e) = do t' <- dqTy t
                           ([n'],e') <- dqingE [n] (dqExpr e)
                           return (RWCLam n' t' e')
dqExpr (RWCLet n e1 e2) = do e1' <- dqExpr e1
                             ([n'],e2') <- dqingE [n] (dqExpr e2)
                             return (RWCLet n' e1' e2')
dqExpr (RWCVar n t)     = do t' <- dqTy t
                             ism <- askInScopeE
                             case Map.lookup n ism of
                               Just n' -> return (RWCVar n' t')
                               Nothing -> return (RWCVar n t')
dqExpr (RWCCon dci t)   = do t' <- dqTy t
                             return (RWCCon dci t')
dqExpr (RWCLiteral l)   = return (RWCLiteral l)
dqExpr (RWCCase e alts) = do e'    <- dqExpr e
                             alts' <- mapM dqAlt alts
                             return (RWCCase e' alts')

dqDataCon (RWCDataCon dci ts) = do ts' <- mapM dqTy ts
                                   return (RWCDataCon dci ts')

dqDataDecl (RWCData n tvs dcs) = do (tvs',dcs') <- dqingT tvs (mapM dqDataCon dcs)
                                    return (RWCData n tvs' dcs')

dqPrimDecl = return

dqDefn (RWCDefn n (tvs :-> t) e) = do (tvs',(t',e')) <- dqingT tvs (do
                                        t' <- dqTy t
                                        e' <- dqExpr e
                                        return (t',e'))
                                      return (RWCDefn n (tvs' :-> t') e')

dqProg :: RWCProg -> DQM RWCProg
dqProg (RWCProg dds pds ds) = do dds' <- mapM dqDataDecl dds
                                 pds' <- mapM dqPrimDecl pds
                                 ds'  <- mapM dqDefn ds
                                 return (RWCProg dds' pds' ds')

deUniquify :: RWCProg -> RWCProg
deUniquify p = runIdentity (runReaderT (dqProg p) (DQEnv Map.empty Set.empty Map.empty Set.empty))

cmdDeUniquify :: TransCommand
cmdDeUniquify _ p = (Just (deUniquify p),Nothing)
