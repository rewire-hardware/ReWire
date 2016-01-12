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
variants s = s : (s ++ "'") : (s ++ "''") : [s ++ show n | n <- [0::Integer ..]]

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

pvs :: RWCPat -> [Id RWCExp]
pvs (RWCPatCon _ _ ps)  = concatMap pvs ps
pvs (RWCPatLiteral _ _) = []
pvs (RWCPatVar _ x _)   = [x]

dqPat :: RWCPat -> DQM RWCPat
dqPat (RWCPatCon an dci ps) = do ps' <- mapM dqPat ps
                                 return (RWCPatCon an dci ps')
dqPat (RWCPatLiteral an l)  = return (RWCPatLiteral an l)
dqPat (RWCPatVar an n t)    = do t' <- dqTy t
                                 ism <- askInScopeE
                                 case Map.lookup n ism of
                                   Just n' -> return (RWCPatVar an n' t')
                                   Nothing -> return (RWCPatVar an n t')

dqAlt :: RWCAlt -> DQM RWCAlt
dqAlt (RWCAlt an p e) = do let vs      =  pvs p
                           (_,(p',e')) <- dqingE vs (do
                             p' <- dqPat p
                             e' <- dqExpr e
                             return (p',e'))
                           return (RWCAlt an p' e')

dqExpr :: RWCExp -> DQM RWCExp
dqExpr (RWCApp an e1 e2)      = do e1' <- dqExpr e1
                                   e2' <- dqExpr e2
                                   return (RWCApp an e1' e2')
dqExpr (RWCLam an n t e)      = do t' <- dqTy t
                                   ([n'],e') <- dqingE [n] (dqExpr e)
                                   return (RWCLam an n' t' e')
dqExpr (RWCVar an n t)        = do t' <- dqTy t
                                   ism <- askInScopeE
                                   case Map.lookup n ism of
                                     Just n' -> return (RWCVar an n' t')
                                     Nothing -> return (RWCVar an n t')
dqExpr (RWCCon an dci t)      = do t' <- dqTy t
                                   return (RWCCon an dci t')
dqExpr (RWCLiteral an l)      = return (RWCLiteral an l)
dqExpr (RWCCase an e alts)    = do e'    <- dqExpr e
                                   alts' <- mapM dqAlt alts
                                   return (RWCCase an e' alts')
dqExpr (RWCNativeVHDL an n e) = do e' <- dqExpr e
                                   return (RWCNativeVHDL an n e')

dqDataCon :: RWCDataCon -> DQM RWCDataCon
dqDataCon (RWCDataCon an dci ts) = do ts' <- mapM dqTy ts
                                      return (RWCDataCon an dci ts')

dqDataDecl :: RWCData -> DQM RWCData
dqDataDecl (RWCData an n tvs k dcs) = do (tvs',dcs') <- dqingT tvs (mapM dqDataCon dcs)
                                         return (RWCData an n tvs' k dcs')

dqDefn :: RWCDefn -> DQM RWCDefn
dqDefn (RWCDefn an n (tvs :-> t) b e) = do (tvs',(t',e')) <- dqingT tvs (do
                                             t' <- dqTy t
                                             e' <- dqExpr e
                                             return (t',e'))
                                           return (RWCDefn an n (tvs' :-> t') b e')

dqModule :: RWCProgram -> DQM RWCProgram
dqModule (RWCProgram dds ds) = do dds' <- mapM dqDataDecl dds
                                  ds'  <- mapM dqDefn ds
                                  return (RWCProgram dds' ds')

deUniquify :: RWCProgram -> RWCProgram
deUniquify m = runIdentity (runReaderT (dqModule m) (DQEnv Map.empty Set.empty Map.empty Set.empty))

cmdDeUniquify :: TransCommand
cmdDeUniquify _ m = (Just (deUniquify m),Nothing)
