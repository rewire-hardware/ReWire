{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.Uniquify where

import ReWire.Core.Syntax
import ReWire.Scoping
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as BS
import ReWire.Core.Transformations.Types

type UQM = ReaderT (Map (Id RWCExp) (Id RWCExp)) (ReaderT (Map (Id RWCTy) (Id RWCTy)) (StateT Int Identity))

base32 :: Int -> String
base32 0 = "0"
base32 n = dropWhile (=='0') (reverse $ b32 n)

b32 :: Int -> String
b32 0 = "0"
b32 n = let (q,r) = quotRem n 32
            sq    = b32 q
            c     = (['0'..'9']++['A'..]) !! r
        in  c:sq

askT :: UQM (Map (Id RWCTy) (Id RWCTy))
askT = lift ask

localT :: (Map (Id RWCTy) (Id RWCTy) -> Map (Id RWCTy) (Id RWCTy)) -> UQM a -> UQM a
localT f m = ReaderT $ \ rhoE -> local f (runReaderT m rhoE)

askE :: UQM (Map (Id RWCExp) (Id RWCExp))
askE = ask

localE :: (Map (Id RWCExp) (Id RWCExp) -> Map (Id RWCExp) (Id RWCExp)) -> UQM a -> UQM a
localE = local

askUniqueT :: Id RWCTy -> UQM (Maybe (Id RWCTy))
askUniqueT n = do m <- askT
                  return (Map.lookup n m)

askUniqueE :: Id RWCExp -> UQM (Maybe (Id RWCExp))
askUniqueE n = do m <- askE
                  return (Map.lookup n m)

fresh :: Id a -> UQM (Id a)
fresh (Id s x) = do n <- get
                    put (n+1)
                    return (Id s (BS.append x (BS.pack ("@" ++ base32 n))))

uniquingT :: [Id RWCTy] -> UQM t -> UQM ([Id RWCTy],t)
uniquingT ns m = do ns'    <- mapM fresh ns
                    let bs =  Map.fromList (zip ns ns')
                    v      <- localT (Map.union bs) m
                    return (ns',v)

uniquingE :: [Id RWCExp] -> UQM t -> UQM ([Id RWCExp],t)
uniquingE ns m = do ns'    <- mapM fresh ns
                    let bs =  Map.fromList (zip ns ns')
                    v      <- localE (Map.union bs) m
                    return (ns',v)

uniquifyTy :: RWCTy -> UQM RWCTy
uniquifyTy (RWCTyApp t1 t2)  = do t1' <- uniquifyTy t1
                                  t2' <- uniquifyTy t2
                                  return (RWCTyApp t1' t2')
uniquifyTy (RWCTyCon i)      = return (RWCTyCon i)
uniquifyTy (RWCTyVar n)      = do mn <- askUniqueT n
                                  case mn of
                                    Nothing -> return (RWCTyVar n)
                                    Just n' -> return (RWCTyVar n')
uniquifyTy (RWCTyComp t1 t2) = do t1' <- uniquifyTy t1
                                  t2' <- uniquifyTy t2
                                  return (RWCTyComp t1' t2')

uniquifyDataCon :: RWCDataCon -> UQM RWCDataCon
uniquifyDataCon (RWCDataCon dci ts) = do ts' <- mapM uniquifyTy ts
                                         return (RWCDataCon dci ts')

uniquifyDataDecl :: RWCData -> UQM RWCData
uniquifyDataDecl (RWCData i vs k dcs) = do (vs',dcs') <- uniquingT vs $
                                             mapM uniquifyDataCon dcs
                                           return (RWCData i vs' k dcs')

pvs :: RWCPat -> [Id RWCExp]
pvs (RWCPatCon _ ps)  = concatMap pvs ps
pvs (RWCPatLiteral _) = []
pvs (RWCPatVar x _)   = [x]

uniquifyPat :: RWCPat -> UQM RWCPat
uniquifyPat (RWCPatCon dci ps) = do ps' <- mapM uniquifyPat ps
                                    return (RWCPatCon dci ps')
uniquifyPat (RWCPatLiteral l)  = return (RWCPatLiteral l)
uniquifyPat (RWCPatVar n t)    = do t' <- uniquifyTy t
                                    mn <- askUniqueE n
                                    case mn of
                                      Just n' -> return (RWCPatVar n' t')
                                      Nothing -> return (RWCPatVar n t') -- shouldn't happen

uniquifyAlt :: RWCAlt -> UQM RWCAlt
uniquifyAlt (RWCAlt p e) = do let vs      =  pvs p
                              (_,(p',e')) <- uniquingE vs (do
                                p' <- uniquifyPat p
                                e' <- uniquifyExpr e
                                return (p',e'))
                              return (RWCAlt p' e')

uniquifyExpr :: RWCExp -> UQM RWCExp
uniquifyExpr (RWCApp e1 e2)      = do e1' <- uniquifyExpr e1
                                      e2' <- uniquifyExpr e2
                                      return (RWCApp e1' e2')
uniquifyExpr (RWCLam n t e)      = do t'        <- uniquifyTy t
                                      ([n'],e') <- uniquingE [n] (uniquifyExpr e)
                                      return (RWCLam n' t' e')
uniquifyExpr (RWCVar n t)        = do t' <- uniquifyTy t
                                      mn <- askUniqueE n
                                      case mn of
                                        Nothing -> return (RWCVar n t')
                                        Just n' -> return (RWCVar n' t')
uniquifyExpr (RWCCon dci t)      = do t' <- uniquifyTy t
                                      return (RWCCon dci t')
uniquifyExpr (RWCLiteral l)      = return (RWCLiteral l)
uniquifyExpr (RWCCase e alts)    = do e'    <- uniquifyExpr e
                                      alts' <- mapM uniquifyAlt alts
                                      return (RWCCase e' alts')
uniquifyExpr (RWCNativeVHDL n e) = do e' <- uniquifyExpr e
                                      return (RWCNativeVHDL n e')

uniquifyDefn :: RWCDefn -> UQM RWCDefn
uniquifyDefn (RWCDefn n (tvs :-> t) b e) = do (tvs',(t',e')) <- uniquingT tvs (do
                                                t' <- uniquifyTy t
                                                e' <- uniquifyExpr e
                                                return (t',e'))
                                              return (RWCDefn n (tvs' :-> t') b e')

uniquifyModule :: RWCProgram -> UQM RWCProgram
uniquifyModule (RWCProgram dds ds) = do dds' <- mapM uniquifyDataDecl dds
                                        ds'  <- mapM uniquifyDefn ds
                                        return (RWCProgram dds' ds')

uniquifyE :: Int -> RWCExp -> (RWCExp,Int)
uniquifyE ctr e = runIdentity $ runStateT (runReaderT (runReaderT (uniquifyExpr e) Map.empty) Map.empty) ctr

uniquify :: Int -> RWCProgram -> (RWCProgram,Int)
uniquify ctr m = runIdentity $ runStateT (runReaderT (runReaderT (uniquifyModule m) Map.empty) Map.empty) ctr

cmdUniquify :: TransCommand
cmdUniquify _ m = (Just (fst $ uniquify 0 m),Nothing)
