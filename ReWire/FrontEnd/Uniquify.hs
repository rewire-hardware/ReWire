module ReWire.FrontEnd.Uniquify
  ( base32,b32
  , fresh
  , pvs
  , askT,localT
  , askE,localE
  , askUniqueT,askUniqueE
  , uniquingT,uniquingE
  , uniquifyTy
  , uniquifyE
  , uniquifyDataCon
  , uniquifyDataDecl
  , uniquifyPat
  , uniquifyExpr
  , uniquifyDefn
  , uniquifyModule
  , uniquify
  ) where

import ReWire.FrontEnd.Syntax
import ReWire.Scoping

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as BS

type UQM = ReaderT (Map (Id RWMExp) (Id RWMExp)) (ReaderT (Map (Id RWCTy) (Id RWCTy)) (StateT Int Identity))

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

askE :: UQM (Map (Id RWMExp) (Id RWMExp))
askE = ask

localE :: (Map (Id RWMExp) (Id RWMExp) -> Map (Id RWMExp) (Id RWMExp)) -> UQM a -> UQM a
localE = local

askUniqueT :: Id RWCTy -> UQM (Maybe (Id RWCTy))
askUniqueT n = do m <- askT
                  return (Map.lookup n m)

askUniqueE :: Id RWMExp -> UQM (Maybe (Id RWMExp))
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

uniquingE :: [Id RWMExp] -> UQM t -> UQM ([Id RWMExp],t)
uniquingE ns m = do ns'    <- mapM fresh ns
                    let bs =  Map.fromList (zip ns ns')
                    v      <- localE (Map.union bs) m
                    return (ns',v)

uniquifyTy :: RWCTy -> UQM RWCTy
uniquifyTy (RWCTyApp an t1 t2)  = do t1' <- uniquifyTy t1
                                     t2' <- uniquifyTy t2
                                     return (RWCTyApp an t1' t2')
uniquifyTy (RWCTyCon an i)      = return (RWCTyCon an i)
uniquifyTy (RWCTyVar an n)      = do mn <- askUniqueT n
                                     case mn of
                                       Nothing -> return (RWCTyVar an n)
                                       Just n' -> return (RWCTyVar an n')
uniquifyTy (RWCTyComp an t1 t2) = do t1' <- uniquifyTy t1
                                     t2' <- uniquifyTy t2
                                     return (RWCTyComp an t1' t2')

uniquifyDataCon :: RWCDataCon -> UQM RWCDataCon
uniquifyDataCon (RWCDataCon an dci ts) = do ts' <- mapM uniquifyTy ts
                                            return (RWCDataCon an dci ts')

uniquifyDataDecl :: RWMData -> UQM RWMData
uniquifyDataDecl (RWMData an i vs k dcs) = do (vs',dcs') <- uniquingT vs $ mapM uniquifyDataCon dcs
                                              return (RWMData an i vs' k dcs')

pvs :: RWMPat -> [Id RWMExp]
pvs (RWMPatCon _ _ ps)  = concatMap pvs ps
pvs (RWMPatLiteral _ _) = []
pvs (RWMPatVar _ x _)   = [x]

uniquifyPat :: RWMPat -> UQM RWMPat
uniquifyPat (RWMPatCon an dci ps) = do ps' <- mapM uniquifyPat ps
                                       return (RWMPatCon an dci ps')
uniquifyPat (RWMPatLiteral an l)  = return (RWMPatLiteral an l)
uniquifyPat (RWMPatVar an n t)    = do t' <- uniquifyTy t
                                       mn <- askUniqueE n
                                       case mn of
                                         Just n' -> return (RWMPatVar an n' t')
                                         Nothing -> return (RWMPatVar an n t') -- shouldn't happen


uniquifyExpr :: RWMExp -> UQM RWMExp
uniquifyExpr (RWMApp an e1 e2)      = do e1' <- uniquifyExpr e1
                                         e2' <- uniquifyExpr e2
                                         return (RWMApp an e1' e2')
uniquifyExpr (RWMLam an n t e)      = do t'        <- uniquifyTy t
                                         ([n'],e') <- uniquingE [n] (uniquifyExpr e)
                                         return (RWMLam an n' t' e')
uniquifyExpr (RWMVar an n t)        = do t' <- uniquifyTy t
                                         mn <- askUniqueE n
                                         case mn of
                                           Nothing -> return (RWMVar an n t')
                                           Just n' -> return (RWMVar an n' t')
uniquifyExpr (RWMCon an dci t)      = do t' <- uniquifyTy t
                                         return (RWMCon an dci t')
uniquifyExpr (RWMLiteral an l)      = return (RWMLiteral an l)
uniquifyExpr (RWMCase an e p e1 e2) = do e'    <- uniquifyExpr e
                                         let vs = pvs p
                                         (_,(p',e1')) <- uniquingE vs (do
                                           p' <- uniquifyPat p
                                           e1' <- uniquifyExpr e1
                                           return (p',e1'))
                                         e2' <- uniquifyExpr e2
                                         return (RWMCase an e' p' e1' e2')
uniquifyExpr (RWMNativeVHDL an n e) = do e' <- uniquifyExpr e
                                         return (RWMNativeVHDL an n e')
uniquifyExpr (RWMError an m t)      = do t' <- uniquifyTy t
                                         return (RWMError an m t')

uniquifyDefn :: RWMDefn -> UQM RWMDefn
uniquifyDefn (RWMDefn an n (tvs :-> t) b e) = do (tvs',(t',e')) <- uniquingT tvs (do
                                                   t' <- uniquifyTy t
                                                   e' <- uniquifyExpr e
                                                   return (t',e'))
                                                 return (RWMDefn an n (tvs' :-> t') b e')

uniquifyModule :: RWMProgram -> UQM RWMProgram
uniquifyModule (RWMProgram dds ds) = do dds' <- mapM uniquifyDataDecl dds
                                        ds'  <- mapM uniquifyDefn ds
                                        return (RWMProgram dds' ds')

uniquifyE :: Int -> RWMExp -> (RWMExp,Int)
uniquifyE ctr e = runIdentity $ runStateT (runReaderT (runReaderT (uniquifyExpr e) Map.empty) Map.empty) ctr

uniquify :: Int -> RWMProgram -> (RWMProgram,Int)
uniquify ctr m = runIdentity $ runStateT (runReaderT (runReaderT (uniquifyModule m) Map.empty) Map.empty) ctr
