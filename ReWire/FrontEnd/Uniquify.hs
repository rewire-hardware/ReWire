{-# LANGUAGE LambdaCase #-}
module ReWire.FrontEnd.Uniquify (uniquify) where

import ReWire.FrontEnd.Syntax
import ReWire.Scoping

import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT (..), ask, local)
import Control.Monad.State (StateT (..), get, put)
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as BS

type UQM m = ReaderT (Map (Id RWMExp) (Id RWMExp)) (ReaderT (Map (Id RWCTy) (Id RWCTy)) (StateT Int m))

base32 :: Int -> String
base32 0 = "0"
base32 n = dropWhile (== '0') $ reverse $ b32 n

b32 :: Int -> String
b32 0 = "0"
b32 n = c : sq
      where (q, r) = quotRem n 32
            sq     = b32 q
            c      = (['0'..'9'] ++ ['A'..]) !! r

askT :: Monad m => UQM m (Map (Id RWCTy) (Id RWCTy))
askT = lift ask

localT :: Monad m => (Map (Id RWCTy) (Id RWCTy) -> Map (Id RWCTy) (Id RWCTy)) -> UQM m a -> UQM m a
localT f m = ReaderT $ \ rhoE -> local f $ runReaderT m rhoE

askE :: Monad m => UQM m (Map (Id RWMExp) (Id RWMExp))
askE = ask

localE :: Monad m => (Map (Id RWMExp) (Id RWMExp) -> Map (Id RWMExp) (Id RWMExp)) -> UQM m a -> UQM m a
localE = local

askUniqueT :: Monad m => Id RWCTy -> UQM m (Maybe (Id RWCTy))
askUniqueT n = do
      m <- askT
      return $ Map.lookup n m

askUniqueE :: Monad m => Id RWMExp -> UQM m (Maybe (Id RWMExp))
askUniqueE n = do
      m <- askE
      return $ Map.lookup n m

fresh :: Monad m => Id a -> UQM m (Id a)
fresh (Id s x) = do
      n <- get
      put (n+1)
      return $ Id s $ BS.append x $ BS.pack $ "@" ++ base32 n

uniquingT :: Monad m => [Id RWCTy] -> UQM m t -> UQM m ([Id RWCTy], t)
uniquingT ns m = do
      ns'    <- mapM fresh ns
      let bs =  Map.fromList (zip ns ns')
      v      <- localT (Map.union bs) m
      return (ns', v)

uniquingE :: Monad m => [Id RWMExp] -> UQM m t -> UQM m ([Id RWMExp], t)
uniquingE ns m = do
      ns'    <- mapM fresh ns
      let bs =  Map.fromList (zip ns ns')
      v      <- localE (Map.union bs) m
      return (ns', v)

uniquifyTy :: Monad m => RWCTy -> UQM m RWCTy
uniquifyTy = \ case
      RWCTyApp an t1 t2  -> RWCTyApp an <$> uniquifyTy t1 <*> uniquifyTy t2
      RWCTyCon an i      -> return $ RWCTyCon an i
      RWCTyVar an n      -> do
            mn <- askUniqueT n
            return $ case mn of
                  Nothing -> RWCTyVar an n
                  Just n' -> RWCTyVar an n'
      RWCTyComp an t1 t2 -> RWCTyComp an <$> uniquifyTy t1 <*> uniquifyTy t2

uniquifyDataCon :: Monad m => RWCDataCon -> UQM m RWCDataCon
uniquifyDataCon (RWCDataCon an dci ts) = do
      ts' <- mapM uniquifyTy ts
      return $ RWCDataCon an dci ts'

uniquifyDataDecl :: Monad m => RWMData -> UQM m RWMData
uniquifyDataDecl (RWMData an i vs k dcs) = do
      (vs', dcs') <- uniquingT vs $ mapM uniquifyDataCon dcs
      return $ RWMData an i vs' k dcs'

uniquifyPat :: Monad m => RWMPat -> UQM m RWMPat
uniquifyPat = \ case
      RWMPatCon an dci ps -> RWMPatCon an dci <$> mapM uniquifyPat ps
      RWMPatLiteral an l  -> return $ RWMPatLiteral an l
      RWMPatVar an n t    -> do
            mn <- askUniqueE n
            case mn of
                  Just n' -> RWMPatVar an n' <$> uniquifyTy t
                  Nothing -> RWMPatVar an n <$> uniquifyTy t -- shouldn't happen

uniquifyExpr :: Monad m => RWMExp -> UQM m RWMExp
uniquifyExpr = \ case
      RWMApp an e1 e2      -> RWMApp an <$> uniquifyExpr e1 <*> uniquifyExpr e2
      RWMLam an n t e      -> do
            t'         <- uniquifyTy t
            ([n'], e') <- uniquingE [n] (uniquifyExpr e)
            return $ RWMLam an n' t' e'
      RWMVar an n t        -> do
            mn <- askUniqueE n
            case mn of
                  Nothing -> RWMVar an n <$> uniquifyTy t
                  Just n' -> RWMVar an n' <$> uniquifyTy t
      RWMCon an dci t      -> RWMCon an dci <$> uniquifyTy t
      RWMLiteral an l      -> return $ RWMLiteral an l
      RWMCase an e p e1 e2 -> do
            e'             <- uniquifyExpr e
            (_, (p', e1')) <- uniquingE (pvs p) $ (,) <$> uniquifyPat p <*> uniquifyExpr e1
            e2' <- uniquifyExpr e2
            return $ RWMCase an e' p' e1' e2'
      RWMNativeVHDL an n e -> RWMNativeVHDL an n <$> uniquifyExpr e
      RWMError an m t      -> RWMError an m <$> uniquifyTy t
      where pvs :: RWMPat -> [Id RWMExp]
            pvs = \ case
                  RWMPatCon _ _ ps  -> concatMap pvs ps
                  RWMPatLiteral _ _ -> []
                  RWMPatVar _ x _   -> [x]

uniquifyDefn :: Monad m => RWMDefn -> UQM m RWMDefn
uniquifyDefn (RWMDefn an n (tvs :-> t) b e) = do
      (tvs', (t', e')) <- uniquingT tvs $ (,) <$> uniquifyTy t <*> uniquifyExpr e
      return $ RWMDefn an n (tvs' :-> t') b e'

uniquifyModule :: Monad m => RWMProgram -> UQM m RWMProgram
uniquifyModule (RWMProgram dds ds) = do
      dds' <- mapM uniquifyDataDecl dds
      ds'  <- mapM uniquifyDefn ds
      return $ RWMProgram dds' ds'

uniquify :: Monad m => RWMProgram -> m RWMProgram
uniquify p = fst <$> runStateT (runReaderT (runReaderT (uniquifyModule p) Map.empty) Map.empty) 0
