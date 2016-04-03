{-# LANGUAGE LambdaCase #-}
module ReWire.FrontEnd.Expand (expand, neuterPrims, liftLambdas) where

import ReWire.Annotation
import ReWire.FrontEnd.Monad
import ReWire.FrontEnd.Syntax
import ReWire.Scoping
import ReWire.Pretty
import ReWire.SYB hiding (query)

import Control.Monad.Catch (MonadCatch)
import Control.Monad (foldM)
import Control.Monad.State (StateT (..), get, put, modify)
import Control.Monad.Trans (lift)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

expandExpr :: Monad m => [Id RWMExp] -> RWMExp -> RWT m RWMExp
expandExpr ns = \ case
      RWMApp an e1 e2       -> RWMApp an <$> expandExpr ns e1 <*> expandExpr ns e2
      RWMLam an n t e       -> RWMLam an n t <$> expandExpr ns e
      RWMVar an n t
            | n `elem` ns   -> askVar t n >>= \ case
                  Just e  -> expandExpr ns e
                  Nothing -> return $ RWMVar an n t
            | otherwise     -> return $ RWMVar an n t
      RWMCon an dci t       -> return $ RWMCon an dci t
      RWMCase an e p e1 e2  -> RWMCase an <$> expandExpr ns e <*> return p <*> expandExpr ns e1 <*> expandExpr ns e2
      RWMNativeVHDL an n e  -> return $ RWMNativeVHDL an n e -- FIXME(?!): special case here!
      RWMError an m t       -> return $ RWMError an m t

expandDefn :: Monad m => [Id RWMExp] -> RWMDefn -> RWT m RWMDefn
expandDefn ns (RWMDefn an n pt b vs e) = RWMDefn an n pt b vs <$> expandExpr ns e

expand :: Monad m => [Id RWMExp] -> RWMProgram -> RWT m RWMProgram
expand ns (RWMProgram dds defns) = RWMProgram dds <$> mapM (expandDefn ns) defns

type FreshT = StateT Int

fresh :: Monad m => FreshT m (Id RWMExp)
fresh = do
      x <- get
      modify $ const $ x + 1
      return $ mkId $ "$LL" ++ show x

neuterPrims :: MonadCatch m => RWMProgram -> m RWMProgram
neuterPrims = runT $ transform $
      \ (RWMNativeVHDL an s e) -> return $ RWMNativeVHDL an s (RWMError an "nativeVHDL expression placeholder" $ typeOf e)

liftLambdas :: MonadCatch m => RWMProgram -> RWT m RWMProgram
liftLambdas m = addDecls <$> runStateT (fst <$> runStateT (runT liftLambdas' m) 0) []
      where liftLambdas' :: MonadCatch m => Transform (FreshT (StateT [RWMDefn] (RWT m)))
            liftLambdas' =  transform $ \ case
                  l@(RWMLam an x _ e) -> do
                        let fvs = filter (not . (`elem` x:gvs)) $ fv e
                        newApp (fvs ++ [x]) l e
                  -- RWMCase an e p e1 e2 -> do
                  --       let pvs = patvars p
                  --           fvs = filter (not . (`elem` gvs ++ pvs)) $ fv e1
                  --       e1' <- newApp (fvs ++ pvs) e1 e1
                  --       return $ RWMCase an e p e1' e2

            newApp :: Monad m => [Id RWMExp] -> RWMExp -> RWMExp -> FreshT (StateT [RWMDefn] (RWT m)) RWMExp
            newApp fvs e body = do
                  fvts <- mapM (lift . lookupType) fvs
                  let t = foldr mkArrow (typeOf e) $ reverse fvts
                  f    <- fresh
                  ds   <- lift $ get
                  lift $ put $ RWMDefn (ann e) f ([] :-> t) False fvs body : ds
                  return $ foldl (\ e' (v, vt) -> RWMApp (ann e) e' $ RWMVar (ann e) v vt) (RWMVar (ann e) f t)
                         $ zip fvs fvts

            getVar :: RWMDefn -> Id RWMExp
            getVar (RWMDefn _ x _ _ _ _)  = x

            getType :: RWMDefn -> RWCTy
            getType (RWMDefn _ _ ([] :-> t) _ _ _)  = t
            getType _ = error "getType: impossible ??"

            lookupType :: Monad m => Id RWMExp -> StateT [RWMDefn] (RWT m) RWCTy
            lookupType v = query v >>= \ case
                  -- TODO(chathhorn) Unify!?!
                  Just (GlobalVar d)  -> return $ getType d
                  Just (LocalVar t) -> return t
                  _ -> getType . fromMaybe (error $ "liftLambdas: failed to find: " ++ show v) . find ((== v) . getVar) <$> get

            addDecls :: (RWMProgram, [RWMDefn]) -> RWMProgram
            addDecls (p, ds') = p <> RWMProgram [] ds'

            gvs :: [Id RWMExp]
            gvs = map getVar (defns  m) ++ map mkId builtins

            builtins :: [String]
            builtins = [ "nativeVhdl"
                       , ">>="
                       , "return"
                       , "get"
                       , "put"
                       , "signal"
                       , "lift"
                       , "extrude"
                       ]

