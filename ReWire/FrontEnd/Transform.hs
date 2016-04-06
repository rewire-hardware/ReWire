{-# LANGUAGE LambdaCase, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module ReWire.FrontEnd.Transform
      ( inline, reduce
      , neuterPrims
      , liftLambdas
      , purge
      ) where

import ReWire.Annotation
import ReWire.FrontEnd.Syntax
import ReWire.SYB hiding (query)

import Control.Monad.Catch (MonadCatch)
import Control.Monad.State (State, runState, StateT (..), get, put)
import Data.Maybe (fromJust)
import Data.List (find, nub, intersect)

import Unbound.Generics.LocallyNameless (Fresh (..), string2Name, substs)

-- expandExpr :: Fresh m => [RWMDefn] -> RWMExp -> m RWMExp
-- expandExpr ns = \ case
--       RWMApp an e1 e2       -> RWMApp an <$> expandExpr ns e1 <*> expandExpr ns e2
--       RWMLam an t e         -> do
--             (p, e') <- unbind e
--             e''     <- expandExpr ns e'
--             return $ RWMLam an t $ bind p e''
--       RWMVar _ _ (lkup -> Just (RWMDefn _ _ _ _ (Embed e))) -> do
--             ([], e') <- unbind e
--             expandExpr ns e'
--       RWMCase an e e1 e2    -> do
--             (p, e1') <- unbind e1
--             e1''     <- expandExpr ns e1'
--             RWMCase an <$> expandExpr ns e <*> pure (bind p e1'') <*> expandExpr ns e2
--       e                     -> return e
--       where lkup :: Name RWMExp -> Maybe RWMDefn
--             lkup = (flip findDefn) ns
-- 
-- expandDefn :: Fresh m => [RWMDefn] -> RWMDefn -> m RWMDefn
-- expandDefn ns (RWMDefn an n t b (Embed e)) = do
--       (p, e') <- unbind e
--       e'' <- expandExpr ns e'
--       return $ RWMDefn an n t b $ Embed $ bind p e''

-- expand :: Fresh m => [RWMDefn] -> [RWMDefn] -> m [RWMDefn]
-- expand ns (RWMProgram dds defns) = do
--       ds <- untrec defns
--       ds' <- mapM (expandDefn ns) ds
--       return $ RWMProgram dds $ trec ds'

inline :: Fresh m => RWMProgram -> m RWMProgram
inline (RWMProgram ts ds) = do
      ds'  <- untrec ds
      subs <- mapM toSubst $ filter defnInline ds'
      return $ RWMProgram ts $ trec $ map (substs subs) ds'

toSubst :: Fresh m => RWMDefn -> m (Name RWMExp, RWMExp)
toSubst (RWMDefn _ n _ _ (Embed e)) = do
      ([], e') <- unbind e
      return (n, e')

neuterPrims :: (MonadCatch m, Fresh m) => RWMProgram -> m RWMProgram
neuterPrims = transProg $ transform $
      \ (RWMNativeVHDL an s e) -> return $ RWMNativeVHDL an s (RWMError an (typeOf e) "nativeVHDL expression placeholder")

liftLambdas :: (MonadCatch m, Fresh m) => RWMProgram -> m RWMProgram
liftLambdas p = runStateT (transProg liftLambdas' p) [] >>= addDecls
      where liftLambdas' :: (MonadCatch m, Fresh m) => Transform (StateT [RWMDefn] m)
            liftLambdas' =  transform $ \ case
                  l@(RWMLam _ _ e) -> do
                        gvs <- map defnName <$> (untrec $ defns p) -- TODO (chathhorn) non-global locals
                        (x, e') <- unbind e
                        let fvs = filter (not . (`elem` x:gvs)) $ fv e'
                        newApp (fvs ++ [x]) l e'
                  RWMCase an e e1 e2 -> do
                        gvs <- map defnName <$> (untrec $ defns p) -- TODO (chathhorn) non-global locals
                        (p, e1') <- unbind e1
                        let pvs = patVars p
                            fvs = filter (not . (`elem` gvs ++ pvs)) $ fv e1'
                        e1'' <- newApp (fvs ++ pvs) e1' e1'
                        return $ RWMCase an e (bind p e1'') e2

            newApp :: (Monad m, Fresh m) => [Name RWMExp] -> RWMExp -> RWMExp -> StateT [RWMDefn] m RWMExp
            newApp fvs e body = do
                  fvts <- mapM lookupType fvs
                  let t = foldr mkArrow (typeOf e) $ reverse fvts
                  f    <- fresh $ string2Name "LL"
                  ds   <- get
                  put $ RWMDefn (ann e) f ([] |-> t) False (Embed $ bind fvs body) : ds
                  return $ foldl (\ e' (v, vt) -> RWMApp (ann e) e' $ RWMVar (ann e) vt v) (RWMVar (ann e) t f)
                         $ zip fvs fvts

            -- getType :: RWMDefn -> RWMTy
            -- getType (RWMDefn _ _ ([] :-> t) _ _ _)  = t
            -- getType _ = error "getType: impossible ??"

            lookupType :: (Monad m, Fresh m) => Name RWMExp -> StateT [RWMDefn] m RWMTy
            lookupType _ = return tblank
                  -- TODO(chathhorn)
                  -- query n >>= \ case
                  -- -- TODO(chathhorn) Unify!?!
                  -- Just (GlobalVar d)  -> return $ getType d
                  -- Just (LocalVar t) -> return t
                  -- _ -> getType . fromMaybe (error $ "liftLambdas: failed to find: " ++ show v) . findDefn v <$> get

            addDecls :: (Monad m, Fresh m) => (RWMProgram, [RWMDefn]) -> m RWMProgram
            addDecls (RWMProgram ts vs1, vs2) = do
                  vs1' <- untrec vs1
                  return $ RWMProgram ts $ trec $ vs1' ++ vs2


-- Purge.

purge :: Fresh m => Name RWMExp -> RWMProgram -> m RWMProgram
purge n (RWMProgram ts vs) = do
      vs' <- untrec vs
      return $ RWMProgram ts $ trec $ inuseDefn n vs'

data Defns = Defns { allDefns :: [RWMDefn], someNames :: [Name RWMExp] }

inuseDefn :: Name RWMExp -> [RWMDefn] -> [RWMDefn]
inuseDefn n ds = map (findDefn' ds) $ someNames $ snd $ runState (inuseDefn' n) $ Defns ds [n]
      where inuseDefn' :: Name RWMExp -> State Defns ()
            inuseDefn' n = do
                  defns     <- allDefns <$> get
                  inuse     <- someNames <$> get
                  let inuse' = nub $ inuse ++ fvDefn n defns
                  put $ Defns defns inuse'
                  mapM_ inuseDefn' $ inuse' `intersect` inuse

findDefn :: Name RWMExp -> [RWMDefn] -> Maybe RWMDefn
findDefn n = find ((==n) . defnName)

findDefn' :: [RWMDefn] -> Name RWMExp -> RWMDefn
findDefn' ds = fromJust . flip findDefn ds

fvDefn :: Name RWMExp -> [RWMDefn] -> [Name RWMExp]
fvDefn n ds = fv $ findDefn' ds n

-- Reduce.

reduce :: Monad m => a -> m a
reduce = return

-- reduceExp :: Monad m => RWMExp -> RWT m RWMExp
-- reduceExp = \ case
--       RWMApp an e1 e2      -> do
--             e1' <- reduceExp e1
--             e2' <- reduceExp e2
--             case e1' of
--                   RWMLam _ n _ b -> fsubstE n e2' b >>= reduceExp
--                   _              -> return $ RWMApp an e1' e2'
--       RWMLam an n t e      -> RWMLam an n t <$> reduceExp e
--       e@RWMVar {}          -> return e
--       e@RWMCon {}          -> return e
--       RWMCase an e p e1 e2 -> do
--             e' <- reduceExp e
--             mr <- matchpat e' p
--             case mr of
--                   MatchYes sub -> fsubstsE sub e1 >>= reduceExp
--                   MatchMaybe   -> RWMCase an e' p <$> reduceExp e1 <*> reduceExp e2
--                   MatchNo      -> reduceExp e2
--       e@RWMNativeVHDL {}   -> return e
--       e@RWMError {}        -> return e
-- 
-- data MatchResult = MatchYes [(Id RWMExp, RWMExp)]
--                  | MatchMaybe
--                  | MatchNo
--                  deriving Show
-- 
-- mergematches :: [MatchResult] -> MatchResult
-- mergematches []     = MatchYes []
-- mergematches (m:ms) = case mergematches ms of
--       MatchYes bs -> case m of
--             MatchYes bs' -> MatchYes $ bs' ++ bs
--             MatchNo      -> MatchNo
--             MatchMaybe   -> MatchMaybe
--       MatchNo     -> MatchNo
--       MatchMaybe  -> case m of
--             MatchYes _ -> MatchMaybe
--             MatchNo    -> MatchNo
--             MatchMaybe -> MatchMaybe
-- 
-- matchpat :: Monad m => RWMExp -> RWMPat -> RWT m MatchResult
-- matchpat e = \ case
--       RWMPatCon _ i pats -> case flattenApp e of
--             RWMCon _ c _:es
--                   | c == i && length es == length pats -> do
--                         ms <- zipWithM matchpat es pats
--                         return $ mergematches ms
--                   | otherwise                          -> return MatchNo
--             _                                          -> return MatchMaybe
--       RWMPatVar _ n _    -> return $ MatchYes [(n, e)]
-- 
-- reddefn :: Monad m => RWMDefn -> RWT m RWMDefn
-- reddefn (RWMDefn an n pt b vs e) = RWMDefn an n pt b vs <$> reduceExp e
-- 
-- reduce :: Monad m => RWMProgram -> RWT m RWMProgram
-- reduce m = do
--       ds' <- mapM reddefn $ defns m
--       return m { defns = ds' }
