{-# LANGUAGE LambdaCase, ViewPatterns, ScopedTypeVariables, GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module ReWire.FrontEnd.Transform
      ( inline, reduce
      , neuterPrims
      , shiftLambdas
      , liftLambdas
      , purge
      ) where

import ReWire.Error
import ReWire.FrontEnd.Syntax
import ReWire.SYB

import Control.Arrow ((***))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.State (State, evalStateT, execState, StateT (..), get, modify)
import Control.Monad (zipWithM, replicateM)
import Data.Data (Data)
import Data.List (nub, find, foldl')
import Data.Maybe (fromJust, fromMaybe)

import Data.Set (Set, singleton, union, (\\))
import qualified Data.Set as Set

import Unbound.Generics.LocallyNameless
      ( Fresh (..), string2Name, name2String
      , substs, subst, unembed
      , isFreeName, runFreshM
      )
import Unbound.Generics.LocallyNameless.Name (Name (..))

-- | Inlines defs marked for inlining. Must run before lambda lifting.
inline :: Fresh m => RWMProgram -> m RWMProgram
inline (RWMProgram p) = do
      (ts, ds) <- untrec p
      subs     <- mapM toSubst $ filter defnInline ds
      return $ RWMProgram $ trec (ts,  map (substs subs) ds)
      where toSubst :: Fresh m => RWMDefn -> m (Name RWMExp, RWMExp)
            toSubst (RWMDefn _ n _ _ (Embed e)) = do
                  ([], e') <- unbind e
                  return (n, e')

-- | Replaces the expression in RWMNativeVHDL so we don't descend into it
--   during other transformations.
neuterPrims :: (MonadCatch m, Fresh m) => RWMProgram -> m RWMProgram
neuterPrims = transProg $ \ arr -> transform $
      \ (RWMNativeVHDL an s e) -> return $ RWMNativeVHDL an s (RWMError an (typeOf arr e) "nativeVHDL expression placeholder")

shiftLambdas :: Fresh m => RWMProgram -> m RWMProgram
shiftLambdas (RWMProgram p) = do
      (ts, vs) <- untrec p
      vs' <- mapM shiftLambdas' vs
      return $ RWMProgram $ trec (ts, vs')
      where shiftLambdas' :: Fresh m => RWMDefn -> m RWMDefn
            shiftLambdas' (RWMDefn an n t inl (Embed e)) = RWMDefn an n t inl <$> (Embed <$> mash e)

            mash :: Fresh m => Bind [Name RWMExp] RWMExp -> m (Bind [Name RWMExp] RWMExp)
            mash e = do
                  (vs, e') <- unbind e
                  case e' of
                        RWMLam _ _ b -> do
                              (v, b') <- unbind b
                              mash $ bind (vs ++ [v]) b'
                        _ -> return e

-- | This is a hacky SYB-based lambda lifter, it requires some ugly mucking
--   with the internals of unbound-generics.
liftLambdas :: (MonadCatch m, Fresh m) => RWMProgram -> m RWMProgram
liftLambdas p = evalStateT (transProg liftLambdas' p) []
      where liftLambdas' :: (MonadCatch m, Fresh m) => Name TyConId -> Transform (StateT [RWMDefn] m)
            liftLambdas' arr =  \ case
                  RWMLam an t b -> do
                        (x, e)    <- unbind b
                        -- The only free names should be globally-bound
                        -- variables (and x) at this point and we can also
                        -- assume every bound name in e' was bound at a level above e'.
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr (mkArrow arr) (typeOf arr e') $ map snd bvs ++ [t]
                        f     <- fresh $ string2Name "$LL"

                        modify $ (:) $ RWMDefn an f (fv t' |-> t') False (Embed $ bind (fvs ++ [x]) e')
                        return $ foldl' (\ e' (v, vt) -> RWMApp an e' $ RWMVar an vt v) (RWMVar an t' f) $ map (promote *** id) bvs
                  RWMCase an t e1 b e2 -> do
                        (p, e)    <- unbind b
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let pvs = patVars p

                        let t' = foldr (mkArrow arr) (typeOf arr e) $ map snd bvs ++ map snd pvs
                        f     <- fresh $ string2Name "$LL"

                        modify $ (:) $ RWMDefn an f (fv t' |-> t') False (Embed $ bind (fvs ++ map fst pvs) e')
                        return $ RWMMatch an t e1 (transPat p) (RWMVar an t' f) (map (\ (v, vt) -> RWMVar an vt (promote v)) bvs) e2
                  ||> (\ ([] :: [RWMDefn]) -> get) -- this is cute!
                  ||> TId

            freshen :: (MonadCatch m, Fresh m) => RWMExp -> m ([Name RWMExp], RWMExp)
            freshen e = do
                  let bvs  = bv e
                  fvs  <- replicateM (length bvs) $ fresh $ string2Name "LA"
                  e' <- substs' (zip (map fst bvs) fvs) e
                  return (fvs, e')

            substs' :: MonadCatch m => [(Name RWMExp, Name RWMExp)] -> RWMExp -> m RWMExp
            substs' subs = runT (transform $ \ n -> return $ fromMaybe n (lookup n subs))

            bv :: Data a => a -> [(Name RWMExp, RWMTy)]
            bv = nub . runQ (query $ \ case
                  RWMVar _ t n | not $ isFreeName n -> [(n, t)]
                  _                                 -> [])

            patVars :: RWMPat -> [(Name RWMExp, RWMTy)]
            patVars = \ case
                  RWMPatCon _ _ ps        -> concatMap patVars ps
                  RWMPatVar _ (Embed t) x -> [(x, t)]

            promote :: Name a -> Name a
            promote (Bn l k) | l >= 0 = Bn (l - 1) k
            promote b = b

            transPat :: RWMPat -> RWMMatchPat
            transPat = \ case
                  RWMPatCon an (Embed c) ps -> RWMMatchPatCon an c $ map transPat ps
                  RWMPatVar an (Embed t) _  -> RWMMatchPatVar an t

-- | Purge.

purge :: Fresh m => RWMProgram -> m RWMProgram
purge (RWMProgram p) = do
      (ts, vs) <- untrec p
      return $ RWMProgram $ trec (inuseData (fv $ trec $ inuseDefn vs) ts, inuseDefn vs)
      where inuseData :: [Name DataConId] -> [RWMData] -> [RWMData]
            inuseData ns = map $ inuseData' ns

            inuseData' :: [Name DataConId] -> RWMData -> RWMData
            inuseData' ns (RWMData an n k cs) = RWMData an n k $ filter ((flip Set.member (Set.fromList ns)) . dataConName) cs

            dataConName :: RWMDataCon -> Name DataConId
            dataConName (RWMDataCon _ n _) = n

inuseDefn :: [RWMDefn] -> [RWMDefn]
inuseDefn ds = case find ((=="Main.start") . name2String . defnName) ds of
      Just n  -> map toDefn $ Set.elems $ execState (inuseDefn' $ defnName n) (singleton $ defnName n)
      Nothing -> []
      where inuseDefn' :: Name RWMExp -> State (Set (Name RWMExp)) ()
            inuseDefn' n = do
                  inuse  <- get
                  modify $ union (fvs n)
                  inuse' <- get
                  mapM_ inuseDefn' $ inuse' \\ inuse

            fvs :: Name RWMExp -> Set (Name RWMExp)
            fvs = Set.fromList . fv . unembed . defnBody . toDefn

            toDefn :: Name RWMExp -> RWMDefn
            toDefn n = fromJust $ find ((==n) . defnName) ds

-- | Reduce.

reduce :: (SyntaxError m, Fresh m) => RWMProgram -> m RWMProgram
reduce (RWMProgram p) = do
      (ts, vs) <- untrec p
      vs'      <- mapM reduceDefn vs
      return $ RWMProgram $ trec (ts, vs')

reduceDefn :: (SyntaxError m, Fresh m) => RWMDefn -> m RWMDefn
reduceDefn (RWMDefn an n pt b (Embed e)) = do
      (vs, e') <- unbind e
      RWMDefn an n pt b <$> Embed <$> bind vs <$> reduceExp e'

reduceExp :: (SyntaxError m, Fresh m) => RWMExp -> m RWMExp
reduceExp = \ case
      RWMApp an e1 e2      -> do
            e1' <- reduceExp e1
            e2' <- reduceExp e2
            case e1' of
                  RWMLam _ _ e -> do
                        (x, e') <- unbind e
                        reduceExp $ subst x e2' e'
                  _              -> return $ RWMApp an e1' e2'
      RWMLam an t e      -> do
            (x, e') <- unbind e
            RWMLam an t <$> bind x <$> reduceExp e'
      RWMCase an t e e1 e2 -> do
            (p, e1') <- unbind e1
            e' <- reduceExp e
            mr <- matchPat e' p
            case mr of
                  MatchYes sub -> reduceExp $ substs sub e1'
                  MatchMaybe   -> case e2 of
                        Nothing  -> RWMCase an t e' <$> (bind p <$> reduceExp e1') <*> pure Nothing
                        Just e2' -> RWMCase an t e' <$> (bind p <$> reduceExp e1') <*> (Just <$> reduceExp e2')
                  MatchNo      -> case e2 of
                        Nothing  -> failAt an "Reduce: pattern match failure"
                        Just e2' -> reduceExp e2'
      e -> return e

data MatchResult = MatchYes [(Name RWMExp, RWMExp)]
                 | MatchMaybe
                 | MatchNo
                 deriving Show

mergeMatches :: [MatchResult] -> MatchResult
mergeMatches []     = MatchYes []
mergeMatches (m:ms) = case mergeMatches ms of
      MatchYes bs -> case m of
            MatchYes bs' -> MatchYes $ bs' ++ bs
            MatchNo      -> MatchNo
            MatchMaybe   -> MatchMaybe
      MatchNo     -> MatchNo
      MatchMaybe  -> case m of
            MatchYes _ -> MatchMaybe
            MatchNo    -> MatchNo
            MatchMaybe -> MatchMaybe

matchPat :: Fresh m => RWMExp -> RWMPat -> m MatchResult
matchPat e = \ case
      RWMPatCon _ (Embed i) pats -> case flattenApp e of
            RWMCon _ _ c : es
                  | c == i && length es == length pats -> do
                        ms <- zipWithM matchPat es pats
                        return $ mergeMatches ms
                  | otherwise                          -> return MatchNo
            _                                          -> return MatchMaybe
      RWMPatVar _ _ x            -> return $ MatchYes [(x, e)]

mkArrow' :: Name TyConId -> RWMTy -> Bind (Name RWMExp) RWMExp -> RWMTy
mkArrow' arr t b = runFreshM $ do
      (_, e) <- unbind b
      return $ mkArrow arr t $ typeOf arr e

typeOf :: Name TyConId -> RWMExp -> RWMTy
typeOf arr = \ case
      RWMApp _ e _           -> arrowRight $ typeOf arr e
      RWMLam _ t e           -> mkArrow' arr t e
      RWMVar _ t _           -> t
      RWMCon _ t _           -> t
      RWMCase _ t _ _ _      -> t
      RWMMatch _ t _ _ _ _ _ -> t
      RWMNativeVHDL _ _ e    -> typeOf arr e
      RWMError _ t _         -> t

transProg :: (MonadCatch m, Fresh m) => (Name TyConId -> Transform m) -> RWMProgram -> m RWMProgram
transProg f (RWMProgram p) = do
      (ts, vs) <- untrec p
      p'       <- runT (f $ getArrow ts) (ts, vs)
      return $ RWMProgram $ trec p'
