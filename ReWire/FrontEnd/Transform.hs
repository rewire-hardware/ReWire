{-# LANGUAGE LambdaCase, ViewPatterns, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE Safe #-}
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
import ReWire.FrontEnd.Unbound
      ( Fresh (..), string2Name, name2String
      , substs, subst, unembed
      , isFreeName, runFreshM, runFreshMT
      , Name (..)
      )
import ReWire.SYB

import Control.Arrow ((***))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.State (State, evalStateT, execState, StateT (..), get, modify)
import Control.Monad (replicateM)
import Data.Data (Data)
import Data.List (nub, find, foldl')
import Data.Maybe (fromJust, fromMaybe)

import Data.Set (Set, singleton, union, (\\))
import qualified Data.Set as Set

-- | Inlines defs marked for inlining. Must run before lambda lifting.
inline :: Monad m => FreeProgram -> m FreeProgram
inline (ts, ds) = return (ts,  map (substs subs) ds)
      where toSubst :: Defn -> (Name Exp, Exp)
            toSubst (Defn _ n _ _ (Embed e)) = runFreshM $ do
                  ([], e') <- unbind e
                  return (n, e')
            subs = map toSubst $ filter defnInline ds

-- | Replaces the expression in NativeVHDL so we don't descend into it
--   during other transformations.
neuterPrims :: MonadCatch m => FreeProgram -> m FreeProgram
neuterPrims = transProg $ \ arr -> transform $
      \ (NativeVHDL an s e) -> return $ NativeVHDL an s (Error an (typeOf arr e) "nativeVHDL expression placeholder")

shiftLambdas :: Monad m => FreeProgram -> m FreeProgram
shiftLambdas (ts, vs) = return (ts, map shiftLambdas' vs)
      where shiftLambdas' :: Defn -> Defn
            shiftLambdas' (Defn an n t inl (Embed e)) = Defn an n t inl (Embed $ mash e)

            mash :: Bind [Name Exp] Exp -> Bind [Name Exp] Exp
            mash e = runFreshM $ do
                  (vs, e') <- unbind e
                  case e' of
                        Lam _ _ b -> do
                              (v, b') <- unbind b
                              return $ mash $ bind (vs ++ [v]) b'
                        _ -> return e

-- | This is a hacky SYB-based lambda lifter, it requires some ugly mucking
--   with the internals of unbound-generics.
liftLambdas :: MonadCatch m => FreeProgram -> m FreeProgram
liftLambdas p = runFreshMT $ evalStateT (transProg liftLambdas' p) []
      where liftLambdas' :: (MonadCatch m, Fresh m) => Name TyConId -> Transform (StateT [Defn] m)
            liftLambdas' arr =  \ case
                  Lam an t b -> do
                        (x, e)    <- unbind b
                        -- The only free names should be globally-bound
                        -- variables (and x) at this point and we can also
                        -- assume every bound name in e' was bound at a level above e'.
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let t' = foldr (mkArrow arr) (typeOf arr e') $ map snd bvs ++ [t]
                        f     <- fresh $ string2Name "$LL."

                        modify $ (:) $ Defn an f (fv t' |-> t') False (Embed $ bind (fvs ++ [x]) e')
                        return $ foldl' (\ e' (v, vt) -> App an e' $ Var an vt v) (Var an t' f) $ map (promote *** id) bvs
                  Case an t e1 b e2 -> do
                        (p, e)    <- unbind b
                        let bvs   = bv e
                        (fvs, e') <- freshen e

                        let pvs = patVars p

                        let t' = foldr (mkArrow arr) (typeOf arr e) $ map snd bvs ++ map snd pvs
                        f     <- fresh $ string2Name "$LL."

                        modify $ (:) $ Defn an f (fv t' |-> t') False (Embed $ bind (fvs ++ map fst pvs) e')
                        return $ Match an t e1 (transPat p) (Var an t' f) (map (\ (v, vt) -> Var an vt (promote v)) bvs) e2
                  ||> (\ ([] :: [Defn]) -> get) -- this is cute!
                  ||> TId

            freshen :: (MonadCatch m, Fresh m) => Exp -> m ([Name Exp], Exp)
            freshen e = do
                  let bvs  = bv e
                  fvs  <- replicateM (length bvs) $ fresh $ string2Name "LA"
                  e' <- substs' (zip (map fst bvs) fvs) e
                  return (fvs, e')

            substs' :: MonadCatch m => [(Name Exp, Name Exp)] -> Exp -> m Exp
            substs' subs = runT (transform $ \ n -> return $ fromMaybe n (lookup n subs))

            bv :: Data a => a -> [(Name Exp, Ty)]
            bv = nub . runQ (query $ \ case
                  Var _ t n | not $ isFreeName n -> [(n, t)]
                  _                                 -> [])

            patVars :: Pat -> [(Name Exp, Ty)]
            patVars = \ case
                  PatCon _ _ _ ps      -> concatMap patVars ps
                  PatVar _ (Embed t) x -> [(x, t)]

            promote :: Name a -> Name a
            promote (Bn l k) | l >= 0 = Bn (l - 1) k
            promote b = b

            transPat :: Pat -> MatchPat
            transPat = \ case
                  PatCon an (Embed t) (Embed c) ps -> MatchPatCon an t c $ map transPat ps
                  PatVar an (Embed t) _            -> MatchPatVar an t

-- | Purge.

purge :: Monad m => FreeProgram -> m FreeProgram
purge (ts, vs) = return $ (inuseData (fv $ trec $ inuseDefn vs) ts, filterBuiltins $ inuseDefn vs)
      where inuseData :: [Name DataConId] -> [DataDefn] -> [DataDefn]
            inuseData ns = map $ inuseData' ns

            inuseData' :: [Name DataConId] -> DataDefn -> DataDefn
            inuseData' ns (DataDefn an n k cs)
                  | name2String n == "Prelude.Either" = DataDefn an n k cs
                  | otherwise                         = DataDefn an n k $ filter ((flip Set.member (Set.fromList ns)) . dataConName) cs

            dataConName :: DataCon -> Name DataConId
            dataConName (DataCon _ n _) = n

            filterBuiltins :: [Defn] -> [Defn]
            filterBuiltins = filter (elem '.' . name2String . defnName)

inuseDefn :: [Defn] -> [Defn]
inuseDefn ds = case find ((=="Main.start") . name2String . defnName) ds of
      Just n  -> map toDefn $ Set.elems $ execState (inuseDefn' $ defnName n) (singleton $ defnName n)
      Nothing -> []
      where inuseDefn' :: Name Exp -> State (Set (Name Exp)) ()
            inuseDefn' n = do
                  inuse  <- get
                  modify $ union (fvs n)
                  inuse' <- get
                  mapM_ inuseDefn' $ inuse' \\ inuse

            fvs :: Name Exp -> Set (Name Exp)
            fvs = Set.fromList . fv . unembed . defnBody . toDefn

            toDefn :: Name Exp -> Defn
            toDefn n = fromJust $ find ((==n) . defnName) ds

-- | Reduce.

reduce :: SyntaxError m => FreeProgram -> m FreeProgram
reduce (ts, vs) = do
      vs'      <- mapM reduceDefn vs
      return (ts, vs')

reduceDefn :: SyntaxError m => Defn -> m Defn
reduceDefn (Defn an n pt b (Embed e)) = runFreshMT $ do
      (vs, e') <- unbind e
      Defn an n pt b <$> Embed <$> bind vs <$> reduceExp e'

reduceExp :: (Fresh m, SyntaxError m) => Exp -> m Exp
reduceExp = \ case
      App an e1 e2      -> do
            e1' <- reduceExp e1
            e2' <- reduceExp e2
            case e1' of
                  Lam _ _ e -> do
                        (x, e') <- unbind e
                        reduceExp $ subst x e2' e'
                  _              -> return $ App an e1' e2'
      Lam an t e      -> do
            (x, e') <- unbind e
            Lam an t <$> bind x <$> reduceExp e'
      Case an t e e1 e2 -> do
            (p, e1') <- unbind e1
            e' <- reduceExp e
            let mr = matchPat e' p
            case mr of
                  MatchYes sub -> reduceExp $ substs sub e1'
                  MatchMaybe   -> case e2 of
                        Nothing  -> Case an t e' <$> (bind p <$> reduceExp e1') <*> pure Nothing
                        Just e2' -> Case an t e' <$> (bind p <$> reduceExp e1') <*> (Just <$> reduceExp e2')
                  MatchNo      -> case e2 of
                        Nothing  -> failAt an "Reduce: pattern match failure"
                        Just e2' -> reduceExp e2'
      e -> return e

data MatchResult = MatchYes [(Name Exp, Exp)]
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

matchPat :: Exp -> Pat -> MatchResult
matchPat e = \ case
      PatCon _ _ (Embed i) pats -> case flattenApp e of
            Con _ _ c : es
                  | c == i && length es == length pats -> mergeMatches $ zipWith matchPat es pats
                  | otherwise                          -> MatchNo
            _                                          -> MatchMaybe
      PatVar _ _ x            -> MatchYes [(x, e)]

mkArrow' :: Name TyConId -> Ty -> Bind (Name Exp) Exp -> Ty
mkArrow' arr t b = runFreshM $ do
      (_, e) <- unbind b
      return $ mkArrow arr t $ typeOf arr e

typeOf :: Name TyConId -> Exp -> Ty
typeOf arr = \ case
      App _ e _           -> arrowRight $ typeOf arr e
      Lam _ t e           -> mkArrow' arr t e
      Var _ t _           -> t
      Con _ t _           -> t
      Case _ t _ _ _      -> t
      Match _ t _ _ _ _ _ -> t
      NativeVHDL _ _ e    -> typeOf arr e
      Error _ t _         -> t

transProg :: MonadCatch m => (Name TyConId -> Transform m) -> FreeProgram -> m FreeProgram
transProg f (ts, vs) = runT (f $ getArrow ts) (ts, vs)
