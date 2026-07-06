{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Process-level cleanups (doc/eidos.md §6.5 of the migration plan):
--
--   * /Epsilon-block inlining/: a block with no commands whose terminator
--     is a single goto is glue; references to it re-target its successor
--     directly (with the argument substitution applied). A pause target
--     is never inlined away — it is a machine state. Chains terminate
--     because the goto-only subgraph is acyclic (the machine lint's
--     guardedness rule, checked before this runs).
--
--   * /Alpha-equal block merge/ (the retired duplicate-definition merge's
--     structural successor): blocks whose bodies are alpha-equivalent
--     (binders renumbered; labels and cells compared by identity) merge,
--     and references redirect to the survivor. Iterated to a fixpoint:
--     each round of merging can unify the targets of further blocks.
--     This is what restores state-count parity — an INLINE-duplicated
--     continuation mints many identical pause targets.
module ReWire.Eidos.ProcOpt (optimizeProc) where

import ReWire.Eidos.Pretty ()
import ReWire.Eidos.Subst (substVars)
import ReWire.Eidos.Syntax
import ReWire.Pretty (prettyPrint)

import Control.Monad.State.Strict (State, evalState, get, put)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap.Strict  as IM

optimizeProc :: Proc -> Proc
optimizeProc = fixpoint (mergeBlocks . inlineEpsilon)
      where fixpoint :: (Proc -> Proc) -> Proc -> Proc
            fixpoint f pr = let pr' = f pr in if sameShape pr pr' then pr' else fixpoint f pr'

            sameShape :: Proc -> Proc -> Bool
            sameShape a b = map (idUniq . fst) (procBlocks a) == map (idUniq . fst) (procBlocks b)

---
--- Epsilon-block inlining.
---

inlineEpsilon :: Proc -> Proc
inlineEpsilon pr = pr { procEntry  = retermB $ procEntry pr
                      , procBlocks = [ (l, retermB b) | (l, b) <- procBlocks pr, not $ IM.member (idUniq l) eps ]
                      }
      where -- Blocks that pause-resume must survive (they are states).
            pauseTargets :: IM.IntMap ()
            pauseTargets = IM.fromList [ (u, ()) | b <- procEntry pr : map snd (procBlocks pr), u <- pt (blkTerm b) ]
                  where pt :: Term -> [Uniq]
                        pt = \ case
                              Pause _ _ l _  -> [idUniq l]
                              TCase _ _ alts -> concat [ pt t | TAlt _ _ _ t <- alts ]
                              _              -> []

            -- Command-free single-goto blocks, by label unique.
            eps :: IM.IntMap ([Id], Id, [Exp])
            eps = IM.fromList [ (idUniq l, (blkParams b, l', as))
                              | (l, b) <- procBlocks pr
                              , not $ IM.member (idUniq l) pauseTargets
                              , null $ blkCmds b
                              , Goto _ l' as <- [blkTerm b]
                              , idUniq l' /= idUniq l ]

            -- Follow (acyclic) epsilon chains from a goto site. Arguments
            -- can be compound (primitive applications are
            -- naming-transparent in ANF), so the substitution is a full
            -- expression substitution, not an atom swap.
            resolve :: Id -> [Exp] -> (Id, [Exp])
            resolve l as = case IM.lookup (idUniq l) eps of
                  Just (ps, l', as') ->
                        let sub = IM.fromList $ zip (map idUniq ps) as
                        in resolve l' $ map (substVars sub) as'
                  Nothing            -> (l, as)

            retermB :: Block -> Block
            retermB b = b { blkTerm = reterm $ blkTerm b }

            reterm :: Term -> Term
            reterm = \ case
                  Goto an l as    -> let (l', as') = resolve l as in Goto an l' as'
                  TCase an a alts -> TCase an a [ TAlt aan c xs (reterm t) | TAlt aan c xs t <- alts ]
                  t               -> t

---
--- Alpha-equal block merge.
---

mergeBlocks :: Proc -> Proc
mergeBlocks pr = case dups of
      [] -> pr
      _  -> pr { procEntry  = redirB $ procEntry pr
               , procBlocks = [ (l, redirB b) | (l, b) <- procBlocks pr, not $ IM.member (idUniq l) redirect ]
               }
      where keyed :: [(Text, Id)]
            keyed = [ (blockKey b, l) | (l, b) <- procBlocks pr ]

            -- For each key, the first label survives; the rest redirect.
            redirect :: IM.IntMap Id
            redirect = IM.fromList dups

            dups :: [(Uniq, Id)]
            dups = [ (idUniq l, survivor)
                   | (_, survivor : rest) <- Map.toList grouped, l <- rest ]

            grouped :: Map.HashMap Text [Id]
            grouped = Map.fromListWith (flip (<>)) [ (k, [l]) | (k, l) <- keyed ]

            redirB :: Block -> Block
            redirB b = b { blkTerm = go $ blkTerm b }
                  where go :: Term -> Term
                        go = \ case
                              Pause an a l as -> Pause an a (re l) as
                              Goto an l as    -> Goto an (re l) as
                              TCase an a alts -> TCase an a [ TAlt aan c xs (go t) | TAlt aan c xs t <- alts ]
                              t               -> t

                        re :: Id -> Id
                        re l = fromMaybe l $ IM.lookup (idUniq l) redirect

-- | A canonical rendering of a block: binder uniques renumbered densely
--   from a disjoint range in traversal order; labels and cell names
--   compared by identity (label agreement is what the merge fixpoint
--   converges on).
blockKey :: Block -> Text
blockKey b0 = prettyPrint canon
      where canon :: Block
            canon = evalState (renB b0) (-2000000000, mempty)

            renB :: Block -> State (Uniq, IM.IntMap Uniq) Block
            renB (Block an ps cmds term) = do
                  ps'   <- mapM bind' ps
                  cmds' <- mapM renC cmds
                  term' <- renT term
                  pure $ Block an ps' cmds' term'

            bind' :: Id -> State (Uniq, IM.IntMap Uniq) Id
            bind' x = do
                  (n, m) <- get
                  put (n + 1, IM.insert (idUniq x) n m)
                  pure x { idUniq = n }

            occ' :: Id -> State (Uniq, IM.IntMap Uniq) Id
            occ' x = do
                  (_, m) <- get
                  pure $ maybe x (\ n -> x { idUniq = n }) $ IM.lookup (idUniq x) m

            renC :: Cmd -> State (Uniq, IM.IntMap Uniq) Cmd
            renC = \ case
                  CmdBind an x e -> do
                        e' <- renE e
                        x' <- bind' x
                        pure $ CmdBind an x' e'
                  CmdGet an x s  -> do
                        x' <- bind' x
                        pure $ CmdGet an x' s
                  CmdPut an s e  -> CmdPut an s <$> renE e

            renT :: Term -> State (Uniq, IM.IntMap Uniq) Term
            renT = \ case
                  Pause an a l as -> Pause an <$> renE a <*> pure l <*> mapM renE as
                  Goto an l as    -> Goto an l <$> mapM renE as
                  Halt an a       -> Halt an <$> renE a
                  TCase an a alts -> TCase an <$> renE a <*> mapM renA alts

            renA :: TAlt -> State (Uniq, IM.IntMap Uniq) TAlt
            renA (TAlt an c xs t) = do
                  xs' <- mapM bind' xs
                  TAlt an c xs' <$> renT t

            renE :: Exp -> State (Uniq, IM.IntMap Uniq) Exp
            renE e = case e of
                  Var an x        -> Var an <$> occ' x
                  App an f a      -> App an <$> renE f <*> renArg a
                  Lam an x b      -> do
                        b' <- renE b -- (binders inside expressions are rare in ANF cmds)
                        x' <- bind' x
                        pure $ Lam an x' b'
                  Let an bnd body -> do
                        bnd'  <- renBnd bnd
                        body' <- renE body
                        pure $ Let an bnd' body'
                  Jump an j es    -> Jump an j <$> mapM renE es
                  Case an t s cb alts -> do
                        s'    <- renE s
                        cb'   <- bind' cb
                        alts' <- mapM (\ (Alt aan c xs bb) -> do
                              xs' <- mapM bind' xs
                              Alt aan c xs' <$> renE bb) alts
                        pure $ Case an t s' cb' alts'
                  LitList an t es -> LitList an t <$> mapM renE es
                  LitVec an t es  -> LitVec an t <$> mapM renE es
                  _               -> pure e

            renArg :: Arg -> State (Uniq, IM.IntMap Uniq) Arg
            renArg = \ case
                  EArg e -> EArg <$> renE e
                  t      -> pure t

            renBnd :: Bind -> State (Uniq, IM.IntMap Uniq) Bind
            renBnd = \ case
                  NonRec x rhs -> do
                        rhs' <- renE rhs
                        x'   <- bind' x
                        pure $ NonRec x' rhs'
                  Rec bs       -> Rec <$> mapM (\ (x, rhs) -> (,) <$> bind' x <*> renE rhs) bs
                  Join j ps bb -> do
                        ps' <- mapM bind' ps
                        Join j ps' <$> renE bb
