{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The Eidos simplifier: occurrence-driven, let-preserving partial
--   evaluation (the retired reduce/specialize/purge loop's successor,
--   doc/eidos.md §8). 'simplify' repeats specialize-on-values, purge, and
--   reduce until every definition is synthable and dictionary-free,
--   bounded by @--depth@ with the retired loop's diagnostic.
--
--   * 'reduceExp' is let-preserving: beta redexes become lets, and a let
--     is inlined only when its binder is dead, used once, or bound to an
--     atom; a representable multi-use binding KEEPS its let (sharing).
--     The one lambda-lifting policy surviving from the retired pipeline
--     is explicit here (Clash's LiftNonRep): a multi-use *function-typed*
--     let cannot stay (it is not representable), so it is lifted to a
--     top-level definition over its captured locals, named with the
--     retired lift's @$LL.@ prefix (the retained duplicate-merge pass
--     matches on it). Top-level references are never unfolded — the
--     function hierarchy is preserved; higher-orderness dies by argument
--     baking, not call inlining.
--
--   * 'specialize' on values (the retired value-argument specializer):
--     a call to a top-level definition with *closed* arguments (free
--     variables all top-level) bakes those arguments into a memoized
--     clone named @f$s<i>@. Self-calls in the clone still reference the
--     origin — the memo closes recursive loops. Memo keys are
--     alpha-canonical: arguments are renumbered from a disjoint unique
--     supply and printed. Baking is also how non-representable scalar
--     arguments (Integer, String) disappear: their call sites pass
--     closed values.
--
--   * 'purge': definitions unreachable from the device root (plus the
--     builtin signature carriers, which the retained pipeline needs).
--
--   Join points are preserved (dead ones are dropped); datatypes are left
--   untouched (the retained pipeline prunes them).
module ReWire.Eidos.Simplify (simplify, purge, reduceProgram, reduceExp, SimpT, runSimpT) where

import ReWire.Annotation (Annote, noAnn)
import ReWire.Builtins (builtins)
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Eidos.Pretty ()
import ReWire.Eidos.Subst (nextUniq, instantiateDefn, refreshDefn, refreshExp, substVars, substVarsRefreshing, occCounts, freeUniqs)
import ReWire.Eidos.Syntax
import ReWire.Eidos.Types (typeOf, flattenApp, flattenArrow, hasArrow, synthable)
import ReWire.Pretty (prettyPrint, showt)

import Control.Monad ((>=>))
import Control.Monad.State.Strict (StateT, evalStateT, evalState, runStateT, get, put, gets, modify, lift)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.List (sortOn)
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text, intercalate)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified Data.IntMap.Strict  as IM
import qualified Data.IntSet         as IS

-- | Simplifier state: the unique supply, definitions minted this phase
--   (lifted functions and specializations, drained into the program), the
--   persistent specialization memo, and per-origin counters for clone
--   naming.
data SimpSt = SimpSt
      { stSupply :: !Uniq
      , stNew    :: ![Defn]
      , stMemo   :: !(HashMap (Uniq, Text) Defn)
      , stCnt    :: !(IM.IntMap Int)
      }

type SimpT m = StateT SimpSt m

-- | Run a simplifier action with a supply seeded above the program.
runSimpT :: Monad m => Program -> SimpT m a -> m a
runSimpT p m = evalStateT m $ SimpSt (nextUniq p) [] mempty mempty

-- | Run a Subst primitive on the simplifier's supply.
supplied :: Monad m => StateT Uniq m a -> SimpT m a
supplied m = do
      st       <- get
      (a, sup) <- lift $ runStateT m $ stSupply st
      put st { stSupply = sup }
      pure a

mint :: Monad m => Defn -> SimpT m ()
mint d = modify $ \ st -> st { stNew = stNew st <> [d] }

drainNew :: Monad m => SimpT m [Defn]
drainNew = do
      st <- get
      put st { stNew = [] }
      pure $ stNew st

primNames :: HashSet Text
primNames = Set.fromList $ map fst builtins

---
--- The driver.
---

-- | Partial evaluation to the synthable, dictionary-free fixpoint,
--   bounded by the given depth.
simplify :: forall m. MonadError AstError m => Natural -> Program -> m Program
simplify depth p = runSimpT p $ go depth p
      where go :: Natural -> Program -> SimpT m Program
            go n pr
                  | done pr   = pure pr
                  | n == 0    = failAt noAnn
                        $ "Partial evaluation not terminating (mutually recursive definitions?). Not synthable: "
                        <> intercalate ", " (stuck pr)
                  | otherwise = step pr >>= go (n - 1)

            step :: Program -> SimpT m Program
            step = specialize >=> (pure . purge) >=> reduceProgram

            done :: Program -> Bool
            done pr = all synthableDefn (progDefns pr) && dictFree pr

            stuck :: Program -> [Text]
            stuck pr = [ prettyPrint (defnId d) <> " :: " <> prettyPrint (sigTy $ idSig $ defnId d)
                       | d <- progDefns pr, not $ synthableDefn d ]

-- | Prim-named definitions are signature carriers; everything else must
--   reach a representable type.
synthableDefn :: Defn -> Bool
synthableDefn d = Set.member (idOcc $ defnId d) primNames
               || synthable (sigTy $ idSig $ defnId d)

-- | No definition's signature mentions a datatype with a function-typed
--   constructor field (a class dictionary).
dictFree :: Program -> Bool
dictFree (Program datas defns _ _) = null bad || all ok defns
      where bad :: [Text]
            bad = [ dataName d | d <- datas, any arrowField $ dataCons d ]

            arrowField :: DataCon -> Bool
            arrowField (DataCon _ _ (Sig _ t)) = any hasArrow $ fst $ flattenArrow t

            ok :: Defn -> Bool
            ok d = not $ any (`elem` bad) $ tyCons $ sigTy $ idSig $ defnId d

            tyCons :: Ty -> [Text]
            tyCons = \ case
                  TyCon _ c   -> [c]
                  TyApp _ a b -> tyCons a <> tyCons b
                  Arrow _ a b -> tyCons a <> tyCons b
                  _           -> []

---
--- Purge: definition-level DCE.
---

-- | Definitions transitively reachable from the device root, the builtin
--   signature carriers, and any process's embedded expressions. Datatypes
--   are left untouched.
purge :: Program -> Program
purge (Program datas defns procs top) = Program datas [ d | d <- defns, IS.member (idUniq $ defnId d) live ] procs top
      where tops :: IS.IntSet
            tops = IS.fromList $ map (idUniq . defnId) defns

            refs :: IM.IntMap IS.IntSet
            refs = IM.fromList [ (idUniq $ defnId d, freeUniqs (defnBody d) `IS.intersection` tops) | d <- defns ]

            roots :: [Uniq]
            roots = idUniq top
                  : [ idUniq $ defnId d | d <- defns, Set.member (idOcc $ defnId d) primNames ]
                  <> IS.toList (IS.unions (map procRefs procs) `IS.intersection` tops)

            procRefs :: Proc -> IS.IntSet
            procRefs pr = IS.unions $ map freeUniqs $ procExps pr

            procExps :: Proc -> [Exp]
            procExps pr = [ e | Just e <- map cellInit $ procCells pr ]
                  <> concatMap blockExps (procEntry pr : map snd (procBlocks pr))
                  where blockExps :: Block -> [Exp]
                        blockExps b = concatMap cmdExps (blkCmds b) <> termExps (blkTerm b)

                        cmdExps :: Cmd -> [Exp]
                        cmdExps = \ case
                              CmdBind _ _ e -> [e]
                              CmdGet {}     -> []
                              CmdPut _ _ e  -> [e]

                        termExps :: Term -> [Exp]
                        termExps = \ case
                              Pause _ a _ args -> a : args
                              Goto _ _ args    -> args
                              Halt _ a         -> [a]
                              TCase _ a alts   -> a : concatMap (\ (TAlt _ _ _ t) -> termExps t) alts

            live :: IS.IntSet
            live = go mempty roots
                  where go :: IS.IntSet -> [Uniq] -> IS.IntSet
                        go seen []       = seen
                        go seen (u : us)
                              | IS.member u seen = go seen us
                              | otherwise        = go (IS.insert u seen) $ IS.toList (IM.findWithDefault mempty u refs) <> us

---
--- Reduce: let-preserving local reduction.
---

reduceProgram :: MonadError AstError m => Program -> SimpT m Program
reduceProgram (Program datas defns procs top) = do
      let tops = IS.fromList $ map (idUniq . defnId) defns
      defns' <- mapM (\ d -> (\ b -> d { defnBody = b }) <$> reduceExp tops (defnBody d)) defns
      new    <- drainNew
      pure $ Program datas (defns' <> new) procs top

-- | One bottom-up reduction pass over an expression: beta redexes become
--   lets; lets inline when dead, single-use, or atom-bound; multi-use
--   function-typed lets are lifted to top level (LiftNonRep); known-
--   constructor and known-literal cases select their alternative; lambdas
--   eta-reduce; dead join points drop. Top-level references are never
--   unfolded. The first argument is the set of top-level uniques (a
--   lift's captures are the free variables outside it).
reduceExp :: forall m. MonadError AstError m => IS.IntSet -> Exp -> SimpT m Exp
reduceExp tops = go
      where go :: Exp -> SimpT m Exp
            go e = case e of
                  App an f a -> do
                        f' <- go f
                        a' <- goArg a
                        case (f', a') of
                              (Lam _ x b, EArg rhs) -> go $ Let an (NonRec x rhs) b
                              _                     -> pure $ App an f' a'
                  Lam an x b -> etaReduce an x <$> go b
                  Let an (NonRec x rhs) body -> do
                        rhs' <- go rhs
                        if atomic rhs'
                              then go $ substVars (IM.singleton (idUniq x) rhs') body
                              else do
                                    body' <- go body
                                    let occ = IM.findWithDefault 0 (idUniq x) $ occCounts body'
                                    if | occ == 0                   -> pure body'
                                       | occ == 1                   -> go $ substVars (IM.singleton (idUniq x) rhs') body'
                                       | hasArrow (sigTy $ idSig x) -> do
                                             ref <- liftNonRep an x rhs'
                                             pure $ substVars (IM.singleton (idUniq x) ref) body'
                                       | otherwise                  -> pure $ Let an (NonRec x rhs') body'
                  Let an (Rec bs) body -> do
                        bs'   <- mapM (\ (x, rhs) -> (x, ) <$> go rhs) bs
                        body' <- go body
                        let occs = occCounts body'
                        if any (\ (x, _) -> IM.member (idUniq x) occs) bs'
                              then pure $ Let an (Rec bs') body'
                              else pure body'
                  Let an (Join j ps b) body -> do
                        b'    <- go b
                        body' <- go body
                        if IM.member (idUniq $ jpId j) $ occCounts body'
                              then pure $ Let an (Join j ps b') body'
                              else pure body'
                  Jump an j es -> Jump an j <$> mapM go es
                  Case an t scrut cb alts -> do
                        scrut' <- go scrut
                        case selectAlt scrut' alts of
                              Just (Alt aan _ xs rhs, args) -> go
                                    $ Let an (NonRec cb scrut')
                                    $ foldr (\ (x, a) acc -> Let aan (NonRec x a) acc) rhs
                                    $ zip xs args
                              Nothing -> do
                                    alts' <- mapM (\ (Alt aan c xs b) -> Alt aan c xs <$> go b) alts
                                    pure $ Case an t scrut' cb alts'
                  LitList an t es -> LitList an t <$> mapM go es
                  LitVec an t es  -> LitVec an t <$> mapM go es
                  Var {}          -> pure e
                  Con {}          -> pure e
                  Prim {}         -> pure e
                  LitInt {}       -> pure e
                  LitStr {}       -> pure e

            goArg :: Arg -> SimpT m Arg
            goArg = \ case
                  EArg e -> EArg <$> go e
                  t      -> pure t

            -- \ x -> f x  ==>  f   (x not free in f)
            etaReduce :: Annote -> Id -> Exp -> Exp
            etaReduce an x = \ case
                  App _ f (EArg (Var _ x'))
                        | idUniq x' == idUniq x
                        , not $ IM.member (idUniq x) $ occCounts f -> f
                  b -> Lam an x b

            -- Substitutable everywhere without loss of sharing.
            atomic :: Exp -> Bool
            atomic = \ case
                  Var {}    -> True
                  LitInt {} -> True
                  LitStr {} -> True
                  Prim {}   -> True
                  Con _ t _ -> null $ fst $ flattenArrow t -- nullary constructor
                  _         -> False

            -- LiftNonRep: a multi-use function-typed binding becomes a
            -- top-level definition over its captured locals; occurrences
            -- become an application to those locals.
            liftNonRep :: Annote -> Id -> Exp -> SimpT m Exp
            liftNonRep an x rhs = do
                  let caps = capturedIds (freeUniqs rhs IS.\\ tops) rhs
                      t    = foldr (Arrow an . sigTy . idSig) (typeOf rhs) caps
                  d <- supplied $ refreshDefn Defn
                        { defnAnnote = an
                        , defnId     = x { idOcc = "$LL." <> idOcc x, idSig = Sig [] t }
                        , defnParams = caps
                        , defnBody   = rhs
                        , defnAttr   = Nothing
                        , defnOrigin = Nothing
                        }
                  mint d
                  pure $ foldl (\ f c -> App an f $ EArg $ Var an c) (Var an $ defnId d) caps

            -- The captured locals' Ids: every occurrence carries its
            -- binder's signature, so the first occurrence serves.
            capturedIds :: IS.IntSet -> Exp -> [Id]
            capturedIds us rhs = mapMaybe (`IM.lookup` occIds) $ IS.toList us
                  where occIds :: IM.IntMap Id
                        occIds = collect rhs

                        collect :: Exp -> IM.IntMap Id
                        collect = \ case
                              Var _ v         -> IM.singleton (idUniq v) v
                              App _ f a       -> collect f <> collectArg a
                              Lam _ _ b       -> collect b
                              Let _ b body    -> collectBind b <> collect body
                              Jump _ _ es     -> IM.unions $ map collect es
                              Case _ _ s _ as -> collect s <> IM.unions [ collect b | Alt _ _ _ b <- as ]
                              LitList _ _ es  -> IM.unions $ map collect es
                              LitVec _ _ es   -> IM.unions $ map collect es
                              _               -> mempty

                        collectArg :: Arg -> IM.IntMap Id
                        collectArg = \ case
                              EArg e -> collect e
                              _      -> mempty

                        collectBind :: Bind -> IM.IntMap Id
                        collectBind = \ case
                              NonRec _ rhs' -> collect rhs'
                              Rec bs        -> IM.unions $ map (collect . snd) bs
                              Join _ _ b    -> collect b

-- | Select the alternative for a known scrutinee: a constructor-spine
--   scrutinee picks its 'DataAlt' (fields as a parallel argument list) or
--   the default; a literal scrutinee picks its 'LitAlt' or the default.
--   Unknown scrutinees select nothing.
selectAlt :: Exp -> [Alt] -> Maybe (Alt, [Exp])
selectAlt scrut alts = case flattenApp scrut of
      (Con _ _ c, args) ->
            let es = [ a | EArg a <- args ]
            in case [ alt | alt@(Alt _ (DataAlt c') _ _) <- alts, c' == c ] of
                  alt@(Alt _ _ xs _) : _
                        | length xs == length es -> Just (alt, es)
                        | otherwise              -> Nothing -- ill-arity (unreachable on lint-clean input)
                  [] -> (, []) <$> defaultAlt
      (LitInt _ _ n, []) -> case [ alt | alt@(Alt _ (LitAlt n') _ _) <- alts, n' == n ] of
                  alt : _ -> Just (alt, [])
                  []      -> (, []) <$> defaultAlt
      _ -> Nothing
      where defaultAlt :: Maybe Alt
            defaultAlt = case alts of
                  alt@(Alt _ DefaultAlt _ _) : _ -> Just alt
                  _                              -> Nothing

---
--- Specialize on values (argument baking).
---

-- | Bake closed arguments of calls to top-level definitions into memoized
--   clones. An argument is bakeable when all its free variables are
--   top-level; a definition participates unless NOINLINE or prim-named.
--   Leading body lambdas are first promoted into the parameter telescope
--   (the bridge emits every definition with an empty telescope).
specialize :: forall m. MonadError AstError m => Program -> SimpT m Program
specialize (Program datas defns0 procs top) = do
      defns' <- mapM specDefn defns
      new    <- drainNew
      -- Re-add memoized specializations dropped by an earlier purge but
      -- possibly referenced again (the retired specializer's behavior;
      -- the next purge removes unused ones). Sorted for determinism.
      let have = IS.fromList $ map (idUniq . defnId) $ defns' <> new
      memoed <- gets $ sortOn (idUniq . defnId) . filter (\ d -> not $ IS.member (idUniq $ defnId d) have) . Map.elems . stMemo
      pure $ Program datas (defns' <> new <> memoed) procs top
      where defns :: [Defn]
            defns = map promote defns0

            -- Promote leading body lambdas into the parameter telescope,
            -- up to the signature's arity.
            promote :: Defn -> Defn
            promote d = d { defnParams = defnParams d <> ps, defnBody = b }
                  where arity     = length $ fst $ flattenArrow $ sigTy $ idSig $ defnId d
                        (ps, b)   = peel (arity - length (defnParams d)) $ defnBody d

                        peel :: Int -> Exp -> ([Id], Exp)
                        peel n (Lam _ x e) | n > 0 = let (xs, e') = peel (n - 1) e in (x : xs, e')
                        peel _ e                   = ([], e)

            tops :: IS.IntSet
            tops = IS.fromList $ map (idUniq . defnId) defns

            gs :: IM.IntMap Defn
            gs = IM.fromList [ (idUniq $ defnId d, d) | d <- defns ]

            specDefn :: Defn -> SimpT m Defn
            specDefn d = do
                  b <- specExp $ defnBody d
                  pure d { defnBody = b }

            specExp :: Exp -> SimpT m Exp
            specExp e = case e of
                  App an _ _ -> do
                        let (h, args) = flattenApp e
                        args' <- mapM specArg args
                        let eas = [ a | EArg a <- args' ]
                        case h of
                              Var _ g | Just d <- IM.lookup (idUniq g) gs
                                        , inlinable d
                                        , not $ null $ defnParams d
                                        , bs <- zipWith (\ _ a -> if closed a then Just a else Nothing) (defnParams d) eas
                                        , not $ all isNothing bs -> do
                                    g' <- specialization d bs
                                    let kept = [ EArg a | (b, a) <- zip (bs <> repeat Nothing) eas, isNothing b ]
                                    pure $ foldl (App an) (Var an g') kept
                              _ -> do
                                    h' <- specExp h
                                    pure $ foldl (App an) h' args'
                  Lam an x b      -> Lam an x <$> specExp b
                  Let an b body   -> Let an <$> specBind b <*> specExp body
                  Jump an j es    -> Jump an j <$> mapM specExp es
                  Case an t s cb alts -> do
                        s'    <- specExp s
                        alts' <- mapM (\ (Alt aan c xs b) -> Alt aan c xs <$> specExp b) alts
                        pure $ Case an t s' cb alts'
                  LitList an t es -> LitList an t <$> mapM specExp es
                  LitVec an t es  -> LitVec an t <$> mapM specExp es
                  _               -> pure e

            specArg :: Arg -> SimpT m Arg
            specArg = \ case
                  EArg a -> EArg <$> specExp a
                  t      -> pure t

            specBind :: Bind -> SimpT m Bind
            specBind = \ case
                  NonRec x rhs -> NonRec x <$> specExp rhs
                  Rec bs       -> Rec <$> mapM (\ (x, rhs) -> (x, ) <$> specExp rhs) bs
                  Join j ps b  -> Join j ps <$> specExp b

            closed :: Exp -> Bool
            closed a = freeUniqs a `IS.isSubsetOf` tops

            inlinable :: Defn -> Bool
            inlinable d = defnAttr d /= Just NoInline
                       && not (Set.member (idOcc $ defnId d) primNames)

            -- The memoized clone for a definition at the given argument
            -- baking (Just = bake).
            specialization :: Defn -> [Maybe Exp] -> SimpT m Id
            specialization d bs = do
                  let key = (idUniq $ defnId d, canonKey bs)
                  gets (Map.lookup key . stMemo) >>= \ case
                        Just d' -> pure $ defnId d'
                        Nothing -> do
                              d' <- mkSpec d bs
                              modify $ \ st -> st { stMemo = Map.insert key d' $ stMemo st }
                              mint d'
                              pure $ defnId d'

            -- Bake: clone the definition (self-references still name the
            -- origin — the memo closes recursive loops on the next round),
            -- substitute the baked arguments for the corresponding cloned
            -- parameters, and drop them from the parameter list and the
            -- signature's arrow spine.
            mkSpec :: Defn -> [Maybe Exp] -> SimpT m Defn
            mkSpec d bs = do
                  i  <- gets $ (+ 1) . IM.findWithDefault 0 (idUniq $ defnId d) . stCnt
                  modify $ \ st -> st { stCnt = IM.insert (idUniq $ defnId d) i $ stCnt st }
                  dr <- supplied $ instantiateDefn (idOcc (defnId d) <> "$s" <> showt i) [] d
                  let an          = defnAnnote dr
                      ps          = defnParams dr
                      bs'         = bs <> repeat Nothing
                      baked       = IM.fromList [ (idUniq p, a) | (p, Just a) <- zip ps bs ]
                      keptPs      = [ p | (p, b) <- zip ps bs', isNothing b ]
                      Sig _ t     = idSig $ defnId dr
                      (doms, res) = flattenArrow t
                      domsKept    = [ dom | (dom, b) <- zip (take (length ps) doms) bs', isNothing b ]
                                    <> drop (length ps) doms
                      x'          = (defnId dr) { idSig = Sig [] $ foldr (Arrow an) res domsKept }
                  body' <- supplied $ substVarsRefreshing baked $ defnBody dr
                  pure $ Defn an x' keptPs body' (defnAttr d) (defnOrigin d)

            -- Alpha-canonical text of the baked-argument list: renumber
            -- binders from a disjoint (large negative) supply, then print.
            canonKey :: [Maybe Exp] -> Text
            canonKey = intercalate "|" . map (maybe "_" pp)
                  where pp :: Exp -> Text
                        pp a = prettyPrint (evalState (refreshExp a) canonBase :: Exp)

canonBase :: Uniq
canonBase = -1000000000
