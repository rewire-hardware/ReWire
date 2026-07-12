{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Safe #-}
-- | Hyle-to-Hyle transformations. Currently: defn inlining (the
--   uses-at-most-once heuristic that used to live in each RTL backend, plus
--   flatten-everything) and dead-defn purging. Inlining is sound by the
--   substitution property (doc/hyle.md, section 7); bound names in inlined
--   bodies are freshened to avoid capture. Defns are processed callee-first
--   (the call graph is acyclic), so chains of single-use defns collapse
--   fully.
module ReWire.Hyle.Transform (inline, inlineBy, purgeUnused, partialEval, purgeDevLets, purgeZeroWidth, dedupe, optimize, hoistInstances) where

import ReWire.Annotation (Annote)
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Fix (fixPure)
import ReWire.Hyle.Interp (evalExp, evalOp, IEnv (..))
import ReWire.Hyle.Syntax
import ReWire.Pretty (showt)

import ReWire.BitVector (BV, nat)
import Control.Arrow ((>>>))
import Control.Monad (foldM)
import Numeric.Natural (Natural)

import qualified ReWire.BitVector as BV

import Control.Monad.State.Strict (State, StateT, evalState, evalStateT, get, put, gets, modify)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.List (sort)
import Data.Maybe (fromMaybe)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified Data.Text           as T

-- | Inline definitions into their call sites: all of them when @flatten@,
--   otherwise those used at most once. Definitions referenced as extern
--   models are never inlined (the reference is by name), and neither are
--   defns marked noinline -- the pin wins over @--flatten@, so a
--   noinline defn is always findable as a module in the output. The
--   inlined-away defns are removed by the final purge; a follow-up
--   partialEval fuses the slice/concat plumbing exposed by inlining and
--   drops the argument wires that fusion leaves unused.
inline :: Bool -> Program -> Program
inline flatten p@(Program exts ds dev) = (partialEval >>> purgeDevLets >>> purgeUnused) $ inlineBy inlinable p
      where inlinable :: GId -> Bool
            inlinable g = not (g `Set.member` models)
                       && not (g `Set.member` pinned)
                       && (flatten || Map.findWithDefault 0 g uses <= (1 :: Int))

            models :: HashSet GId
            models = Set.fromList [ g | e <- exts, Just g <- [extModel e] ]

            pinned :: HashSet GId
            pinned = Set.fromList [ defnName d | d <- ds, defnNoInline d ]

            uses :: HashMap GId Int
            uses = Map.fromListWith (+) $ map (, 1) $ concatMap expCalls
                 $ map defnBody ds <> map stmtExp (devBody dev)

-- | Inline definitions satisfying the predicate into their call sites.
inlineBy :: (GId -> Bool) -> Program -> Program
inlineBy inlinable (Program exts ds dev) = purgeUnused $ Program exts ds' dev'
      where -- | Each defn with inlining applied, built callee-first so a
            --   body pulled in by 'beta' is already fully inlined.
            doneMap :: HashMap GId Defn
            doneMap = foldl' step mempty $ topo ds

            step :: HashMap GId Defn -> Defn -> HashMap GId Defn
            step m d = Map.insert (defnName d) (d { defnBody = inlineExp m $ defnBody d }) m

            ds' :: [Defn]
            ds' = [ fromMaybe d $ Map.lookup (defnName d) doneMap | d <- ds ]

            dev' :: Device
            dev' = dev { devBody = map inlineStmt $ devBody dev }

            inlineStmt :: Stmt -> Stmt
            inlineStmt = \ case
                  SLet an x e      -> SLet an x $ inlineExp doneMap e
                  SOutput an x e   -> SOutput an x $ inlineExp doneMap e
                  SNext an x e     -> SNext an x $ inlineExp doneMap e
                  SInstIn an x q e -> SInstIn an x q $ inlineExp doneMap e

            -- | Callee-first (post-order DFS over the call graph).
            topo :: [Defn] -> [Defn]
            topo defs = reverse $ snd $ foldl' visit (mempty, []) defs
                  where defMap :: HashMap GId Defn
                        defMap = Map.fromList $ map (\ d -> (defnName d, d)) defs

                        visit :: (HashSet GId, [Defn]) -> Defn -> (HashSet GId, [Defn])
                        visit acc@(seen, out) d
                              | defnName d `Set.member` seen = acc
                              | otherwise =
                                    let (seen', out') = foldl' visitName (Set.insert (defnName d) seen, out)
                                                      $ expCalls $ defnBody d
                                    in (seen', d : out')

                        visitName :: (HashSet GId, [Defn]) -> GId -> (HashSet GId, [Defn])
                        visitName acc g = case Map.lookup g defMap of
                              Just d  -> visit acc d
                              Nothing -> acc

            inlineExp :: HashMap GId Defn -> Exp -> Exp
            inlineExp done e0 = evalState (go e0) 0
                  where go :: Exp -> State Int Exp
                        go = \ case
                              e@Lit {}            -> pure e
                              e@Undef {}          -> pure e
                              e@Var {}            -> pure e
                              Cat an e1 e2        -> Cat an <$> go e1 <*> go e2
                              Slice an i k e      -> Slice an i k <$> go e
                              Prim an sz op es    -> Prim an sz op <$> mapM go es
                              XCall an sz x cs es -> XCall an sz x cs <$> mapM go es
                              If an sz c t e      -> If an sz <$> go c <*> go t <*> go e
                              Let an sz x e1 e2   -> Let an sz x <$> go e1 <*> go e2
                              Call an sz g es     -> do
                                    es' <- mapM go es
                                    case Map.lookup g done of
                                          Just d | inlinable g -> beta an d es'
                                          _                    -> pure $ Call an sz g es'

                        -- | Substitute the arguments into the (freshened)
                        --   body: atomic arguments (variables, literals)
                        --   substitute directly; the rest are bound to
                        --   freshened parameters. The body is already fully
                        --   inlined (callee-first order).
                        beta :: Annote -> Defn -> [Exp] -> State Int Exp
                        beta an (Defn _ _ _ ps body _ _) es = do
                              binds <- mapM bind $ zip ps es
                              body' <- freshenExp (Map.fromList $ map fst binds) body
                              pure $ foldr (\ (x, e) b -> Let an (sizeOf b) x e b) body' $ concatMap snd binds
                              where bind :: (Name, Exp) -> State Int ((Name, Exp), [(Name, Exp)])
                                    bind (p, e) | atomic e  = pure ((p, e), [])
                                                | otherwise = do
                                                      p' <- freshName
                                                      pure ((p, Var an (sizeOf e) p'), [(p', e)])

                                    atomic :: Exp -> Bool
                                    atomic = \ case
                                          Var {}   -> True
                                          Lit {}   -> True
                                          Undef {} -> True
                                          _        -> False

                        freshName :: State Int Name
                        freshName = do
                              i <- gets id
                              modify (+ 1)
                              pure $ "$i" <> showt (i :: Int)

                        -- | Substitute the given free names and rename all
                        --   bound names.
                        freshenExp :: HashMap Name Exp -> Exp -> State Int Exp
                        freshenExp m = \ case
                              e@Lit {}            -> pure e
                              e@Undef {}          -> pure e
                              e@(Var _ _ x)       -> pure $ fromMaybe e $ Map.lookup x m
                              Cat an e1 e2        -> Cat an <$> freshenExp m e1 <*> freshenExp m e2
                              Slice an i k e      -> Slice an i k <$> freshenExp m e
                              Prim an sz op es    -> Prim an sz op <$> mapM (freshenExp m) es
                              XCall an sz x cs es -> XCall an sz x cs <$> mapM (freshenExp m) es
                              If an sz c t e      -> If an sz <$> freshenExp m c <*> freshenExp m t <*> freshenExp m e
                              Call an sz g es     -> Call an sz g <$> mapM (freshenExp m) es
                              Let an sz x e1 e2   -> do
                                    e1' <- freshenExp m e1
                                    x'  <- freshName
                                    Let an sz x' e1' <$> freshenExp (Map.insert x (Var an (sizeOf e1) x') m) e2

stmtExp :: Stmt -> Exp
stmtExp = \ case
      SLet _ _ e      -> e
      SOutput _ _ e   -> e
      SNext _ _ e     -> e
      SInstIn _ _ _ e -> e

expFreeVars :: Exp -> HashSet Name
expFreeVars = \ case
      Lit {}            -> mempty
      Undef {}          -> mempty
      Var _ _ x         -> Set.singleton x
      Cat _ e1 e2       -> expFreeVars e1 <> expFreeVars e2
      Slice _ _ _ e     -> expFreeVars e
      Prim _ _ _ es     -> foldMap expFreeVars es
      XCall _ _ _ _ es  -> foldMap expFreeVars es
      If _ _ c t e      -> expFreeVars c <> expFreeVars t <> expFreeVars e
      Let _ _ x e1 e2   -> expFreeVars e1 <> Set.delete x (expFreeVars e2)
      Call _ _ _ es     -> foldMap expFreeVars es

expCalls :: Exp -> [GId]
expCalls = \ case
      Lit {}            -> []
      Undef {}          -> []
      Var {}            -> []
      Cat _ e1 e2       -> expCalls e1 <> expCalls e2
      Slice _ _ _ e     -> expCalls e
      Prim _ _ _ es     -> concatMap expCalls es
      XCall _ _ _ _ es  -> concatMap expCalls es
      If _ _ c t e      -> expCalls c <> expCalls t <> expCalls e
      Let _ _ _ e1 e2   -> expCalls e1 <> expCalls e2
      Call _ _ g es     -> g : concatMap expCalls es

-- | Drop defns unreachable from the device body (following calls and the
--   models of called externs) and extern decls that are never referenced.
--   Not gated on noinline: pinning affects inlining, not liveness, so dead
--   pinned defns don't accumulate.
purgeUnused :: Program -> Program
purgeUnused (Program exts ds dev) = Program exts' ds' dev
      where defnMap :: HashMap GId Defn
            defnMap = Map.fromList $ map (\ d -> (defnName d, d)) ds

            extMap :: HashMap Name Extern
            extMap = Map.fromList $ map (\ e -> (extName e, e)) exts

            (liveDefns, liveExts) = foldl' visitExp (mempty, Set.fromList [ ex | Instance _ _ ex _ <- devInstances dev ])
                  $ map stmtExp $ devBody dev

            visitExp :: (HashSet GId, HashSet Name) -> Exp -> (HashSet GId, HashSet Name)
            visitExp acc@(lds, lxs) = \ case
                  Lit {}           -> acc
                  Undef {}         -> acc
                  Var {}           -> acc
                  Cat _ e1 e2      -> visitExp (visitExp acc e1) e2
                  Slice _ _ _ e    -> visitExp acc e
                  Prim _ _ _ es    -> foldl' visitExp acc es
                  If _ _ c t e     -> foldl' visitExp acc [c, t, e]
                  Let _ _ _ e1 e2  -> visitExp (visitExp acc e1) e2
                  Call _ _ g es    -> foldl' visitExp (visitDefn acc g) es
                  XCall _ _ x _ es -> foldl' visitExp acc' es
                        where acc' = case Map.lookup x extMap >>= extModel of
                                    Just g  -> visitDefn (lds, Set.insert x lxs) g
                                    Nothing -> (lds, Set.insert x lxs)

            visitDefn :: (HashSet GId, HashSet Name) -> GId -> (HashSet GId, HashSet Name)
            visitDefn acc@(lds, lxs) g
                  | g `Set.member` lds             = acc
                  | Just d <- Map.lookup g defnMap = visitExp (Set.insert g lds, lxs) $ defnBody d
                  | otherwise                      = acc

            ds' :: [Defn]
            ds' = filter ((`Set.member` liveDefns) . defnName) ds

            exts' :: [Extern]
            exts' = filter ((`Set.member` liveExts) . extName) exts

---
--- Optimization passes: slice merging, partial evaluation, dedupe.
---

-- | The standard optimization pipeline, iterated to a fixpoint (bounded by
--   the --rtl-opt level).
optimize :: Natural -> Program -> Program
optimize n = fixPure n $ partialEval >>> purgeDevLets >>> purgeZeroWidth >>> dedupe >>> purgeUnused

-- | Constant folding and wire fusion. Primitives, muxes, slices, and
--   concatenations of literals fold via the interpreter's own evaluator;
--   calls (and extern calls with models) whose arguments are all literals
--   evaluate fully. Lets bound to atomic expressions (literals, variables,
--   don't-cares) substitute away; lets bound to pure wiring (slices and
--   concatenations of atoms) are fused through where they are sliced, so
--   slice-of-concat plumbing simplifies across named wires (both expression
--   lets and device-level wires). Unused expression lets are dropped here;
--   unused device wires are dropped by 'purgeDevLets'.
partialEval :: Program -> Program
partialEval (Program exts ds dev) = Program exts (map peDefn ds) dev'
      where env :: IEnv
            env = IEnv (Map.fromList $ map (\ d -> (defnName d, d)) ds)
                       (Map.fromList $ map (\ e -> (extName e, e)) exts)

            peDefn :: Defn -> Defn
            peDefn d = d { defnBody = pe mempty $ defnBody d }

            dev' :: Device
            dev' = dev { devBody = snd $ foldl' peStmt (mempty, []) $ devBody dev }

            -- | Device wires bound to pure wiring join the environment so
            --   later statements fuse through them; the wires themselves
            --   stay (purgeDevLets drops the ones fusion leaves unused).
            peStmt :: (HashMap Name Exp, [Stmt]) -> Stmt -> (HashMap Name Exp, [Stmt])
            peStmt (binds, acc) = \ case
                  SLet an x e ->
                        let e' = pe binds e
                        in (if wiring e' then Map.insert x e' binds else binds, acc <> [SLet an x e'])
                  SOutput an x e   -> (binds, acc <> [SOutput an x $ pe binds e])
                  SNext an x e     -> (binds, acc <> [SNext an x $ pe binds e])
                  SInstIn an x q e -> (binds, acc <> [SInstIn an x q $ pe binds e])

            pe :: HashMap Name Exp -> Exp -> Exp
            pe binds = \ case
                  e@(Var _ _ x)
                        | Just b <- Map.lookup x binds, atomic b -> b
                        | otherwise                              -> e
                  e@Lit {}   -> e
                  e@Undef {} -> e
                  Cat an e1 e2 -> mergeCat an $ concatMap gather [pe binds e1, pe binds e2]
                  Slice an i k e -> peSlice binds an i k $ pe binds e
                  Prim an sz op es ->
                        let es' = map (pe binds) es in
                        case mapM litVal es' of
                              Just bvs | Right bv <- evalOp an op bvs -> Lit an bv
                              _ -> case (op, es') of
                                    -- The SMT-LIB division-by-zero equations
                                    -- (doc/hyle.md, section 5.2), applicable
                                    -- whenever the divisor is a zero literal.
                                    (UDiv, [_, Lit _ bv]) | nat bv == 0 -> Lit an $ BV.ones $ fromIntegral sz
                                    (UMod, [a, Lit _ bv]) | nat bv == 0 -> a
                                    -- Boolean-mux peephole (1-bit only:
                                    -- Hyle widths are load-bearing).
                                    -- Comparing a 1-bit value to a
                                    -- 1-bit literal is the value or its
                                    -- negation; double negation cancels.
                                    (Eq, [a, Lit _ bv]) | sizeOf a == 1, BV.width bv == 1 ->
                                          if nat bv == 1 then a else pe binds $ Prim an 1 Not [a]
                                    (Eq, [Lit _ bv, a]) | sizeOf a == 1, BV.width bv == 1 ->
                                          if nat bv == 1 then a else pe binds $ Prim an 1 Not [a]
                                    (Not, [Prim _ _ Not [a]])           -> a
                                    _                                   -> Prim an sz op es'
                  Call an sz g es ->
                        let es' = map (pe binds) es in
                        case mapM litVal es' of
                              Just _ | Right bv <- evalExp env mempty (Call an sz g es') -> Lit an bv
                              _                                                          -> Call an sz g es'
                  XCall an sz x cs es ->
                        let es' = map (pe binds) es in
                        case mapM litVal es' of
                              Just _ | Just ex <- Map.lookup x $ envExterns env
                                     , Just _  <- extModel ex
                                     , Right bv <- evalExp env mempty (XCall an sz x cs es') -> Lit an bv
                              _                                                              -> XCall an sz x cs es'
                  If an sz c t e -> case (pe binds c, pe binds t, pe binds e) of
                        (Lit _ bv, t', e') -> if nat bv /= 0 then t' else e'
                        -- Boolean-mux peephole: a 1-bit mux between the
                        -- two 1-bit literals is the condition or its
                        -- negation (which re-enters 'pe' so double
                        -- negations cancel).
                        (c', t', e')
                              | sz == 1, sizeOf c' == 1
                              , Just tv <- litVal t', Just ev <- litVal e'
                              , BV.width tv == 1, BV.width ev == 1, nat tv /= nat ev ->
                                    if nat tv == 1 then c' else pe binds $ Prim an 1 Not [c']
                              | otherwise -> If an sz c' t' e'
                  Let an sz x e1 e2 ->
                        let e1'    = pe binds e1
                            -- Invalidate bindings an inner shadowing let
                            -- would capture (shadowing can only come from
                            -- hand-written input).
                            binds' = Map.filter (not . Set.member x . expFreeVars) $ Map.delete x binds
                            e2'    = pe (if wiring e1' then Map.insert x e1' binds' else binds') e2
                        in if x `Set.member` expFreeVars e2' then Let an sz x e1' e2' else e2'

            litVal :: Exp -> Maybe BV
            litVal = \ case
                  Lit _ bv -> Just bv
                  _        -> Nothing

            -- | Pure wiring: evaluates to a rearrangement of its free
            --   variables' bits, with no logic. Fusing through wiring (or
            --   replicating it) duplicates no hardware.
            wiring :: Exp -> Bool
            wiring = \ case
                  Lit {}        -> True
                  Undef {}      -> True
                  Var {}        -> True
                  Cat _ e1 e2   -> wiring e1 && wiring e2
                  Slice _ _ _ e -> wiring e
                  _             -> False

            atomic :: Exp -> Bool
            atomic = \ case
                  Lit {}   -> True
                  Undef {} -> True
                  Var {}   -> True
                  _        -> False

            -- | Adjacent literals, don't-cares, and slices of the same base
            --   merge; a single piece stands alone.
            mergeCat :: Annote -> [Exp] -> Exp
            mergeCat an = go' >>> \ case
                  []  -> Lit an mempty
                  [e] -> e
                  es  -> foldr1 (Cat an) es
                  where go' :: [Exp] -> [Exp]
                        go' = \ case
                              Lit a bv : Lit _ bv' : es   -> go' $ Lit a (bv <> bv') : es
                              Undef a n : Undef _ m : es  -> go' $ Undef a (n + m) : es
                              -- The left piece supplies the high bits, so
                              -- adjacency means the left slice begins where
                              -- the right one ends.
                              Slice a iL kL b : Slice _ iR kR b' : es
                                    | b == b', iL == iR + kR -> go' $ unSlice (Slice a iR (kL + kR) b) : es
                              e : es                      -> e : go' es
                              []                          -> []

                        unSlice :: Exp -> Exp
                        unSlice = \ case
                              Slice _ 0 k e | k == sizeOf e -> e
                              e                             -> e

            -- | Slices of literals and don't-cares, identity slices, slices
            --   of slices, slices of concatenations (split at the piece
            --   boundaries), and slices of named wires bound to pure wiring
            --   (fused through the binding).
            peSlice :: HashMap Name Exp -> Annote -> Index -> Size -> Exp -> Exp
            peSlice binds an i k e
                  | k == 0                = Lit an mempty
                  | i == 0, k == sizeOf e = e
                  | otherwise = case e of
                        Lit _ bv        -> Lit an $ subBV bv i k
                        Undef _ _       -> Undef an k
                        Slice _ i' _ e' -> peSlice binds an (i + i') k e'
                        Var _ _ x | Just b <- Map.lookup x binds, not (atomic b)
                                        -> peSlice binds an i k b
                        Cat {}          -> mergeCat an $ map subPiece $ filter overlaps pieces
                              where pieces :: [(Exp, Index)] -- MSB-first, with LSB offsets
                                    pieces = snd $ foldr (\ e' (o, acc) -> (o + sizeOf e', (e', o) : acc)) (0, []) $ gather e

                                    overlaps :: (Exp, Index) -> Bool
                                    overlaps (e', off) = off < i + k && i < off + sizeOf e'

                                    subPiece :: (Exp, Index) -> Exp
                                    subPiece (e', off) = peSlice binds an (max i off - off) (min (i + k) (off + sizeOf e') - max i off) e'
                        _               -> Slice an i k e

            subBV :: BV -> Index -> Size -> BV
            subBV bv i k = BV.bitVec (fromIntegral k) $ nat bv `div` (2 ^ toInteger i)

-- | Drop device wires (SLets) never read by a later statement (fusion in
--   'partialEval' is what strands them).
purgeDevLets :: Program -> Program
purgeDevLets (Program exts ds dev) = Program exts ds dev'
      where dev' :: Device
            dev' = dev { devBody = snd $ foldr keep (mempty, []) $ devBody dev }

            keep :: Stmt -> (HashSet Name, [Stmt]) -> (HashSet Name, [Stmt])
            keep s (used, acc) = case s of
                  SLet _ x e | not (x `Set.member` used) -> (used, acc)
                             | otherwise                 -> (used <> expFreeVars e, s : acc)
                  _                                      -> (used <> expFreeVars (stmtExp s), s : acc)

-- | Drop zero-width parameters (and the corresponding arguments at call
--   sites) and zero-width-result defns (calls to them become nil).
purgeZeroWidth :: Program -> Program
purgeZeroWidth (Program exts ds dev) = Program exts (map goDefn $ filter ((> 0) . sizeOf) ds) dev'
      where sigs :: HashMap GId Sig
            sigs = Map.fromList $ map (\ d -> (defnName d, defnSig d)) ds

            goDefn :: Defn -> Defn
            goDefn (Defn an g (Sig san argSzs res) ps body ni docs) = Defn an g (Sig san (filter (> 0) argSzs) res) ps' (goExp body) ni docs
                  where ps' = [ p | (p, sz) <- zip ps argSzs, sz > 0 ]

            dev' :: Device
            dev' = dev { devBody = map goStmt $ devBody dev }

            goStmt :: Stmt -> Stmt
            goStmt = \ case
                  SLet an x e      -> SLet an x $ goExp e
                  SOutput an x e   -> SOutput an x $ goExp e
                  SNext an x e     -> SNext an x $ goExp e
                  SInstIn an x q e -> SInstIn an x q $ goExp e

            goExp :: Exp -> Exp
            goExp = \ case
                  e@Lit {}   -> e
                  e@Undef {} -> e
                  e@Var {}   -> e
                  Cat an e1 e2 -> Cat an (goExp e1) $ goExp e2
                  Slice an i k e -> Slice an i k $ goExp e
                  Prim an sz op es -> Prim an sz op $ map goExp es
                  XCall an sz x cs es -> XCall an sz x cs $ map goExp es
                  If an sz c t e -> If an sz (goExp c) (goExp t) $ goExp e
                  Let an sz x e1 e2 -> Let an sz x (goExp e1) $ goExp e2
                  Call an sz g es
                        | sz == 0   -> Lit an mempty
                        | otherwise -> case Map.lookup g sigs of
                              Just (Sig _ argSzs _) -> Call an sz g [ goExp e | (e, asz) <- zip es argSzs, asz > 0 ]
                              Nothing               -> Call an sz g $ map goExp es

-- | Redirect calls to definitions with identical signatures and bodies to a
--   single representative. Should be followed by purgeUnused. Defns marked
--   noinline are name-pinned: they take no part in merging (neither as
--   survivors nor as redirected losers). Each survivor records the losers
--   redirected to it with an appended "also: ..." doc line.
dedupe :: Program -> Program
dedupe (Program exts ds dev) = Program (map ddExt exts) (map (addAlso . ddDefn) ds) dev'
      where ddDefn :: Defn -> Defn
            ddDefn d = d { defnBody = ddExp $ defnBody d }

            -- | Append one doc line to each survivor naming the losers
            --   redirected to it (sorted; always distinct from the
            --   survivor by ddMap's construction).
            addAlso :: Defn -> Defn
            addAlso d = case Map.lookup (defnName d) losers of
                  Just ls -> d { defnDoc = Blind $ unBlind (defnDoc d) <> ["also: " <> T.intercalate ", " ls] }
                  Nothing -> d

            losers :: HashMap GId [GId]
            losers = sort <$> Map.fromListWith (<>) [ (g', [g]) | (g, g') <- Map.toList ddMap ]

            ddExt :: Extern -> Extern
            ddExt e = e { extModel = (\ g -> Map.findWithDefault g g ddMap) <$> extModel e }

            dev' :: Device
            dev' = dev { devBody = map ddStmt $ devBody dev }

            ddStmt :: Stmt -> Stmt
            ddStmt = \ case
                  SLet an x e      -> SLet an x $ ddExp e
                  SOutput an x e   -> SOutput an x $ ddExp e
                  SNext an x e     -> SNext an x $ ddExp e
                  SInstIn an x q e -> SInstIn an x q $ ddExp e

            ddExp :: Exp -> Exp
            ddExp = \ case
                  e@Lit {}   -> e
                  e@Undef {} -> e
                  e@Var {}   -> e
                  Cat an e1 e2 -> Cat an (ddExp e1) $ ddExp e2
                  Slice an i k e -> Slice an i k $ ddExp e
                  Prim an sz op es -> Prim an sz op $ map ddExp es
                  XCall an sz x cs es -> XCall an sz x cs $ map ddExp es
                  If an sz c t e -> If an sz (ddExp c) (ddExp t) $ ddExp e
                  Let an sz x e1 e2 -> Let an sz x (ddExp e1) $ ddExp e2
                  Call an sz g es -> Call an sz (Map.findWithDefault g g ddMap) $ map ddExp es

            -- Note: Annote's Eq/Hashable are trivial, so annotations don't
            -- perturb the keys. noinline defns are excluded on both sides
            -- (they are name-pinned).
            ddMap :: HashMap GId GId
            ddMap = foldr (\ (Defn _ g sig ps body _ _) -> maybe id (\ g' -> if g' /= g then Map.insert g g' else id)
                              $ Map.lookup (sig, ps, body) bodies) mempty unpinned
                  where bodies :: HashMap (Sig, [Name], Exp) GId
                        bodies = foldr (\ (Defn _ g sig ps body _ _) -> Map.insertWith keepBetter (sig, ps, body) g) mempty unpinned

                        unpinned :: [Defn]
                        unpinned = filter (not . defnNoInline) ds

            -- | The surviving name for a set of identical defns: prefer
            --   user-derived names (no '$') over instance names
            --   ('iter$W8') over compiler-fresh ones ('$'-led); then
            --   shortest, then lexicographic — a total order independent
            --   of program order and machine, so the winner never flips
            --   between compiles.
            keepBetter :: GId -> GId -> GId
            keepBetter a b | rank a <= rank b = a
                           | otherwise        = b
                  where rank :: GId -> (Int, Int, GId)
                        rank g = (cls g, T.length g, g)

                        cls :: GId -> Int
                        cls g | not ("$" `T.isInfixOf` g) = 0
                              | "$" `T.isPrefixOf` g      = 2
                              | otherwise                 = 1

---
--- Hoisting clocked externs into device instances.
---

-- | Defns containing (transitively) calls to sequential externs are inlined
--   into the device body; sequential-extern calls in the device body then
--   become instances: nested lets are flattened to device-level lets (sound:
--   the language is pure and total) and each seq-XCall is replaced by the
--   concatenation of a fresh instance's output ports, with its inputs driven
--   from the call's arguments.
hoistInstances :: forall m. MonadError AstError m => Program -> m Program
hoistInstances p@(Program exts ds _)
      | Set.null clocked = pure p
      | otherwise = flip evalStateT (0 :: Int) $ do
            -- This inlineBy is a correctness transform (clocked-extern
            -- calls must reach the device body to become instances), so it
            -- deliberately ignores defnNoInline.
            let Program exts' ds' dev' = inlineBy (`Set.member` clocked) p
            (insts, stmts) <- foldM hoistStmt ([], []) $ devBody dev'
            pure $ Program exts' ds' $ dev' { devInstances = reverse insts, devBody = reverse stmts }
      where externs :: HashMap Name Extern
            externs = Map.fromList $ map (\ e -> (extName e, e)) exts

            isSeq :: Name -> Bool
            isSeq x = case extKind <$> Map.lookup x externs of
                  Just (Seq _ _) -> True
                  _              -> False

            -- | Defns whose bodies transitively contain seq-extern calls.
            clocked :: HashSet GId
            clocked = fixSet step direct
                  where direct = Set.fromList [ defnName d | d <- ds, anySeq $ defnBody d ]

                        fixSet f s | s' == s   = s
                                   | otherwise = fixSet f s'
                              where s' = f s

                        step s = Set.fromList [ defnName d
                                              | d <- ds
                                              , defnName d `Set.member` s
                                                    || any (`Set.member` s) (expCalls $ defnBody d) ]

            anySeq :: Exp -> Bool
            anySeq = \ case
                  XCall _ _ x _ es -> isSeq x || any anySeq es
                  Cat _ e1 e2      -> anySeq e1 || anySeq e2
                  Slice _ _ _ e    -> anySeq e
                  Prim _ _ _ es    -> any anySeq es
                  Call _ _ _ es    -> any anySeq es
                  If _ _ c t e     -> anySeq c || anySeq t || anySeq e
                  Let _ _ _ e1 e2  -> anySeq e1 || anySeq e2
                  _                -> False

            freshHoisted :: Name -> StateT Int m Name
            freshHoisted pfx = do
                  i <- get
                  put $ i + 1
                  pure $ pfx <> showt i

            -- | Flatten an expression's nested lets to device-level lets
            --   (renamed fresh, seeded from the source binder and the
            --   extern's name so the readable base survives the hoist),
            --   then convert its seq-XCalls.
            hoistStmt :: ([Instance], [Stmt]) -> Stmt -> StateT Int m ([Instance], [Stmt])
            hoistStmt (insts, stmts) stmt = do
                  let (an, rebuild, e) = openStmt stmt
                  (e', insts', stmts') <- hoistExp an mempty (insts, stmts) e
                  pure (insts', rebuild e' : stmts')

            openStmt :: Stmt -> (Annote, Exp -> Stmt, Exp)
            openStmt = \ case
                  SLet an x e      -> (an, SLet an x, e)
                  SOutput an x e   -> (an, SOutput an x, e)
                  SNext an x e     -> (an, SNext an x, e)
                  SInstIn an x q e -> (an, SInstIn an x q, e)

            -- | Bottom-up: substitute renamed lets, lift each Let binding to
            --   a device-level SLet, and convert seq-XCalls to instances.
            hoistExp :: Annote -> HashMap Name Exp -> ([Instance], [Stmt]) -> Exp -> StateT Int m (Exp, [Instance], [Stmt])
            hoistExp an sub acc@(insts, stmts) = \ case
                  e@Lit {}   -> pure (e, insts, stmts)
                  e@Undef {} -> pure (e, insts, stmts)
                  e@(Var _ _ x) -> pure (fromMaybe e $ Map.lookup x sub, insts, stmts)
                  Cat an' e1 e2 -> do
                        (e1', i1, s1) <- hoistExp an sub acc e1
                        (e2', i2, s2) <- hoistExp an sub (i1, s1) e2
                        pure (Cat an' e1' e2', i2, s2)
                  Slice an' i k e -> do
                        (e', i1, s1) <- hoistExp an sub acc e
                        pure (Slice an' i k e', i1, s1)
                  Prim an' sz op es -> do
                        (es', i1, s1) <- hoistExps an sub acc es
                        pure (Prim an' sz op es', i1, s1)
                  Call an' sz g es -> do
                        (es', i1, s1) <- hoistExps an sub acc es
                        pure (Call an' sz g es', i1, s1)
                  If an' sz c t e -> do
                        (c', i1, s1) <- hoistExp an sub acc c
                        (t', i2, s2) <- hoistExp an sub (i1, s1) t
                        (e', i3, s3) <- hoistExp an sub (i2, s2) e
                        pure (If an' sz c' t' e', i3, s3)
                  Let an' _ x e1 e2 -> do
                        (e1', i1, s1) <- hoistExp an sub acc e1
                        x' <- freshHoisted $ x <> "$h"
                        hoistExp an (Map.insert x (Var an' (sizeOf e1') x') sub) (i1, SLet an' x' e1' : s1) e2
                  XCall an' sz x cs es
                        | isSeq x -> do
                              (es', i1, s1) <- hoistExps an sub acc es
                              i  <- freshHoisted $ x <> "$x"
                              case Map.lookup x externs of
                                    Nothing -> failAt an' $ "unknown extern: " <> x
                                    Just e  -> do
                                          let inst   = Instance an' i x cs
                                              drives = [ SInstIn an' i p arg | ((p, _), arg) <- zip (extInputs e) es' ]
                                              result = cat [ Var an' psz (i <> "." <> q) | (q, psz) <- extOutputs e ]
                                          pure (result, inst : i1, reverse drives <> s1)
                        | otherwise -> do
                              (es', i1, s1) <- hoistExps an sub acc es
                              pure (XCall an' sz x cs es', i1, s1)

            hoistExps :: Annote -> HashMap Name Exp -> ([Instance], [Stmt]) -> [Exp] -> StateT Int m ([Exp], [Instance], [Stmt])
            hoistExps an sub acc = foldM
                  (\ (es', i1, s1) e -> (\ (e', i2, s2) -> (es' <> [e'], i2, s2)) <$> hoistExp an sub (i1, s1) e)
                  ([], fst acc, snd acc)
