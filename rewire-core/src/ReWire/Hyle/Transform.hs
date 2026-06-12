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
module ReWire.Hyle.Transform (inline, inlineBy, purgeUnused, partialEval, purgeZeroWidth, dedupe, optimize) where

import ReWire.Annotation (Annote)
import ReWire.Fix (fixPure)
import ReWire.Hyle.Interp (evalExp, evalOp, IEnv (..))
import ReWire.Hyle.Syntax
import ReWire.Pretty (showt)

import ReWire.BitVector (BV, nat)
import Control.Arrow ((>>>))
import Data.List (find)
import Numeric.Natural (Natural)

import qualified ReWire.BitVector as BV

import Control.Monad.State.Strict (State, evalState, gets, modify)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Maybe (fromMaybe)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set

-- | Inline definitions into their call sites: all of them when @flatten@,
--   otherwise those used at most once. Definitions referenced as extern
--   models are never inlined (the reference is by name). The inlined-away
--   defns are removed by the final purge.
inline :: Bool -> Program -> Program
inline flatten p@(Program exts ds dev) = inlineBy inlinable p
      where inlinable :: GId -> Bool
            inlinable g = not (g `Set.member` models)
                       && (flatten || Map.findWithDefault 0 g uses <= (1 :: Int))

            models :: HashSet GId
            models = Set.fromList [ g | e <- exts, Just g <- [extModel e] ]

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
                        beta an (Defn _ _ _ ps body) es = do
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
optimize n = fixPure n $ partialEval >>> purgeZeroWidth >>> dedupe >>> purgeUnused

-- | Constant folding: primitives, muxes, slices, and concatenations of
--   literals fold via the interpreter's own evaluator; calls (and extern
--   calls with models) whose arguments are all literals evaluate fully.
--   Literal-bound lets substitute away.
partialEval :: Program -> Program
partialEval (Program exts ds dev) = Program exts (map peDefn ds) dev'
      where env :: IEnv
            env = IEnv (Map.fromList $ map (\ d -> (defnName d, d)) ds)
                       (Map.fromList $ map (\ e -> (extName e, e)) exts)

            peDefn :: Defn -> Defn
            peDefn d = d { defnBody = pe mempty $ defnBody d }

            dev' :: Device
            dev' = dev { devBody = snd $ foldl' peStmt (mempty, []) $ devBody dev }

            peStmt :: (HashMap Name BV, [Stmt]) -> Stmt -> (HashMap Name BV, [Stmt])
            peStmt (lits, acc) = \ case
                  SLet an x e -> case pe lits e of
                        Lit _ bv -> (Map.insert x bv lits, acc <> [SLet an x $ Lit an bv])
                        e'       -> (lits, acc <> [SLet an x e'])
                  SOutput an x e   -> (lits, acc <> [SOutput an x $ pe lits e])
                  SNext an x e     -> (lits, acc <> [SNext an x $ pe lits e])
                  SInstIn an x q e -> (lits, acc <> [SInstIn an x q $ pe lits e])

            pe :: HashMap Name BV -> Exp -> Exp
            pe lits = \ case
                  e@(Var an _ x)
                        | Just bv <- Map.lookup x lits -> Lit an bv
                        | otherwise                    -> e
                  e@Lit {}   -> e
                  e@Undef {} -> e
                  Cat an e1 e2 -> mergeCat an $ concatMap gather [pe lits e1, pe lits e2]
                  Slice an i k e -> peSlice an i k $ pe lits e
                  Prim an sz op es ->
                        let es' = map (pe lits) es in
                        case mapM litVal es' of
                              Just bvs | Right bv <- evalOp an op bvs -> Lit an bv
                              _                                       -> Prim an sz op es'
                  Call an sz g es ->
                        let es' = map (pe lits) es in
                        case mapM litVal es' of
                              Just _ | Right bv <- evalExp env mempty (Call an sz g es') -> Lit an bv
                              _                                                          -> Call an sz g es'
                  XCall an sz x cs es ->
                        let es' = map (pe lits) es in
                        case mapM litVal es' of
                              Just _ | Just ex <- Map.lookup x $ envExterns env
                                     , Just _  <- extModel ex
                                     , Right bv <- evalExp env mempty (XCall an sz x cs es') -> Lit an bv
                              _                                                              -> XCall an sz x cs es'
                  If an sz c t e -> case pe lits c of
                        Lit _ bv | nat bv /= 0 -> pe lits t
                                 | otherwise   -> pe lits e
                        c'                     -> If an sz c' (pe lits t) $ pe lits e
                  Let an sz x e1 e2 -> case pe lits e1 of
                        Lit _ bv -> pe (Map.insert x bv lits) e2
                        e1'      -> Let an sz x e1' $ pe lits e2

            litVal :: Exp -> Maybe BV
            litVal = \ case
                  Lit _ bv -> Just bv
                  _        -> Nothing

            -- | Adjacent literals merge; a single piece stands alone.
            mergeCat :: Annote -> [Exp] -> Exp
            mergeCat an = go' >>> \ case
                  []  -> Lit an mempty
                  [e] -> e
                  es  -> foldr1 (Cat an) es
                  where go' :: [Exp] -> [Exp]
                        go' = \ case
                              Lit a bv : Lit _ bv' : es -> go' $ Lit a (bv <> bv') : es
                              e : es                    -> e : go' es
                              []                        -> []

            -- | Slices of literals, identity slices, slices of slices, and
            --   slices falling within one component of a concatenation.
            peSlice :: Annote -> Index -> Size -> Exp -> Exp
            peSlice an i k e
                  | k == 0                = Lit an mempty
                  | i == 0, k == sizeOf e = e
                  | otherwise = case e of
                        Lit _ bv       -> Lit an $ subBV bv i k
                        Slice _ i' _ e' -> peSlice an (i + i') k e'
                        Cat {}         -> case find (\ (e', _, off) -> fromIntegral off <= i && i + fromIntegral k <= off + fromIntegral (sizeOf e')) pieces of
                              Just (e', _, off) -> peSlice an (i - off) k e'
                              Nothing           -> Slice an i k e
                              where pieces = offs 0 $ reverse $ gather e
                                    offs _ [] = []
                                    offs o (e' : es') = (e', sizeOf e', o) : offs (o + fromIntegral (sizeOf e')) es'
                        _              -> Slice an i k e

            subBV :: BV -> Index -> Size -> BV
            subBV bv i k = BV.bitVec (fromIntegral k) $ nat bv `div` (2 ^ toInteger i)

-- | Drop zero-width parameters (and the corresponding arguments at call
--   sites) and zero-width-result defns (calls to them become nil).
purgeZeroWidth :: Program -> Program
purgeZeroWidth (Program exts ds dev) = Program exts (map goDefn $ filter ((> 0) . sizeOf) ds) dev'
      where sigs :: HashMap GId Sig
            sigs = Map.fromList $ map (\ d -> (defnName d, defnSig d)) ds

            goDefn :: Defn -> Defn
            goDefn (Defn an g (Sig san argSzs res) ps body) = Defn an g (Sig san (filter (> 0) argSzs) res) ps' $ goExp body
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
--   single representative. Should be followed by purgeUnused.
dedupe :: Program -> Program
dedupe (Program exts ds dev) = Program (map ddExt exts) (map ddDefn ds) dev'
      where ddDefn :: Defn -> Defn
            ddDefn d = d { defnBody = ddExp $ defnBody d }

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
            -- perturb the keys.
            ddMap :: HashMap GId GId
            ddMap = foldr (\ (Defn _ g sig ps body) -> maybe id (\ g' -> if g' /= g then Map.insert g g' else id)
                              $ Map.lookup (sig, ps, body) bodies) mempty ds
                  where bodies :: HashMap (Sig, [Name], Exp) GId
                        bodies = foldr (\ (Defn _ g sig ps body) -> Map.insert (sig, ps, body) g) mempty ds
