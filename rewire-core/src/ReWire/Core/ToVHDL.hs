{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | The VHDL backend: a direct translation from Core to VHDL-2008, mirroring
--   the structure of the Verilog backend (ReWire.Core.ToVerilog) so that the
--   two backends agree behaviorally; the rwc-test cosimulation check enforces
--   this. Verilog expression semantics (unsigned operations, self-determined
--   width rules, assignment truncation/extension) are implemented by the
--   rw_helpers package emitted with every design (see ReWire.VHDL.Syntax).
module ReWire.Core.ToVHDL (compileProgram, testbench) where

import ReWire.Annotation (noAnn, ann)
import ReWire.BitVector (BV, width, bitVec, zeros, ones, lsb1, (==.), (@.))
import ReWire.Config (Config, ResetFlag (..), vhdlPackages)
import ReWire.Core.Analysis (DefnMap, defnMap, defnUses, clockReset, inputValue, yamlPrefixes)
import ReWire.Core.Interp (Ins, patApply', patMatches', subRange, dispatchWires, pausePadding, resumptionSize, Wiring')
import ReWire.Core.Mangle (mangleFresh, mangleMod)
import ReWire.Core.Syntax as C hiding (Name, Size, Index)
import ReWire.Error (failAt, AstError, MonadError)
import ReWire.Pretty (prettyPrint, showt)
import qualified ReWire.VHDL.Syntax as H

import qualified ReWire.BitVector as BV

import Control.Arrow ((&&&), first, second)
import Control.Lens ((^.), (.~))
import Control.Monad (liftM2, zipWithM)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.State (MonadState, runStateT, modify, gets)
import Data.HashMap.Strict (HashMap)
import Data.List (sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T
import qualified ReWire.Config       as C

type Name = H.Name

data FreshMode = FreshInit | FreshRun
type Fresh = (FreshMode, HashMap Text Int)

-- | Reader environment: defn bodies/uses/purity, plus defn signatures (VHDL
--   needs them to emit component declarations at instantiation sites).
type Env = (DefnMap, HashMap GId C.Sig)

data TS = TS
      { tsFresh  :: !Fresh
      , tsSigs   :: ![H.Signal]
      , tsComps  :: !(HashMap Text H.Component)
      , tsWidths :: !(HashMap Text H.Size)
      }

freshInit0 :: TS
freshInit0 = TS (FreshInit, mempty) [] mempty mempty

fresh' :: MonadState TS m => Text -> m Name
fresh' s = do
      (mode, ctrs) <- gets tsFresh
      let ctr = fromMaybe 0 $ Map.lookup s ctrs
      modify $ \ ts -> ts { tsFresh = (mode, Map.insert s (ctr + 1) ctrs) }
      pure $ s <> modeTag mode <> ctrTag ctr
      where modeTag :: FreshMode -> Text
            modeTag = \ case
                  FreshInit -> "S0"
                  _         -> mempty

            ctrTag :: Int -> Text
            ctrTag = \ case
                  n | n > 0 -> "R" <> showt n
                  _         -> mempty

fresh :: MonadState TS m => Text -> m Name
fresh s = fresh' $ T.toLower $ mangleFresh s

setFreshMode :: MonadState TS m => FreshMode -> m ()
setFreshMode mode = modify $ \ ts -> ts { tsFresh = first (const mode) $ tsFresh ts }

newWire :: MonadState TS m => H.Size -> Name -> m Name
newWire sz n = fresh n >>= addWire sz

newWire' :: MonadState TS m => H.Size -> Name -> m Name
newWire' sz n = fresh' n >>= addWire sz

addWire :: MonadState TS m => H.Size -> Name -> m Name
addWire sz n' = do
      modify $ \ ts -> ts { tsSigs   = tsSigs ts <> [H.Signal n' sz Nothing]
                          , tsWidths = Map.insert n' sz $ tsWidths ts }
      pure n'

-- | Register the width of an externally-declared signal (e.g., a port).
addWidth :: MonadState TS m => (Name, H.Size) -> m ()
addWidth (n, sz) = modify $ \ ts -> ts { tsWidths = Map.insert n sz $ tsWidths ts }

addComponent :: MonadState TS m => H.Component -> m ()
addComponent c@(H.Component n _ _) = modify $ \ ts -> ts { tsComps = Map.insert n c $ tsComps ts }

lookupWidth :: MonadState TS m => Name -> m (Maybe H.Size)
lookupWidth n = gets $ Map.lookup n . tsWidths

compileProgram :: (MonadFail m, MonadError AstError m) => Config -> C.Device -> m H.Device
compileProgram conf p@(C.Device topLevel w loop state0 ds)
      | conf^.C.flatten = H.Device . pure <$> runReaderT (compileStart conf' topLevel w loop state0) env
      | otherwise       = flip runReaderT env $ do
            st' <- compileStart conf' topLevel w loop state0
            -- Initial state should be inlined, so we can filter out its defn.
            ds' <- mapM (compileDefn conf') $ filter (inuse . defnName) $ loop : ds
            pure $ H.Device $ st' : ds'
      where env :: Env
            env = ( Map.mapKeys mangleMod $ defnMap p
                  , Map.fromList $ map (mangleMod . defnName &&& defnSig) $ loop : state0 : ds
                  )

            inuse :: GId -> Bool
            inuse g = case Map.lookup g $ defnUses p of
                  Nothing -> False
                  Just 0  -> False
                  Just 1  -> False
                  _       -> True

            conf' :: Config
            conf' = C.clock.~maybe mempty id mclk $ C.reset.~maybe mempty id mrst $ conf

            (mclk, mrst) = clockReset conf p

compileStart :: (MonadError AstError m, MonadFail m, MonadReader Env m)
                 => Config -> Name -> C.Wiring -> C.Defn -> C.Defn -> m H.Unit
compileStart conf topLevel w loop state0 = do
      ((stmts, initBV), ts) <- flip runStateT freshInit0 $ do
            mapM_ addWidth $ inputs <> outputs <> pausePadding w' <> dispatchWires w' <> nextDispatchWires

            (rStart, ssStart) <- compileCall (C.flatten.~True $ conf) (mangleMod $ defnName state0) (resumptionSize w') []

            setFreshMode FreshRun
            (rLoop, ssLoop)   <- compileCall conf (mangleMod $ defnName loop) (resumptionSize w')
                  $  [ hCat $ map (H.Var . fst) $ dispatchWires w' | dispatchSz > 0 ]
                  <> [ hCat $ map (H.Var . fst) $ inputWires w     | inputSz > 0 ]

            loopStmts <- if loopSz > 0 then (ssLoop <>) <$> assignMulti pauseWires rLoop
                         else pure []

            if dispatchSz == 0 then pure (ssStart <> loopStmts, Nothing)
            else do
                  initBV <- initState rStart
                  pure (ssStart <> loopStmts <> [stateProcess initBV], Just initBV)

      let sigs = map (uncurry sig') (pausePadding w' <> nextDispatchWires)
              <> map (dispatchSig $ fromMaybe BV.nil initBV) (dispatchWires w')
              <> tsSigs ts
      pure $ H.Unit topLevel (unitPackages conf) (map (portIn . portSig) inputs <> map (portOut . portSig) outputs)
                   (sortOn compName $ Map.elems $ tsComps ts) sigs stmts

      where clockSig :: Maybe (Name, H.Size)
            clockSig | T.null (conf^.C.clock) = Nothing
                     | otherwise              = pure (conf^.C.clock, 1)

            resetSig :: Maybe (Name, H.Size)
            resetSig | T.null (conf^.C.reset) = Nothing
                     | otherwise              = pure (conf^.C.reset, 1)

            loopSz :: H.Size
            loopSz | Sig _ _ sz <- defnSig loop = sz

            inputs, outputs :: [(Name, H.Size)]
            inputs  = catMaybes [clockSig, resetSig] <> inputWires w
            outputs = outputWires w

            portSig :: (Name, H.Size) -> (Name, H.Size)
            portSig = id

            portIn, portOut :: (Name, H.Size) -> H.Port
            portIn  (n, sz) = H.Port n H.In sz
            portOut (n, sz) = H.Port n H.Out sz

            sig' :: Name -> H.Size -> H.Signal
            sig' n sz = H.Signal n sz Nothing

            dispatchSig :: BV -> (Name, H.Size) -> H.Signal
            dispatchSig initBV (n, sz) = H.Signal n sz $ lookup n $ splitBV initBV $ dispatchWires w'

            dispatchSz :: H.Size
            dispatchSz = sum $ snd <$> dispatchWires w'

            inputSz :: H.Size
            inputSz = sum $ snd <$> inputWires w

            -- | The state-update process, with an (a)synchronous reset per the
            --   configured reset flags.
            stateProcess :: BV -> H.Stmt
            stateProcess initBV = case (resetSig, syncRst) of
                  (Just (rstn, _), False) -> H.Process [conf^.C.clock, rstn] []
                        [H.SIf [(rstCond rstn, rstAssigns initBV), (H.CondRising $ conf^.C.clock, nextAssigns)] []]
                  (Just (rstn, _), True)  -> H.Process [conf^.C.clock] []
                        [H.SIf [(H.CondRising $ conf^.C.clock, [H.SIf [(rstCond rstn, rstAssigns initBV)] nextAssigns])] []]
                  (Nothing, _)            -> H.Process [conf^.C.clock] []
                        [H.SIf [(H.CondRising $ conf^.C.clock, nextAssigns)] []]

            rstCond :: Name -> H.Cond
            rstCond rstn = H.CondEq rstn $ bitVec 1 $ fromEnum $ not invertRst

            rstAssigns :: BV -> [H.SeqStmt]
            rstAssigns initBV = map (\ (n, bv) -> H.SAssign (H.LVName n) $ H.Lit bv)
                  $ splitBV initBV $ dispatchWires w'

            nextAssigns :: [H.SeqStmt]
            nextAssigns = zipWith (\ (n, _) (n', _) -> H.SAssign (H.LVName n) $ H.Var n') (dispatchWires w') nextDispatchWires

            invertRst :: Bool
            invertRst = Inverted `elem` (conf^.C.resetFlags)

            syncRst :: Bool
            syncRst = Synchronous `elem` (conf^.C.resetFlags)

            -- | Initial/reset state.
            initState :: MonadError AstError m => H.Exp -> m BV
            initState e = case (hExpToBV e, fromIntegral dispatchSz :: Int) of
                  (Just bv, n) | n > 0 -> pure $ subRange (0, n - 1) bv
                  (_, n)       | n > 0 -> pure $ zeros n -- TODO(chathhorn): make configurable?
                  st                   -> failAt noAnn $ "ToVHDL: compileStart: could not calculate initial state: " <> showt (snd st)

            pauseWires :: [(Name, H.Size)]
            pauseWires = pausePadding w' <> outputWires w <> nextDispatchWires

            nextDispatchWires :: [(Name, H.Size)]
            nextDispatchWires = first (<> "_next") <$> dispatchWires w'

            w' :: Wiring'
            w' = (w, defnSig loop, defnSig state0)

            compName :: H.Component -> Text
            compName (H.Component cn _ _) = cn

compileDefn :: (MonadFail m, MonadError AstError m, MonadReader Env m) => Config -> C.Defn -> m H.Unit
compileDefn conf (C.Defn _ n (Sig _ inps outp) body) = do
      isPure' <- isPureDefn $ mangleMod n
      let ports = (if isPure' then [] else [(conf^.C.clock, 1), (conf^.C.reset, 1)]) <> zip argNames inps
      ((e, stmts), ts) <- flip runStateT (freshInit0 { tsFresh = (FreshRun, mempty) }) $ do
            mapM_ addWidth $ ports <> [("res", outp)]
            (e, stmts) <- compileExp conf (map (H.Var . fst) $ zip argNames inps) body
            e'         <- wcast outp e
            pure (e', stmts)
      pure $ H.Unit (mangleMod n) (unitPackages conf) (map (\ (pn, sz) -> H.Port pn H.In sz) ports <> [H.Port "res" H.Out outp])
                    (sortOn (\ (H.Component cn _ _) -> cn) $ Map.elems $ tsComps ts)
                    (tsSigs ts)
                    (stmts <> [H.Assign (H.LVName "res") e])
      where argNames :: [Name]
            argNames = zipWith (\ _ x -> "arg" <> showt x) inps [0 :: Int ..]

isPureDefn :: MonadReader Env m => GId -> m Bool
isPureDefn g = asks (Map.lookup g . fst) >>= \ case
      Just (_, (_, b)) -> pure b
      _                -> pure False

-- | Inlines a defn or instantiates an already-compiled defn.
compileCall :: (MonadState TS m, MonadFail m, MonadError AstError m, MonadReader Env m)
             => Config -> GId -> H.Size -> [H.Exp] -> m (H.Exp, [H.Stmt])
compileCall conf g sz lvars = asks (Map.lookup g . fst) >>= \ case
      Just (body, (uses, _)) | uses == 1 || conf^.C.flatten -> do
            (e, stmts) <- compileExp conf lvars body
            e'         <- wcast sz e
            pure (e', stmts)
      Just (_, (_, isPure'))                                -> do
            Sig _ inps outp <- asks (Map.lookup g . snd) >>= maybe (failAt noAnn $ "ToVHDL: compileCall: no signature for " <> g) pure
            let ports = (if isPure' then [] else [(conf^.C.clock, 1), (conf^.C.reset, 1)])
                     <> zip (map (("arg" <>) . showt) [0 :: Int ..]) inps
            addComponent $ H.Component g [] $ map (\ (pn, psz) -> H.Port pn H.In psz) ports <> [H.Port "res" H.Out outp]
            mr               <- newWire sz $ g <> "_out"
            inst'            <- fresh' "inst"
            (conns, hoists)  <- unzip <$> zipWithM connect (map snd ports)
                  ((if isPure' then [] else [H.Var $ conf^.C.clock, H.Var $ conf^.C.reset]) <> lvars)
            pure (H.Var mr, concat hoists <> [H.Instantiate g inst' [] $ map (mempty, ) $ conns <> [H.Var mr]])
      _ -> failAt noAnn $ "ToVHDL: compileCall: failed to find definition for " <> g <> " while flattening."

-- | A port-map actual: signal references connect directly; anything else is
--   hoisted into a temporary of exactly the port's width (VHDL port
--   associations are strict about form and width). Clock signals in
--   particular must connect directly (an intermediate signal assignment would
--   introduce a delta-cycle skew).
connect :: (MonadState TS m, MonadError AstError m) => H.Size -> H.Exp -> m (H.Exp, [H.Stmt])
connect psz e = do
      w <- expWidth e
      if trivial e && w == Just psz then pure (e, [])
      else do
            e'  <- wcast psz e
            tmp <- newWire psz "conn"
            pure (H.Var tmp, [H.Assign (H.LVName tmp) e'])
      where trivial :: H.Exp -> Bool
            trivial = \ case
                  H.Var {}   -> True
                  H.Slice {} -> True
                  H.Elem {}  -> True
                  H.Lit {}   -> True
                  _          -> False

instantiate :: (MonadFail m, MonadState TS m, MonadError AstError m) => Config -> ExternSig -> GId -> Text -> H.Size -> [H.Exp] -> m (H.Exp, [H.Stmt])
instantiate conf (ExternSig an ps theirClock theirReset args res) g inst sz lvars = do
      mr              <- newWire sz "extRes"
      (args', lvars') <- addReset (args, lvars) >>= addClock
      inst'           <- fresh' $ if T.null inst then "inst" else inst
      let cports       = nameAnon $ map (\ (n, w) -> H.Port n H.In w) args' <> map (\ (n, w) -> H.Port n H.Out w) res
      addComponent $ H.Component g (map fst ps) cports
      (conns, hoists) <- unzip <$> zipWithM connect (map snd args') lvars'
      let actuals      = conns <> toSubRanges mr (snd <$> res)
          named        = all (not . T.null . fst) (args' <> res)
          pm | named     = zip (map (\ (H.Port pn _ _) -> pn) cports) actuals
             | otherwise = map (mempty, ) actuals
      pure (H.Var mr, concat hoists <> [H.Instantiate g inst' (map (second toInteger) ps) pm])
      where clk :: H.Exp
            clk = H.Var $ conf^.C.clock

            rst :: H.Exp
            rst = H.Var $ conf^.C.reset

            addClock :: MonadError AstError m => ([(Text, H.Size)], [H.Exp]) -> m ([(Text, H.Size)], [H.Exp])
            addClock (args'', lvars'')
                  | T.null theirClock            = pure (args'', lvars'')
                  | not (T.null $ conf^.C.clock) = pure ((theirClock, 1) : args'', clk : lvars'')
                  | otherwise                    = failAt an "ToVHDL: external module requires a clock signal, but we have no clock to give it."

            addReset :: MonadError AstError m => ([(Text, H.Size)], [H.Exp]) -> m ([(Text, H.Size)], [H.Exp])
            addReset (args'', lvars'')
                  | T.null theirReset            = pure (args'', lvars'')
                  | not (T.null $ conf^.C.reset) = pure ((theirReset, 1) : args'', rst : lvars'')
                  | otherwise                    = failAt an "ToVHDL: external module requires a reset signal, but we have no reset to give it."

            nameAnon :: [H.Port] -> [H.Port]
            nameAnon = zipWith nameAnon' [0 :: Int ..]
                  where nameAnon' i = \ case
                              H.Port "" d psz -> H.Port ("p" <> showt i) d psz
                              p               -> p

compileExps :: (MonadState TS m, MonadFail m, MonadError AstError m, MonadReader Env m)
            => Config -> [H.Exp] -> [C.Exp] -> m ([H.Exp], [H.Stmt])
compileExps conf lvars es = (map fst &&& concatMap snd) <$> mapM (compileExp conf lvars) es

compileExp :: (MonadState TS m, MonadFail m, MonadError AstError m, MonadReader Env m)
            => Config -> [H.Exp] -> C.Exp -> m (H.Exp, [H.Stmt])
compileExp conf lvars = \ case
      LVar _  _ (lkupLVal -> Just x)               -> pure (x, [])
      LVar an _ _                                  -> failAt an "ToVHDL: compileExp: encountered unknown LVar."
      Lit _ bv                                     -> pure (litExp bv, [])
      C.Concat _ e1 e2                             -> first hCat <$> compileExps conf lvars (gather e1 <> gather e2)
      Call _ sz (Global g) e ps els                -> mkCall g e ps els $ compileCall conf (mangleMod g) sz
      Call an sz (SetRef r) e ps els               -> mkCall2' "setRef" e ps els $ \ (a, b) -> do
            wa <- expWidth a >>= maybe (failAt an "ToVHDL: setRef: cannot determine reference width.") pure
            r' <- newWire' wa r
            b' <- wcast sz b
            pure (b', [H.Assign (H.LVName r') a])
      Call _ sz (GetRef r) _ _ _                   -> (, []) <$> wcast sz (H.Var r)
      Call _ sz (Prim (binOp -> Just op)) e ps els -> mkCall2 "binOp"     e ps els $ \ (x, y) -> wcast sz (H.FunCall op [x, y])
      Call _ sz (Prim (unOp -> Just op)) e ps els  -> mkCall1 "unOp"      e ps els $ \ x      -> wcast sz (H.FunCall op [x])
      Call _ sz (Prim MSBit) e ps els              -> mkCall1 "msbit"     e ps els $ \ x      -> wcast sz (projBit (fromIntegral (argsSize ps) - 1) x)
      Call _ sz (Prim Resize) e ps els             -> mkCall1 "resize"    e ps els $ \ x      -> wcast sz x
      Call _ sz (Prim Id) e ps els                 -> mkCall "id"         e ps els $ \ xs     -> (, []) <$> wcast sz (hCat xs)
      Call _ sz (Prim Reverse) e ps els            -> mkCall "reverse"    e ps els $ \ xs     -> (, []) <$> wcast sz (hCat $ reverse xs)
      Call _ sz (Prim (Replicate n)) e ps els      -> mkCall1 "replicate" e ps els $ \ x      -> wcast sz (if n > 0 then H.FunCall "rw_repl" [H.Num n, x] else H.Lit BV.nil)
      Call a _  (Prim p) _ _ _                     -> failAt a $ "ToVHDL: compileExp: encountered unknown primitive: " <> showt p
      Call _ sz (Extern sig ex inst) e ps els      -> mkCall ex           e ps els $ instantiate conf sig ex inst sz
      Call _ sz (Const bv) e ps els                -> mkCall "lit"        e ps els $ \ _      -> (, []) <$> wcast sz (litExp bv)
      where mkCall2 :: (MonadState TS m, MonadFail m, MonadError AstError m, MonadReader Env m)
                    => Name -> C.Exp -> [Pat] -> C.Exp -> ((H.Exp, H.Exp) -> m H.Exp) -> m (H.Exp, [H.Stmt])
            mkCall2 s e ps els f = mkCall2' s e ps els $ fmap (, []) . f

            mkCall2' :: (MonadState TS m, MonadFail m, MonadError AstError m, MonadReader Env m)
                    => Name -> C.Exp -> [Pat] -> C.Exp -> ((H.Exp, H.Exp) -> m (H.Exp, [H.Stmt])) -> m (H.Exp, [H.Stmt])
            mkCall2' s e ps els f = mkCall s e ps els $ \ case
                  [e1, e2] -> f (e1, e2)
                  es       -> failAt (ann e) $ "ToVHDL: primitive " <> s <> ": expected two arguments, got: " <> showt (length es)

            mkCall1 :: (MonadState TS m, MonadFail m, MonadError AstError m, MonadReader Env m)
                    => Name -> C.Exp -> [Pat] -> C.Exp -> (H.Exp -> m H.Exp) -> m (H.Exp, [H.Stmt])
            mkCall1 s e ps els f = mkCall s e ps els $ \ case
                  [e1] -> (, []) <$> f e1
                  es   -> failAt (ann e) $ "ToVHDL: primitive " <> s <> ": expected one argument, got: " <> showt (length es)

            mkCall :: (MonadState TS m, MonadFail m, MonadError AstError m, MonadReader Env m)
                    => Name -> C.Exp -> [Pat] -> C.Exp -> ([H.Exp] -> m (H.Exp, [H.Stmt])) -> m (H.Exp, [H.Stmt])
            mkCall s e ps els f = do
                  (e', stmts)    <- compileExp conf lvars e
                  (els', stmts') <- compileExp conf lvars els
                  case litVal e' of
                        Just bv -> do
                              (fes, fstmts) <- f $ patApplyLit bv ps
                              pure (if patMatchesLit bv ps then fes else els', stmts <> stmts' <> fstmts)
                        _       -> do
                              n <- newWire (sizeOf e) $ s <> "_in"
                              scrut <- wcast (sizeOf e) e'
                              (fes, fstmts) <- f $ patApply n ps
                              pure  ( let c = patMatches n ps in
                                      if | litTrue c || C.isNil els -> fes
                                         | litFalse c               -> els'
                                         | otherwise                -> H.FunCall "rw_cond" [c, fes, els']
                                    , stmts <> stmts' <> [ H.Assign (H.LVName n) scrut ] <> fstmts
                                    )

            litTrue :: H.Exp -> Bool
            litTrue e = case litVal e of
                  Just v  -> v /= zeros 1
                  Nothing -> False

            litFalse :: H.Exp -> Bool
            litFalse e = case litVal e of
                  Just v  -> v == zeros 1
                  Nothing -> False

            litVal :: H.Exp -> Maybe BV
            litVal = \ case
                  H.Lit b -> Just b
                  _       -> Nothing

            lkupLVal :: LId -> Maybe H.Exp
            lkupLVal = flip lookup (zip [0 :: LId ..] lvars)

            projBit :: H.Index -> H.Exp -> H.Exp
            projBit ix = \ case
                  H.Slice n i _          -> H.Elem n $ ix + i
                  H.Var n                -> H.Elem n ix
                  H.Lit bv | bv @. ix    -> H.Lit $ ones 1
                           | otherwise   -> H.Lit $ zeros 1
                  e                      -> e -- TODO(chathhorn): kludgy.

-- | Bit-width casting, mirroring ReWire.Core.ToVerilog.wcast: pad the
--   operands of width-contextual operations (so e.g. addition happens at the
--   assignment's width, preserving carries), truncate or zero-extend
--   everything else (Verilog assignment semantics, via rw_resize).
wcast :: (MonadError AstError m, MonadState TS m) => H.Size -> H.Exp -> m H.Exp
wcast sz e = expWidth e >>= \ case
      Just sz' | sz < sz' -> pure $ resize sz e
               | sz > sz' -> case e of
                  H.FunCall f [e1, e2] | f `elem` padBothOps -> (\ a b -> H.FunCall f [a, b]) <$> pad e1 <*> pad e2
                  H.FunCall "rw_cond" [c, e1, e2]            -> (\ a b -> H.FunCall "rw_cond" [c, a, b]) <$> pad e1 <*> pad e2
                  H.FunCall f [e1, e2] | f `elem` padLeftOps -> (\ a -> H.FunCall f [a, e2]) <$> pad e1
                  H.FunCall "rw_not" [e1]                    -> H.FunCall "rw_not" . pure <$> pad e1
                  _                                          -> pad e
      _                   -> pure e
      where pad :: (MonadError AstError m, MonadState TS m) => H.Exp -> m H.Exp
            pad e' = expWidth e' >>= \ case
                  Just sz' | sz > sz' -> pure $ resize sz e'
                  _                   -> pure e'

            padBothOps :: [Name]
            padBothOps = ["rw_add", "rw_sub", "rw_mul", "rw_div", "rw_mod", "rw_and", "rw_or", "rw_xor", "rw_xnor"]

            padLeftOps :: [Name]
            padLeftOps = ["rw_pow", "rw_shiftl", "rw_shiftr", "rw_ashiftr"]

resize :: H.Size -> H.Exp -> H.Exp
resize sz = \ case
      H.FunCall "rw_resize" [e, H.Num _] -> H.FunCall "rw_resize" [e, H.Num $ fromIntegral sz]
      e                                  -> H.FunCall "rw_resize" [e, H.Num $ fromIntegral sz]

-- | The "natural" (self-determined) width of a generated VHDL expression.
expWidth :: (MonadError AstError m, MonadState TS m) => H.Exp -> m (Maybe H.Size)
expWidth = \ case
      H.Lit bv      -> pure $ Just $ fromIntegral $ width bv
      H.Var n       -> lookupWidth n
      H.Slice _ i j -> pure $ Just $ fromIntegral $ j - i + 1
      H.Elem _ _    -> pure $ Just 1
      H.Cat es      -> sumMaybes <$> mapM expWidth es
      H.Num _       -> pure Nothing
      H.FunCall f es -> case (f, es) of
            ("rw_resize", [_, H.Num n]) -> pure $ Just $ fromIntegral n
            ("rw_repl", [H.Num n, e])   -> fmap (* fromIntegral n) <$> expWidth e
            ("rw_cond", [_, e1, e2])    -> largest e1 e2
            ("rw_not", [e])             -> expWidth e
            (_, [e1, e2]) | f `elem` maxOps   -> largest e1 e2
                          | f `elem` leftOps  -> expWidth e1
            _ | f `elem` oneBitOps      -> pure $ Just 1
            _                           -> failAt noAnn $ "ToVHDL: expWidth: unsupported expression (rwc bug): " <> prettyPrint (H.FunCall f es)
      where largest :: (MonadError AstError m, MonadState TS m) => H.Exp -> H.Exp -> m (Maybe H.Size)
            largest e1 e2 = liftM2 max <$> expWidth e1 <*> expWidth e2

            maxOps :: [Name]
            maxOps = ["rw_add", "rw_sub", "rw_mul", "rw_div", "rw_mod", "rw_and", "rw_or", "rw_xor", "rw_xnor"]

            leftOps :: [Name]
            leftOps = ["rw_pow", "rw_shiftl", "rw_shiftr", "rw_ashiftr"]

            oneBitOps :: [Name]
            oneBitOps = [ "rw_land", "rw_lor", "rw_lnot", "rw_rand", "rw_rnand", "rw_ror", "rw_rnor"
                        , "rw_rxor", "rw_rxnor", "rw_eq", "rw_neq", "rw_lt", "rw_gt", "rw_lteq", "rw_gteq" ]

sumMaybes :: Num a => [Maybe a] -> Maybe a
sumMaybes = foldMaybes (+)

foldMaybes :: (a -> a -> a) -> [Maybe a] -> Maybe a
foldMaybes f = \ case
      a : b : ms -> foldMaybes f $ (f <$> a <*> b) : ms
      [a]        -> a
      _          -> Nothing

binOp :: Prim -> Maybe Name
binOp = flip lookup primBinOps

unOp :: Prim -> Maybe Name
unOp = flip lookup primUnOps

primBinOps :: [(Prim, Name)]
primBinOps =
      [ ( C.Add         , "rw_add")
      , ( C.Sub         , "rw_sub")
      , ( C.Mul         , "rw_mul")
      , ( C.Div         , "rw_div")
      , ( C.Mod         , "rw_mod")
      , ( C.Pow         , "rw_pow")
      , ( C.LAnd        , "rw_land")
      , ( C.LOr         , "rw_lor")
      , ( C.And         , "rw_and")
      , ( C.Or          , "rw_or")
      , ( C.XOr         , "rw_xor")
      , ( C.XNor        , "rw_xnor")
      , ( C.LShift      , "rw_shiftl")
      , ( C.RShift      , "rw_shiftr")
      , ( C.RShiftArith , "rw_ashiftr")
      , ( C.Eq          , "rw_eq")
      , ( C.Gt          , "rw_gt")
      , ( C.GtEq        , "rw_gteq")
      , ( C.Lt          , "rw_lt")
      , ( C.LtEq        , "rw_lteq")
      ]

primUnOps :: [(Prim, Name)]
primUnOps =
      [ ( C.LNot  , "rw_lnot")
      , ( C.Not   , "rw_not")
      , ( C.RAnd  , "rw_rand")
      , ( C.RNAnd , "rw_rnand")
      , ( C.ROr   , "rw_ror")
      , ( C.RNor  , "rw_rnor")
      , ( C.RXOr  , "rw_rxor")
      , ( C.RXNor , "rw_rxnor")
      ]

-- | Returns an expression (one-bit vector) that is true when the pattern matches.
patMatches :: Name -> [Pat] -> H.Exp
patMatches x ps = case patMatches' (\ i j bv -> [H.FunCall "rw_eq" [H.Slice x (fromIntegral i) (fromIntegral j), H.Lit bv]]) ps of
      ps'@(_ : _) -> foldr1 (\ a b -> H.FunCall "rw_land" [a, b]) ps'
      []          -> H.Lit $ ones 1

patMatchesLit :: BV -> [Pat] -> Bool
patMatchesLit bv = and . patMatches' (\ i j bv' -> [subRange (i, j) bv ==. bv'])

-- | Returns a list of slices bound by pattern variables.
patApply :: Name -> [Pat] -> [H.Exp]
patApply x = patApply' (\ i j -> [H.Slice x (fromIntegral i) (fromIntegral j)])

patApplyLit :: BV -> [Pat] -> [H.Exp]
patApplyLit bv = patApply' (\ i j -> [H.Lit $ subRange (i, j) bv])

toSubRanges :: Name -> [H.Size] -> [H.Exp]
toSubRanges n = patApply' (\ i j -> [H.Slice n (fromIntegral i) (fromIntegral j)]) . map (PatVar noAnn . fromIntegral)

argsSize :: [Pat] -> H.Size
argsSize = sum . map patToSize
      where patToSize :: Pat -> H.Size
            patToSize = \ case
                  PatVar _ sz -> sz
                  _           -> 0

-- | Concatenation, filtering zero-width parts (and big literals are broken up
--   as replications, mirroring ReWire.Core.ToVerilog.bvToExp).
hCat :: [H.Exp] -> H.Exp
hCat es = case filter (not . hIsNil) es of
      [e] -> e
      es' -> H.Cat es'

hIsNil :: H.Exp -> Bool
hIsNil = \ case
      H.Lit bv -> width bv <= 0
      _        -> False

-- | Attempt to break up giant literals.
litExp :: BV -> H.Exp
litExp bv | width bv < maxLit     = H.Lit bv
          | width bv == 0         = H.Lit BV.nil
          | bv == zeros 1         = H.FunCall "rw_repl" [H.Num $ fromIntegral $ width bv, H.Lit $ zeros 1]
          | bv == ones (width bv) = H.FunCall "rw_repl" [H.Num $ fromIntegral $ width bv, H.Lit $ ones 1]
          | zs > 8                = hCat [H.Lit $ subRange (fromIntegral zs, width bv - 1) bv, H.FunCall "rw_repl" [H.Num zs, H.Lit $ zeros 1]]
          | otherwise             = H.Lit bv -- TODO(chathhorn)
      where zs :: Natural -- trailing zeros
            zs | bv == zeros 1 = fromIntegral $ width bv
               | otherwise     = fromIntegral $ lsb1 bv

            maxLit :: Int
            maxLit = 32

-- | Recover a constant from a generated expression (for the initial state).
hExpToBV :: H.Exp -> Maybe BV
hExpToBV = \ case
      H.Lit bv                               -> Just bv
      H.FunCall "rw_repl" [H.Num n, e]       -> BV.replicate (fromIntegral n) <$> hExpToBV e
      H.FunCall "rw_resize" [e, H.Num n]     -> resizeBV (fromIntegral n) <$> hExpToBV e
      H.Cat es                               -> BV.concat <$> mapM hExpToBV es
      _                                      -> Nothing
      where resizeBV :: Int -> BV -> BV
            resizeBV n bv | n <= fromIntegral (width bv) = subRange (0, n - 1) bv
                          | otherwise                    = BV.concat [zeros (n - fromIntegral (width bv)), bv]

-- | MSB-first split of a constant across named wires.
splitBV :: BV -> [(Name, H.Size)] -> [(Name, BV)]
splitBV bv tgts = snd $ foldl' split' (fromIntegral (width bv) - 1, []) tgts
      where split' :: (Int, [(Name, BV)]) -> (Name, H.Size) -> (Int, [(Name, BV)])
            split' (hi, acc) (t, w) = (hi - fromIntegral w, acc <> [(t, subRange (hi - fromIntegral w + 1, hi) bv)])

-- | Assign an expression to several wires (MSB-first): a single target is
--   assigned directly; multiple targets are sliced out of a temporary.
assignMulti :: (MonadState TS m, MonadError AstError m) => [(Name, H.Size)] -> H.Exp -> m [H.Stmt]
assignMulti tgts e = case tgts of
      [(t, w)] -> do
            e' <- wcast w e
            pure [H.Assign (H.LVName t) e']
      _        -> do
            let total = sum $ map snd tgts
            e'  <- wcast total e
            tmp <- newWire total "pause"
            pure $ H.Assign (H.LVName tmp) e' : map (slice tmp) (sliceParts total tgts)
      where slice :: Name -> (Name, (H.Index, H.Index)) -> H.Stmt
            slice tmp (t, (lo, hi)) = H.Assign (H.LVName t) $ H.Slice tmp lo hi

            sliceParts :: H.Size -> [(Name, H.Size)] -> [(Name, (H.Index, H.Index))]
            sliceParts total ts = snd $ foldl' part' (fromIntegral total - 1, []) ts
                  where part' :: (Int, [(Name, (H.Index, H.Index))]) -> (Name, H.Size) -> (Int, [(Name, (H.Index, H.Index))])
                        part' (hi, acc) (t, w) = (hi - fromIntegral w, acc <> [(t, (hi - fromIntegral w + 1, hi))])

-- | The context clause for generated units: the configured VHDL packages
--   (--vhdl-packages) plus the defaults and the emitted rw_helpers package.
unitPackages :: Config -> [Text]
unitPackages conf = foldr (\ pk pks -> if pk `elem` pks then pks else pk : pks)
                          ["ieee.std_logic_1164.all", "ieee.numeric_std.all"]
                          (conf^.vhdlPackages)
                  <> ["work.rw_helpers.all"]

-- | A testbench driving the device with interp-style inputs (one map of
--   input-name-to-value per cycle, already padded to the cycle count) and
--   printing the outputs each cycle in the same YAML format the Core
--   interpreter produces, so a simulation trace can be compared directly
--   against rwc --interpret output. Timing matches the Verilog testbench
--   (see ReWire.Core.ToVerilog.testbench).
testbench :: Config -> C.Device -> [Ins] -> H.Unit
testbench conf dev@(C.Device top w _ _ _) inps = H.Unit "tb"
      ["ieee.std_logic_1164.all", "ieee.numeric_std.all", "std.textio.all"]
      []
      [H.Component top [] $ map (\ (n, sz) -> H.Port n H.In sz) (clkRst <> ins) <> map (\ (n, sz) -> H.Port n H.Out sz) outs]
      (map (\ (n, sz) -> H.Signal n sz Nothing) $ clkRst <> ins <> outs)
      [ H.Instantiate top "dut" [] $ map ((mempty, ) . H.Var . fst) $ clkRst <> ins <> outs
      , H.Process [] [H.LineVar "l" | not (null outs)] $ resetSeq <> concatMap cyc inps <> [H.SFinish]
      ]
      where (mclk, mrst) = clockReset conf dev

            clkRst, ins, outs :: [(Text, H.Size)]
            clkRst = map (, 1) $ concatMap (maybe [] pure) [mclk, mrst]
            ins    = inputWires w
            outs   = outputWires w

            drive :: Ins -> [H.SeqStmt]
            drive i = map (\ (n, sz) -> H.SAssign (H.LVName n) $ H.Lit $ bitVec (fromIntegral sz) $ inputValue sz i n) ins

            bit :: Bool -> H.Exp
            bit = H.Lit . bitVec 1 . fromEnum

            rstActive :: Bool
            rstActive = Inverted `notElem` (conf^.C.resetFlags)

            tick :: Text -> [H.SeqStmt]
            tick clk = [H.SWait 1, H.SAssign (H.LVName clk) $ bit True, H.SWait 5, H.SAssign (H.LVName clk) $ bit False]

            resetSeq :: [H.SeqStmt]
            resetSeq = case (mclk, mrst) of
                  (Just clk, Just rst) ->
                        [ H.SAssign (H.LVName clk) $ bit False, H.SAssign (H.LVName rst) $ bit rstActive ]
                        <> drive (headIns inps)
                        <> concat (replicate 2 [H.SWait 5, H.SAssign (H.LVName clk) $ bit True, H.SWait 5, H.SAssign (H.LVName clk) $ bit False])
                        <> [ H.SAssign (H.LVName rst) $ bit $ not rstActive ]
                  (Just clk, Nothing)  -> [ H.SAssign (H.LVName clk) $ bit False ]
                  _                    -> []

            cyc :: Ins -> [H.SeqStmt]
            cyc i = drive i <> case mclk of
                  Just clk -> [H.SWait 4] <> writes <> tick clk
                  Nothing  -> [H.SWait 5] <> writes <> [H.SWait 5]

            writes :: [H.SeqStmt]
            writes = zipWith (\ pre (n, _) -> H.SWriteLn [H.ChunkLit (pre <> n <> ": '0x"), H.ChunkHex (H.Var n), H.ChunkLit "'"]) yamlPrefixes outs

            headIns :: [Ins] -> Ins
            headIns = \ case
                  i : _ -> i
                  _     -> mempty
