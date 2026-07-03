{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Safe #-}
-- | The Crust-to-Hyle producer: pattern matches compile to slices, equality
--   guards, and muxes; primitives map to the Hyle operator set (doc/hyle.md,
--   section 3.3); the purified state machine becomes an explicit device
--   (registers with initial values evaluated by the Hyle interpreter,
--   per-output and per-register slice equations); externs become per-module
--   declarations with mandatory port names; and clocked extern calls are
--   hoisted into device-level instances by inlining their containing defns
--   into the device body.
module ReWire.Crust.ToHyle (toHyle) where

import ReWire.Config (Config, inputSigs, outputSigs, stateSigs, top)
import ReWire.Annotation (Annote, noAnn, Annotated (ann))
import ReWire.Error (failAt, failAtWith, failInternal, warnAt, relocatingTo, AstError, MonadError, Warning (..))
import ReWire.Pretty (showt, prettyPrint)
import ReWire.Unbound (Name, Fresh, runFreshM, Embed (..), unbind, n2s)
import ReWire.BitVector (BV, bitVec, zeros, nbits)
import ReWire.Hyle.Interp (evalExp, IEnv (..))

import Control.Arrow ((&&&))
import Control.Lens ((^.))
import Control.Monad (unless, foldM)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, lift, mapReaderT)
import Control.Monad.State (StateT (..), MonadState, gets, modify)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.List (find, findIndex, genericLength, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict     as Map
import qualified Data.HashSet            as Set
import qualified Data.Text               as T
import qualified ReWire.BitVector        as BV
import qualified ReWire.Crust.Syntax     as M
import qualified ReWire.Crust.Types      as M
import qualified ReWire.Crust.Util       as M
import qualified ReWire.Hyle.Syntax    as A
import qualified ReWire.Hyle.Transform as A

type SizeMap       = HashMap M.Ty A.Size
type GlobalNameMap = HashMap (Name M.Exp) A.GId

data S = S
      { sSizes   :: !SizeMap
      , sNames   :: !GlobalNameMap
      , sWarns   :: ![Warning]
      , sCtr     :: !Int
      , sExterns :: !(HashMap A.Name A.Extern)
      }

s0 :: S
s0 = S mempty mempty [] 0 mempty

type ConMap = (HashMap (Name M.TyConId) [Name M.DataConId], HashMap (Name M.DataConId) M.Ty)
type TCM m  = ReaderT ConMap (ReaderT (HashMap (Name M.Exp) A.Name) m)

-- | (device name, (inputs, outputs, state wires), loop, state0)
type StartDefn = (A.Name, ([(A.Name, A.Size)], [(A.Name, A.Size)], [(A.Name, A.Size)]), A.GId, A.GId)

toHyle :: (Fresh m, MonadError AstError m, MonadFail m, MonadIO m) => Config -> Name M.Exp -> M.FreeProgram -> m A.Program
toHyle conf start (ts, _, vs) = do
      (p, s) <- flip runStateT s0 $ toHyle' conf start (ts, vs)
      mapM_ (\ (Warning an msg) -> warnAt conf an msg) $ nubOrd $ sWarns s
      pure p

toHyle' :: (Fresh m, MonadError AstError m, MonadFail m) => Config -> Name M.Exp -> ([M.DataDefn], [M.Defn]) -> StateT S m A.Program
toHyle' conf start (ts, vs) = do
      mapM_ (\ x -> ((`runReaderT` conMap) . sizeOf (n2s (M.dataName x)) (ann x) . M.TyCon (ann x) . M.dataName) x) ts
      let intSz = 128 -- width of the Integer type
      modify $ \ s -> s { sSizes = Map.singleton (M.intTy noAnn) intSz }
      buildNameMap vs
      vs' <- mapM (transDefn conf start conMap) $ filter (not . M.isPrim . M.defnName) vs
      case partitionEithers vs' of
            ([(topName, wires, loop, state0)], defns)
                  | Just defLoop   <- find ((== loop) . A.defnName) defns
                  , Just defState0 <- find ((== state0) . A.defnName) defns -> do
                        let others = filter (\ d -> A.defnName d `notElem` [loop, state0] ) defns
                        dev  <- mkDevice topName wires defLoop defState0 others
                        exts <- gets $ sortOn A.extName . Map.elems . sExterns
                        -- state0 is evaluated into the register initials,
                        -- but a restarting device's dispatch can also call
                        -- it as an ordinary defn; keep it when referenced.
                        let live = defLoop : others
                            state0Live = any (elem state0 . expCalls . A.defnBody) live
                        hoistInstances $ A.Program exts (live <> [ defState0 | state0Live ]) dev
            _ -> failAt noAnn $ "no definition found: " <> prettyPrint start
      where conMap :: ConMap
            conMap = ( Map.fromList $ map (M.dataName &&& map projId . M.dataCons) ts
                     , Map.fromList $ map (projId &&& projType) (concatMap M.dataCons ts)
                     )

            projId :: M.DataCon -> Name M.DataConId
            projId (M.DataCon _ n _) = n

            projType :: M.DataCon -> M.Ty
            projType (M.DataCon _ _ (Embed (M.Poly t))) = runFreshM (snd <$> unbind t)

-- | Assemble the device: registers from the dispatch wires (initial values
--   by evaluating state0 with the Hyle interpreter), and slice equations
--   from the loop's result layout (padding | outputs | dispatch). This is
--   the one remaining home of the resumption-layout arithmetic.
mkDevice :: (MonadError AstError m, MonadState S m) => A.Name -> ([(A.Name, A.Size)], [(A.Name, A.Size)], [(A.Name, A.Size)]) -> A.Defn -> A.Defn -> [A.Defn] -> m A.Device
mkDevice topName (inWires, outWires, stWires) defLoop defState0 others = do
      let an       = ann defLoop
          A.Sig _ loopArgSzs resSz = A.defnSig defLoop

          tagSz :: A.Size
          tagSz = case (loopArgSzs, inWires) of
                (a : _ : _, _ : _) -> a - sum (map snd stWires)
                (a : _,     [])    -> a - sum (map snd stWires)
                _                  -> 0

          regWires = [ ("__resumption_tag", tagSz) | tagSz > 0 ] <> stWires
          stInps   = regWires <> inWires
          padding  = resSz - sum (map snd outWires) - sum (map snd regWires)
          layout   = wireOffsets $ ("__padding", padding) : outWires <> regWires

      unless (sum (map snd stInps) == sum loopArgSzs) $
            failInternal an "width mismatch between the loop signature and the device's state and input wires"

      regs <- if null regWires then pure []
            else do
                  exts  <- gets sExterns
                  defns <- pure $ Map.fromList $ map (A.defnName &&& id) $ defLoop : defState0 : others
                  -- state0's arguments (the stores at the initial pause, when
                  -- the device pauses before extruding) are unobservable;
                  -- zero-fill them.
                  let A.Sig _ argSzs _ = A.defnSig defState0
                      args0            = Map.fromList $ zip (A.defnParams defState0) $ map (zeros . fromIntegral) argSzs
                  init0 <- evalExp (IEnv defns exts) args0 (A.defnBody defState0)
                        `catchError` \ _ -> failAt (ann defState0)
                              "cannot evaluate the initial state (does it involve an extern?)"
                  pure [ A.Register an x sz $ sliceBV init0 off sz | (x, sz, off) <- wireOffsets regWires ]

      let inExp    = A.cat [ A.Var an sz x | (x, sz) <- stInps ]
          inVar    = A.Var an (sum $ map snd stInps) "$in"
          loopArgs = [ slice0 an off sz inVar | (_, sz, off) <- wireOffsets (map ((), ) loopArgSzs) ]
          resVar   = A.Var an resSz "$res"
          outNames = Set.fromList $ map fst outWires
          regNames = Set.fromList $ map fst regWires

      pure $ A.Device an topName inWires outWires regs []
           $  [ A.SLet an "$in" inExp
              , A.SLet an "$res" $ A.Call an resSz (A.defnName defLoop) loopArgs ]
           <> [ A.SOutput an x $ slice0 an off sz resVar | (x, sz, off) <- layout, x `Set.member` outNames ]
           <> [ A.SNext an x $ slice0 an off sz resVar   | (x, sz, off) <- layout, x `Set.member` regNames ]
      where sliceBV :: BV -> A.Index -> A.Size -> BV
            sliceBV bv off sz = bitVec (fromIntegral sz) $ BV.nat bv `div` (2 ^ toInteger off)

expCalls :: A.Exp -> [A.GId]
expCalls = \ case
      A.Call _ _ g es    -> g : concatMap expCalls es
      A.Cat _ e1 e2      -> expCalls e1 <> expCalls e2
      A.Slice _ _ _ e    -> expCalls e
      A.Prim _ _ _ es    -> concatMap expCalls es
      A.XCall _ _ _ _ es -> concatMap expCalls es
      A.If _ _ c t e     -> expCalls c <> expCalls t <> expCalls e
      A.Let _ _ _ e1 e2  -> expCalls e1 <> expCalls e2
      _                  -> []

wireOffsets :: [(a, A.Size)] -> [(a, A.Size, A.Index)]
wireOffsets = snd . foldr step (0, [])
      where step (x, sz) (off, acc) = (off + fromIntegral sz, (x, sz, off) : acc)

slice0 :: Annote -> A.Index -> A.Size -> A.Exp -> A.Exp
slice0 an off sz e | sz == 0   = A.Lit an BV.nil
                   | otherwise = A.Slice an off sz e

transDefn :: (MonadError AstError m, Fresh m, MonadState S m, MonadFail m) => Config -> Name M.Exp -> ConMap -> M.Defn -> m (Either StartDefn A.Defn)
transDefn conf start conMap = \ case
      M.Defn an n (Embed (M.Poly t)) _ (Embed e) | n == start -> do
            (_, t')  <- unbind t
            case t' of
                  M.TyApp _ (M.TyApp _ (M.TyApp _ (M.TyApp _ (M.TyCon _ (n2s -> "ReacT")) t_in) t_out) (M.TyCon _ (n2s -> "Identity"))) _ -> do
                        (_, e') <- unbind e
                        case M.flattenApp e' of
                              (M.Builtin _ _ _ M.Unfold, [M.Var _ _ _ loop, M.Var _ _ (Just state0Ty) state0]) -> do
                                    t_st              <- getRegsTy state0Ty
                                    (t_in' : t_ins)   <- mapM ((`runReaderT` conMap) . sizeOf "start In" an) $ t_in  : detuple t_in
                                    (t_out' : t_outs) <- mapM ((`runReaderT` conMap) . sizeOf "start Out" an) $ t_out : detuple t_out
                                    (t_st' : t_sts)   <- mapM ((`runReaderT` conMap) . sizeOf "start State" an) $ t_st  : concatMap detuple (detuple t_st)
                                    loop'             <- transName loop
                                    state0'           <- transName state0
                                    let t_ins'         = t_in' - sum t_ins : t_ins
                                        t_outs'        = t_out' - sum t_outs : t_outs
                                        t_sts'         = t_st' - sum t_sts : t_sts
                                        wires          = ( zip (conf^.inputSigs)  (filter (> 0) t_ins')
                                                         , zip (conf^.outputSigs) (filter (> 0) t_outs')
                                                         , zip (conf^.stateSigs)  (filter (> 0) t_sts')
                                                         )
                                    pure $ Left (conf^.top, wires, loop', state0')
                              _ -> failAt an $ "definition of " <> prettyPrint start <> " must have form `unfold n m' where n and m are global IDs; got " <> prettyPrint e'
                  _ -> failAt an $ prettyPrint start <> " has unsupported type: " <> M.prettyTy t'
      M.Defn an n (Embed (M.Poly t)) _ (Embed e) -> do
            (_, t')  <- unbind t
            (xs, e') <- unbind e
            if | M.higherOrder t'       -> failAt an $ prettyPrint n <> " has unsupported higher-order type."
               | not $ M.fundamental t' -> failAt an $ prettyPrint n <> " has un-translatable String or Integer arguments."
               | otherwise              -> do
                     modify $ \ s -> s { sCtr = 0 }
                     n'   <- transName n
                     sig  <- runReaderT (transType t') conMap
                     let ps = uniquifyLocals xs
                     body <- relocatingTo an $ runReaderT (runReaderT (transExp e') conMap) (Map.fromList $ zip xs ps)
                     pure $ Right $ A.Defn an n' sig ps body
      where -- | state0 takes the (dead) initial stores as arguments when the
            --   device pauses before extruding; look through them.
            getRegsTy :: MonadError AstError m => M.Ty -> m M.Ty
            getRegsTy t = case M.codomTy t of
                  M.TyApp _ (M.TyApp _ (M.TyCon _ (n2s -> "PuRe")) s) _ -> pure s
                  _                                                     -> failAt (ann t) $ "definition of " <> prettyPrint start <> " must have form `" <> prettyPrint start <> " = unfold n m' where m has type PuRe s o."

            detuple :: M.Ty -> [M.Ty]
            detuple t = case t of
                  M.TyApp _ (M.TyApp _ (M.TyCon _ (n2s -> "Vec")) _) _  -> [t]
                  M.TyApp _ (M.TyCon _ (n2s -> "Finite")) _             -> [t]
                  _                                                     -> M.flattenTyApp t

-- | Give each (already distinct) local binder a readable Hyle name derived
--   from its source name, disambiguating base-name collisions with a numeric
--   suffix so the parameter list stays pairwise distinct (cf. 'buildNameMap'
--   for globals). Source identifiers never begin with '$', so they cannot
--   clash with the generated '$t'/'$in'/'$res' names.
uniquifyLocals :: [Name M.Exp] -> [A.Name]
uniquifyLocals = uniquifyLocalsIn mempty

-- | As 'uniquifyLocals', but avoiding a set of names already in scope, so a
--   case-bound variable never shadows (and so captures) an enclosing binding.
uniquifyLocalsIn :: HashSet A.Name -> [Name M.Exp] -> [A.Name]
uniquifyLocalsIn = go
      where go :: HashSet A.Name -> [Name M.Exp] -> [A.Name]
            go _    []       = []
            go used (v : vs) = let nm = pick used (n2s v) (0 :: Int) in nm : go (Set.insert nm used) vs

            pick :: HashSet A.Name -> Text -> Int -> A.Name
            pick used base i | cand `Set.member` used = pick used base (i + 1)
                             | otherwise              = cand
                  where cand = if i == 0 then base else base <> showt i

-- | Run a sub-translation with extra local name bindings in scope (used to
--   bind case-pattern variables to their discriminant slices).
withLocals :: Monad m => [(Name M.Exp, A.Name)] -> TCM m a -> TCM m a
withLocals binds = mapReaderT (local (Map.union (Map.fromList binds)))

---
--- Expression translation.
---

freshLocal :: MonadState S m => Text -> m A.Name
freshLocal pfx = do
      i <- gets sCtr
      modify $ \ s -> s { sCtr = i + 1 }
      pure $ pfx <> showt i

-- | Names an expression unless it is already atomic.
bindAtom :: MonadState S m => Annote -> A.Exp -> (A.Exp -> m A.Exp) -> m A.Exp
bindAtom an e k = case e of
      A.Lit {} -> k e
      A.Var {} -> k e
      _        -> do
            x  <- freshLocal "$t"
            e' <- k $ A.Var an (A.sizeOf e) x
            pure $ A.Let an (A.sizeOf e') x e e'

conj :: Annote -> [A.Exp] -> A.Exp
conj an = foldr1 $ \ a b -> A.Prim an 1 A.And [a, b]

-- | A pattern field: a variable binding, a literal to match, or padding.
data Field = FVar !A.Size | FLit !BV | FWild !A.Size

fieldSize :: Field -> A.Size
fieldSize = \ case
      FVar sz  -> sz
      FLit bv  -> fromIntegral $ BV.width bv
      FWild sz -> sz

-- | Compile a flat field vector against a discriminator: the bound
--   subfields (one per FVar) and the match conditions (one equality test
--   per nonempty FLit).
destruct :: MonadState S m => Annote -> A.Exp -> [Field] -> (([A.Exp], [A.Exp]) -> m A.Exp) -> m A.Exp
destruct an disc fields k = case fields of
      [FVar _] -> k ([disc], [])
      _        -> bindAtom an disc $ \ d -> k $ foldr (field d) ([], []) $ wireOffsets [ (f, fieldSize f) | f <- fields ]
      where field :: A.Exp -> (Field, A.Size, A.Index) -> ([A.Exp], [A.Exp]) -> ([A.Exp], [A.Exp])
            field d (f, sz, off) (args, conds) = case f of
                  FVar _              -> (slice0 an off sz d : args, conds)
                  FWild _             -> (args, conds)
                  FLit bv
                        | BV.width bv == 0 -> (args, conds)
                        | otherwise        -> (args, A.Prim an 1 A.Eq [slice0 an off sz d, A.Lit an bv] : conds)

-- | Compile a binding 'Pat' to a flat field vector together with the source
--   binder name of each 'FVar', in field order, so a case can bind its pattern
--   variables to named discriminant slices.
patFields :: (MonadError AstError m, Fresh m, MonadState S m, MonadReader ConMap m) => M.Pat -> m ([Field], [Name M.Exp])
patFields = \ case
      M.PatCon an _ (Embed t) (Embed d) ps -> do
            (v, w) <- ctorTag an t d
            sz     <- sizeOf' ("PatCon " <> n2s d) an t
            szArgs <- sum <$> mapM (sizeOf' "PatCon args" an . M.typeOf) ps
            subs   <- mapM patFields ps
            pure ([FLit $ bitVec (fromIntegral w) v, FWild $ sz - w - szArgs] <> concatMap fst subs, concatMap snd subs)
      M.PatVar an _ (Embed t) x -> do
            sz <- sizeOf' "PatVar" an t
            pure ([FVar sz], [x])
      M.PatWildCard an _ (Embed t) -> do
            sz <- sizeOf' "PatWildCard" an t
            pure ([FWild sz], [])

{- HLINT ignore "Redundant multi-way if" -}
transExp :: (MonadError AstError m, Fresh m, MonadState S m) => M.Exp -> TCM m A.Exp
transExp e = case e of
      M.App an _ _ _ _                    -> case M.flattenApp e of
            (M.Builtin an' _ _ b       , args)                               -> transBuiltin (ann e) (M.typeOf e) an' (b, args)
            (e'                        , _) | Just t' <- M.typeOf e'
                                            , not $ M.concrete t'            -> failAt an $ "could not infer a concrete type in an application. Inferred type: " <> M.prettyTy t'
            (M.Var _ _ _ x             , args)                               -> do
                  sz    <- sizeOf' ("Applied Var " <> n2s x) an $ M.typeOf e
                  args' <- mapM transExp args
                  x'    <- transName x
                  pure $ A.Call an sz x' args'
            (M.Con an' _ t d           , args)                               -> do
                  (v, w)     <- ctorTag an' (M.codomTy <$> t) d
                  args'      <- mapM transExp args
                  (tag, pad) <- ctorRep an' (M.typeOf e) (v, w) $ sum $ map A.sizeOf args'
                  pure $ A.cat ([A.Lit an' tag, A.Lit an' pad] <> args')
            _                                                                -> failInternal an $ "encountered ill-formed application:\n" <> prettyPrint e
      M.Builtin an _ _ b                  -> transBuiltin (ann e) (M.typeOf e) an (b, [])
      M.Var an _ t x                      -> do
            sz <- sizeOf' ("Var " <> n2s x) an t
            lift (asks $ Map.lookup x) >>= \ case
                  Just p  -> pure $ A.Var an sz p
                  Nothing -> do
                        x' <- transName x
                        pure $ A.Call an sz x' []
      M.Con an _ t d                      -> do
            (v, w)     <- ctorTag an t d
            (tag, pad) <- ctorRep an t (v, w) 0
            pure $ A.cat [A.Lit an tag, A.Lit an pad]
      M.Case an _ t disc bnd els          -> do
            sz           <- sizeOf' "Case" an t
            disc'        <- transExp disc
            (p, body)    <- unbind bnd
            (fields, ns) <- patFields p
            els'         <- mapM transExp els
            destruct an disc' fields $ \ (args, conds) -> do
                  inScope <- lift $ asks $ Set.fromList . Map.elems
                  let anames = uniquifyLocalsIn inScope ns
                  body'  <- withLocals (zip ns anames) $ transExp body
                  let lets = foldr (\ (nm, slc) acc -> A.Let an (A.sizeOf acc) nm slc acc) body' $ zip anames args
                  case els' of
                        Just els'' | not (null conds) -> pure $ A.If an sz (conj an conds) lets els''
                        _                             -> pure lets
      M.LitInt an _ n                     -> do
            sz <- sizeOf' "LitInt" an $ M.typeOf e
            pure $ A.Lit an $ bitVec (fromIntegral sz) n
      M.LitVec _ _ _ es                   -> A.cat <$> mapM transExp es
      _                                   -> failAt (ann e) $ "unsupported expression: " <> prettyPrint e
      where ctorRep :: (Fresh m, MonadReader ConMap m, MonadState S m, MonadError AstError m) => Annote -> Maybe M.Ty -> (A.Value, A.Size) -> A.Size -> m (BV, BV)
            ctorRep an Nothing _ _ = failInternal an "ctorRep: encountered untyped constructor (rwc bug)."
            ctorRep an (Just t) (v, w) szArgs = do
                  sz <- sizeOf "ctorRep" an t
                  if | w + szArgs <= sz -> pure (bitVec (fromIntegral w) v, zeros $ fromIntegral sz - fromIntegral w - fromIntegral szArgs)
                     | otherwise        -> failInternal an $ "failing to calculate the bitvector representation of a constructor of type (sz: "
                                                    <> showt sz <> " w: " <> showt w <> " szArgs: " <> showt szArgs <> "):\n" <> M.prettyTy t

---
--- Builtins.
---

-- | Builtins in expression position (with untranslated Crust arguments).
transBuiltin :: (MonadError AstError m, Fresh m, MonadState S m) => Annote -> Maybe M.Ty -> Annote -> (M.Builtin, [M.Exp]) -> TCM m A.Exp
transBuiltin an' t' an theExp = case theExp of
      (M.Error, args) -> do
            sz <- sizeOf' "rwPrimError" an t'
            addWarning an' $ "encountered a live call to the built-in \"error\" function; compiling to a zero (don't-care) value of width " <> showt sz <> "."
                  <> (case args of
                        [M.LitStr _ _ x] -> "\nError message: " <> showt x
                        _                -> mempty)
            pure $ A.Undef an' sz
      (M.Bits, [arg]) -> transExp arg
      (M.Resize, [arg]) -> do
            sz <- sizeOf' "rwPrimResize" an t'
            resize an sz =<< transExp arg
      (M.BitIndex, [arg, M.App _ _ _ (M.Builtin _ _ _ M.Finite) (M.LitInt _ _ i)]) -> subElems an arg ((-i) - 1) 1
      (M.BitSlice, [arg, M.App _ _ _ (M.Builtin _ _ _ M.Finite) (M.LitInt _ _ j)
                       , M.App _ _ _ (M.Builtin _ _ _ M.Finite) (M.LitInt _ _ i)]) -> do
            unless (j + 1 >= i) $ failAt (ann arg)
                  $ "invalid bit slice (j: " <> showt j <> ", i: " <> showt i <> ")."
            let nBits = fromIntegral $ j + 1 - i
                off   = (-i) - fromIntegral nBits
            subElems an arg off nBits
      (M.BitSlice, _) -> failAt an' "rwPrimBitSlice must have arguments (finite j) (finite i) with LitInts"
      (M.VecIndex, [arg, i]) -> vecIndex arg i
      (M.VecIndexProxy, [arg, p]) -> do
            i <- checkProxyArg "rwPrimVecIndexProxy" p
            subElems an arg i 1
      (M.NatVal, [p]) -> do
            i <- checkProxyArg "rwPrimNatVal" p
            transExp $ M.LitInt an Nothing i
      (M.VecSlice, [p, arg]) -> do
            i      <- checkProxyArg "rwPrimVecSlice" p
            nElems <- checkVecArgSize "rwPrimVecSlice" t'
            subElems an arg i nElems
      (M.VecRSlice, [p, arg]) -> do
            i      <- checkProxyArg "rwPrimVecRSlice" p
            nElems <- checkVecArgSize "rwPrimVecRSlice" t'
            subElems an arg ((- i) - fromIntegral nElems) nElems
      (M.VecReverse, [arg]) -> do
            arg'   <- transExp arg
            nElems <- checkVecArgSize "rwPrimVecReverse" t'
            tyElem <- checkVecArgType "rwPrimVecReverse" t'
            szElem <- sizeOf "rwPrimVecReverse" an tyElem
            bindAtom an arg' $ \ a ->
                  pure $ A.cat $ reverse [ slice0 an off szElem a | (_, _, off) <- wireOffsets $ replicate (fromIntegral nElems) ((), szElem) ]
      (M.VecReplicate, [arg]) -> do
            sz     <- sizeOf' "rwPrimVecReplicate" an t'
            arg'   <- transExp arg
            nElems <- checkVecArgSize "rwPrimVecReplicate" t'
            pure $ if nElems == 0 || sz == 0 then A.Lit an BV.nil
                   else A.Prim an sz (A.Rep nElems) [arg']
      (M.VecMap, [f, arg]) -> do
            nElems <- checkVecArgSize "rwPrimVecMap" t'
            transExp $ M.LitVec an Nothing Nothing $ map (M.mkApp an f . pure . vecIndexProxy arg) [0 .. nElems - 1]
      (M.VecGenerate, [f]) -> do
            nElems <- checkVecArgSize "rwPrimVecGenerate" t'
            transExp $ M.LitVec an Nothing Nothing $ map (M.mkApp an f . pure . finite nElems) [0 .. nElems - 1]
      (M.VecConcat, [arg1, arg2]) -> A.cat <$> mapM transExp [arg1, arg2]
      (M.Finite, [arg]) -> do
            arg' <- transExp arg
            case arg' of
                  A.Lit an'' (BV.nat -> i) -> do
                        finSz <- checkFinTypeMax "rwPrimFinite" t'
                        unless (i >= 0 && i < fromIntegral finSz)
                              $ failAt (ann arg) ("rwPrimFinite: Integer " <> showt i <> " is not representable in Finite " <> showt finSz <> ".")
                        pure $ A.Lit an'' $ bitVec (fromIntegral $ nbits finSz) i
                  _ -> failAt (ann arg) "rwPrimFinite: can't determine argument value at compile-time."
      (M.FiniteMinBound, []) -> do
            finSz <- checkFinTypeMax "rwPrimFiniteMinBound" t'
            unless (finSz > 0)
                  $ failAt an "rwPrimFiniteMinBound: Finite 0 is uninhabited."
            transExp $ finite finSz 0
      (M.FiniteMaxBound, []) -> do
            finSz <- checkFinTypeMax "rwPrimFiniteMaxBound" t'
            unless (finSz > 0)
                  $ failAt an "rwPrimFiniteMaxBound: Finite 0 is uninhabited."
            transExp $ finite finSz $ finSz - 1
      (M.ToFinite, [arg]) -> do
            finSz  <- checkFinTypeMax "rwPrimToFinite" t'
            sz     <- sizeOf' "rwPrimToFinite" an t'
            argTy  <- maybe (failAt an' $ "rwPrimToFinite: invalid argument type: " <> showt (M.prettyTy <$> M.typeOf arg)) pure
                        $ M.typeOf arg
            nBits  <- checkVecArgSize "rwPrimToFinite" (Just argTy)
            unless (2 ^ nBits <= (fromIntegral finSz :: Integer))
                  $ failAt (ann arg) ("rwPrimToFinite: bitvector argument (size " <> showt nBits <> ") is not representable in Finite " <> showt finSz <> ".")
            resize an sz =<< transExp arg
      (M.ToFiniteMod, [arg]) -> do
            finSz   <- checkFinTypeMax "rwPrimToFiniteMod" t'
            sz      <- sizeOf' "rwPrimToFiniteMod" an t'
            argTy   <- maybe (failAt an' "rwPrimToFiniteMod: invalid argument type.") pure
                        $ M.typeOf arg
            nBits   <- checkVecArgSize "rwPrimToFiniteMod" (Just argTy)
            arg'    <- transExp arg
            if 2 ^ nBits <= (fromIntegral finSz :: Integer) then resize an sz arg'
            else do
                  -- arg `mod` finSz, at the width of the wider operand, then
                  -- resized to the Finite's width.
                  let szLit  = fromIntegral $ nbits finSz
                      w      = max (fromIntegral nBits) szLit
                  a <- resize an w arg'
                  resize an sz $ A.Prim an w A.UMod [a, A.Lit an $ bitVec (fromIntegral w) $ toInteger finSz]
      (M.FromFinite, [arg]) -> do
            finSz <- checkFinTypeMax "rwPrimFromFinite" (M.typeOf arg)
            nBits <- checkVecArgSize "rwPrimFromFinite" t'
            unless ((fromIntegral finSz :: Integer) <= 2 ^ nBits)
                  $ failAt (ann arg) ("rwPrimFromFinite: Finite " <> showt finSz <> " is not representable in bitvector of size " <> showt nBits <> ".")
            resize an (fromIntegral nBits) =<< transExp arg
      (b, args) | Just _ <- toPrim b 0 0 -> do
            sz    <- sizeOf' (showt b) an t'
            args' <- mapM transExp args
            lift $ lift $ applyPrim an sz b args'
      (M.Extern, M.LitList _ _ _ ps : M.LitStr _ _ clk : M.LitStr _ _ rst : M.LitList _ _ _ as : M.LitList _ _ _ rs : M.LitStr _ _ s : a : M.LitStr _ _ _inst : args)
            | (arity <$> M.typeOf a) == Just (length args) -> do
            sz    <- sizeOf' "rwPrimExtern" an t'
            args' <- mapM transExp args
            applyExtern an' sz (ps, clk, rst, as, rs, s) a args'
      (M.Extern,  _) -> failInternal an "encountered not-fully-applied extern (after inlining)."
      (b, _) | b `elem` [M.Bind, M.Return]
                     -> failAt an ("encountered unsupported builtin use: rwPrim" <> showt b
                          <> " (a monadic operator at a monad other than the ReacT/StateT/Identity stack?).")
      (b, _)         -> failAt an ("encountered unsupported builtin use: rwPrim" <> showt b <> ".")

      where subElems :: (Fresh m, MonadError AstError m, MonadState S m) => Annote -> M.Exp -> Integer -> Natural -> TCM m A.Exp
            subElems an'' arg i nElems = do
                  tyElem <- maybe (failAt (ann arg) "non-vector type argument to built-in vector function") pure
                          $ M.typeOf arg >>= M.vecElemTy
                  szElem <- fromIntegral <$> sizeOf "subElems" an'' tyElem
                  arg'   <- transExp arg

                  let sz, off, n :: Natural
                      sz  = fromIntegral $ A.sizeOf arg'
                      off = fromIntegral $ (if i < 0 then fromIntegral sz else 0) + i * szElem
                      n   = nElems * fromIntegral szElem

                  unless (sz >= off + n)
                        $ failAt an'' $ "invalid bit slice (offset: " <> showt i <> ", num elems: " <> showt nElems <> ") from object size " <> showt sz <> "."

                  -- LSB offset of the slice: fields count from the MSB end.
                  let lsbOff = sz - off - n
                  bindAtom an'' arg' $ \ a -> pure $ slice0 an'' (fromIntegral lsbOff) (fromIntegral n) a

            vecIndexProxy :: M.Exp -> Natural -> M.Exp
            vecIndexProxy v i = M.mkApp an (M.Builtin an Nothing Nothing M.VecIndexProxy) [v, M.proxy i]

            finite :: Natural -> Natural -> M.Exp
            finite n i = M.mkApp an (M.Builtin an Nothing (Just $ M.intTy an `M.arr` M.finiteTy an n) M.Finite) [lit i]

            -- | index v i = resize szElem (v >> ((n - i - 1) * szElem)), with
            --   the shift amount computed at the index's width (or the
            --   literal width, whichever is wider).
            vecIndex :: (MonadError AstError m, Fresh m, MonadState S m) => M.Exp -> M.Exp -> TCM m A.Exp
            vecIndex v i = do
                  tyVec  <- maybe (failAt an "rwPrimIndex: invalid vector argument.") pure $ M.typeOf v
                  szVec  <- sizeOf "rwPrimIndex" an tyVec
                  n      <- maybe (failAt an "rwPrimIndex: invalid Vec argument.") pure $ M.vecSize tyVec
                  tyElem <- maybe (failAt an "rwPrimIndex: non-vector type argument.") pure $ M.vecElemTy tyVec
                  szElem <- sizeOf "rwPrimIndex" an tyElem
                  tyIdx  <- maybe (failAt an "rwPrimIndex: invalid index argument.") pure $ M.typeOf i
                  szIdx  <- sizeOf "rwPrimIndex" an tyIdx
                  szLit  <- sizeOf "rwPrimIndex" an $ M.intTy an

                  v' <- transExp v
                  i' <- transExp i

                  -- ((n - i) - 1) * szElem, all at width w.
                  let w     = max szIdx szLit
                  i''  <- resize an w i'
                  let amt = A.Prim an w A.Mul
                              [ A.Prim an w A.Sub
                                    [ A.Prim an w A.Sub [A.Lit an (bitVec (fromIntegral w) $ toInteger n), i'']
                                    , A.Lit an $ bitVec (fromIntegral w) (1 :: Integer) ]
                              , A.Lit an $ bitVec (fromIntegral w) $ toInteger szElem ]
                  resize an szElem $ A.Prim an szVec A.LShr [v', amt]

            lit :: Integral n => n -> M.Exp
            lit = M.LitInt an Nothing . fromIntegral

            checkProxyArg :: (MonadError AstError m) => Text -> M.Exp -> TCM m Integer
            checkProxyArg t p = maybe (failAt an' $ t <> ": invalid Proxy argument.  " <> prettyPrint p <> " : " <> showt (M.prettyTy <$> M.typeOf p)) (pure . fromIntegral)
                        $ M.typeOf p >>= M.proxyNat

            checkVecArgSize :: (MonadError AstError m) => Text -> Maybe M.Ty -> TCM m Natural
            checkVecArgSize s t = maybe (failAt an' $ s <> ": invalid Vec size argument: " <> showt (M.prettyTy <$> t)) pure
                        $ t >>= M.vecSize

            checkVecArgType :: (MonadError AstError m) => Text -> Maybe M.Ty -> TCM m M.Ty
            checkVecArgType s t = maybe (failAt an' $ s <> ": invalid Vec type argument: " <> showt (M.prettyTy <$> t)) pure
                        $ t >>= M.vecElemTy

            checkFinTypeMax :: (MonadError AstError m) => Text -> Maybe M.Ty -> TCM m Natural
            checkFinTypeMax s t = maybe (failAt an' $ s <> ": invalid Finite type: "  <> showt (M.prettyTy <$> t)) pure
                        $ t >>= M.finSz

-- | Apply a (width-implicit, Crust-level) primitive to translated arguments,
--   expanding to the Hyle operator set (doc/hyle.md, section 3.3).
applyPrim :: (MonadError AstError m, MonadState S m) => Annote -> A.Size -> M.Builtin -> [A.Exp] -> m A.Exp
applyPrim an sz b args = do
      x <- freshLocal "$t" -- may go unused (MSBit needs one)
      case toPrim b sz (case args of { a : _ -> A.sizeOf a; _ -> 0 }) of
            Just f  -> either (failAt an) pure $ f an x args
            Nothing -> failInternal an $ "applyPrim: unsupported primitive: rwPrim" <> showt b <> " with " <> showt (length args) <> " arguments."

-- | The primitive table: given the result width and the (first) operand
--   width, a pure applicator (taking a pre-generated fresh name for the
--   cases that need a binding). Mirrors the bridge's transPrim.
toPrim :: M.Builtin -> A.Size -> A.Size -> Maybe (Annote -> A.Name -> [A.Exp] -> Either Text A.Exp)
toPrim b sz w = case b of
      M.Add         -> bin A.Add
      M.Sub         -> bin A.Sub
      M.Mul         -> bin A.Mul
      M.Div         -> bin A.UDiv
      M.Mod         -> bin A.UMod
      M.Pow         -> bin A.Pow
      M.LAnd        -> Just $ \ an _ -> two an $ \ a b' -> pure $ A.Prim an 1 A.And [redor an a, redor an b']
      M.LOr         -> Just $ \ an _ -> two an $ \ a b' -> pure $ A.Prim an 1 A.Or  [redor an a, redor an b']
      M.And         -> bin A.And
      M.Or          -> bin A.Or
      M.XOr         -> bin A.XOr
      M.XNor        -> Just $ \ an _ -> two an $ \ a b' -> pure $ A.Prim an sz A.Not [A.Prim an sz A.XOr [a, b']]
      M.LShift      -> bin A.Shl
      M.RShift      -> bin A.LShr
      M.RShiftArith -> bin A.AShr
      M.Eq          -> bin A.Eq
      M.Gt          -> bin A.UGt
      M.GtEq        -> bin A.UGe
      M.Lt          -> bin A.ULt
      M.LtEq        -> bin A.ULe
      M.LNot        -> Just $ \ an _ -> one an $ \ a -> pure $ A.Prim an 1 A.Not [redor an a]
      M.Not         -> Just $ \ an _ -> one an $ \ a -> pure $ A.Prim an sz A.Not [a]
      M.RAnd        -> red A.RedAnd
      M.RNAnd       -> redNot A.RedAnd
      M.ROr         -> red A.RedOr
      M.RNor        -> redNot A.RedOr
      M.RXOr        -> red A.RedXOr
      M.RXNor       -> redNot A.RedXOr
      M.MSBit       -> Just $ \ an x -> one an $ \ a ->
            if w >= 1 then pure $ case a of
                  A.Var {} -> A.Slice an (w - 1) 1 a
                  A.Lit {} -> A.Slice an (w - 1) 1 a
                  _        -> A.Let an 1 x a $ A.Slice an (w - 1) 1 $ A.Var an w x
            else Left "toHyle: MSBit of a zero-width value."
      _             -> Nothing
      where bin :: A.Op -> Maybe (Annote -> A.Name -> [A.Exp] -> Either Text A.Exp)
            bin op = Just $ \ an _ -> two an $ \ a b' -> pure $ A.Prim an sz op [a, b']

            red :: A.Op -> Maybe (Annote -> A.Name -> [A.Exp] -> Either Text A.Exp)
            red op = Just $ \ an _ -> one an $ \ a -> pure $ A.Prim an 1 op [a]

            redNot :: A.Op -> Maybe (Annote -> A.Name -> [A.Exp] -> Either Text A.Exp)
            redNot op = Just $ \ an _ -> one an $ \ a -> pure $ A.Prim an 1 A.Not [A.Prim an 1 op [a]]

            redor :: Annote -> A.Exp -> A.Exp
            redor an a = A.Prim an 1 A.RedOr [a]

            two :: Annote -> (A.Exp -> A.Exp -> Either Text A.Exp) -> [A.Exp] -> Either Text A.Exp
            two an k = \ case
                  [a, b'] -> k a b'
                  es      -> Left $ "toHyle: primitive arity mismatch at " <> showt an <> " (expected 2, got " <> showt (length es) <> ")."

            one :: Annote -> (A.Exp -> Either Text A.Exp) -> [A.Exp] -> Either Text A.Exp
            one an k = \ case
                  [a] -> k a
                  es  -> Left $ "toHyle: primitive arity mismatch at " <> showt an <> " (expected 1, got " <> showt (length es) <> ")."

resize :: (MonadError AstError m, MonadState S m) => Annote -> A.Size -> A.Exp -> m A.Exp
resize an sz a
      | sz == A.sizeOf a = pure a
      | sz >  A.sizeOf a = pure $ A.Prim an sz (A.ZExt sz) [a]
      | otherwise        = pure $ A.Prim an sz (A.Trunc sz) [a]

---
--- Externs.
---

-- | Declare (or merge) the extern and apply it: a combinational extern
--   becomes an XCall; a clocked one becomes an XCall too, to be hoisted into
--   a device-level instance by hoistInstances. The model argument is
--   attached to the declaration when usable.
applyExtern :: (MonadError AstError m, Fresh m, MonadState S m) => Annote -> A.Size -> ([M.Exp], Text, Text, [M.Exp], [M.Exp], Text) -> M.Exp -> [A.Exp] -> TCM m A.Exp
applyExtern an sz (ps, clk, rst, as, rs, s) a args = do
      model <- case a of
            M.Var _ _ _ x
                  | T.null clk && T.null rst -> Just <$> transName x
                  | otherwise                -> do
                        addWarning an $ "Ignoring the Haskell model for clocked extern '" <> s
                                     <> "': clocked externs are stateful and cannot be modeled by a pure function."
                        pure Nothing
            (M.flattenApp -> (M.Builtin _ _ _ M.Error, _)) -> pure Nothing -- The neutered placeholder.
            _ -> do
                  addWarning an $ "Ignoring the Haskell model for extern '" <> s
                               <> "': the model reference did not survive transformation (rwc bug?)."
                  pure Nothing

      let kind | T.null clk && T.null rst = A.Comb
               | otherwise                = A.Seq (mb clk) (mb rst)
          (gnames, gvals) = unzip $ generics ps
          argSzs   = map A.sizeOf args
          ins      = named (portOffset kind) $ fromMaybe (map (mempty, ) argSzs) $ ports as
          outs     = named (portOffset kind + length ins) $ fromMaybe [(mempty, sz)] $ ports rs
          decl     = A.Extern an s gnames kind ins outs model

      old <- gets $ Map.lookup s . sExterns
      decl' <- maybe (pure decl) (mergeExt decl) old
      modify $ \ st -> st { sExterns = Map.insert s decl' $ sExterns st }
      pure $ A.XCall an sz s gvals args
      where mb :: Text -> Maybe A.Name
            mb x | T.null x  = Nothing
                 | otherwise = Just x

            -- | Generic parameters: (name, value) pairs from the descriptor.
            generics :: [M.Exp] -> [(A.Name, Natural)]
            generics = zipWith generic [0 :: Int ..]
                  where generic i = \ case
                              M.App _ _ _ (M.App _ _ _ (M.Con _ _ _ (n2s -> "(,)")) (M.LitStr _ _ p)) (M.LitInt _ _ v)
                                    | not (T.null p) -> (p, fromIntegral v)
                                    | otherwise      -> ("g" <> showt i, fromIntegral v)
                              _                      -> ("g" <> showt i, 0)

            ports :: [M.Exp] -> Maybe [(Text, A.Size)]
            ports = \ case
                  [] -> Nothing
                  es -> mapM (\ case
                        M.App _ _ _ (M.App _ _ _ (M.Con _ _ _ (n2s -> "(,)")) (M.LitStr _ _ p)) (M.LitInt _ _ v)
                              -> pure (p, fromIntegral v)
                        _     -> Nothing) es

            portOffset :: A.ExternKind -> Int
            portOffset = \ case
                  A.Comb      -> 0
                  A.Seq mc mr -> length $ filter (/= Nothing) [mc, mr]

            -- | Anonymous ports are named p<i> by their position in the
            --   clock-reset-inputs-outputs list (the established convention).
            named :: Int -> [(Text, A.Size)] -> [(A.Name, A.Size)]
            named off = zipWith (\ i (p, psz) -> (if T.null p then "p" <> showt i else p, psz)) [off ..]

            mergeExt :: MonadError AstError m => A.Extern -> A.Extern -> m A.Extern
            mergeExt new old
                  | A.extKind new == A.extKind old
                  , A.extInputs new == A.extInputs old
                  , A.extOutputs new == A.extOutputs old
                  , A.extGenerics new == A.extGenerics old = case (A.extModel new, A.extModel old) of
                        (Just g1, Just g2) | g1 /= g2 -> failAt an $ "Extern '" <> s <> "' has conflicting models (" <> g1 <> ", " <> g2 <> ")."
                        (mg, mg')                     -> pure $ old { A.extModel = maybe mg' Just mg }
                  | otherwise = failAtWith an ("Extern '" <> s <> "' is used with inconsistent signatures.")
                        [(ann old, "First used here:")] []

---
--- Hoisting clocked externs into device instances.
---

-- | Defns containing (transitively) calls to sequential externs are inlined
--   into the device body; sequential-extern calls in the device body then
--   become instances: nested lets are flattened to device-level lets (sound:
--   the language is pure and total) and each seq-XCall is replaced by the
--   concatenation of a fresh instance's output ports, with its inputs driven
--   from the call's arguments.
hoistInstances :: (MonadError AstError m, MonadState S m) => A.Program -> m A.Program
hoistInstances p@(A.Program exts ds _)
      | Set.null clocked = pure p
      | otherwise = do
            let A.Program exts' ds' dev' = A.inlineBy (`Set.member` clocked) p
            (insts, stmts) <- foldM hoistStmt ([], []) $ A.devBody dev'
            pure $ A.Program exts' ds' $ dev' { A.devInstances = reverse insts, A.devBody = reverse stmts }
      where extKinds :: HashMap A.Name A.ExternKind
            extKinds = Map.fromList $ map (A.extName &&& A.extKind) exts

            isSeq :: A.Name -> Bool
            isSeq x = case Map.lookup x extKinds of
                  Just (A.Seq _ _) -> True
                  _                -> False

            -- | Defns whose bodies transitively contain seq-extern calls.
            clocked :: HashSet A.GId
            clocked = fix' step direct
                  where direct = Set.fromList [ A.defnName d | d <- ds, anySeq $ A.defnBody d ]

                        step s = Set.fromList [ A.defnName d
                                              | d <- ds
                                              , A.defnName d `Set.member` s
                                                    || any (`Set.member` s) (expCalls $ A.defnBody d) ]

                        fix' f s | s' == s   = s
                                 | otherwise = fix' f s'
                              where s' = f s

            anySeq :: A.Exp -> Bool
            anySeq = \ case
                  A.XCall _ _ x _ es -> isSeq x || any anySeq es
                  A.Cat _ e1 e2      -> anySeq e1 || anySeq e2
                  A.Slice _ _ _ e    -> anySeq e
                  A.Prim _ _ _ es    -> any anySeq es
                  A.Call _ _ _ es    -> any anySeq es
                  A.If _ _ c t e     -> anySeq c || anySeq t || anySeq e
                  A.Let _ _ _ e1 e2  -> anySeq e1 || anySeq e2
                  _                  -> False

            -- | Flatten an expression's nested lets to device-level lets
            --   (renamed fresh), then convert its seq-XCalls.
            hoistStmt :: (MonadError AstError m, MonadState S m) => ([A.Instance], [A.Stmt]) -> A.Stmt -> m ([A.Instance], [A.Stmt])
            hoistStmt (insts, stmts) stmt = do
                  let (an, rebuild, e) = openStmt stmt
                  (e', insts', stmts') <- hoistExp an mempty (insts, stmts) e
                  pure (insts', rebuild e' : stmts')

            openStmt :: A.Stmt -> (Annote, A.Exp -> A.Stmt, A.Exp)
            openStmt = \ case
                  A.SLet an x e      -> (an, A.SLet an x, e)
                  A.SOutput an x e   -> (an, A.SOutput an x, e)
                  A.SNext an x e     -> (an, A.SNext an x, e)
                  A.SInstIn an x q e -> (an, A.SInstIn an x q, e)

            -- | Bottom-up: substitute renamed lets, lift each Let binding to
            --   a device-level SLet, and convert seq-XCalls to instances.
            hoistExp :: (MonadError AstError m, MonadState S m) => Annote -> HashMap A.Name A.Exp -> ([A.Instance], [A.Stmt]) -> A.Exp -> m (A.Exp, [A.Instance], [A.Stmt])
            hoistExp an sub acc@(insts, stmts) = \ case
                  e@A.Lit {}   -> pure (e, insts, stmts)
                  e@A.Undef {} -> pure (e, insts, stmts)
                  e@(A.Var _ _ x) -> pure (fromMaybe e $ Map.lookup x sub, insts, stmts)
                  A.Cat an' e1 e2 -> do
                        (e1', i1, s1) <- hoistExp an sub acc e1
                        (e2', i2, s2) <- hoistExp an sub (i1, s1) e2
                        pure (A.Cat an' e1' e2', i2, s2)
                  A.Slice an' i k e -> do
                        (e', i1, s1) <- hoistExp an sub acc e
                        pure (A.Slice an' i k e', i1, s1)
                  A.Prim an' sz op es -> do
                        (es', i1, s1) <- hoistExps an sub acc es
                        pure (A.Prim an' sz op es', i1, s1)
                  A.Call an' sz g es -> do
                        (es', i1, s1) <- hoistExps an sub acc es
                        pure (A.Call an' sz g es', i1, s1)
                  A.If an' sz c t e -> do
                        (c', i1, s1) <- hoistExp an sub acc c
                        (t', i2, s2) <- hoistExp an sub (i1, s1) t
                        (e', i3, s3) <- hoistExp an sub (i2, s2) e
                        pure (A.If an' sz c' t' e', i3, s3)
                  A.Let an' _ x e1 e2 -> do
                        (e1', i1, s1) <- hoistExp an sub acc e1
                        x' <- freshLocal "$h"
                        hoistExp an (Map.insert x (A.Var an' (A.sizeOf e1') x') sub) (i1, A.SLet an' x' e1' : s1) e2
                  A.XCall an' sz x cs es
                        | isSeq x -> do
                              (es', i1, s1) <- hoistExps an sub acc es
                              i  <- freshLocal "$x"
                              ex <- gets $ Map.lookup x . sExterns
                              case ex of
                                    Nothing -> failAt an' $ "unknown extern: " <> x
                                    Just e  -> do
                                          let inst   = A.Instance an' i x cs
                                              drives = [ A.SInstIn an' i p arg | ((p, _), arg) <- zip (A.extInputs e) es' ]
                                              result = A.cat [ A.Var an' psz (i <> "." <> q) | (q, psz) <- A.extOutputs e ]
                                          pure (result, inst : i1, reverse drives <> s1)
                        | otherwise -> do
                              (es', i1, s1) <- hoistExps an sub acc es
                              pure (A.XCall an' sz x cs es', i1, s1)

            hoistExps :: (MonadError AstError m, MonadState S m) => Annote -> HashMap A.Name A.Exp -> ([A.Instance], [A.Stmt]) -> [A.Exp] -> m ([A.Exp], [A.Instance], [A.Stmt])
            hoistExps an sub acc = foldM
                  (\ (es', i1, s1) e -> (\ (e', i2, s2) -> (es' <> [e'], i2, s2)) <$> hoistExp an sub (i1, s1) e)
                  ([], fst acc, snd acc)

---
--- Types, sizing, names.
---

arity :: M.Ty -> Int
arity = length . M.paramTys

transType :: (Fresh m, MonadError AstError m, MonadState S m) => M.Ty -> ReaderT ConMap m A.Sig
transType t = A.Sig (ann t) <$> mapM (sizeOf "transType param" $ ann t) (M.paramTys t) <*> sizeOf "transType codomain" (ann t) (M.codomTy t)

matchTy :: MonadError AstError m => Annote -> M.Ty -> M.Ty -> m TySub
matchTy an (M.TyApp _ t1 t2) (M.TyApp _ t1' t2') = do
      s1 <- matchTy an t1 t1'
      s2 <- matchTy an t2 t2'
      merge an s1 s2
matchTy _ (M.TyVar _ _ v) t                    = pure [(showt v, t)]
matchTy _ _ _ = pure []

merge :: MonadError AstError m => Annote -> TySub -> TySub -> m TySub
merge an s'  = \ case
      []          -> pure s'
      (v, t) : s -> case lookup v s' of
            Nothing           -> ((v, t) :) <$> merge an s' s
            Just t' | t == t' -> merge an s' s
            Just t'           -> failInternal an
                  $ "merge: inconsistent assignment of tyvar " <> v
                  <> ": " <> M.prettyTy t <> " vs. " <> M.prettyTy t'

type TySub = [(Text, M.Ty)]

ctorWidth :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState S m) => HashSet M.Ty -> M.Ty -> Name M.DataConId -> m A.Size
ctorWidth visited t d = do
      let t' = M.codomTy t
      getCtorType d >>= \ case
            Just ct -> do
                  let (targs, tres) = M.flattenArrow ct
                  s <- matchTy (ann t') tres t'
                  sum <$> mapM (sizeOfW "ctorWidth" (ann t) visited . apply s) targs
            _ -> pure 0
      where apply :: TySub -> M.Ty -> M.Ty
            apply s = \ case
                  M.TyApp an t1 t2  -> M.TyApp an (apply s t1) $ apply s t2
                  t'@(M.TyVar _ _ i) -> fromMaybe t' $ lookup (showt i) s
                  t'                 -> t'

getCtors :: MonadReader ConMap m => Name M.TyConId -> m [Name M.DataConId]
getCtors n = asks (fromMaybe [] . Map.lookup n . fst)

getCtorType :: MonadReader ConMap m => Name M.DataConId -> m (Maybe M.Ty)
getCtorType n = asks (Map.lookup n . snd)

ctorTag :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState S m) => Annote -> Maybe M.Ty -> Name M.DataConId -> m (A.Value, A.Size)
ctorTag an Nothing _  = failInternal an "ctorTag: encountered untyped constructor (rwc bug)"
ctorTag an (Just t) d = case M.flattenTyApp t of
      M.TyCon _ c : _ -> do
            ctors <- getCtors c
            case findIndex ((== n2s d) . n2s) ctors of
                  Just idx -> pure (toInteger idx, fromIntegral $ nbits $ genericLength ctors)
                  Nothing  -> failInternal an $ "ctorTag: unknown ctor: " <> prettyPrint (n2s d) <> " of type " <> prettyPrint (n2s c)
      _               -> failInternal an $ "ctorTag: unexpected type: " <> M.prettyTy t

sizeOf' :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState S m) => Text -> Annote -> Maybe M.Ty -> m A.Size
sizeOf' s an = \ case
      Nothing -> failInternal an "encountered an untyped expression (rwc bug)."
      Just t  -> sizeOf s an t

sizeOf :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState S m) => Text -> Annote -> M.Ty -> m A.Size
sizeOf s an = sizeOfW s an mempty

sizeOfW :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState S m) => Text -> Annote -> HashSet M.Ty -> M.Ty -> m A.Size
sizeOfW s an visited t = do
      m  <- gets sSizes
      sz <- case Map.lookup t m of
            Nothing -> case M.flattenTyApp t of
                  M.TyCon _ (n2s -> "Vec") : [M.evalNat -> Just n, t''] -> (fromIntegral n *) <$> sizeOfW s an visited t''
                  M.TyCon _ (n2s -> "Vec") : [n, t'']                   -> failAt an $ "can't determine the size of a Vec."
                                                                                    <> " (Vec " <> M.prettyTy n <> " " <> M.prettyTy t'' <> ")"
                  M.TyCon _ (n2s -> "Finite") : [M.evalNat -> Just n]   -> pure $ fromIntegral $ nbits n
                  M.TyCon _ (n2s -> "Finite") : [n]                     -> failAt an $ "can't determine the size of a Finite."
                                                                                    <> " (Finite " <> M.prettyTy n <> ")"
                  M.TyCon _ c              : _
                        | t `Set.member` visited                        -> failAt an $ "can't determine the size of a recursive datatype: " <> n2s c
                        | otherwise                                     -> do
                              ctors      <- getCtors c
                              ctorWidths <- mapM (ctorWidth (Set.insert t visited) t) ctors
                              pure $ fromIntegral (nbits $ genericLength ctors) + maximum (0 : ctorWidths)
                  M.TyApp {}               : _                          -> failInternal an $ s <> ": sizeOf: got TyApp after flattening (rwc bug): " <> M.prettyTy t
                  M.TyVar {}               : _                          -> pure 0
                  _                                                     -> failAt an $ s <> ": couldn't calculate the size of a type: " <> M.prettyTy t
            Just sz -> pure sz
      modify $ \ st -> st { sSizes = Map.insert t sz $ sSizes st }
      pure sz

addWarning :: MonadState S m => Annote -> Text -> m ()
addWarning an msg = modify $ \ s -> s { sWarns = sWarns s <> [Warning an msg] }

transName :: MonadState S m => Name M.Exp -> m A.GId
transName n = gets (Map.lookup n . sNames) <&> fromMaybe (showt n)

buildNameMap :: MonadState S m => [M.Defn] -> m ()
buildNameMap vs = modify $ \ s -> s { sNames = fst $ foldr (build' . M.defnName) mempty vs }
      where build' :: Name M.Exp -> (GlobalNameMap, (HashMap Text Integer, HashSet A.GId)) -> (GlobalNameMap, (HashMap Text Integer, HashSet A.GId))
            build' v (gns, (cs, cns))
                  | Just c <- Map.lookup (n2s v) cs
                  , (n2s v <> showt c) `Set.member` cns = build' v (gns, (Map.insert (n2s v) (c + 1) cs, cns))
                  | Just c <- Map.lookup (n2s v) cs     = let cnam = n2s v <> showt c
                        in (Map.insert v cnam gns, (Map.insert (n2s v) (c + 1) cs, Set.insert cnam cns))
                  | n2s v `Set.member` cns              = build' v (gns, (Map.insert (n2s v) 1 cs, cns))
                  | otherwise                           = let cnam = n2s v
                        in (Map.insert v cnam gns, (Map.insert (n2s v) 1 cs, Set.insert cnam cns))
