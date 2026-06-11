{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Safe #-}
module ReWire.Crust.ToCore (toCore) where

import ReWire.Config (Config, inputSigs, outputSigs, stateSigs, top)
import ReWire.Annotation (Annote, noAnn, Annotated (ann))
import ReWire.Error (failAt, warnAt, AstError, MonadError, Warning (..))
import ReWire.Pretty (showt, prettyPrint)
import ReWire.Unbound (Name, Fresh, runFreshM, Embed (..) , unbind, n2s)
import ReWire.BitVector (bitVec, zeros, BV, nbits)

import Control.Arrow ((&&&), first, second)
import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, lift)
import Control.Monad.State (StateT (..), MonadState, gets, modify)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.List (find, findIndex, genericLength)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified ReWire.BitVector    as BV
import qualified ReWire.Core.Syntax  as C
import qualified ReWire.Crust.Syntax as M
import qualified ReWire.Crust.Types  as M
import qualified ReWire.Crust.Util   as M

type SizeMap       = HashMap M.Ty C.Size
type GlobalNameMap = HashMap (Name M.Exp) C.Name
-- | Warnings are collected here (rather than emitted directly) because this
--   module's monad has no MonadIO; toCore emits them at the end.
type S             = (SizeMap, GlobalNameMap, [Warning])
type ConMap        = (HashMap (Name M.TyConId) [Name M.DataConId], HashMap (Name M.DataConId) M.Ty)
type TCM m         = ReaderT ConMap (ReaderT (HashMap (Name M.Exp) C.LId) m)
type StartDefn     = (C.Name, C.Wiring, C.GId, C.GId)

{- HLINT ignore "Redundant flip" -}
toCore :: (Fresh m, MonadError AstError m, MonadFail m, MonadIO m) => Config -> Name M.Exp -> M.FreeProgram -> m C.Device
toCore conf start (ts, _, vs) = do
      (dev, (_, _, ws)) <- flip runStateT mempty $ toCore' conf start (ts, vs)
      -- Transformations can duplicate expressions, so the same warning can be
      -- collected several times; emit each distinct one once.
      mapM_ (\ (Warning an msg) -> warnAt conf an msg) $ nubOrd ws
      pure dev

toCore' :: (Fresh m, MonadError AstError m, MonadFail m) => Config -> Name M.Exp -> ([M.DataDefn], [M.Defn]) -> StateT S m C.Device
toCore' conf start (ts, vs) = (do
      mapM_ (\ x -> ((`runReaderT` conMap) . sizeOf (n2s (M.dataName x)) noAnn . M.TyCon noAnn . M.dataName) x) ts
      let intSz = 128 -- TODO(chathhorn)
      updateSzMap $ const $ Map.singleton (M.intTy noAnn) intSz
      buildNameMap vs
      vs'    <- mapM (transDefn conf start conMap) $ filter (not . M.isPrim . M.defnName) vs
      case partitionEithers vs' of
            ([(topLevel, w, loop, state0)], defns)
                  | Just defLoop   <- find ((== loop) . C.defnName) defns
                  , Just defState0 <- find ((== state0) . C.defnName) defns
                        -> pure $ C.Device topLevel w defLoop defState0 $ filter (neither loop state0 . C.defnName) defns
            _           -> failAt noAnn $ "toCore: no definition found: " <> prettyPrint start)
      where conMap :: ConMap
            conMap = ( Map.fromList $ map (M.dataName &&& map projId . M.dataCons) ts
                     , Map.fromList $ map (projId &&& projType) (concatMap M.dataCons ts)
                     )

            projId :: M.DataCon -> Name M.DataConId
            projId (M.DataCon _ n _) = n

            projType :: M.DataCon -> M.Ty
            projType (M.DataCon _ _ (Embed (M.Poly t))) = runFreshM (snd <$> unbind t)

            neither :: Eq a => a -> a -> a -> Bool
            neither a b c = (c /= a) && (c /= b)

transDefn :: (MonadError AstError m, Fresh m, MonadState S m, MonadFail m) => Config -> Name M.Exp -> ConMap -> M.Defn -> m (Either StartDefn C.Defn)
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
                                    let wires = C.Wiring (zip (conf^.inputSigs)  (filter (> 0) t_ins'))
                                                         (zip (conf^.outputSigs) (filter (> 0) t_outs'))
                                                         (zip (conf^.stateSigs)  (filter (> 0) t_sts'))
                                    pure $ Left (conf^.top, wires, loop', state0')
                              _ -> failAt an $ "transDefn: definition of " <> prettyPrint start <> " must have form `unfold n m' where n and m are global IDs; got " <> prettyPrint e'
                  _ -> failAt an $ "transDefn: " <> prettyPrint start <> " has unsupported type: " <> M.prettyTy t'
      M.Defn an n (Embed (M.Poly t)) _ (Embed e) -> do
            (_, t')  <- unbind t
            (xs, e') <- unbind e
            if | M.higherOrder t'       -> failAt an $ "transDefn: " <> prettyPrint n <> " has unsupported higher-order type."
               | not $ M.fundamental t' -> failAt an $ "transDefn: " <> prettyPrint n <> " has un-translatable String or Integer arguments."
               | otherwise              -> Right <$> (C.Defn an <$> transName n <*> runReaderT (transType t') conMap <*> runReaderT (runReaderT (transExp e') conMap) (Map.fromList $ zip xs [0..]))
      where getRegsTy :: MonadError AstError m => M.Ty -> m M.Ty
            getRegsTy = \ case
                  M.TyApp _ (M.TyApp _ (M.TyCon _ (n2s -> "PuRe")) s) _ -> pure s
                  t                                                     -> failAt (ann t) $ "transDefn: definition of " <> prettyPrint start <> " must have form `" <> prettyPrint start <> " = unfold n m' where m has type PuRe s o."

            -- TODO: come up with a less brittle way to do this.
            detuple :: M.Ty -> [M.Ty]
            detuple t = case t of
                  M.TyApp _ (M.TyApp _ (M.TyCon _ (n2s -> "Vec")) _) _  -> [t]
                  M.TyApp _ (M.TyCon _ (n2s -> "Finite")) _             -> [t]
                  _                                                     -> M.flattenTyApp t

externSig :: Annote -> [C.Size] -> C.Size -> Text -> Text -> ([M.Exp], [M.Exp], [M.Exp]) -> C.ExternSig
externSig an args res clk rst = \ case
      (params -> Just ps, params -> Just as, params -> Just rs) -> C.ExternSig an ps clk rst (if null as then args' else as) (if null rs then res' else rs)
      _                                                         -> C.ExternSig an [] clk rst args' res'
      where params :: [M.Exp] -> Maybe [(Text, C.Size)]
            params = mapM $ \ case
                  M.App _ _ _ (M.App _ _ _ (M.Con _ _ _ (n2s -> "(,)")) (M.LitStr _ _ p)) (M.LitInt _ _ v)
                        -> pure (p, fromIntegral v)
                  _     -> Nothing

            args' :: [(Text, C.Size)]
            args' = map (mempty, ) args

            res' :: [(Text, C.Size)]
            res'  = [(mempty, res)]

arity :: M.Ty -> Int
arity = length . M.paramTys

transBuiltin :: (MonadError AstError m, Fresh m, MonadState S m) => Annote -> Maybe M.Ty -> Annote -> (M.Builtin, [M.Exp]) -> TCM m C.Exp
transBuiltin an' t' an theExp = case theExp of
      -- | A call to "error" surviving to this point is live (not eliminated
      --   as unreachable): hardware has no failure mechanism, so it compiles
      --   to a zero (don't-care) value, with a warning (fatal under -Werror).
      (M.Error, args) -> do
            sz <- sizeOf' "rwPrimError" an t'
            addWarning an' $ "encountered a live call to the built-in \"error\" function; compiling to a zero (don't-care) value of width " <> showt sz <> "."
                  <> (case args of
                        [M.LitStr _ _ x] -> "\nError message: " <> showt x
                        _                -> mempty)
            pure $ C.Lit an' $ zeros $ fromIntegral sz
      (M.Bits, [arg]) -> transExp arg
      (M.Resize, [arg]) -> do
            sz       <- sizeOf' "rwPrimResize" an t'
            resize an sz arg
      (M.BitIndex, [arg, M.App _ _ _ (M.Builtin _ _ _ M.Finite) (M.LitInt _ _ i)]) -> subElems an arg ((-i) - 1) 1
      (M.BitSlice, [arg, M.App _ _ _ (M.Builtin _ _ _ M.Finite) (M.LitInt _ _ j)
                       , M.App _ _ _ (M.Builtin _ _ _ M.Finite) (M.LitInt _ _ i)]) -> do
            unless (j + 1 >= i) $ failAt (ann arg)
                  $ "transExp: invalid bit slice (j: " <> showt j <> ", i: " <> showt i <> ")."
            let nBits = fromIntegral $ j + 1 - i
                off   = (-i) - fromIntegral nBits
            subElems an arg off nBits
      (M.BitSlice, _) -> failAt an "transExp: rwPrimBitSlice must have arguments (finite j) (finite i) with LitInts"
      (M.VecIndex, [arg, i]) -> vecIndex arg i
      (M.VecIndexProxy, [arg, p]) -> do
            i      <- checkProxyArg "rwPrimVecIndexProxy" p
            subElems an arg i 1
      (M.NatVal, [p]) -> do
            i      <- checkProxyArg "rwPrimNatVal" p
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
            sz     <- sizeOf' "rwPrimVecReverse" an t'
            arg'   <- transExp arg
            nElems <- checkVecArgSize "rwPrimVecReverse" t'
            tyElem <- checkVecArgType "rwPrimVecReverse" t'
            szElem <- sizeOf "rwPrimVecReverse" an tyElem
            pure $ C.Call an sz (C.Prim C.Reverse) arg' (replicate (fromIntegral nElems) $ C.PatVar an szElem) C.nil
      (M.VecReplicate, [arg]) -> do
            sz     <- sizeOf' "rwPrimVecReplicate" an t'
            arg'   <- transExp arg
            nElems <- checkVecArgSize "rwPrimVecReplicate" t'
            tyElem <- checkVecArgType "rwPrimVecReplicate" t'
            szElem <- sizeOf "rwPrimVecReplicate" an tyElem
            pure $ C.Call an sz (C.Prim $ C.Replicate nElems) arg' [C.PatVar an szElem] C.nil
      (M.VecMap, [f, arg]) -> do
            nElems <- checkVecArgSize "rwPrimVecMap" t'
            transExp $ M.LitVec an Nothing Nothing $ map (M.mkApp an f . pure . vecIndexProxy arg) [0 .. nElems - 1]
      (M.VecGenerate, [f]) -> do
            nElems <- checkVecArgSize "rwPrimVecGenerate" t'
            transExp $ M.LitVec an Nothing Nothing $ map (M.mkApp an f . pure . finite nElems) [0 .. nElems - 1]
      (M.SetRef, M.App _ _ _ _ (M.LitStr _ _ r) : args) -> do
            sz       <- sizeOf' "rwPrimSetRef" an t'
            args'    <- mapM transExp args
            let argSizes = map C.sizeOf args'
            pure $ C.Call an sz (C.SetRef r) (C.cat args') (map (C.PatVar an) argSizes) C.nil
      (M.GetRef, [M.App _ _ _ _ (M.LitStr _ _ r)]) -> do
            sz       <- sizeOf' "rwPrimGetRef" an t'
            pure $ C.Call an sz (C.GetRef r) C.nil [] C.nil
      (M.VecConcat, [arg1, arg2]) -> do
            C.cat <$> mapM transExp [arg1, arg2]
      (M.Finite, [arg]) -> do
            arg' <- transExp arg
            case arg' of
                  C.Lit an' (BV.nat -> i) -> do
                        finSz <- checkFinTypeMax "rwPrimFinite" t'
                        unless (i >= 0 && i < fromIntegral finSz)
                              $ failAt (ann arg) ("transExp: rwPrimFinite: Integer " <> showt i <> " is not representable in Finite " <> showt finSz <> ".")
                        pure $ C.Lit an' $ BV.bitVec (fromIntegral $ nbits finSz) i
                  _ -> failAt (ann arg) "transExp: rwPrimFinite: can't determine argument value at compile-time."
      (M.FiniteMinBound, []) -> do
            finSz <- checkFinTypeMax "rwPrimFiniteMinBound" t'
            unless (finSz > 0)
                  $ failAt an "transExp: rwPrimFiniteMinBound: Finite 0 is uninhabited."
            transExp $ finite finSz 0
      (M.FiniteMaxBound, []) -> do
            finSz <- checkFinTypeMax "rwPrimFiniteMaxBound" t'
            unless (finSz > 0)
                  $ failAt an "transExp: rwPrimFiniteMaxBound: Finite 0 is uninhabited."
            transExp $ finite finSz $ finSz - 1
      (M.ToFinite, [arg]) -> do
            finSz  <- checkFinTypeMax "rwPrimToFinite" t'
            sz     <- sizeOf' "rwPrimToFinite" an t'
            argTy  <- maybe (failAt an' $ "transExp: rwPrimToFinite: invalid argument type: " <> showt (M.prettyTy <$> M.typeOf arg)) pure
                        $ M.typeOf arg
            nBits  <- checkVecArgSize "rwPrimToFinite" (Just argTy)
            unless (2 ^ nBits <= (fromIntegral finSz :: Integer))
                  $ failAt (ann arg) ("transExp: rwPrimToFinite: bitvector argument (size " <> showt nBits <> ") is not representable in Finite " <> showt finSz <> ".")
            resize an sz arg
      (M.ToFiniteMod, [arg]) -> do
            finSz   <- checkFinTypeMax "rwPrimToFiniteMod" t'
            sz      <- sizeOf' "rwPrimToFiniteMod" an t'
            argTy   <- maybe (failAt an' $ "transExp: rwPrimToFiniteMod: invalid argument type."
                                          <> " " <> showt (M.prettyTy <$> M.typeOf arg)) pure
                        $ M.typeOf arg
            nBits   <- checkVecArgSize "rwPrimToFiniteMod" (Just argTy)
            finSzTy <- maybe (failAt an' "transExp: rwPrimToFiniteMod: typeof lit finSz?") pure
                     $ M.typeOf $ lit finSz
            sz'     <- sizeOf "rwPrimToFiniteMod" an finSzTy
            if 2 ^ nBits <= (fromIntegral finSz :: Integer) then resize an sz arg
            else resize an sz $ modW argTy (fromIntegral nBits) finSzTy sz' arg $ lit finSz
      (M.FromFinite, [arg]) -> do
            finSz <- checkFinTypeMax "rwPrimFromFinite" (M.typeOf arg)
            nBits <- checkVecArgSize "rwPrimFromFinite" t'
            unless ((fromIntegral finSz :: Integer) <= 2 ^ nBits)
                  $ failAt (ann arg) ("transExp: rwPrimFromFinite: Finite " <> showt finSz <> " is not representable in bitvector of size " <> showt nBits <> ".")
            resize an (fromIntegral nBits) arg
      (toPrim -> Just p, args) -> do
            sz       <- sizeOf' (showt p) an t'
            args'    <- mapM transExp args
            let argSizes = map C.sizeOf args'
            pure $ C.Call an sz (C.Prim p) (C.cat args') (map (C.PatVar an) argSizes) C.nil
      (M.Extern, M.LitList _ _ _ ps : M.LitStr _ _ clk : M.LitStr _ _ rst : M.LitList _ _ _ as : M.LitList _ _ _ rs : M.LitStr _ _ s : a : M.LitStr _ _ inst : args)
            | (arity <$> M.typeOf a) == Just (length args) -> do
            sz       <- sizeOf' "rwPrimExtern" an t'
            args'    <- mapM transExp args
            let argSizes = map C.sizeOf args'
            pure $ C.Call an sz (C.Extern (externSig an argSizes sz clk rst (ps, as, rs)) s inst) (C.cat args') (map (C.PatVar an) argSizes) C.nil
      (M.Extern,  _) -> failAt an "toCore: transExp: encountered not-fully-applied extern (after inlining)."
      (b, _)         -> failAt an ("toCore: transExp: encountered unsupported builtin use: rwPrim" <> showt b <> ".")

      where -- | If i is negative, it represents an offset from the end, where '-1' is the offset for the last element.
            subElems :: (Fresh m, MonadError AstError m, MonadState S m) => Annote -> M.Exp -> Integer -> Natural -> TCM m C.Exp
            subElems an arg i nElems = do
                  tyElem <- maybe (failAt (ann arg) "ToCore: subElems: non-vector type argument to built-in vector function") pure
                          $ M.typeOf arg >>= M.vecElemTy
                  szElem <- fromIntegral <$> sizeOf "subElems" an tyElem
                  arg'   <- transExp arg

                  let sz, off, n, rem :: Natural
                      sz          = fromIntegral $ C.sizeOf arg'
                      off         = fromIntegral $ (if i < 0 then fromIntegral sz else 0) + i * szElem
                      n           = nElems * fromIntegral szElem
                      rem         = if sz >= off + n then sz - off - n else 0

                  unless (sz >= off + n)
                        $ failAt an $ "ToCore: subElems: invalid bit slice (offset: " <> showt i <> ", num elems: " <> showt nElems <> ") from object size " <> showt sz <> "."

                  pure $ subBits an arg' off n rem

            subBits :: Annote -> C.Exp -> Natural -> Natural -> Natural -> C.Exp
            subBits an arg nPre n nPost = C.Call an (fromIntegral n) (C.Prim C.Id) arg
                  [ C.PatWildCard an $ fromIntegral nPre
                  , C.PatVar an      $ fromIntegral n
                  , C.PatWildCard an $ fromIntegral nPost
                  ] C.nil

            toPrim :: M.Builtin -> Maybe C.Prim
            toPrim = \ case
                  M.Add         -> pure C.Add
                  M.Sub         -> pure C.Sub
                  M.Mul         -> pure C.Mul
                  M.Div         -> pure C.Div
                  M.Mod         -> pure C.Mod
                  M.Pow         -> pure C.Pow
                  M.LAnd        -> pure C.LAnd
                  M.LOr         -> pure C.LOr
                  M.And         -> pure C.And
                  M.Or          -> pure C.Or
                  M.XOr         -> pure C.XOr
                  M.XNor        -> pure C.XNor
                  M.LShift      -> pure C.LShift
                  M.RShift      -> pure C.RShift
                  M.RShiftArith -> pure C.RShiftArith
                  M.Eq          -> pure C.Eq
                  M.Gt          -> pure C.Gt
                  M.GtEq        -> pure C.GtEq
                  M.Lt          -> pure C.Lt
                  M.LtEq        -> pure C.LtEq
                  M.LNot        -> pure C.LNot
                  M.Not         -> pure C.Not
                  M.RAnd        -> pure C.RAnd
                  M.RNAnd       -> pure C.RNAnd
                  M.ROr         -> pure C.ROr
                  M.RNor        -> pure C.RNor
                  M.RXOr        -> pure C.RXOr
                  M.RXNor       -> pure C.RXNor
                  M.MSBit       -> pure C.MSBit
                  _             -> Nothing

            vecIndexProxy :: M.Exp -> Natural -> M.Exp
            vecIndexProxy v i = M.mkApp an (M.Builtin an Nothing Nothing M.VecIndexProxy) [v, M.proxy i]

            finite :: Natural -> Natural -> M.Exp
            finite n i = M.mkApp an (M.Builtin an Nothing (Just $ M.intTy an `M.arr` M.finiteTy an n) M.Finite) [lit i]

            -- Where |v| :: Vec n a
            -- index v i = resizeToSizeofA (v >> ((n - i - 1) * sizeof a))
            vecIndex :: (MonadError AstError m, Fresh m, MonadState S m) => M.Exp -> M.Exp -> TCM m C.Exp
            vecIndex v i = do
                  tyVec <- maybe (failAt an "ToCore: rwPrimIndex: invalid vector argument.") pure
                          $ M.typeOf v
                  szVec <- sizeOf "rwPrimIndex" an tyVec

                  n <- maybe (failAt an "ToCore: rwPrimIndex: invalid Vec argument.") pure
                        $ M.vecSize tyVec

                  tyElem <- maybe (failAt an "ToCore: rwPrimIndex: non-vector type argument to built-in vector function.") pure
                          $ M.vecElemTy tyVec
                  szElem <- sizeOf "rwPrimIndex" an tyElem

                  tyIdx <- maybe (failAt an "ToCore: rwPrimIndex: invalid index argument.") pure
                          $ M.typeOf i
                  szIdx <- sizeOf "rwPrimIndex" an tyIdx

                  let tyLit = M.intTy an
                  szLit <- sizeOf "rwPrimIndex" an tyLit

                  let sub1 = sub tyLit szLit tyIdx szIdx (lit n) i
                  tySub1 <- maybe (failAt an "ToCore: rwPrimIndex: invalid index argument (in sub1).") pure
                          $ M.typeOf sub1
                  szSub1 <- sizeOf "rwPrimIndex" an tySub1

                  let sub2 = sub tySub1 szSub1 tyLit szLit sub1 $ lit (1::Int)
                  tySub2 <- maybe (failAt an "ToCore: rwPrimIndex: invalid index argument (in sub2).") pure
                          $ M.typeOf sub2
                  szSub2 <- sizeOf "rwPrimIndex" an tySub2

                  let mul1 = mul tySub2 szSub2 tyLit szLit sub2 $ lit szElem
                  tyMul1 <- maybe (failAt an "ToCore: rwPrimIndex: invalid index argument (in mul1).") pure
                          $ M.typeOf mul1
                  szMul1 <- sizeOf "rwPrimIndex" an tyMul1

                  let rshift' = rshift tyVec szVec tyMul1 szMul1

                  -- resize szElem $ v >> ( ((n - i) - 1) * szElem )
                  resize an szElem $ v `rshift'` mul1

                  where sub :: M.Ty -> C.Size -> M.Ty -> C.Size -> M.Exp -> M.Exp -> M.Exp
                        sub ta sza tb szb a b = M.mkApp an (M.Builtin an Nothing (Just $ t `M.arr` t `M.arr` t) M.Sub) [arg1, arg2]
                              where (t, arg1, arg2) = if sza >= szb
                                          then (ta, a, M.mkApp an (M.Builtin an Nothing (Just $ tb `M.arr` ta) M.Resize) [b])
                                          else (tb, M.mkApp an (M.Builtin an Nothing (Just $ ta `M.arr` tb) M.Resize) [a], b)

                        rshift :: M.Ty -> C.Size -> M.Ty -> C.Size -> M.Exp -> M.Exp -> M.Exp
                        rshift ta sza tb szb a b = M.mkApp an (M.Builtin an Nothing (Just $ t `M.arr` t `M.arr` t) M.RShift) [arg1, arg2]
                              where (t, arg1, arg2) = if sza >= szb
                                          then (ta, a, M.mkApp an (M.Builtin an Nothing (Just $ tb `M.arr` ta) M.Resize) [b])
                                          else (tb, M.mkApp an (M.Builtin an Nothing (Just $ ta `M.arr` tb) M.Resize) [a], b)

                        mul :: M.Ty -> C.Size -> M.Ty -> C.Size -> M.Exp -> M.Exp -> M.Exp
                        mul ta sza tb szb a b = M.mkApp an (M.Builtin an Nothing (Just $ t `M.arr` t `M.arr` t)  M.Mul) [arg1, arg2]
                              where (t, arg1, arg2) = if sza >= szb
                                          then (ta, a, M.mkApp an (M.Builtin an Nothing (Just $ tb `M.arr` ta) M.Resize) [b])
                                          else (tb, M.mkApp an (M.Builtin an Nothing (Just $ ta `M.arr` tb) M.Resize) [a], b)

            lit :: Integral n => n -> M.Exp
            lit = M.LitInt an Nothing . fromIntegral

            modW :: M.Ty -> C.Size -> M.Ty -> C.Size -> M.Exp -> M.Exp -> M.Exp
            modW ta sza tb szb a b = M.mkApp an (M.Builtin an Nothing (Just $ t `M.arr` t `M.arr` t) M.Mod) [arg1, arg2]
                  where (t, arg1, arg2) = if sza >= szb
                              then (ta, a, M.mkApp an (M.Builtin an Nothing (Just $ tb `M.arr` ta) M.Resize) [b])
                              else (tb, M.mkApp an (M.Builtin an Nothing (Just $ ta `M.arr` tb) M.Resize) [a], b)

            resize :: (MonadError AstError m, Fresh m, MonadState S m) => Annote -> C.Size -> M.Exp -> TCM m C.Exp
            resize an sz arg = do
                  arg' <- transExp arg
                  pure $ C.Call an sz (C.Prim C.Resize) arg' [C.PatVar an $ C.sizeOf arg'] C.nil

            checkProxyArg :: (MonadError AstError m) => Text -> M.Exp -> TCM m Integer
            checkProxyArg t p = maybe (failAt an' $ "transExp: " <> t <> ": invalid Proxy argument.  " <> prettyPrint p <> " : " <> showt (M.prettyTy <$> M.typeOf p)) (pure . fromIntegral)
                        $ M.typeOf p >>= M.proxyNat

            checkVecArgSize :: (MonadError AstError m) => Text -> Maybe M.Ty -> TCM m Natural
            checkVecArgSize s t = maybe (failAt an' $ "transExp: " <> s <> ": invalid Vec size argument: " <> showt (M.prettyTy <$> t)) pure
                        $ t >>= M.vecSize

            checkVecArgType :: (MonadError AstError m) => Text -> Maybe M.Ty -> TCM m M.Ty
            checkVecArgType s t = maybe (failAt an' $ "transExp: " <> s <> ": invalid Vec type argument: " <> showt (M.prettyTy <$> t)) pure
                        $ t >>= M.vecElemTy

            checkFinTypeMax :: (MonadError AstError m) => Text -> Maybe M.Ty -> TCM m Natural
            checkFinTypeMax s t = maybe (failAt an' $ "transExp: " <> s <> ": invalid Finite type: "  <> showt (M.prettyTy <$> t)) pure
                        $ t >>= M.finSz

{- HLINT ignore "Redundant multi-way if" -}
transExp :: (MonadError AstError m, Fresh m, MonadState S m) => M.Exp -> TCM m C.Exp
transExp e = case e of
      M.App an _ _ _ _                    -> case M.flattenApp e of
            (M.Builtin an _ _ b        , args)                               -> transBuiltin (ann e) (M.typeOf e) an (b, args)
            (e'                        , _) | Just t' <- M.typeOf e'
                                            , not $ M.concrete t'            -> failAt an $ "transExp: could not infer a concrete type in an application. Inferred type: " <> M.prettyTy t'
            (M.Var _ _ _ x             , args)                               -> do
                  sz       <- sizeOf' ("Applied Var " <> n2s x) an $ M.typeOf e
                  args'    <- mapM transExp args
                  x'       <- transName x
                  let argSizes = map C.sizeOf args'
                  pure $ C.Call an sz (C.Global x') (C.cat args') (map (C.PatVar an) argSizes) C.nil
            (M.Con an _ t d            , args)                               -> do
                  (v, w)     <- ctorTag an (M.codomTy <$> t) d
                  args'      <- mapM transExp args
                  let argSizes = map C.sizeOf args'
                  (tag, pad) <- ctorRep an (M.typeOf e) (v, w) $ sum argSizes
                  pure $ C.cat ([C.Lit an tag, C.Lit an pad] <> args')
            _                                                              -> failAt an $ "transExp: encountered ill-formed application:\n" <> prettyPrint e
      M.Builtin an _ _ b                  -> transBuiltin (ann e) (M.typeOf e) an (b, [])
      M.Var an _ t x                      -> do
            sz <- sizeOf' ("Var " <> n2s x) an t
            x' <- transName x
            lift (asks $ Map.lookup x) <&> \ case
                  Nothing -> C.Call an sz (C.Global x') C.nil [] C.nil
                  Just i  -> C.LVar an sz i
      M.Con an _ t d                      -> do
            (v, w)     <- ctorTag an t d
            (tag, pad) <- ctorRep an t (v, w) 0
            pure $ C.cat [C.Lit an tag, C.Lit an pad]
      M.Match an _ t e ps f els           -> C.Call an <$> sizeOf' "Match" an t    <*> (callTarget =<< transExp f) <*> transExp e <*> transPat ps <*> maybe (pure C.nil) transExp els
      M.LitInt an _ n                     -> do
            sz <- sizeOf' "LitInt" an $ M.typeOf e
            pure $ C.Lit an $ bitVec (fromIntegral sz) n
      M.LitVec _ _ _ es                   -> C.cat <$> mapM transExp es
      _                                   -> failAt (ann e) $ "ToCore: unsupported expression: " <> prettyPrint e
      where callTarget :: MonadError AstError m => C.Exp -> m C.Target
            callTarget = \ case
                  C.Call _ _ x _ _ _ -> pure x
                  e                  -> failAt noAnn $ "ToCore: callTarget: expected Match, got: " <> prettyPrint e

            -- | > ctorRep total_size (tag_value, tag_size) size_args = (tag, pad)
            ctorRep :: (Fresh m, MonadReader ConMap m, MonadState S m, MonadError AstError m) => Annote -> Maybe M.Ty -> (C.Value, C.Size) -> C.Size -> m (BV, BV)
            ctorRep an Nothing _ _ = failAt an "ToCore: ctorRep: encountered untyped constructor (rwc bug)."
            ctorRep an (Just t) (v, w) szArgs = do
                  sz <- sizeOf "ctorRep" an t
                  if | w + szArgs <= sz -> pure (bitVec (fromIntegral w) v, zeros $ fromIntegral sz - fromIntegral w - fromIntegral szArgs)
                     | otherwise        -> failAt an $ "ToCore: failing to calculate the bitvector representation of a constructor of type (sz: "
                                                    <> showt sz <> " w: " <> showt w <> " szArgs: " <> showt szArgs <> "):\n" <> M.prettyTy t

transPat :: (MonadError AstError m, Fresh m, MonadState S m, MonadReader ConMap m) => M.MatchPat -> m [C.Pat]
transPat = \ case
      M.MatchPatCon an _ t d ps -> do
            (v, w) <- ctorTag an t d
            sz     <- sizeOf' ("MatchPatCon " <> n2s d) an t
            szArgs <- sum <$> mapM (sizeOf' "MatchPatCon args" an . M.typeOf) ps
            let tag = C.PatLit an (bitVec (fromIntegral w) v)
                pad = C.PatWildCard an (sz - w - szArgs) -- or lit 0 bits?
            ([tag, pad] <>) <$> (concat <$> mapM transPat ps)
      M.MatchPatVar an _ t      -> pure . C.PatVar an <$> sizeOf' "MatchPatVar" an t
      M.MatchPatWildCard an _ t -> pure . C.PatWildCard an <$> sizeOf' "MatchPatWildCard" an t

transType :: (Fresh m, MonadError AstError m, MonadState S m) => M.Ty -> ReaderT ConMap m C.Sig
transType t = C.Sig (ann t) <$> mapM (sizeOf "transType param" $ ann t) (M.paramTys t) <*> sizeOf "transType codomain" (ann t) (M.codomTy t)

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
            Just t'           -> failAt an
                  $ "ToCore: merge: inconsistent assignment of tyvar " <> v
                  <> ": " <> M.prettyTy t <> " vs. " <> M.prettyTy t'

type TySub = [(Text, M.Ty)]

ctorWidth :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState S m) => HashSet M.Ty -> M.Ty -> Name M.DataConId -> m C.Size
ctorWidth visited t d = do
      let t'            =  M.codomTy t
      getCtorType d >>= \ case
            Just ct -> do
                  let (targs, tres) = M.flattenArrow ct
                  s                <- matchTy (ann t') tres t'
                  sum <$> mapM (sizeOfW "ctorWidth" (ann t) visited . apply s) targs
            _ -> pure 0
      where apply :: TySub -> M.Ty -> M.Ty
            apply s = \ case
                  M.TyApp an t1 t2  -> M.TyApp an (apply s t1) $ apply s t2
                  t@(M.TyVar _ _ i) -> fromMaybe t $ lookup (showt i) s
                  t                 -> t


-- TODO(chathhorn): shouldn't be necessary (should save this info before breaking out ctors)
getCtors :: MonadReader ConMap m => Name M.TyConId -> m [Name M.DataConId]
getCtors n = asks (fromMaybe [] . Map.lookup n . fst)

getCtorType :: MonadReader ConMap m => Name M.DataConId -> m (Maybe M.Ty)
getCtorType n = asks (Map.lookup n . snd)

ctorTag :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState S m) => Annote -> Maybe M.Ty -> Name M.DataConId -> m (C.Value, C.Size)
ctorTag an Nothing _  = failAt an "ToCore: ctorTag: encountered untyped constructor (rwc bug)"
ctorTag an (Just t) d = case M.flattenTyApp t of
      M.TyCon _ c : _ -> do
            ctors      <- getCtors c
            case findIndex ((== n2s d) . n2s) ctors of
                  Just idx -> pure (toInteger idx, fromIntegral $ nbits $ genericLength ctors)
                  Nothing  -> failAt an $ "ToCore: ctorTag: unknown ctor: " <> prettyPrint (n2s d) <> " of type " <> prettyPrint (n2s c)
      _               -> failAt an $ "ToCore: ctorTag: unexpected type: " <> M.prettyTy t

sizeOf' :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState S m) => Text -> Annote -> Maybe M.Ty -> m C.Size
sizeOf' s an = \ case
      Nothing -> failAt an "ToCore: encountered an untyped expression (rwc bug)."
      Just t  -> sizeOf s an t

sizeOf :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState S m) => Text -> Annote -> M.Ty -> m C.Size
sizeOf s an = sizeOfW s an mempty

-- | The worker for sizeOf: tracks the (fully applied) datatypes the
--   computation has descended through (visited) so that sizing a recursive
--   datatype (which has no fixed bit width) is an error instead of an
--   infinite loop. Note: visiting the same constructor twice at different
--   argument types is fine (e.g., nested tuples), so this only catches
--   regular recursion -- which is the only kind ToCrust can produce.
sizeOfW :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState S m) => Text -> Annote -> HashSet M.Ty -> M.Ty -> m C.Size
sizeOfW s an visited t = do
      m <- szMap
      s <- case Map.lookup t m of
            Nothing -> case M.flattenTyApp t of
                  M.TyCon _ (n2s -> "Vec") : [M.evalNat -> Just n, t] -> (fromIntegral n *) <$> sizeOfW s an visited t
                  M.TyCon _ (n2s -> "Vec") : [n,t]                    -> failAt an $ "ToCore: " <> s <> ": sizeOf: can't determine the size of a Vec."
                                                                                  <> " (Vec " <> M.prettyTy n <> " " <> M.prettyTy t <> ")"
                  M.TyCon _ (n2s -> "Finite") : [M.evalNat -> Just n] -> pure $ fromIntegral $ nbits n
                  M.TyCon _ (n2s -> "Finite") : [n]                   -> failAt an $ "ToCore: " <> s <> ": sizeOf: can't determine the size of a Finite."
                                                                                  <> " (Finite " <> M.prettyTy n <> ")"
                  M.TyCon _ c              : _
                        | t `Set.member` visited                      -> failAt an $ "ToCore: " <> s <> ": sizeOf: can't determine the size of a recursive datatype: " <> n2s c
                        | otherwise                                   -> do
                              ctors      <- getCtors c
                              ctorWidths <- mapM (ctorWidth (Set.insert t visited) t) ctors
                              pure $ fromIntegral (nbits $ genericLength ctors) + maximum (0 : ctorWidths)
                  M.TyApp {}               : _                        -> failAt an $ "ToCore: " <> s <> ": sizeOf: got TyApp after flattening (rwc bug): " <> M.prettyTy t
                  M.TyVar {}               : _                        -> pure 0 -- TODO(chathhorn): shouldn't need this.
                  _                                                   -> failAt an $ "ToCore: sizeOf: " <> s <> ": couldn't calculate the size of a type: " <> M.prettyTy t
            Just s -> pure s
      updateSzMap $ Map.insert t s
      pure s

szMap :: MonadState S m => m SizeMap
szMap = gets $ \ (sm, _, _) -> sm

updateSzMap :: MonadState S m => (SizeMap -> SizeMap) -> m ()
updateSzMap f = modify $ \ (sm, nm, ws) -> (f sm, nm, ws)

addWarning :: MonadState S m => Annote -> Text -> m ()
addWarning an msg = modify $ \ (sm, nm, ws) -> (sm, nm, ws <> [Warning an msg])

transName :: MonadState S m => Name M.Exp -> m C.Name
transName n = gets (\ (_, nm, _) -> Map.lookup n nm) >>= \ case
      Just c -> pure c
      _      -> pure $ showt n

buildNameMap :: MonadState S m => [M.Defn] -> m ()
buildNameMap vs = modify
                $ (\ nm (sm, _, ws) -> (sm, nm, ws))
                $ fst
                $ foldr (build' . M.defnName) mempty vs

      where build' :: Name M.Exp -> (GlobalNameMap, (HashMap Text Integer, HashSet C.Name)) -> (GlobalNameMap, (HashMap Text Integer, HashSet C.Name))
            build' v (gns, (cs, cns))
                  | Just c <- Map.lookup (n2s v) cs
                  , (n2s v <> showt c) `Set.member` cns = build' v (gns, (Map.insert (n2s v) (c + 1) cs, cns))
                  | Just c <- Map.lookup (n2s v) cs     = let cnam = n2s v <> showt c
                        in (Map.insert v cnam gns, (Map.insert (n2s v) (c + 1) cs, Set.insert cnam cns))
                  | n2s v `Set.member` cns              = build' v (gns, (Map.insert (n2s v) 1 cs, cns))
                  | otherwise                           = let cnam = n2s v
                        in (Map.insert v cnam gns, (Map.insert (n2s v) 1 cs, Set.insert cnam cns))
