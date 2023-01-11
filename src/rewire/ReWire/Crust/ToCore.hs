{-# LANGUAGE FlexibleContexts, OverloadedStrings, MultiWayIf #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Crust.ToCore (toCore) where

import ReWire.Config (Config, inputSigs, outputSigs, stateSigs)
import ReWire.Annotation
import ReWire.Error
import ReWire.Pretty
import ReWire.Unbound (Name, Fresh, runFreshM, Embed (..) , unbind, n2s)

import Control.Arrow ((&&&))
import Control.Lens ((^.))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, lift)
import Control.Monad.State (StateT (..), MonadState, get, put, unless)
import Data.BitVector (bitVec, zeros, BV)
import Data.Either (partitionEithers)
import Data.HashMap.Strict (HashMap)
import Data.List (findIndex, genericLength)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Numeric.Natural (Natural)
import TextShow (showt)

import qualified Data.HashMap.Strict as Map
import qualified ReWire.Core.Syntax  as C
import qualified ReWire.Crust.Syntax as M

type SizeMap = HashMap M.Ty C.Size
type ConMap = (HashMap (Name M.TyConId) [Name M.DataConId], HashMap (Name M.DataConId) M.Ty)
type TCM m = ReaderT ConMap (ReaderT (HashMap (Name M.Exp) C.LId) m)

toCore :: (Fresh m, MonadError AstError m, MonadFail m) => Config -> Text -> M.FreeProgram -> m C.Program
toCore conf start (ts, _, vs) = fst <$> flip runStateT mempty (do
      mapM_ ((`runReaderT` conMap) . sizeOf noAnn . M.TyCon noAnn . M.dataName) ts
      let intSz = 128 -- TODO(chathhorn)
      put $ Map.singleton (M.intTy noAnn) intSz
      vs'    <- mapM (transDefn conf start conMap) $ filter (not . M.isPrim . M.defnName) vs
      case partitionEithers vs' of
            ([startDefn], defns) -> pure $ C.Program startDefn defns
            _                    -> failAt noAnn $ "toCore: no definition found: " <> start)
      where conMap :: ConMap
            conMap = ( Map.fromList $ map (M.dataName &&& map projId . M.dataCons) ts
                     , Map.fromList $ map (projId &&& projType) (concatMap M.dataCons ts)
                     )

            projId :: M.DataCon -> Name M.DataConId
            projId (M.DataCon _ n _) = n

            projType :: M.DataCon -> M.Ty
            projType (M.DataCon _ _ (Embed (M.Poly t))) = runFreshM (snd <$> unbind t)

transDefn :: (MonadError AstError m, Fresh m, MonadState SizeMap m, MonadFail m) => Config -> Text -> ConMap -> M.Defn -> m (Either C.StartDefn C.Defn)
transDefn conf start conMap = \ case
      M.Defn an n (Embed (M.Poly t)) _ (Embed e) | n2s n == start -> do
            (_, t')  <- unbind t
            case t' of
                  M.TyApp _ (M.TyApp _ (M.TyApp _ (M.TyApp _ (M.TyCon _ (n2s -> "ReacT")) t_in) t_out) (M.TyCon _ (n2s -> "Identity"))) _ -> do
                        (_, e') <- unbind e
                        case M.flattenApp' e' of
                              [M.Builtin _ _ M.Unfold, M.Var _ loopTy loop, M.Var _ state0Ty state0] -> do
                                    t_st              <- getRegsTy state0Ty
                                    (t_in' : t_ins)   <- mapM ((`runReaderT` conMap) . sizeOf an) $ t_in  : detuple t_in
                                    (t_out' : t_outs) <- mapM ((`runReaderT` conMap) . sizeOf an) $ t_out : detuple t_out
                                    (t_st' : t_sts)   <- mapM ((`runReaderT` conMap) . sizeOf an) $ t_st  : concatMap detuple (detuple t_st)
                                    let t_ins'         = t_in' - sum t_ins : t_ins
                                        t_outs'        = t_out' - sum t_outs : t_outs
                                        t_sts'         = t_st' - sum t_sts : t_sts
                                    loopTy'           <- runReaderT (transType loopTy) conMap
                                    state0Ty'         <- runReaderT (transType state0Ty) conMap
                                    let wires = C.Wiring (zip (conf^.inputSigs)  (filter (> 0) t_ins'))
                                                         (zip (conf^.outputSigs) (filter (> 0) t_outs'))
                                                         (zip (conf^.stateSigs)  (filter (> 0) t_sts'))
                                                         loopTy'
                                                         state0Ty'
                                    pure $ Left $ C.StartDefn an wires (n2s loop) (n2s state0)
                              _ -> failAt an $ "transDefn: definition of " <> start <> " must have form `unfold n m' where n and m are global IDs; got " <> prettyPrint e'
                  _ -> failAt an $ "transDefn: " <> start <> " has unsupported type: " <> prettyPrint t'
      M.Defn an n (Embed (M.Poly t)) _ (Embed e) -> do
            (_, t')  <- unbind t
            (xs, e') <- unbind e
            if | M.higherOrder t'       -> failAt an $ "transDefn: " <> prettyPrint n <> " has unsupported higher-order type."
               | not $ M.fundamental t' -> failAt an $ "transDefn: " <> prettyPrint n <> " has un-translatable String or Integer arguments."
               | otherwise              -> Right <$> (C.Defn an (showt n) <$> runReaderT (transType t') conMap <*> runReaderT (runReaderT (transExp e') conMap) (Map.fromList $ zip xs [0..]))
      where getRegsTy :: MonadError AstError m => M.Ty -> m M.Ty
            getRegsTy = \ case
                  M.TyApp _ (M.TyApp _ (M.TyCon _ (n2s -> "PuRe")) s) _ -> pure s
                  t                                                     -> failAt (ann t) "transDefn: definition of Main.start must have form `Main.start = unfold n m' where m has type PuRe s o."

            detuple :: M.Ty -> [M.Ty]
            detuple t = case t of
                  M.TyApp _ (M.TyApp _ (M.TyCon _ (n2s -> "Vec")) _) _  -> [t]
                  _                                                     -> M.flattenTyApp t

externSig :: Annote -> [C.Size] -> C.Size -> Text -> ([M.Exp], [M.Exp], [M.Exp]) -> C.ExternSig
externSig an args res clk = \ case
      (params -> Just ps, params -> Just as, params -> Just rs) -> C.ExternSig an ps clk (if null as then args' else as) (if null rs then res' else rs)
      _                                                         -> C.ExternSig an [] clk args' res'
      where params :: [M.Exp] -> Maybe [(Text, C.Size)]
            params = mapM $ \ case
                  M.App _ _ (M.App _ _ (M.Con _ _ (n2s -> "(,)")) (M.LitStr _ p)) (M.LitInt _ v) -> pure (p, fromIntegral v)
                  _                                                                              -> Nothing
            args' :: [(Text, C.Size)]
            args' = map (mempty, ) args

            res' :: [(Text, C.Size)]
            res'  = [(mempty, res)]

arity :: M.Ty -> Int
arity = length . M.paramTys

transExp :: (MonadError AstError m, Fresh m, MonadState SizeMap m) => M.Exp -> TCM m C.Exp
transExp e = case e of
      M.App an _ _ _                -> case M.flattenApp' e of
            M.Builtin _ _ M.Error  : _                                    -> transError
            M.Builtin an _ M.Bits : [arg]      -> do
                  arg'     <- transExp arg
                  let argSize = C.sizeOf arg'
                  pure $ C.Call an argSize (C.Prim C.Resize) arg' [C.PatVar an argSize] C.nil
            M.Builtin an _ M.Resize : [arg]                               -> do
                  sz       <- sizeOf an $ M.typeOf e
                  arg'     <- transExp arg
                  pure $ C.Call an sz (C.Prim C.Resize) arg' [C.PatVar an $ C.sizeOf arg'] C.nil
            M.Builtin _ _ M.BitIndex   : [arg, M.LitInt _ i]               -> subElems an arg ((-i) - 1) 1
            M.Builtin _ _ M.BitSlice   : [arg, M.LitInt _ j, M.LitInt _ i] -> do
                  unless (j + 1 >= i) $
                        failAt (ann arg) $ "transExp: invalid bit slice (j: " <> showt j <> ", i: " <> showt i <> ")."
                  let nBits = fromIntegral $ j + 1 - i
                      off   = (-i) - fromIntegral nBits
                  subElems an arg off nBits
            M.Builtin an _ M.VecIndex   : [arg, p]                      -> do
                  i      <- maybe (failAt (ann e) "transExp: rwPrimVecIndex: invalid proxy argument.") pure
                              $ fmap fromIntegral $ M.proxyNat $ M.typeOf p
                  subElems an arg i 1
            M.Builtin an _ M.VecSlice   : [p, arg]                      -> do
                  i      <- maybe (failAt (ann e) "transExp: rwPrimVecSlice: invalid proxy argument.") pure
                              $ fmap fromIntegral $ M.proxyNat $ M.typeOf p
                  nElems <- maybe (failAt (ann e) "transExp: rwPrimVecSlice: invalid Vec argument.") pure
                              $ M.vecSize $ M.typeOf e
                  subElems an arg i nElems
            M.Builtin an _ M.VecRSlice  : [p, arg]                      -> do
                  i      <- maybe (failAt (ann e) "transExp: rwPrimVecRSlice: invalid proxy argument.") pure
                              $ fmap fromIntegral $ M.proxyNat $ M.typeOf p
                  nElems <- maybe (failAt (ann e) "transExp: rwPrimVecRSlice: invalid Vec argument.") pure
                              $ M.vecSize $ M.typeOf e
                  subElems an arg ((- i) - i * fromIntegral nElems) nElems
            M.Builtin an _ M.VecReverse : [arg]                         -> do
                  sz     <- sizeOf an $ M.typeOf e
                  arg'   <- transExp arg
                  nElems <- maybe (failAt (ann e) "transExp: rwPrimVecReverse: invalid Vec argument.") pure
                              $ M.vecSize $ M.typeOf e
                  elemTy <- maybe (failAt (ann e) "transExp: rwPrimVecReverse: invalid Vec argument.") pure
                              $ M.vecElemTy $ M.typeOf e
                  elemSz <- sizeOf an elemTy
                  pure $ C.Call an sz (C.Prim C.Reverse) arg' (replicate (fromIntegral nElems) $ C.PatVar an elemSz) C.nil
            M.Builtin an _ M.VecReplicate : [arg]                       -> do
                  sz     <- sizeOf an $ M.typeOf e
                  arg'   <- transExp arg
                  nElems <- maybe (failAt (ann e) "transExp: rwPrimVecReverse: invalid Vec argument.") pure
                              $ M.vecSize $ M.typeOf e
                  elemTy <- maybe (failAt (ann e) "transExp: rwPrimVecReverse: invalid Vec argument.") pure
                              $ M.vecElemTy $ M.typeOf e
                  elemSz <- sizeOf an elemTy
                  pure $ C.Call an sz (C.Prim $ C.Replicate nElems) arg' [C.PatVar an elemSz] C.nil
            M.Builtin an _ M.SetRef : M.App _ _ _ (M.LitStr _ r) : args  -> do
                  sz       <- sizeOf an $ M.typeOf e
                  args'    <- mapM transExp args
                  let argSizes = map C.sizeOf args'
                  pure $ C.Call an sz (C.SetRef r) (C.cat args') (map (C.PatVar an) argSizes) C.nil
            M.Builtin an _ M.GetRef : [M.App _ _ _ (M.LitStr _ r)]       -> do
                  sz       <- sizeOf an $ M.typeOf e
                  pure $ C.Call an sz (C.GetRef r) C.nil [] C.nil
            M.Builtin _ _ M.VecConcat : [arg1, arg2]                     -> do
                  C.cat <$> mapM transExp [arg1, arg2]
            M.Builtin an _ (toPrim -> Just p) : args                     -> do
                  sz       <- sizeOf an $ M.typeOf e
                  args'    <- mapM transExp args
                  let argSizes = map C.sizeOf args'
                  pure $ C.Call an sz (C.Prim p) (C.cat args') (map (C.PatVar an) argSizes) C.nil
            M.Builtin _ _ M.Extern  : M.LitList _ _ ps : M.LitStr _ clk : M.LitList _ _ as : M.LitList _ _ rs : M.LitStr _ s : a : M.LitStr _ inst : args
                  | arity (M.typeOf a) == length args                    -> do
                  sz       <- sizeOf an $ M.typeOf e
                  args'    <- mapM transExp args
                  let argSizes = map C.sizeOf args'
                  pure $ C.Call an sz (C.Extern (externSig an argSizes sz clk (ps, as, rs)) s inst) (C.cat args') (map (C.PatVar an) argSizes) C.nil
            M.Builtin _ _ M.Extern  : _                                  -> failAt an "transExp: encountered not-fully-applied extern (after inlining)."
            e'                      : _ | not $ M.concrete $ M.typeOf e' -> failAt an $ "transExp: could not infer a concrete type in an application. Inferred type: " <> prettyPrint (M.typeOf e')
            M.Var _ _ x             : args                               -> do
                  sz       <- sizeOf an $ M.typeOf e
                  args'    <- mapM transExp args
                  let argSizes = map C.sizeOf args'
                  pure $ C.Call an sz (C.Global $ showt x) (C.cat args') (map (C.PatVar an) argSizes) C.nil
            M.Con an t d            : args                               -> do
                  (v, w)     <- ctorTag an (M.rangeTy t) d
                  args'      <- mapM transExp args
                  let argSizes = map C.sizeOf args'
                  (tag, pad) <- ctorRep an (M.typeOf e) (v, w) $ sum argSizes
                  pure $ C.cat ([C.Lit an tag, C.Lit an pad] <> args')
            _                                                            -> failAt an "transExp: encountered ill-formed application."
      M.Var an t x                      -> lift (asks $ Map.lookup x) >>= \ case
            Nothing -> do
                  sz <- sizeOf an t
                  pure $ C.Call an sz (C.Global $ showt x) C.nil [] C.nil
            Just i  -> do
                  sz <- sizeOf an t
                  pure $ C.LVar an sz i
      M.Con an t d                      -> do
            (v, w)     <- ctorTag an t d
            (tag, pad) <- ctorRep an t (v, w) 0
            pure $ C.cat [C.Lit an tag, C.Lit an pad]
      M.Match an t e ps f (Just e2)     -> C.Call an <$> sizeOf an t <*> (callTarget =<< transExp f) <*> transExp e <*> transPat ps <*> transExp e2
      M.Match an t e ps f Nothing       -> C.Call an <$> sizeOf an t <*> (callTarget =<< transExp f) <*> transExp e <*> transPat ps <*> pure C.nil
      M.LitInt an n                     -> do
            sz <- sizeOf an $ M.typeOf e
            pure $ C.Lit an $ bitVec (fromIntegral sz) n
      M.LitVec _ _ es                   -> C.cat <$> mapM transExp es
      M.Builtin _ _ M.Error             -> transError
      M.Builtin an _ (toPrim -> Just p) -> do
            sz       <- sizeOf an $ M.typeOf e
            pure $ C.Call an sz (C.Prim p) C.nil [] C.nil
      M.TypeAnn _ _ e                   -> transExp e
      _                                 -> failAt (ann e) $ "ToCore: unsupported expression: " <> prettyPrint e
      where callTarget :: MonadError AstError m => C.Exp -> m C.Target
            callTarget = \ case
                  C.Call _ _ x _ _ _ -> pure x
                  e                  -> failAt noAnn $ "ToCore: callTarget: expected Match, got: " <> prettyPrint e

            -- | > ctorRep total_size (tag_value, tag_size) size_args = (tag, pad)
            ctorRep :: (Fresh m, MonadReader ConMap m, MonadState SizeMap m, MonadError AstError m) => Annote -> M.Ty -> (C.Value, C.Size) -> C.Size -> m (BV, BV)
            ctorRep an t (v, w) szArgs = do
                  sz <- sizeOf an t
                  if | w + szArgs <= sz -> pure (bitVec (fromIntegral w) v, zeros $ fromIntegral sz - fromIntegral w - fromIntegral szArgs)
                     | otherwise        -> failAt an $ "ToCore: failing to calculate the bitvector representation of a constructor of type (sz: " <> showt sz <> " w: " <> showt w <> " szArgs: " <> showt szArgs <> "):\n" <> prettyPrint t

            -- | If i is negative, it represents an offset from the end, where '-1' is the offset for the last element.
            subElems :: (Fresh m, MonadError AstError m, MonadState SizeMap m) => Annote -> M.Exp -> Integer -> Natural -> TCM m C.Exp
            subElems an arg i nElems = do
                  elemTy <- maybe (failAt (ann arg) "ToCore: subElems: non-vector type argument to built-in vector function") pure
                          $ M.vecElemTy $ M.typeOf arg
                  elemSz <- fromIntegral <$> sizeOf an elemTy
                  arg'   <- transExp arg

                  let sz, off, n, rem :: Natural
                      sz          = fromIntegral $ C.sizeOf arg'
                      off         = fromIntegral $ (if i < 0 then fromIntegral sz else 0) + i * elemSz
                      n           = nElems * fromIntegral elemSz
                      rem         = if sz >= off + n then sz - off - n else 0

                  unless (sz >= off + n) $
                        failAt an $ "ToCore: subElems: invalid bit slice (offset: " <> showt i <> ", num elems: " <> showt n <> ") from object size " <> showt sz <> "."

                  pure $ subBits an arg' off n rem

            subBits :: Annote -> C.Exp -> Natural -> Natural -> Natural -> C.Exp
            subBits an arg nPre n nPost = C.Call an (fromIntegral n) (C.Prim C.Id) arg
                  [ C.PatWildCard an $ fromIntegral nPre
                  , C.PatVar an      $ fromIntegral n
                  , C.PatWildCard an $ fromIntegral nPost
                  ] C.nil

            toPrim :: M.Builtin -> Maybe C.Prim
            toPrim = \ case
                  -- M.VecFromList
                  -- M.VecReplicate -> pure C.Replicate
                  -- M.VecReverse
                  -- M.VecSlice
                  -- M.VecRSlice
                  -- M.VecIndex
                  -- M.VecConcat
                  -- M.Bits
                  -- M.Resize
                  -- M.BitSlice
                  -- M.BitIndex
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

            -- TODO(chathhorn): possibly make this optional.
            -- callError :: Annote -> C.Size -> C.Exp
            -- callError an sz = C.Call an sz (C.Extern (C.ExternSig an [] mempty [] [(mempty, sz)]) "error" "error") C.nil [] C.nil

            transError :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState SizeMap m) => m C.Exp
            transError = do
                  -- sz     <- sizeOf (ann e) $ M.typeOf e
                  -- pure $ callError (ann e) sz
                  failAt (ann e) $ "Encountered call to built-in \"error\" function that was not eliminated: " <> prettyPrint e

transPat :: (MonadError AstError m, Fresh m, MonadState SizeMap m) => M.MatchPat -> ReaderT ConMap m [C.Pat]
transPat = \ case
      M.MatchPatCon an t d ps -> do
            (v, w) <- ctorTag an t d
            sz     <- sizeOf an t
            szArgs <- sum <$> mapM (sizeOf an . M.typeOf) ps
            let tag = C.PatLit an (bitVec (fromIntegral w) v)
                pad = C.PatWildCard an (sz - w - szArgs) -- or lit 0 bits?
            ([tag, pad] <>) <$> (concat <$> mapM transPat ps)
      M.MatchPatVar an t      -> pure <$> (C.PatVar an <$> sizeOf an t)
      M.MatchPatWildCard an t -> pure <$> (C.PatWildCard an <$> sizeOf an t)

transType :: (Fresh m, MonadError AstError m, MonadState SizeMap m) => M.Ty -> ReaderT ConMap m C.Sig
transType t = C.Sig (ann t) <$> mapM (sizeOf $ ann t) (M.paramTys t) <*> sizeOf (ann t) (M.rangeTy t)

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
                  <> ": " <> prettyPrint t <> " vs. " <> prettyPrint t'

type TySub = [(Text, M.Ty)]

apply :: TySub -> M.Ty -> M.Ty
apply s (M.TyApp an t1 t2) = M.TyApp an (apply s t1) $ apply s t2
apply s t@(M.TyVar _ _ i)  = fromMaybe t $ lookup (showt i) s
apply _ t                  = t

ctorWidth :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState SizeMap m) => M.Ty -> Name M.DataConId -> m C.Size
ctorWidth t d = do
      let t'            =  M.rangeTy t
      getCtorType d >>= \ case
            Just ct -> do
                  let (targs, tres) = M.flattenArrow ct
                  s                <- matchTy (ann t') tres t'
                  sum <$> mapM (sizeOf (ann t) . apply s) targs
            _ -> pure 0

-- TODO(chathhorn): shouldn't be necessary (should save this info before breaking out ctors)
getCtors :: MonadReader ConMap m => Name M.TyConId -> m [Name M.DataConId]
getCtors n = asks (fromMaybe [] . Map.lookup n . fst)

getCtorType :: MonadReader ConMap m => Name M.DataConId -> m (Maybe M.Ty)
getCtorType n = asks (Map.lookup n . snd)

ctorTag :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState SizeMap m) => Annote -> M.Ty -> Name M.DataConId -> m (C.Value, C.Size)
ctorTag an t d = case take 1 $ M.flattenTyApp t of
      [M.TyCon _ c] -> do
            ctors      <- getCtors c
            case findIndex ((== n2s d) . n2s) ctors of
                  Just idx -> pure (toInteger idx, ceilLog2 $ genericLength ctors)
                  Nothing  -> failAt an $ "ToCore: ctorTag: unknown ctor: " <> prettyPrint (n2s d) <> " of type " <> prettyPrint (n2s c)
      _             -> failAt an $ "ToCore: ctorTag: unexpected type: " <> prettyPrint t

sizeOf :: (Fresh m, MonadError AstError m, MonadReader ConMap m, MonadState SizeMap m) => Annote -> M.Ty -> m C.Size
sizeOf an t = do
      m <- get
      s <- case Map.lookup t m of
            Nothing -> case M.flattenTyApp t of
                  M.TyCon _ (n2s -> "Vec") : [M.evalNat -> Just n, t] -> (fromIntegral n *) <$> sizeOf an t
                  M.TyCon _ c              : _                        -> do
                        ctors      <- getCtors c
                        ctorWidths <- mapM (ctorWidth t) ctors
                        pure $ ceilLog2 (genericLength ctors) + maximum (0 : ctorWidths)
                  M.TyApp {}               : _                        -> failAt an $ "ToCore: sizeOf: Got TyApp after flattening (can't happen): " <> prettyPrint t
                  M.TyVar {}               : _                        -> pure 0 -- TODO(chathhorn): shouldn't need this.
                  _                                                   -> failAt an $ "ToCore: sizeOf: couldn't calculate the size of a type: " <> prettyPrint t
            Just s -> pure s
      put $ Map.insert t s m
      pure s

ceilLog2 :: Integral a => a -> a
ceilLog2 n | toInteger n < 1 = 0
ceilLog2 n                   = ceiling $ logBase 2 (fromIntegral n :: Double)
