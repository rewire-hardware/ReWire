{-# LANGUAGE FlexibleContexts, OverloadedStrings, MultiWayIf #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Crust.ToCore (toCore) where

import ReWire.Annotation
import ReWire.Error
import ReWire.Pretty
import ReWire.Unbound (Name, Fresh, runFreshM, Embed (..) , unbind, n2s)

import Control.Arrow ((&&&))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, lift)
import Control.Monad.State (StateT (..), MonadState, get, gets, put, unless)
import Data.BitVector (bitVec, zeros)
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

toCore :: (Fresh m, MonadError AstError m, MonadFail m) => Text -> [Text] -> [Text] -> [Text] -> M.FreeProgram -> m C.Program
toCore start inps outps sts (ts, _, vs) = fst <$> flip runStateT mempty (do
      mapM_ ((`runReaderT` conMap) . sizeOf noAnn . M.TyCon noAnn . M.dataName) ts
      intSz <- gets $ maximum . Map.elems
      put $ Map.singleton (M.intTy noAnn) intSz -- TODO(chathhorn): note the sizeof Integer is the max of all non-Integer-containing types.
      vs'    <- mapM (transDefn start inps outps sts conMap) $ filter (not . M.isPrim . M.defnName) vs
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

transDefn :: (MonadError AstError m, Fresh m, MonadState SizeMap m, MonadFail m) => Text -> [Text] -> [Text] -> [Text] -> ConMap -> M.Defn -> m (Either C.StartDefn C.Defn)
transDefn start inps outps sts conMap = \ case
      M.Defn an n (Embed (M.Poly t)) _ (Embed e) | n2s n == start -> do
            (_, t')  <- unbind t
            case t' of
                  M.TyApp _ (M.TyApp _ (M.TyApp _ (M.TyApp _ (M.TyCon _ (n2s -> "ReT")) t_in) t_out) (M.TyCon _ (n2s -> "I"))) _ -> do
                        (_, e') <- unbind e
                        case e' of
                              M.App _ (M.App _ (M.Builtin _ _ M.Unfold) (M.Var _ loopTy loop)) (M.Var _ state0Ty state0) -> do
                                    t_st              <- getRegsTy state0Ty
                                    (t_in' : t_ins)   <- mapM ((`runReaderT` conMap) . sizeOf an) $ t_in  : detuple t_in
                                    (t_out' : t_outs) <- mapM ((`runReaderT` conMap) . sizeOf an) $ t_out : detuple t_out
                                    (t_st' : t_sts)   <- mapM ((`runReaderT` conMap) . sizeOf an) $ t_st  : concatMap detuple (detuple t_st)
                                    let t_ins'         = t_in' - sum t_ins : t_ins
                                        t_outs'        = t_out' - sum t_outs : t_outs
                                        t_sts'         = t_st' - sum t_sts : t_sts
                                    loopTy'           <- runReaderT (transType loopTy) conMap
                                    state0Ty'         <- runReaderT (transType state0Ty) conMap
                                    let wires = C.Wiring (zip (inps  <> map (("__in" <>) . showt)  [0::C.Index ..]) (filter (> 0) t_ins'))
                                                         (zip (outps <> map (("__out" <>) . showt) [0::C.Index ..]) (filter (> 0) t_outs'))
                                                         (zip (sts <> map (("__st" <>) . showt)    [0::C.Index ..]) (filter (> 0) t_sts'))
                                                         loopTy'
                                                         state0Ty'
                                    pure $ Left $ C.StartDefn an wires (n2s loop) (n2s state0)
                              _ -> failAt an $ "transDefn: definition of " <> start <> " must have form `unfold n m' where n and m are global IDs; got " <> prettyPrint e'
                  _ -> failAt an $ "transDefn: " <> start <> " has illegal type: " <> prettyPrint t'
      M.Defn an n (Embed (M.Poly t)) _ (Embed e) -> do
            (_, t')  <- unbind t
            (xs, e') <- unbind e
            if | M.higherOrder t'       -> failAt an $ "transDefn: " <> prettyPrint n <> " has illegal higher-order type."
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
                  M.App _ (M.App _ (M.Con _ _ (n2s -> "(,)")) (M.LitStr _ p)) (M.LitInt _ v) -> pure (p, fromIntegral v)
                  _                                                                          -> Nothing
            args' :: [(Text, C.Size)]
            args' = map (mempty, ) args

            res' :: [(Text, C.Size)]
            res'  = [(mempty, res)]

callError :: Annote -> C.Size -> C.Exp
callError an sz = C.Call an sz (C.Extern (C.ExternSig an [] mempty [] [(mempty, sz)]) "error" "error") C.nil [] C.nil

arity :: M.Ty -> Int
arity = length . fst . M.flattenArrow

transExp :: (MonadError AstError m, Fresh m, MonadState SizeMap m) => M.Exp -> TCM m C.Exp
transExp e = case e of
      M.App an _ _                  -> case M.flattenApp e of
            M.Error an _ _          : _                                 -> do
                  sz     <- sizeOf an $ M.typeOf e
                  pure $ callError an sz
            e'                      : _ | not $ M.concrete $ M.typeOf e' -> failAt an $ "transExp: could not infer a concrete type in an application. Inferred type: " <> prettyPrint (M.typeOf e')
            M.Var _ _ x             : args                              -> do
                  sz       <- sizeOf an $ M.typeOf e
                  args'    <- C.cat <$> mapM transExp args
                  argSizes <- mapM (sizeOf an . M.typeOf) args
                  pure $ C.Call an sz (C.Global $ showt x) args' (map (C.PatVar an) argSizes) C.nil
            M.Builtin _ t M.Extern  : M.LitList _ _ ps : M.LitStr _ clk : M.LitList _ _ as : M.LitList _ _ rs : M.LitStr _ s : _ : M.LitStr _ inst : args
                  | arity t == 7 + length args                          -> do
                  sz       <- sizeOf an $ M.typeOf e
                  args'    <- C.cat <$> mapM transExp args
                  argSizes <- mapM (sizeOf an . M.typeOf) args
                  pure $ C.Call an sz (C.Extern (externSig an argSizes sz clk (ps, as, rs)) s inst) args' (map (C.PatVar an) argSizes) C.nil
            M.Builtin _ _ M.Extern  : _                                 -> failAt an "transExp: encountered not-fully-applied extern (after inlining)."
            M.Builtin an _ M.BitIndex    : [arg, M.LitInt _ i]          -> do -- TODO(chathhorn): should probably just do a pass to rewrite this in terms of Bits.
                  sz       <- sizeOf an $ M.typeOf e
                  arg'     <- transExp arg
                  argSize  <- fromIntegral <$> sizeOf an (M.typeOf arg)
                  unless (i < argSize && i >= 0) $
                        failAt (ann arg) $ "transExp: invalid bit index " <> showt i <> " in object size " <> showt argSize <> "."
                  pure $ C.Call an sz C.Id arg' [C.PatWildCard an $ fromIntegral $ argSize - i - 1, C.PatVar an 1, C.PatWildCard an $ fromIntegral i] C.nil
            M.Builtin an _ M.BitSlice   : [arg, M.LitInt _ j, M.LitInt _ i] -> do
                  sz       <- sizeOf an $ M.typeOf e
                  arg'     <- transExp arg
                  argSize  <- fromIntegral <$> sizeOf an (M.typeOf arg)
                  unless (j < argSize && i >= 0 && j >= 0 && j - i + 1 > 0) $
                        failAt (ann arg) $ "transExp: invalid bit slice (" <> showt j <> ", " <> showt i <> ") from object size " <> showt argSize <> "."
                  pure $ C.Call an sz C.Id arg' [C.PatWildCard an $ fromIntegral $ argSize - j - 1, C.PatVar an $ fromIntegral $ j - i + 1, C.PatWildCard an $ fromIntegral i] C.nil
            M.Builtin an _ M.SetRef : M.App _ _ (M.LitStr _ r) : args   -> do
                  sz       <- sizeOf an $ M.typeOf e
                  args'    <- C.cat <$> mapM transExp args
                  argSizes <- mapM (sizeOf an . M.typeOf) args
                  pure $ C.Call an sz (C.SetRef r) args' (map (C.PatVar an) argSizes) C.nil
            M.Builtin an _ M.GetRef : [M.App _ _ (M.LitStr _ r)]        -> do
                  sz       <- sizeOf an $ M.typeOf e
                  pure $ C.Call an sz (C.GetRef r) C.nil [] C.nil
            M.Builtin an _ b : [arg] | b == M.Resize || b == M.Bits     -> do
                  sz       <- sizeOf an $ M.typeOf e
                  arg'     <- transExp arg
                  argSize  <- fromIntegral <$> sizeOf an (M.typeOf arg)
                  pure $ C.Call an sz C.Resize arg' [C.PatVar an argSize] C.nil
            M.Con an t d            : args                              -> do
                  (v, w) <- ctorTag an (snd $ M.flattenArrow t) d
                  args'  <- mapM transExp args
                  sz     <- sizeOf an $ M.typeOf e
                  szArgs <- sum <$> mapM (sizeOf an . M.typeOf) args
                  let tag = C.Lit an (bitVec (fromIntegral w) v)
                      pad = C.Lit an (zeros $ fromIntegral sz - fromIntegral w - fromIntegral szArgs) -- ugh
                  pure $ C.cat ([tag, pad] <> args')
            _                           -> failAt an "transExp: encountered ill-formed application."
      M.Var an t x                      -> lift (asks $ Map.lookup x) >>= \ case
            Nothing -> do
                  sz <- sizeOf an t
                  pure $ C.Call an sz (C.Global $ showt x) C.nil [] C.nil
            Just i  -> do
                  sz <- sizeOf an t
                  pure $ C.LVar an sz i
      M.Con an t d                      -> do
            (v, w) <- ctorTag an t d
            sz     <- sizeOf an t
            let tag = C.Lit an $ bitVec (fromIntegral w) v
                pad = C.Lit an (zeros $ fromIntegral sz - fromIntegral w)
            pure $ C.cat [tag, pad]
      M.Match an t e ps f (Just e2)     -> C.Call an <$> sizeOf an t <*> (callTarget =<< transExp f) <*> transExp e <*> transPat ps <*> transExp e2
      M.Match an t e ps f Nothing       -> C.Call an <$> sizeOf an t <*> (callTarget =<< transExp f) <*> transExp e <*> transPat ps <*> pure C.nil
      M.LitInt an n                     -> do
            sz <- sizeOf an $ M.typeOf e
            pure $ C.Lit an $ bitVec (fromIntegral sz) n
      M.LitVec _ _ es                   -> C.cat <$> mapM transExp es
      M.Error an t _                    -> do
            sz <- sizeOf an t
            pure $ callError an sz
      M.TypeAnn _ _ e                   -> transExp e
      _                                 -> failAt (ann e) $ "ToCore: unsupported expression: " <> prettyPrint e
      where callTarget :: MonadError AstError m => C.Exp -> m C.Target
            callTarget = \ case
                  C.Call _ _ x _ _ _ -> pure x
                  e                  -> failAt noAnn $ "ToCore: callTarget: expected Match, got: " <> prettyPrint e

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
transType t = C.Sig (ann t) <$> mapM (sizeOf $ ann t) (fst $ M.flattenArrow t) <*> sizeOf (ann t) (snd $ M.flattenArrow t)

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
      let t'            =  snd $ M.flattenArrow t
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
                  M.TyCon _ (n2s -> "Vec") : [n, t]           -> (*) <$> evalNat n <*> sizeOf an t
                  M.TyCon _ c              : _                -> do
                        ctors      <- getCtors c
                        ctorWidths <- mapM (ctorWidth t) ctors
                        pure $ ceilLog2 (genericLength ctors) + maximum (0 : ctorWidths)
                  M.TyApp {}               : _                -> failAt an $ "ToCore: sizeOf: Got TyApp after flattening (can't happen): " <> prettyPrint t
                  _                                           -> pure 0
            Just s -> pure s
      put $ Map.insert t s m
      pure s

      where toWord :: Natural -> Word
            toWord = fromInteger . toInteger

            evalNat :: MonadError AstError m => M.Ty -> m Word
            evalNat = \ case
                  M.TyNat _ n                                          -> pure $ toWord n
                  M.TyApp _ (M.TyApp _ (M.TyCon _ (n2s -> "+")) n1) n2 -> (+) <$> evalNat n1 <*> evalNat n2
                  -- t                                                    -> failAt (ann t) "ToCore: sizeOf: Could not evalute type-level Nat to a constant value."
                  t                                                    -> pure 0

ceilLog2 :: Integral a => a -> a
ceilLog2 n | toInteger n < 1 = 0
ceilLog2 n                   = ceiling $ logBase 2 (fromIntegral n :: Double)
