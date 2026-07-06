{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
-- | The Eidos-to-Hyle producer (the direct fold; doc/eidos.md §7.3): the
--   pure fragment translates per-construct (n-ary cases compile to
--   if-chains over constructor tags; joins are lifted to definitions
--   first), and each process folds to an explicit device: one definition
--   per block (stores threaded as trailing parameters), one dispatch
--   definition (an if-chain over the label tag), and register initials
--   obtained by evaluating the entry block with the Hyle interpreter.
--
--   The machine-step record reproduces the retired purifier's bit layout
--   exactly (so interpreter traces are unchanged): a step is
--   @halted | out | label-tag | pad | args | cells@, where the @halted@
--   bit is present only when a halt is reachable (the successor of the
--   PuRe constructor tag), the label field mirrors the generic ADT layout
--   of the retired @R_@ datatype (tag at the field's MSB, arguments
--   LSB-aligned), and a halt step carries the retired @A_@ encoding (one
--   tag per distinct answer type) in place of @out | label@.
module ReWire.Eidos.ToHyle (eidosToHyle) where

import ReWire.Annotation (Annote, noAnn, Annotated (ann))
import ReWire.BitVector (BV, bitVec, zeros, nbits)
import ReWire.Builtins (Builtin (..))
import ReWire.Config (Config, inputSigs, outputSigs, stateSigs, top)
import ReWire.Error (failAt, failAtWith, failInternal, warnAt, AstError, MonadError, Warning (..), relocatingTo)
import ReWire.Eidos.Pretty ()
import ReWire.Eidos.Syntax
import ReWire.Eidos.ToCrust (liftJoins)
import ReWire.Eidos.Types (typeOf, flattenApp, flattenArrow, flattenTyApp, evalNat, substTv, higherOrder, fundamental, reacOrStateT)
import ReWire.Hyle.Interp (evalExp, IEnv (..))
import ReWire.Hyle.Transform (hoistInstances)
import ReWire.Pretty (showt, prettyPrint)

import Control.Lens ((^.))
import Control.Monad (unless, foldM)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (StateT (..), MonadState, gets, modify)
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.List (findIndex, genericLength, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified Data.IntMap.Strict  as IM
import qualified Data.Text           as T
import qualified ReWire.BitVector    as BV
import qualified ReWire.Hyle.Syntax  as A
import qualified ReWire.Annotation   as Ann

---
--- Monad and environments.
---

data Env = Env
      { envCtors   :: HashMap TyConId [DataConId]      -- ^ Constructors of each datatype, in declaration order.
      , envCtorSig :: HashMap DataConId Sig            -- ^ Each constructor's declared signature.
      , envTops    :: IM.IntMap A.GId                  -- ^ Hyle names of the translated top-level definitions.
      }

data S = S
      { sSizes   :: !(HashMap Ty A.Size)
      , sWarns   :: ![Warning]
      , sCtr     :: !Int
      , sExterns :: !(HashMap A.Name A.Extern)
      }

s0 :: S
s0 = S mempty [] 0 mempty

type TM m = StateT S m

addWarning :: MonadState S m => Annote -> Text -> m ()
addWarning an msg = modify $ \ s -> s { sWarns = sWarns s <> [Warning an msg] }

freshLocal :: MonadState S m => Text -> m A.Name
freshLocal pfx = do
      i <- gets sCtr
      modify $ \ s -> s { sCtr = i + 1 }
      pure $ pfx <> showt i

-- | Local scope: Eidos binder uniques map to Hyle atoms (a parameter or
--   let-bound name, or the case discriminant's atom for case binders).
data Scope = Scope
      { scMap  :: !(IM.IntMap A.Exp)
      , scUsed :: !(HashSet A.Name)
      }

scope0 :: Scope
scope0 = Scope mempty mempty

-- | Bind a local to a readable Hyle name derived from its source name,
--   disambiguating collisions with a numeric suffix (cf. the retained
--   producer's uniquifyLocals). Source identifiers never begin with '$',
--   so they cannot clash with the generated '$t'/'$in'/'$res' names.
bindLocal :: Annote -> Scope -> Id -> A.Size -> (Scope, A.Name)
bindLocal an sc x sz = bindLocalText an sc (idUniq x) (idOcc x) sz

bindLocalText :: Annote -> Scope -> Uniq -> Text -> A.Size -> (Scope, A.Name)
bindLocalText an sc u base sz = (sc', nm)
      where nm  = pick (scUsed sc) base (0 :: Int)
            sc' = Scope (IM.insert u (A.Var an sz nm) $ scMap sc) (Set.insert nm $ scUsed sc)

            pick :: HashSet A.Name -> Text -> Int -> A.Name
            pick used b i | cand `Set.member` used = pick used b (i + 1)
                          | otherwise              = cand
                  where cand = if i == 0 then b else b <> showt i

-- | A fresh name in scope, not tied to a binder (cell values).
scopeName :: Scope -> Text -> (Scope, A.Name)
scopeName sc base = (sc { scUsed = Set.insert nm $ scUsed sc }, nm)
      where nm = pick (scUsed sc) base (0 :: Int)
            pick :: HashSet A.Name -> Text -> Int -> A.Name
            pick used b i | cand `Set.member` used = pick used b (i + 1)
                          | otherwise              = cand
                  where cand = if i == 0 then b else b <> showt i

---
--- Entry point.
---

eidosToHyle :: (MonadIO m, MonadError AstError m) => Config -> Program -> m A.Program
eidosToHyle conf p0 = do
      let Program datas defns procs _top = liftJoins p0
      pr <- case procs of
            [pr] -> pure pr
            _    -> failAt noAnn $ "hyle-m: expected exactly one process, got " <> showt (length procs)
      (p, s) <- flip runStateT s0 $ do
            let pures = filter emit defns
                env   = Env { envCtors   = Map.fromList [ (dataName d, [ c | DataCon _ c _ <- dataCons d ]) | d <- datas ]
                            , envCtorSig = Map.fromList [ (c, sig) | d <- datas, DataCon _ c sig <- dataCons d ]
                            , envTops    = buildNameMap pures
                            }
            pureDefns <- mapM (transDefn env) pures
            transProc conf env pr pureDefns
      mapM_ (\ (Warning an msg) -> warnAt conf an msg) $ nubOrd $ sWarns s
      pure p
      where -- Primitive-named definitions (undotted; the polymorphic prim
            -- carriers among them ride through the front half untranslated)
            -- and the reactive fragment (subsumed by the processes) don't
            -- lower.
            emit :: Defn -> Bool
            emit d = not (isPrim $ idOcc $ defnId d)
                  && null (sigTVs $ idSig $ defnId d)
                  && not (reacOrStateT $ sigTy $ idSig $ defnId d)

            isPrim :: Text -> Bool
            isPrim = T.all (/= '.')

-- | Hyle names for the top-level definitions: the occurrence text,
--   disambiguated by a numeric suffix in definition order.
buildNameMap :: [Defn] -> IM.IntMap A.GId
buildNameMap = fst . foldl' step (mempty, mempty)
      where step :: (IM.IntMap A.GId, HashMap Text Int) -> Defn -> (IM.IntMap A.GId, HashMap Text Int)
            step (m, used) d =
                  let occ = idOcc $ defnId d
                      i   = Map.findWithDefault 0 occ used
                      nm  = if i == 0 then occ else occ <> showt i
                  in (IM.insert (idUniq $ defnId d) nm m, Map.insert occ (i + 1) used)

globalName :: MonadError AstError m => Env -> Annote -> Id -> TM m A.GId
globalName env an x = maybe (failAt an $ "hyle-m: unknown global: " <> idOcc x) pure
      $ IM.lookup (idUniq x) $ envTops env

---
--- Type sizing.
---

isTupleCon :: Text -> Bool
isTupleCon c = T.length c >= 2 && T.head c == '(' && T.last c == ')'
            && T.all (== ',') (T.init $ T.tail c)

intTy :: Ty
intTy = TyCon noAnn "Integer"

sizeOf :: MonadError AstError m => Env -> Text -> Annote -> Ty -> TM m A.Size
sizeOf env s an = sizeOfW env s an mempty

sizeOfW :: MonadError AstError m => Env -> Text -> Annote -> HashSet Ty -> Ty -> TM m A.Size
sizeOfW env s an visited t = do
      m  <- gets sSizes
      sz <- case Map.lookup t m of
            Just sz -> pure sz
            Nothing -> case flattenTyApp t of
                  (TyCon _ "Vec", [n, te])
                        | Just k <- evalNat n     -> (fromIntegral k *) <$> sizeOfW env s an visited te
                        | otherwise               -> failAt an $ "can't determine the size of a Vec. (" <> prettyPrint (unAnnT t) <> ")"
                  (TyCon _ "Finite", [n])
                        | Just k <- evalNat n     -> pure $ fromIntegral $ nbits k
                        | otherwise               -> failAt an $ "can't determine the size of a Finite. (" <> prettyPrint (unAnnT t) <> ")"
                  (TyCon _ "Integer", [])         -> pure 128
                  (TyCon _ "Proxy", _)            -> pure 0
                  (TyCon _ c, args)
                        | isTupleCon c            -> sum <$> mapM (sizeOfW env s an visited) args
                        | t `Set.member` visited  -> failAt an $ "can't determine the size of a recursive datatype: " <> c
                        | Just ctors <- Map.lookup c (envCtors env) -> do
                              ws <- mapM (ctorWidth env an (Set.insert t visited) t) ctors
                              pure $ fromIntegral (nbits $ genericLength ctors) + maximum (0 : ws)
                  (TyVarT {}, _)                  -> pure 0
                  _                               -> failAt an $ s <> ": couldn't calculate the size of a type: " <> prettyPrint (unAnnT t)
      modify $ \ st -> st { sSizes = Map.insert t sz $ sSizes st }
      pure sz
      where unAnnT :: Ty -> Ty
            unAnnT = Ann.unAnn

ctorWidth :: MonadError AstError m => Env -> Annote -> HashSet Ty -> Ty -> DataConId -> TM m A.Size
ctorWidth env an visited t d = case Map.lookup d (envCtorSig env) of
      Just (Sig _ ct) -> do
            let (targs, tres) = flattenArrow ct
            sub <- matchTy an tres t
            sum <$> mapM (sizeOfW env "ctorWidth" an visited . substTv sub) targs
      Nothing         -> pure 0

matchTy :: MonadError AstError m => Annote -> Ty -> Ty -> m (HashMap TyVar Ty)
matchTy an (TyApp _ t1 t2) (TyApp _ t1' t2') = do
      s1 <- matchTy an t1 t1'
      s2 <- matchTy an t2 t2'
      merge s1 s2
      where merge :: MonadError AstError m => HashMap TyVar Ty -> HashMap TyVar Ty -> m (HashMap TyVar Ty)
            merge s1 s2 | and (Map.intersectionWith (==) s1 s2) = pure $ Map.union s1 s2
                        | otherwise = failInternal an "matchTy: inconsistent assignment of a type variable"
matchTy _ (TyVarT _ v) t = pure $ Map.singleton v t
matchTy _ _ _            = pure mempty

-- | The tag value and width of a constructor of the given (concrete) type.
ctorTag :: MonadError AstError m => Env -> Annote -> Ty -> DataConId -> TM m (A.Value, A.Size)
ctorTag env an t d = case flattenTyApp t of
      (TyCon _ c, _)
            | isTupleCon c -> pure (0, 0)
            | Just ctors <- Map.lookup c (envCtors env) -> case findIndex (== d) ctors of
                  Just idx -> pure (toInteger idx, fromIntegral $ nbits $ genericLength ctors)
                  Nothing  -> failInternal an $ "ctorTag: unknown ctor: " <> d <> " of type " <> c
      _ -> failInternal an $ "ctorTag: unexpected type: " <> prettyPrint (Ann.unAnn t)

wireOffsets :: [(a, A.Size)] -> [(a, A.Size, A.Index)]
wireOffsets = snd . foldr step (0, [])
      where step (x, sz) (off, acc) = (off + fromIntegral sz, (x, sz, off) : acc)

slice0 :: Annote -> A.Index -> A.Size -> A.Exp -> A.Exp
slice0 an off sz e | sz == 0   = A.Lit an BV.nil
                   | otherwise = A.Slice an off sz e

---
--- Pure definitions.
---

transDefn :: MonadError AstError m => Env -> Defn -> TM m A.Defn
transDefn env d@(Defn an did _ _ _ _) = do
      let t   = sigTy $ idSig did
          occ = idOcc did
      if | higherOrder t     -> failAt an $ occ <> " has unsupported higher-order type."
         | not $ fundamental t -> failAt an $ occ <> " has un-translatable String or Integer arguments."
         | otherwise         -> do
               modify $ \ s -> s { sCtr = 0 }
               n' <- globalName env an did
               let (ps0, body0)  = (defnParams d, defnBody d)
                   (ps1, body1)  = peelLams body0
                   ps            = ps0 <> ps1
                   (ptys, codTy) = flattenArrow t
               -- Eta-expand an unsaturated definition (a definition whose
               -- body returns a function can't lower to a sized result).
               (ps', body') <- etaExpand an (length ps) (drop (length ps) ptys) ps body1
               ptSzs <- mapM (sizeOf env "transDefn param" an) $ take (length ps') ptys
               codSz <- sizeOf env "transDefn codomain" an codTy
               let sig = A.Sig an ptSzs codSz
               (sc, pnames) <- pure $ bindParams an scope0 (zip ps' ptSzs)
               body'' <- relocatingTo an $ transExp env sc body'
               pure $ A.Defn an n' sig pnames body''
      where peelLams :: Exp -> ([Id], Exp)
            peelLams = \ case
                  Lam _ x b -> let (xs, b') = peelLams b in (x : xs, b')
                  e         -> ([], e)

            etaExpand :: MonadError AstError m => Annote -> Int -> [Ty] -> [Id] -> Exp -> TM m ([Id], Exp)
            etaExpand _ _ [] ps b = pure (ps, b)
            etaExpand an' n missing ps b = do
                  let extras = [ Id ("$eta" <> showt i) (- (idUniq (defnId d) * 1000 + i)) (monoSig ty)
                               | (i, ty) <- zip [0 :: Int ..] missing ]
                  pure (ps <> extras, foldl' (\ acc x -> App an' acc (EArg $ Var an' x)) b extras)
                  where _ = n

bindParams :: Annote -> Scope -> [(Id, A.Size)] -> (Scope, [A.Name])
bindParams an = go
      where go sc = \ case
                  []             -> (sc, [])
                  (x, sz) : rest -> let (sc', nm)   = bindLocal an sc x sz
                                        (sc'', nms) = go sc' rest
                                    in (sc'', nm : nms)

---
--- Expression translation.
---

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

transExp :: MonadError AstError m => Env -> Scope -> Exp -> TM m A.Exp
transExp env sc e = case e of
      App {} -> case flattenApp e of
            (Prim an' _ b, args)      -> transBuiltin env sc (ann e) (typeOf e) an' (b, [ a | EArg a <- args ])
            (Con an' _ d, args)       -> do
                  args'      <- mapM (transExp env sc) [ a | EArg a <- args ]
                  (v, w)     <- ctorTag env an' (typeOf e) d
                  (tag, pad) <- ctorRep an' (typeOf e) (v, w) $ sum $ map A.sizeOf args'
                  pure $ A.cat ([A.Lit an' tag, A.Lit an' pad] <> args')
            (Var an' x, args)         -> do
                  let eargs = [ a | EArg a <- args ]
                  sz <- sizeOf env ("applied Var " <> idOcc x) an' $ typeOf e
                  case IM.lookup (idUniq x) $ scMap sc of
                        Just _  -> failAt an' $ "hyle-m: unsupported application of a local variable: " <> idOcc x
                        Nothing -> A.Call an' sz <$> globalName env an' x <*> mapM (transExp env sc) eargs
            (Lam an' p b, EArg a : rest) ->
                  transExp env sc $ Let an' (NonRec p a) $ foldl' (App an') b rest
            -- A let-headed application: hoist the application inside (the
            -- arguments predate the binder, so this is scope-safe).
            (Let lan bnd lbody, args)  ->
                  transExp env sc $ Let lan bnd $ foldl' (App lan) lbody args
            -- A case-headed application: commute into the arms (the
            -- arguments predate the alternative binders).
            (Case can _ cd ccb calts, args) ->
                  transExp env sc $ Case can (typeOf e) cd ccb
                        [ Alt aan c xs $ foldl' (App can) b args | Alt aan c xs b <- calts ]
            _                         -> failInternal (ann e) $ "hyle-m: encountered ill-formed application:\n" <> prettyPrint (Ann.unAnn e)
      Prim an _ b     -> transBuiltin env sc an (typeOf e) an (b, [])
      Var an x        -> case IM.lookup (idUniq x) $ scMap sc of
            Just atom -> pure atom
            Nothing   -> do
                  sz <- sizeOf env ("Var " <> idOcc x) an $ sigTy $ idSig x
                  A.Call an sz <$> globalName env an x <*> pure []
      Con an t d      -> do
            (v, w)     <- ctorTag env an t d
            (tag, pad) <- ctorRep an t (v, w) 0
            pure $ A.cat [A.Lit an tag, A.Lit an pad]
      LitInt an t n   -> do
            sz <- sizeOf env "LitInt" an t
            pure $ A.Lit an $ bitVec (fromIntegral sz) n
      LitVec _ _ es   -> A.cat <$> mapM (transExp env sc) es
      Case an t disc cb alts -> do
            sz    <- sizeOf env "Case" an t
            disc' <- transExp env sc disc
            bindAtom (ann disc) disc' $ \ datom ->
                  caseChain env (sc { scMap = IM.insert (idUniq cb) datom $ scMap sc }) an sz (typeOf disc) datom
                        [ (aan, c, xs, b) | Alt aan c xs b <- alts ]
                        (transExp env)
      Let an (NonRec x rhs) body -> do
            rhs' <- transExp env sc rhs
            case rhs' of
                  A.Lit {} -> transExp env (sc { scMap = IM.insert (idUniq x) rhs' $ scMap sc }) body
                  A.Var {} -> transExp env (sc { scMap = IM.insert (idUniq x) rhs' $ scMap sc }) body
                  _        -> do
                        let (sc', nm) = bindLocal an sc x $ A.sizeOf rhs'
                        body' <- transExp env sc' body
                        pure $ A.Let an (A.sizeOf body') nm rhs' body'
      Let an (Rec {}) _  -> failAt an "hyle-m: unsupported recursive local binding."
      Let an (Join {}) _ -> failInternal an "hyle-m: join point survived lifting (rwc bug)."
      Jump an _ _        -> failInternal an "hyle-m: jump survived lifting (rwc bug)."
      Lam an _ _         -> failAt an "hyle-m: unsupported lambda expression."
      _                  -> failAt (ann e) $ "hyle-m: unsupported expression: " <> prettyPrint (Ann.unAnn e)
      where ctorRep :: MonadError AstError m => Annote -> Ty -> (A.Value, A.Size) -> A.Size -> TM m (BV, BV)
            ctorRep an t (v, w) szArgs = do
                  sz <- sizeOf env "ctorRep" an t
                  if | w + szArgs <= sz -> pure (bitVec (fromIntegral w) v, zeros $ fromIntegral sz - fromIntegral w - fromIntegral szArgs)
                     | otherwise        -> failInternal an $ "failing to calculate the bitvector representation of a constructor of type (sz: "
                                                    <> showt sz <> " w: " <> showt w <> " szArgs: " <> showt szArgs <> "):\n" <> prettyPrint (Ann.unAnn t)

-- | The if-chain for an n-ary case, generic over the body language (pure
--   expressions and machine terminators). The default alternative (first,
--   the Core convention) becomes the final else; without one, the last
--   alternative is unconditional.
caseChain :: forall m body. MonadError AstError m
          => Env -> Scope -> Annote -> A.Size -> Ty -> A.Exp
          -> [(Annote, AltCon, [Id], body)]
          -> (Scope -> body -> TM m A.Exp)
          -> TM m A.Exp
caseChain env sc an sz discTy datom alts transBody = case alts of
      (aan, DefaultAlt, _, b) : rest -> go rest . Just =<< transBody sc b <* pure aan
      rest                           -> go rest Nothing
      where go :: [(Annote, AltCon, [Id], body)] -> Maybe A.Exp -> TM m A.Exp
            go [] (Just els) = pure els
            go []  Nothing   = failInternal an "hyle-m: encountered an empty case."
            go [a] Nothing   = altExp a Nothing
            go (a : rest) mels = altExp a . Just =<< go rest mels

            altExp :: (Annote, AltCon, [Id], body) -> Maybe A.Exp -> TM m A.Exp
            altExp (aan, c, xs, b) mrest = do
                  (conds, binds) <- case c of
                        DataAlt d  -> do
                              (v, w) <- ctorTag env aan discTy d
                              szT    <- sizeOf env "caseChain" aan discTy
                              szXs   <- mapM (sizeOf env "caseChain field" aan . sigTy . idSig) xs
                              let fields :: [(Maybe Id, A.Size, A.Index)]
                                  fields = wireOffsets $ (Nothing, w) : (Nothing, szT - w - sum szXs) : zipWith (\ x szx -> (Just x, szx)) xs szXs
                                  conds  = [ A.Prim aan 1 A.Eq [slice0 aan off w datom, A.Lit aan $ bitVec (fromIntegral w) v]
                                           | (Nothing, _, off) <- take 1 fields, w > 0 ]
                                  binds  = [ (x, slice0 aan off szx datom) | (Just x, szx, off) <- drop 2 fields ]
                              pure (conds, binds)
                        LitAlt i   -> do
                              szT <- sizeOf env "caseChain" aan discTy
                              pure ([ A.Prim aan 1 A.Eq [datom, A.Lit aan $ bitVec (fromIntegral szT) i] ], [])
                        DefaultAlt -> failInternal aan "hyle-m: default alternative not first."
                  (sc', binds') <- pure $ foldl' (\ (s, acc) (x, slc) ->
                              let (s', nm) = bindLocal aan s x (A.sizeOf slc) in (s', acc <> [(nm, slc)]))
                        (sc, []) binds
                  body' <- transBody sc' b
                  let lets = foldr (\ (nm, slc) acc -> A.Let aan (A.sizeOf acc) nm slc acc) body' binds'
                  pure $ case (mrest, conds) of
                        (Just rest', _ : _) -> A.If aan sz (conj aan conds) lets rest'
                        _                   -> lets

---
--- Builtins.
---

transBuiltin :: forall m. MonadError AstError m => Env -> Scope -> Annote -> Ty -> Annote -> (Builtin, [Exp]) -> TM m A.Exp
transBuiltin env sc an' t' an theExp = case theExp of
      (Error, args) -> do
            sz <- sizeOf env "rwPrimError" an t'
            addWarning an' $ "encountered a live call to the built-in \"error\" function; compiling to a zero (don't-care) value of width " <> showt sz <> "."
                  <> (case args of
                        [LitStr _ x] -> "\nError message: " <> showt x
                        _            -> mempty)
            pure $ A.Undef an' sz
      (Bits, [arg]) -> transExp env sc arg
      (Resize, [arg]) -> do
            sz <- sizeOf env "rwPrimResize" an t'
            resize an sz =<< transExp env sc arg
      (BitIndex, [arg, finLit -> Just i]) -> subElems an arg ((-i) - 1) 1
      (BitSlice, [arg, finLit -> Just j, finLit -> Just i]) -> do
            unless (j + 1 >= i) $ failAt (ann arg)
                  $ "invalid bit slice (j: " <> showt j <> ", i: " <> showt i <> ")."
            let nBits = fromIntegral $ j + 1 - i
                off   = (-i) - fromIntegral nBits
            subElems an arg off nBits
      (BitSlice, _) -> failAt an' "rwPrimBitSlice must have arguments (finite j) (finite i) with LitInts"
      (VecIndex, [arg, i]) -> vecIndex arg i
      (VecIndexProxy, [arg, p]) -> do
            i <- checkProxyArg "rwPrimVecIndexProxy" p
            subElems an arg i 1
      (NatVal, [p]) -> do
            i  <- checkProxyArg "rwPrimNatVal" p
            sz <- sizeOf env "rwPrimNatVal" an t'
            pure $ A.Lit an $ bitVec (fromIntegral sz) i
      (VecSlice, [p, arg]) -> do
            i      <- checkProxyArg "rwPrimVecSlice" p
            nElems <- checkVecArgSize "rwPrimVecSlice" t'
            subElems an arg i nElems
      (VecRSlice, [p, arg]) -> do
            i      <- checkProxyArg "rwPrimVecRSlice" p
            nElems <- checkVecArgSize "rwPrimVecRSlice" t'
            subElems an arg ((- i) - fromIntegral nElems) nElems
      (VecReverse, [arg]) -> do
            arg'   <- transExp env sc arg
            nElems <- checkVecArgSize "rwPrimVecReverse" t'
            tyElem <- checkVecArgType "rwPrimVecReverse" t'
            szElem <- sizeOf env "rwPrimVecReverse" an tyElem
            bindAtom an arg' $ \ a ->
                  pure $ A.cat $ reverse [ slice0 an off szElem a | (_, _, off) <- wireOffsets $ replicate (fromIntegral nElems) ((), szElem) ]
      (VecReplicate, [arg]) -> do
            sz     <- sizeOf env "rwPrimVecReplicate" an t'
            arg'   <- transExp env sc arg
            nElems <- checkVecArgSize "rwPrimVecReplicate" t'
            pure $ if nElems == 0 || sz == 0 then A.Lit an BV.nil
                   else A.Prim an sz (A.Rep nElems) [arg']
      (VecMap, [f, arg]) -> do
            nElems  <- checkVecArgSize "rwPrimVecMap" t'
            tyElemO <- checkVecArgType "rwPrimVecMap" t'
            szElemO <- sizeOf env "rwPrimVecMap" an tyElemO
            tyElemI <- checkVecArgType "rwPrimVecMap" $ typeOf arg
            szElemI <- sizeOf env "rwPrimVecMap" an tyElemI
            arg'    <- transExp env sc arg
            let szV = A.sizeOf arg'
            bindAtom an arg' $ \ a ->
                  A.cat <$> mapM (\ i -> applyFn an szElemO f
                              [slice0 an (fromIntegral szV - (fromIntegral i + 1) * fromIntegral szElemI) szElemI a])
                        [0 .. fromIntegral nElems - 1 :: Int]
      (VecGenerate, [f]) -> do
            nElems  <- checkVecArgSize "rwPrimVecGenerate" t'
            tyElemO <- checkVecArgType "rwPrimVecGenerate" t'
            szElemO <- sizeOf env "rwPrimVecGenerate" an tyElemO
            let finW = fromIntegral $ nbits nElems
            A.cat <$> mapM (\ i -> applyFn an szElemO f [A.Lit an $ bitVec finW i])
                  [0 .. toInteger nElems - 1]
      (VecConcat, [arg1, arg2]) -> A.cat <$> mapM (transExp env sc) [arg1, arg2]
      (VecFromList, [LitList _ _ els]) -> A.cat <$> mapM (transExp env sc) els
      (VecFromList, _) -> failAt an' "rwPrimVecFromList: argument must be a list literal."
      (Finite, [arg]) -> do
            arg' <- transExp env sc arg
            case arg' of
                  A.Lit an'' (BV.nat -> i) -> do
                        finSz <- checkFinTypeMax "rwPrimFinite" t'
                        unless (i >= 0 && i < fromIntegral finSz)
                              $ failAt (ann arg) ("rwPrimFinite: Integer " <> showt i <> " is not representable in Finite " <> showt finSz <> ".")
                        pure $ A.Lit an'' $ bitVec (fromIntegral $ nbits finSz) i
                  _ -> failAt (ann arg) "rwPrimFinite: can't determine argument value at compile-time."
      (FiniteMinBound, []) -> do
            finSz <- checkFinTypeMax "rwPrimFiniteMinBound" t'
            unless (finSz > 0)
                  $ failAt an "rwPrimFiniteMinBound: Finite 0 is uninhabited."
            pure $ A.Lit an $ bitVec (fromIntegral $ nbits finSz) (0 :: Integer)
      (FiniteMaxBound, []) -> do
            finSz <- checkFinTypeMax "rwPrimFiniteMaxBound" t'
            unless (finSz > 0)
                  $ failAt an "rwPrimFiniteMaxBound: Finite 0 is uninhabited."
            pure $ A.Lit an $ bitVec (fromIntegral $ nbits finSz) $ toInteger finSz - 1
      (ToFinite, [arg]) -> do
            finSz  <- checkFinTypeMax "rwPrimToFinite" t'
            sz     <- sizeOf env "rwPrimToFinite" an t'
            nBits  <- checkVecArgSize "rwPrimToFinite" $ typeOf arg
            unless (2 ^ nBits <= (fromIntegral finSz :: Integer))
                  $ failAt (ann arg) ("rwPrimToFinite: bitvector argument (size " <> showt nBits <> ") is not representable in Finite " <> showt finSz <> ".")
            resize an sz =<< transExp env sc arg
      (ToFiniteMod, [arg]) -> do
            finSz   <- checkFinTypeMax "rwPrimToFiniteMod" t'
            sz      <- sizeOf env "rwPrimToFiniteMod" an t'
            nBits   <- checkVecArgSize "rwPrimToFiniteMod" $ typeOf arg
            arg'    <- transExp env sc arg
            if 2 ^ nBits <= (fromIntegral finSz :: Integer) then resize an sz arg'
            else do
                  let szLit  = fromIntegral $ nbits finSz
                      w      = max (fromIntegral nBits) szLit
                  a <- resize an w arg'
                  resize an sz $ A.Prim an w A.UMod [a, A.Lit an $ bitVec (fromIntegral w) $ toInteger finSz]
      (FromFinite, [arg]) -> do
            finSz <- checkFinTypeMax "rwPrimFromFinite" $ typeOf arg
            nBits <- checkVecArgSize "rwPrimFromFinite" t'
            unless ((fromIntegral finSz :: Integer) <= 2 ^ nBits)
                  $ failAt (ann arg) ("rwPrimFromFinite: Finite " <> showt finSz <> " is not representable in bitvector of size " <> showt nBits <> ".")
            resize an (fromIntegral nBits) =<< transExp env sc arg
      (b, args) | Just _ <- toPrim b 0 0 -> do
            sz    <- sizeOf env (showt b) an t'
            args' <- mapM (transExp env sc) args
            applyPrim an sz b args'
      (Extern, LitList _ _ ps : (litStr -> Just clk) : (litStr -> Just rst) : LitList _ _ as : LitList _ _ rs : (litStr -> Just s) : a : (litStr -> Just _inst) : args)
            | length (fst $ flattenArrow $ typeOf a) == length args -> do
            sz    <- sizeOf env "rwPrimExtern" an t'
            args' <- mapM (transExp env sc) args
            applyExtern an' sz (ps, clk, rst, as, rs, s) a args'
      (Extern,  _) -> failInternal an "encountered not-fully-applied extern (after inlining)."
      (b, _) | b `elem` [Bind, Return]
                     -> failAt an ("encountered unsupported builtin use: rwPrim" <> showt b
                          <> " (a monadic operator at a monad other than the ReacT/StateT/Identity stack?).")
      (b, _)         -> failAt an ("encountered unsupported builtin use: rwPrim" <> showt b <> ".")

      where litStr :: Exp -> Maybe Text
            litStr = \ case
                  LitStr _ x -> Just x
                  _          -> Nothing

            finLit :: Exp -> Maybe Integer
            finLit e = case flattenApp e of
                  (Prim _ _ Finite, [EArg (LitInt _ _ i)]) -> Just i
                  _                                        -> Nothing

            -- | Apply a function-valued argument of a built-in vector
            --   operation to translated arguments: a reference to a global
            --   definition becomes a call; a lambda binds in place.
            applyFn :: Annote -> A.Size -> Exp -> [A.Exp] -> TM m A.Exp
            applyFn an'' szOut f args' = case flattenApp f of
                  (Var _ x, pre) | not (IM.member (idUniq x) $ scMap sc) -> do
                        pre' <- mapM (transExp env sc) [ e | EArg e <- pre ]
                        A.Call an'' szOut <$> globalName env an'' x <*> pure (pre' <> args')
                  (Lam _ p b, []) | [arg'] <- args' -> do
                        x <- freshLocal "$f"
                        let sc' = sc { scMap = IM.insert (idUniq p) (A.Var an'' (A.sizeOf arg') x) $ scMap sc }
                        b' <- transExp env sc' b
                        pure $ A.Let an'' (A.sizeOf b') x arg' b'
                  _ -> failAt an'' "hyle-m: unsupported function argument to a built-in vector operation."

            subElems :: Annote -> Exp -> Integer -> Natural -> TM m A.Exp
            subElems an'' arg i nElems = do
                  tyElem <- maybe (failAt (ann arg) "non-vector type argument to built-in vector function") pure
                          $ vecElemTy $ typeOf arg
                  szElem <- fromIntegral <$> sizeOf env "subElems" an'' tyElem
                  arg'   <- transExp env sc arg

                  let sz, off, n :: Natural
                      sz  = fromIntegral $ A.sizeOf arg'
                      off = fromIntegral $ (if i < 0 then fromIntegral sz else 0) + i * szElem
                      n   = nElems * fromIntegral szElem

                  unless (sz >= off + n)
                        $ failAt an'' $ "invalid bit slice (offset: " <> showt i <> ", num elems: " <> showt nElems <> ") from object size " <> showt sz <> "."

                  -- LSB offset of the slice: fields count from the MSB end.
                  let lsbOff = sz - off - n
                  bindAtom an'' arg' $ \ a -> pure $ slice0 an'' (fromIntegral lsbOff) (fromIntegral n) a

            -- | index v i = resize szElem (v >> ((n - i - 1) * szElem)), with
            --   the shift amount computed at the index's width (or the
            --   literal width, whichever is wider).
            vecIndex :: Exp -> Exp -> TM m A.Exp
            vecIndex v i = do
                  let tyVec = typeOf v
                  szVec  <- sizeOf env "rwPrimIndex" an tyVec
                  n      <- maybe (failAt an "rwPrimIndex: invalid Vec argument.") pure $ vecSize tyVec
                  tyElem <- maybe (failAt an "rwPrimIndex: non-vector type argument.") pure $ vecElemTy tyVec
                  szElem <- sizeOf env "rwPrimIndex" an tyElem
                  szIdx  <- sizeOf env "rwPrimIndex" an $ typeOf i
                  szLit  <- sizeOf env "rwPrimIndex" an intTy

                  v' <- transExp env sc v
                  i' <- transExp env sc i

                  -- ((n - i) - 1) * szElem, all at width w.
                  let w = max szIdx szLit
                  i''  <- resize an w i'
                  let amt = A.Prim an w A.Mul
                              [ A.Prim an w A.Sub
                                    [ A.Prim an w A.Sub [A.Lit an (bitVec (fromIntegral w) $ toInteger n), i'']
                                    , A.Lit an $ bitVec (fromIntegral w) (1 :: Integer) ]
                              , A.Lit an $ bitVec (fromIntegral w) $ toInteger szElem ]
                  resize an szElem $ A.Prim an szVec A.LShr [v', amt]

            checkProxyArg :: Text -> Exp -> TM m Integer
            checkProxyArg t p = maybe (failAt an' $ t <> ": invalid Proxy argument.  " <> prettyPrint (Ann.unAnn p)) (pure . fromIntegral)
                        $ proxyNat $ typeOf p

            checkVecArgSize :: Text -> Ty -> TM m Natural
            checkVecArgSize s t = maybe (failAt an' $ s <> ": invalid Vec size argument: " <> prettyPrint (Ann.unAnn t)) pure
                        $ vecSize t

            checkVecArgType :: Text -> Ty -> TM m Ty
            checkVecArgType s t = maybe (failAt an' $ s <> ": invalid Vec type argument: " <> prettyPrint (Ann.unAnn t)) pure
                        $ vecElemTy t

            checkFinTypeMax :: Text -> Ty -> TM m Natural
            checkFinTypeMax s t = maybe (failAt an' $ s <> ": invalid Finite type: " <> prettyPrint (Ann.unAnn t)) pure
                        $ finSz t

vecSize :: Ty -> Maybe Natural
vecSize t = case flattenTyApp t of
      (TyCon _ "Vec", [n, _]) -> evalNat n
      _                       -> Nothing

vecElemTy :: Ty -> Maybe Ty
vecElemTy t = case flattenTyApp t of
      (TyCon _ "Vec", [_, te]) -> Just te
      _                        -> Nothing

proxyNat :: Ty -> Maybe Natural
proxyNat t = case flattenTyApp t of
      (TyCon _ "Proxy", [n]) -> evalNat n
      _                      -> Nothing

finSz :: Ty -> Maybe Natural
finSz t = case flattenTyApp t of
      (TyCon _ "Finite", [n]) -> evalNat n
      _                       -> Nothing

-- | Apply a (width-implicit) primitive to translated arguments, expanding
--   to the Hyle operator set (doc/hyle.md, section 3.3).
applyPrim :: (MonadError AstError m, MonadState S m) => Annote -> A.Size -> Builtin -> [A.Exp] -> m A.Exp
applyPrim an sz b args = do
      x <- freshLocal "$t" -- may go unused (MSBit needs one)
      case toPrim b sz (case args of { a : _ -> A.sizeOf a; _ -> 0 }) of
            Just f  -> either (failAt an) pure $ f an x args
            Nothing -> failInternal an $ "applyPrim: unsupported primitive: rwPrim" <> showt b <> " with " <> showt (length args) <> " arguments."

-- | The primitive table: given the result width and the (first) operand
--   width, a pure applicator (taking a pre-generated fresh name for the
--   cases that need a binding).
toPrim :: Builtin -> A.Size -> A.Size -> Maybe (Annote -> A.Name -> [A.Exp] -> Either Text A.Exp)
toPrim b sz w = case b of
      Add         -> bin A.Add
      Sub         -> bin A.Sub
      Mul         -> bin A.Mul
      Div         -> bin A.UDiv
      Mod         -> bin A.UMod
      Pow         -> bin A.Pow
      LAnd        -> Just $ \ an _ -> two an $ \ a b' -> pure $ A.Prim an 1 A.And [redor an a, redor an b']
      LOr         -> Just $ \ an _ -> two an $ \ a b' -> pure $ A.Prim an 1 A.Or  [redor an a, redor an b']
      And         -> bin A.And
      Or          -> bin A.Or
      XOr         -> bin A.XOr
      XNor        -> Just $ \ an _ -> two an $ \ a b' -> pure $ A.Prim an sz A.Not [A.Prim an sz A.XOr [a, b']]
      LShift      -> bin A.Shl
      RShift      -> bin A.LShr
      RShiftArith -> bin A.AShr
      Eq          -> bin A.Eq
      Gt          -> bin A.UGt
      GtEq        -> bin A.UGe
      Lt          -> bin A.ULt
      LtEq        -> bin A.ULe
      LNot        -> Just $ \ an _ -> one an $ \ a -> pure $ A.Prim an 1 A.Not [redor an a]
      Not         -> Just $ \ an _ -> one an $ \ a -> pure $ A.Prim an sz A.Not [a]
      RAnd        -> red A.RedAnd
      RNAnd       -> redNot A.RedAnd
      ROr         -> red A.RedOr
      RNor        -> redNot A.RedOr
      RXOr        -> red A.RedXOr
      RXNor       -> redNot A.RedXOr
      MSBit       -> Just $ \ an x -> one an $ \ a ->
            if w >= 1 then pure $ case a of
                  A.Var {} -> A.Slice an (w - 1) 1 a
                  A.Lit {} -> A.Slice an (w - 1) 1 a
                  _        -> A.Let an 1 x a $ A.Slice an (w - 1) 1 $ A.Var an w x
            else Left "hyle-m: MSBit of a zero-width value."
      _           -> Nothing
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
                  es      -> Left $ "hyle-m: primitive arity mismatch at " <> showt an <> " (expected 2, got " <> showt (length es) <> ")."

            one :: Annote -> (A.Exp -> Either Text A.Exp) -> [A.Exp] -> Either Text A.Exp
            one an k = \ case
                  [a] -> k a
                  es  -> Left $ "hyle-m: primitive arity mismatch at " <> showt an <> " (expected 1, got " <> showt (length es) <> ")."

resize :: (MonadError AstError m, MonadState S m) => Annote -> A.Size -> A.Exp -> m A.Exp
resize an sz a
      | sz == A.sizeOf a = pure a
      | sz >  A.sizeOf a = pure $ A.Prim an sz (A.ZExt sz) [a]
      | otherwise        = pure $ A.Prim an sz (A.Trunc sz) [a]

---
--- Externs.
---

-- | Declare (or merge) the extern and apply it: a combinational extern
--   becomes an XCall; a clocked one becomes an XCall too, to be hoisted
--   into a device-level instance by hoistInstances. The model argument is
--   attached to the declaration when usable.
applyExtern :: forall m. MonadError AstError m => Annote -> A.Size -> ([Exp], Text, Text, [Exp], [Exp], Text) -> Exp -> [A.Exp] -> TM m A.Exp
applyExtern an sz (ps, clk, rst, as, rs, s) a args = do
      model <- case a of
            Var _ x
                  | T.null clk && T.null rst -> pure $ Just $ idOcc x
                  | otherwise                -> do
                        addWarning an $ "Ignoring the Haskell model for clocked extern '" <> s
                                     <> "': clocked externs are stateful and cannot be modeled by a pure function."
                        pure Nothing
            (flattenApp -> (Prim _ _ Error, _)) -> pure Nothing -- The neutered placeholder.
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
            generics :: [Exp] -> [(A.Name, Natural)]
            generics = zipWith generic [0 :: Int ..]
                  where generic i e = case pair e of
                              Just (p, v) | not (T.null p) -> (p, fromIntegral v)
                                          | otherwise      -> ("g" <> showt i, fromIntegral v)
                              _                            -> ("g" <> showt i, 0)

            pair :: Exp -> Maybe (Text, Integer)
            pair e = case flattenApp e of
                  (Con _ _ "(,)", [EArg (LitStr _ p), EArg (LitInt _ _ v)]) -> Just (p, v)
                  _                                                         -> Nothing

            ports :: [Exp] -> Maybe [(Text, A.Size)]
            ports = \ case
                  [] -> Nothing
                  es -> mapM (fmap (fmap fromIntegral) . pair) es

            portOffset :: A.ExternKind -> Int
            portOffset = \ case
                  A.Comb      -> 0
                  A.Seq mc mr -> length $ filter (/= Nothing) [mc, mr]

            -- | Anonymous ports are named p<i> by their position in the
            --   clock-reset-inputs-outputs list (the established convention).
            named :: Int -> [(Text, A.Size)] -> [(A.Name, A.Size)]
            named off = zipWith (\ i (p, psz) -> (if T.null p then "p" <> showt i else p, psz)) [off ..]

            mergeExt :: A.Extern -> A.Extern -> TM m A.Extern
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
--- The machine fold.
---

-- | The step-record accounting for one process (see the module header for
--   the layout).
data Layout = Layout
      { loRecW    :: !A.Size                      -- ^ Total record width.
      , loPTagW   :: !A.Size                      -- ^ The halted flag: 1 if a halt is reachable, else 0.
      , loOutW    :: !A.Size
      , loRTagW   :: !A.Size                      -- ^ Label tag width.
      , loRPayW   :: !A.Size                      -- ^ Max summed pause-argument width.
      , loCells   :: ![(Text, A.Size)]            -- ^ Cell names and widths, in declaration order.
      , loTargets :: ![(Uniq, Integer, [A.Size])] -- ^ Pause targets: unique, tag value, argument widths.
      , loHalts   :: !(HashMap Text (Integer, A.Size)) -- ^ Halt answer types (by rendered key): tag value and width.
      , loATagW   :: !A.Size
      , loAPayW   :: !A.Size
      }

loRW, loCellsW, loAW :: Layout -> A.Size
loRW l     = loRTagW l + loRPayW l
loCellsW l = sum $ map snd $ loCells l
loAW l     = loATagW l + loAPayW l

mkLayout :: MonadError AstError m => Env -> Proc -> TM m Layout
mkLayout env pr = do
      outW  <- sizeOf env "proc output" (procAnnote pr) $ procOutTy pr
      cells <- mapM (\ c -> (cellName c, ) <$> sizeOf env "cell" (cellAnnote c) (cellTy c)) $ procCells pr
      targets <- mapM (\ (i, (l, b)) -> do
                  szs <- mapM (sizeOf env "pause target param" (blkAnnote b) . sigTy . idSig) $ initSafe $ blkParams b
                  pure (idUniq l, toInteger i, szs))
            $ zip [0 :: Int ..] [ (l, b) | (l, b) <- procBlocks pr, idUniq l `Set.member` pauseTargets ]
      haltSzs <- mapM (\ t -> (prettyPrint (Ann.unAnn t), ) <$> sizeOf env "halt answer" (procAnnote pr) t) haltTys
      let haltList = nubOrd haltSzs
          rTagW  = fromIntegral $ nbits $ genericLength targets
          rPayW  = maximum $ 0 : [ sum szs | (_, _, szs) <- targets ]
          aTagW  = fromIntegral $ nbits $ genericLength haltList
          aPayW  = maximum $ 0 : map snd haltList
          pTagW  = if null haltList then 0 else 1
          cellsW = sum $ map snd cells
          pauseLoad = outW + rTagW + rPayW + cellsW
          doneLoad  = aTagW + aPayW + cellsW
          recW   = pTagW + max pauseLoad (if null haltList then 0 else doneLoad)
      pure $ Layout { loRecW  = recW,  loPTagW = pTagW, loOutW  = outW
                    , loRTagW = rTagW, loRPayW = rPayW, loCells = cells
                    , loTargets = targets
                    , loHalts = Map.fromList [ (k, (i, w)) | (i, (k, w)) <- zip [0 ..] haltList ]
                    , loATagW = aTagW, loAPayW = aPayW
                    }
      where blocks :: [Block]
            blocks = procEntry pr : map snd (procBlocks pr)

            pauseTargets :: HashSet Uniq
            pauseTargets = Set.fromList $ concatMap (pt . blkTerm) blocks
                  where pt :: Term -> [Uniq]
                        pt = \ case
                              Pause _ _ l _  -> [idUniq l]
                              TCase _ _ alts -> concat [ pt t | TAlt _ _ _ t <- alts ]
                              _              -> []

            haltTys :: [Ty]
            haltTys = concatMap (ht . blkTerm) blocks
                  where ht :: Term -> [Ty]
                        ht = \ case
                              Halt _ a       -> [typeOf a]
                              TCase _ _ alts -> concat [ ht t | TAlt _ _ _ t <- alts ]
                              _              -> []

            initSafe :: [a] -> [a]
            initSafe = \ case
                  [] -> []
                  xs -> init xs

-- | The step record for a pause: @halted=Pause | pad | out | tag | pad | args | cells@.
buildPause :: MonadError AstError m => Layout -> Annote -> A.Exp -> Uniq -> [A.Exp] -> [A.Exp] -> TM m A.Exp
buildPause lo an o tgt args cells = do
      tagv <- case [ v | (u, v, _) <- loTargets lo, u == tgt ] of
            [v] -> pure v
            _   -> failInternal an "hyle-m: pause to an unknown label (rwc bug)."
      let padW  = fromIntegral (loRecW lo) - fromIntegral (loPTagW lo) - fromIntegral (loOutW lo) - fromIntegral (loRW lo) - fromIntegral (loCellsW lo) :: Integer
          rPadW = fromIntegral (loRPayW lo) - fromIntegral (sum $ map A.sizeOf args) :: Integer
      pure $ A.cat $ [ A.Lit an $ bitVec (fromIntegral $ loPTagW lo) (1 :: Integer) | loPTagW lo > 0 ]
                  <> [ A.Lit an $ zeros $ fromIntegral padW ]
                  <> [ o ]
                  <> [ A.Lit an $ bitVec (fromIntegral $ loRTagW lo) tagv | loRTagW lo > 0 ]
                  <> [ A.Lit an $ zeros $ fromIntegral rPadW ]
                  <> args <> cells

-- | The step record for a halt: @halted=Done | pad | A-tag | pad | answer | cells@.
buildHalt :: MonadError AstError m => Layout -> Annote -> Ty -> A.Exp -> [A.Exp] -> TM m A.Exp
buildHalt lo an aty a cells = do
      (tagv, _) <- maybe (failInternal an "hyle-m: halt at an unknown answer type (rwc bug).") pure
            $ Map.lookup (prettyPrint $ Ann.unAnn aty) $ loHalts lo
      let padW  = fromIntegral (loRecW lo) - fromIntegral (loPTagW lo) - fromIntegral (loAW lo) - fromIntegral (loCellsW lo) :: Integer
          aPadW = fromIntegral (loAPayW lo) - fromIntegral (A.sizeOf a) :: Integer
      pure $ A.cat $ [ A.Lit an $ bitVec (fromIntegral $ loPTagW lo) (0 :: Integer) | loPTagW lo > 0 ]
                  <> [ A.Lit an $ zeros $ fromIntegral padW ]
                  <> [ A.Lit an $ bitVec (fromIntegral $ loATagW lo) tagv | loATagW lo > 0 ]
                  <> [ A.Lit an $ zeros $ fromIntegral aPadW ]
                  <> [ a ] <> cells

-- | Translate one process into its Hyle definitions and the device.
transProc :: forall m. MonadError AstError m => Config -> Env -> Proc -> [A.Defn] -> TM m A.Program
transProc conf env pr pureDefns = do
      lo <- mkLayout env pr
      let qual n = procName pr <> "." <> n
          blockGid l = qual $ idOcc l <> "$" <> showt (idUniq l)

      inW <- sizeOf env "proc input" (procAnnote pr) $ procInTy pr

      blockDefns <- mapM (\ (l, b) -> transBlock lo (blockGid l) blockGid b) $ procBlocks pr
      entryDefn  <- transBlock lo (qual "$entry") blockGid $ procEntry pr
      dispatch   <- dispatchDefn lo (qual "$dispatch") blockGid inW

      exts <- gets $ sortOn A.extName . Map.elems . sExterns
      dev  <- mkDeviceM lo inW entryDefn dispatch (blockDefns <> pureDefns) exts

      hoistInstances $ A.Program exts (dispatch : blockDefns <> pureDefns) dev
      where -- | A block becomes one definition: parameters, then the cells
            --   as trailing parameters; commands become lets; the
            --   terminator builds the step record (pause/halt) or
            --   tail-calls the target block (goto).
            transBlock :: Layout -> A.GId -> (Id -> A.GId) -> Block -> TM m A.Defn
            transBlock lo gid blockGid (Block an ps cmds term) = do
                  modify $ \ s -> s { sCtr = 0 }
                  pSzs <- mapM (sizeOf env "block param" an . sigTy . idSig) ps
                  let (sc0, pnames)   = bindParams an scope0 (zip ps pSzs)
                      (sc1, cellNms)  = foldl' (\ (s, acc) (c, w) ->
                                          let (s', nm) = scopeName s c in (s', acc <> [(c, nm, w)]))
                                          (sc0, []) (loCells lo)
                      cellAtoms       = Map.fromList [ (c, A.Var an w nm) | (c, nm, w) <- cellNms ]
                  body <- goCmds lo blockGid sc1 cellAtoms cmds term
                  pure $ A.Defn an gid (A.Sig an (pSzs <> map snd (loCells lo)) (loRecW lo)) (pnames <> [ nm | (_, nm, _) <- cellNms ]) body

            goCmds :: Layout -> (Id -> A.GId) -> Scope -> HashMap Text A.Exp -> [Cmd] -> Term -> TM m A.Exp
            goCmds lo blockGid sc cells = \ case
                  [] -> transTerm lo blockGid sc cells
                  (CmdBind an x e : rest) -> \ term -> do
                        e' <- transExp env sc e
                        case e' of
                              A.Lit {} -> goCmds lo blockGid (sc { scMap = IM.insert (idUniq x) e' $ scMap sc }) cells rest term
                              A.Var {} -> goCmds lo blockGid (sc { scMap = IM.insert (idUniq x) e' $ scMap sc }) cells rest term
                              _        -> do
                                    let (sc', nm) = bindLocal an sc x $ A.sizeOf e'
                                    body <- goCmds lo blockGid sc' cells rest term
                                    pure $ A.Let an (A.sizeOf body) nm e' body
                  (CmdGet _ x c : rest) -> \ term -> do
                        atom <- maybe (failInternal (procAnnote pr) "hyle-m: get from an unknown cell (rwc bug).") pure
                              $ Map.lookup c cells
                        goCmds lo blockGid (sc { scMap = IM.insert (idUniq x) atom $ scMap sc }) cells rest term
                  (CmdPut an c e : rest) -> \ term -> do
                        e' <- transExp env sc e
                        case e' of
                              A.Lit {} -> goCmds lo blockGid sc (Map.insert c e' cells) rest term
                              A.Var {} -> goCmds lo blockGid sc (Map.insert c e' cells) rest term
                              _        -> do
                                    let (sc', nm) = scopeName sc c
                                    body <- goCmds lo blockGid sc' (Map.insert c (A.Var an (A.sizeOf e') nm) cells) rest term
                                    pure $ A.Let an (A.sizeOf body) nm e' body

            transTerm :: Layout -> (Id -> A.GId) -> Scope -> HashMap Text A.Exp -> Term -> TM m A.Exp
            transTerm lo blockGid sc cells term = case term of
                  Pause an o l es -> do
                        o'  <- transExp env sc o
                        es' <- mapM (transExp env sc) es
                        buildPause lo an o' (idUniq l) es' $ cellVals cells
                  Goto an l es    -> do
                        es' <- mapM (transExp env sc) es
                        pure $ A.Call an (loRecW lo) (blockGid l) $ es' <> cellVals cells
                  Halt an a       -> do
                        a' <- transExp env sc a
                        buildHalt lo an (typeOf a) a' $ cellVals cells
                  TCase an scrut alts -> do
                        scrut' <- transExp env sc scrut
                        bindAtom (ann scrut) scrut' $ \ datom ->
                              caseChain env sc an (loRecW lo) (typeOf scrut) datom
                                    [ (aan, c, xs, t) | TAlt aan c xs t <- alts ]
                                    (\ sc' t -> transTerm lo blockGid sc' cells t)
                  where cellVals :: HashMap Text A.Exp -> [A.Exp]
                        cellVals m = [ fromMaybe (A.Lit noAnn $ zeros $ fromIntegral w) $ Map.lookup c m | (c, w) <- loCells lo ]

            -- | @dispatch (label-tag | args | cells) i@: an if-chain over
            --   the label tag calling the pause-target blocks.
            dispatchDefn :: Layout -> A.GId -> (Id -> A.GId) -> A.Size -> TM m A.Defn
            dispatchDefn lo gid blockGid inW = do
                  let an     = procAnnote pr
                      stW    = loRW lo + loCellsW lo
                      disc   = A.Var an stW "disc"
                      i      = A.Var an inW "i"
                      cellsW = loCellsW lo
                      tag    = slice0 an (fromIntegral (loRPayW lo) + fromIntegral cellsW) (loRTagW lo) disc
                      cellSlices = [ slice0 an off w disc | ((), w, off) <- wireOffsets [ ((), w) | (_, w) <- loCells lo ] ]
                      -- Arguments live LSB-aligned under the tag.
                      argSlices szs = [ slice0 an (off + fromIntegral cellsW) w disc | ((), w, off) <- wireOffsets [ ((), w) | w <- szs ] ]
                      gidMap = IM.fromList [ (idUniq l, blockGid l) | (l, _) <- procBlocks pr ]
                      call (u, _, szs) = A.Call an (loRecW lo) (IM.findWithDefault "" u gidMap) $ argSlices szs <> [i] <> cellSlices
                  body <- case loTargets lo of
                        []  -> failInternal an "hyle-m: a process with no pause targets (rwc bug)."
                        tgts -> pure $ foldr (\ t@(_, v, _) acc ->
                                          A.If an (loRecW lo) (A.Prim an 1 A.Eq [tag, A.Lit an $ bitVec (fromIntegral $ loRTagW lo) v]) (call t) acc)
                                    (call $ last tgts) (init tgts)
                  pure $ A.Defn an gid (A.Sig an [stW, inW] (loRecW lo)) ["disc", "i"] body

            -- | Assemble the device: registers (initials by evaluating the
            --   entry definition with zero-filled cells), the dispatch
            --   call, and the output/next-state slice equations.
            mkDeviceM :: Layout -> A.Size -> A.Defn -> A.Defn -> [A.Defn] -> [A.Extern] -> TM m A.Device
            mkDeviceM lo inW entryDefn dispatch defns exts = do
                  let an = procAnnote pr
                  inSzs  <- detupleSizes an $ procInTy pr
                  outSzs <- detupleSizes an $ procOutTy pr
                  stSzs  <- concat <$> mapM (detupleSizes an . cellTy) (procCells pr)
                  let inWires  = zip (conf^.inputSigs)  inSzs
                      outWires = zip (conf^.outputSigs) outSzs
                      stWires  = zip (conf^.stateSigs)  stSzs
                      rW       = loRW lo
                      regWires = [ ("__resumption_tag", rW) | rW > 0 ] <> stWires
                      padding  = loRecW lo - loOutW lo - sum (map snd regWires)
                      layout   = wireOffsets $ ("__padding", padding) : outWires <> regWires

                  regs <- if null regWires then pure []
                        else do
                              let ienv  = IEnv (Map.fromList $ map (\ d -> (A.defnName d, d)) $ dispatch : entryDefn : defns)
                                          (Map.fromList $ map (\ e -> (A.extName e, e)) exts)
                                  A.Sig _ argSzs _ = A.defnSig entryDefn
                                  args0 = Map.fromList $ zip (A.defnParams entryDefn) $ map (zeros . fromIntegral) argSzs
                              checkInitAcyclic (ann entryDefn) (envDefns ienv) $ A.defnBody entryDefn
                              init0 <- evalExp ienv args0 (A.defnBody entryDefn)
                                    `catchError` \ _ -> failAt (ann entryDefn)
                                          "cannot evaluate the initial state (does it involve an extern?)"
                              pure [ A.Register an x sz $ sliceBV init0 off sz | (x, sz, off) <- wireOffsets regWires ]

                  let stInps   = regWires <> inWires
                      inExp    = A.cat [ A.Var an sz x | (x, sz) <- stInps ]
                      loopArgs = [ slice0 an off sz (A.Var an (sum $ map snd stInps) "$in") | (_, sz, off) <- wireOffsets (map ((), ) [rW + loCellsW lo, inW]) ]
                      resVar   = A.Var an (loRecW lo) "$res"
                      outNames = Set.fromList $ map fst outWires
                      regNames = Set.fromList $ map fst regWires

                  pure $ A.Device an (conf^.top) inWires outWires regs []
                       $  [ A.SLet an "$in" inExp
                          , A.SLet an "$res" $ A.Call an (loRecW lo) (A.defnName dispatch) loopArgs ]
                       <> [ A.SOutput an x $ slice0 an off sz resVar | (x, sz, off) <- layout, x `Set.member` outNames ]
                       <> [ A.SNext an x $ slice0 an off sz resVar   | (x, sz, off) <- layout, x `Set.member` regNames ]
                  where sliceBV :: BV -> A.Index -> A.Size -> BV
                        sliceBV bv off sz = bitVec (fromIntegral sz) $ BV.nat bv `div` (2 ^ toInteger off)

                        -- The retired producer's port convention: one level
                        -- of type-application splitting (the bare head
                        -- sizes the constructor tag; Vec and Finite stay
                        -- whole), a leading residual component, zero
                        -- widths dropped.
                        detupleSizes :: Annote -> Ty -> TM m [A.Size]
                        detupleSizes an' t = do
                              whole <- sizeOf env "device port" an' t
                              parts <- mapM (sizeOf env "device port" an') $ detuple t
                              pure $ map fromIntegral $ filter (> 0)
                                   $ (toInteger whole - sum (map toInteger parts)) : map toInteger parts

                        detuple :: Ty -> [Ty]
                        detuple t = case flattenTyApp t of
                              (TyCon _ "Vec", _)    -> [t]
                              (TyCon _ "Finite", _) -> [t]
                              (h, args)             -> h : args

-- | Rejects a cyclic call graph reachable from the initial-state
--   expression: the register-initial evaluation interprets it with no
--   step bound, so a reachable cycle means divergence without this check.
checkInitAcyclic :: MonadError AstError m => Annote -> HashMap A.GId A.Defn -> A.Exp -> m ()
checkInitAcyclic an defns e0 = () <$ go mempty mempty e0
      where go :: MonadError AstError m => HashSet A.GId -> HashSet A.GId -> A.Exp -> m (HashSet A.GId)
            go stack done e = foldM goCall done $ expCalls e
                  where goCall :: MonadError AstError m => HashSet A.GId -> A.GId -> m (HashSet A.GId)
                        goCall done' g
                              | g `Set.member` stack = failAt an
                                    $ "cannot evaluate the initial state: definition is recursive (is recursion guarded by signal?): " <> g
                              | g `Set.member` done' = pure done'
                              | Just d <- Map.lookup g defns = Set.insert g <$> go (Set.insert g stack) done' (A.defnBody d)
                              | otherwise = pure done'

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
