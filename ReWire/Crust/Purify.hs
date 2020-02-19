{-# LANGUAGE FlexibleContexts, LambdaCase, ViewPatterns, TupleSections #-}
{-# LANGUAGE Safe #-}

module ReWire.Crust.Purify (purify) where

import ReWire.Annotation
import ReWire.Unbound
      ( Fresh (..), s2n, n2s
      )
import ReWire.Error
import ReWire.Crust.Syntax
import ReWire.Pretty

import Control.Arrow (first, second, (&&&))
import Control.Monad.State
import Control.Monad.Fail (MonadFail)
import Data.List (foldl', intercalate)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Bool (bool)

import Data.Set (Set, singleton, insert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Containers.ListUtils (nubOrdOn)

atMay :: (Eq i, Num i) => [a] -> i -> Maybe a
atMay []       _ = Nothing
atMay (x : _)  0 = Just x
atMay (_ : xs) n = atMay xs $ n - 1

isPrim :: Name Exp -> Bool
isPrim = notElem '.' . n2s

isStart :: Name Exp -> Bool
isStart = (== "Main.start") . n2s

freshVar :: Fresh m => String -> m (Name a)
freshVar n = fresh $ s2n n

freshVars :: Fresh m => String -> [b] -> m [(Name a, b)]
freshVars v = mapM (\ (i, b) -> (, b) <$> freshVar (v ++ show i)) . zip [0 :: Int ..]

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item : b)
      where (a, _ : b) = splitAt n ls

poly2Ty :: Fresh m => Poly -> m Ty
poly2Ty (Poly p) = snd <$> unbind p

projDefnTy :: Fresh m => Defn -> m Ty
projDefnTy Defn { defnPolyTy = Embed t } = poly2Ty t

purify :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) => FreeProgram -> m FreeProgram
purify (ts, ds) = do
      (smds, pds)         <- partitionEithers <$> mapM isStateMonadicDefn ds
      (rmds, ods)         <- partitionEithers <$> mapM isResMonadicDefn pds
      let (startd, rmds') =  partitionEithers  $  map  isStartDefn rmds
      let rmds = rmds' ++ startd -- TODO(chathhorn): kludge to make sure start is the last thing purified.

      when (null startd) $ failAt noAnn "No definition for Main.start found!"

      let nameNpolyS = map (defnName &&& defnPolyTy) smds
          nameNpolyR = map (defnName &&& defnPolyTy) rmds

      (i, o, ms, _) <- getCanonReTy rmds >>= liftMaybe noAnn "Inconsistent ReT types!"

      rhoS      <- mkPureEnv nameNpolyS
      rhoR      <- mkPureEnv nameNpolyR
      let rho = rhoS ++ rhoR

      pure_smds <- mapM (purifyStateDefn rho) smds

      iv <- freshVar "i"

      (pure_rmds, PSto dcs pes _ allAs) <- runStateT (mapM (purifyResDefn rho ms) rmds) $ PSto [] [] iv mempty

      -- TODO(chathhorn): entirely to remove duplicate R_return.
      let dcs' = nubOrdOn dcName dcs
          pes' = nubOrdOn (patStr . fst) pes
          dcName (DataCon _ d _) = n2s d
          patStr (PatCon _ _ (Embed n) _) = n2s n
          patStr (PatVar _ _ n) = n2s n
      -- ^ TODO(chathhorn) hackity hack

      disp <- mkDispatch i o ms iv pes'

      pure ( filter (not . isAorR) ts ++ [mkRDatatype dcs'] ++ [mkADatatype $ Map.toList allAs]
           , mkStart i o ms
           : disp
           : ods ++ pure_smds ++ pure_rmds)

      -- TODO(chathhorn): what if I retype extrude and unfold (et al.?) with concrete types and re-run type inference?
      where getCanonReTy :: Fresh m => [Defn] -> m (Maybe (Ty, Ty, [Ty], Set Ty))
            getCanonReTy = (>>= pure . getCanonReTy' . catMaybes) . mapM (projDefnTy >=> pure . dstReT)

            getCanonReTy' :: [(Ty, Ty, [Ty], Ty)] -> Maybe (Ty, Ty, [Ty], Set Ty)
            getCanonReTy' = \ case
                  []                  -> Nothing
                  (i, o, ms, a) : rs  -> Just $ foldr mergeReTys (i, o, ms, singleton a) rs

            -- TODO(chathhorn): check for ReT consistency here.
            mergeReTys :: (Ty, Ty, [Ty], Ty) -> (Ty, Ty, [Ty], Set Ty) -> (Ty, Ty, [Ty], Set Ty)
            mergeReTys (i, o, ms, a) (_ , _ , ms', a') | length ms > length ms' = (i, o, ms, insert a a')
            mergeReTys (_, _, _ , a) (i', o', ms', a')                          = (i', o', ms', insert a a')

            isAorR :: DataDefn -> Bool
            isAorR = uncurry (||) . ((== "A_") &&& (== "R_")) . n2s . dataName

-- In addition to all the above, we re-tie the recursive knot by adding a new
--   "start" as follows
--         start :: ReT In Out I T
--         start = unfold dispatch start_pure
--           start :: In -> (Either T (O, R), ()) --- drop the "()"?

-- Correcting mkStart.
-- In the type of Main.start.
-- t (return type of Main.start) needs to be combined with ms (list of StT s, stores).
mkStart :: Ty -> Ty -> [Ty] -> Defn
mkStart i o ms = Defn
      { defnAnnote = MsgAnnote "start function"
      , defnName   = s2n "Main.start"
      , defnPolyTy = [] |-> startTy
      , defnInline = False
      , defnBody   = appl
      }
      where etor       = mkRangeTy o ms
            resultTy   = if null ms then aTy else mkPairTy aTy $ mkTupleTy ms

            reT i o a  = TyCon (MsgAnnote "Purify: reT") (s2n "ReT") `tyApp` i `tyApp` o `tyApp` a
            startTy    = tycomp (MsgAnnote "Purify: startTy")
                              (reT i o (TyCon (MsgAnnote "Purify: startTy") (s2n "I")))
                              resultTy

            unfold     = Var (MsgAnnote "Purify: unfold") (mkArrowTy [dispatchTy i o ms, etor] startTy) $ s2n "unfold"
            dispatch   = Var (MsgAnnote "Purify: dispatch") (dispatchTy i o ms) $ s2n "$Pure.dispatch"
            start_pure = Var (MsgAnnote "Purify: $Pure.start") etor $ s2n "$Pure.start"
            appl       = Embed $ bind [] $ App (MsgAnnote "Purify: appl") (App (MsgAnnote "Purify: appl") unfold dispatch) start_pure

            tyApp :: Ty -> Ty -> Ty
            tyApp = TyApp $ MsgAnnote "reT"

-- Converts
--    (extrude (extrude (... (extrude dev s1) ...) s(n-1)) sn)
-- Into
--    (dev, [s1, ..., sn])
-- Won't work if called on a non-extrude application.
flattenExtr :: Exp -> (Exp, [Exp])
flattenExtr = \ case
      App _ (App _ (Var _ _ (n2s -> "extrude")) arg1) arg2 -> second (++ [arg2]) $ flattenExtr arg1
      e@(App _ _ rand)                                     -> first (const e) $ flattenExtr rand
      e                                                    -> (e, [])

-- dispatch (R_g e1 ... ek) i = g_pure e1 ... ek i
mkDispatch :: (MonadError AstError m, Fresh m) => Ty -> Ty -> [Ty] -> Name Exp -> [(Pat, Exp)] -> m Defn
mkDispatch _ _ _  _  [] = failAt NoAnnote "Purify: Empty dispatch: invalid ReWire (is recursion guarded by signal?)"
mkDispatch i o ms iv (p : pes) = do
      let ty    = dispatchTy i o ms
          domTy = if null ms then rTy else (mkPairTy rTy . mkTupleTy) ms
      dsc     <- freshVar "dsc"
      let body  = Embed $ bind [dsc :: Name Exp, iv :: Name Exp] cases
          cases = mkCase an (Var an domTy dsc) p pes
          an    = MsgAnnote $ "Purify: Generated dispatch function: " ++ prettyPrint cases

      pure Defn
            { defnAnnote = an
            , defnName   = s2n "$Pure.dispatch"
            , defnPolyTy = [] |-> ty
            , defnInline = False
            , defnBody   = body
            }

mkCase :: Annote -> Exp -> (Pat, Exp) -> [(Pat, Exp)] -> Exp
mkCase an dsc (p, e) = \ case
      []         -> Case an (typeOf e) dsc (bind p e) Nothing
      (pe : pes) -> Case an (typeOf e) dsc (bind p e) . Just $ mkCase an dsc pe pes

mkRDatatype :: [DataCon] -> DataDefn
mkRDatatype dcs = DataDefn
       { dataAnnote = MsgAnnote "Purify: R Datatype"
       , dataName   = s2n "R_"
       , dataKind   = KStar
       , dataCons   = dcs
       }

mkRDataCon :: Annote -> Name DataConId -> [Ty] -> DataCon
mkRDataCon an r_g ts = DataCon an r_g $ [] |-> mkArrowTy ts rTy

mkADatatype :: [(Ty, Name DataConId)] -> DataDefn
mkADatatype allAs = DataDefn
       { dataAnnote = MsgAnnote "Purify: A Datatype"
       , dataName   = s2n "A_"
       , dataKind   = KStar
       , dataCons   = map mkADataCon allAs
       }

mkADataCon :: (Ty, Name DataConId) -> DataCon
mkADataCon (t, n) = DataCon (MsgAnnote "Purify: Generated A data ctor") n $ [] |-> mkArrowTy [t] aTy

type PureEnv = [(Name Exp, Ty)]

mkPureEnv :: (Fresh m, MonadError AstError m) => [(Name Exp, Embed Poly)] -> m PureEnv
mkPureEnv []                     = pure []
mkPureEnv ((n, Embed phi) : nps) = do
      ty     <- poly2Ty phi
      purety <- purifyTy (ann ty) (getStates ty) ty
      ((n, purety) :) <$> mkPureEnv nps

getStates :: Ty -> [Ty]
getStates t = case dstReT $ rangeTy t of
      Just (_, _, sts', _) -> sts'
      _                    -> fromMaybe [] $ dstCompM (rangeTy t) >>= dstStT

lookupPure :: MonadError AstError m => Annote -> Name Exp -> PureEnv -> m Ty
lookupPure an x = maybe (failAt an $ "No pure binding for variable: " ++ n2s x) pure . lookup x

isStateMonadicDefn :: Fresh m => Defn -> m (Either Defn Defn)
isStateMonadicDefn = \ case
      d@Defn { defnName = n } | isPrim n -> pure $ Right d
      d@Defn { defnPolyTy = Embed poly } -> bool (Right d) (Left d) . isStateMonad <$> poly2Ty poly
      where isStateMonad :: Ty -> Bool
            isStateMonad ty = case rangeTy ty of
                  TyApp an (TyApp _ (TyApp _ (TyCon _ (n2s -> "StT")) _) m) a -> isStateMonad (tycomp an m a)
                  TyApp _ (TyCon _ (n2s -> "I")) _                            -> True
                  _                                                           -> False

isResMonadicDefn :: Fresh m => Defn -> m (Either Defn Defn)
isResMonadicDefn = \ case
      d@Defn { defnName = n } | isPrim n -> pure $ Right d
      d@Defn { defnPolyTy = Embed poly } -> bool (Right d) (Left d) . isResMonad <$> poly2Ty poly
      where isResMonad :: Ty -> Bool
            isResMonad ty = case rangeTy ty of
                  TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReT")) _) _) _) _ -> True
                  _                                                                      -> False

isStartDefn :: Defn -> Either Defn Defn
isStartDefn d = if isStart $ defnName d then Left d else Right d

purifyStateDefn :: (Fresh m, MonadError AstError m, MonadIO m) =>
                   PureEnv -> Defn -> m Defn
purifyStateDefn rho d = do
      ty           <- poly2Ty phi
      let ms        = getStates ty
      p_pure       <- lookupPure (ann d) (defnName d) rho
      let d_pure    = defnName d
      (args, e)    <- unbind body
      stys         <- liftMaybe (ann d) "failed at purifyStateDefn" $ dstCompM (rangeTy ty) >>= dstStT
      nstos        <- freshVars "sigma" stys
      let stos      = map (mkVar $ ann d) nstos
      e'           <- purifyStateBody rho stos stys 0 ms e
      let b_pure    = bind (args ++ map fst nstos) e'
      pure $ d { defnName = d_pure, defnPolyTy = [] |-> p_pure, defnBody = Embed b_pure }
      where Embed body = defnBody d
            Embed phi  = defnPolyTy d

liftMaybe :: MonadError AstError m => Annote -> String -> Maybe a -> m a
liftMaybe an msg = maybe (failAt an msg) pure

purifyResDefn :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) =>
                 PureEnv -> [Ty] -> Defn -> StateT PSto m Defn
purifyResDefn rho ms d = do
      ty           <- projDefnTy d
      (i, o, _, a) <- liftMaybe (ann d) "failed at purifyResDefn" $ dstReT $ rangeTy ty
      pure_ty      <- purifyTy (ann d) ms ty
      (args, e)    <- unbind body


      (nstos, stos, ms) <- do
            nstos   <- freshVars "sto" ms
            pure (map fst nstos, map (mkVar an) nstos, ms)
      e'           <- purifyResBody rho i o a stos ms e

      --
      -- Below is an egregious hack to compile the start symbol slightly differently.
      --
      let args'  = if isStart dname then [] else args
          nstos' = if isStart dname then [] else nstos
          b_pure = bind (args' ++ nstos') e'
          p_pure = if isStart dname then [] |-> rangeTy pure_ty else [] |-> pure_ty
          d_pure = if isStart dname then s2n "$Pure.start" else dname

      pure $ d { defnName = d_pure, defnPolyTy = p_pure, defnBody = Embed b_pure }
      where
            dname      = defnName d
            Embed body = defnBody d
            an         = defnAnnote d

---------------------------
-- Purifying Types
---------------------------

data TyVariety = Arrow Ty Ty | ReTApp | StTApp | IdApp | PairApp Ty Ty | Pure

purifyTy :: MonadError AstError m => Annote -> [Ty] -> Ty -> m Ty
purifyTy an ms t = case classifyTy t of
      Arrow t1 t2   -> TyApp an <$> (TyApp an (TyCon an $ s2n "->") <$> purifyTy an ms t1) <*> purifyTy an ms t2
      ReTApp        -> liftMaybe an ( "Purify: failed to purify ResT type: " ++ prettyPrint t)
                                    $ purifyResTy t
      StTApp        -> liftMaybe an ( "Purify: failed to purify StT type: " ++ prettyPrint t)
                                    $ purifyStTTy t
      PairApp t1 t2 -> TyApp an <$> (TyApp an (TyCon an $ s2n "(,)") <$> purifyTy an ms t1) <*> purifyTy an ms t2
      Pure          -> pure t
      _             -> failAt an $ "Purify: failed to purify type: " ++ prettyPrint t

            -- This takes a Ty of the form
            --      T1 -> T2 -> ... -> Tn -> ReT In Out (StT S1 (StT S2 (... (StT Sm I)))) T
            -- and returns a Ty of the form
            --      T1 -> T2 -> ... -> Tn -> In -> S1 -> ... -> Sm -> (Either T (O, R), (S1, (..., Sm)))
      where purifyResTy :: Ty -> Maybe Ty
            purifyResTy t = do
                  (_, o, _, _) <- dstReT $ rangeTy t
                  pure $ mkArrowTy (paramTys t ++ ms) $ mkRangeTy o ms

            -- Takes
            --      T1 -> T2 -> ... -> Tn -> StT S1 (StT S2 (... (StT Sm I))) T
            -- and replaces it by
            --      T1 -> ... -> Tn -> S1 -> ... -> Sm -> (T, (S1, (..., Sm)))
            --
            -- I'm going to rewrite this to stick precisely to Adam's description.
            -- N.b., the product in the codomain associates to the right. The
            -- definition of purifyStTTy required a little hackery to get the result
            -- into that form.
            purifyStTTy :: Ty -> Maybe Ty
            purifyStTTy t = do
                  let r        = rangeTy t
                  a           <- dstCompR r
                  stos        <- dstCompM r >>= dstStT
                  pure $ mkArrowTy (paramTys t ++ stos) $ mkTupleTy (a : stos)

            classifyTy :: Ty -> TyVariety
            classifyTy = \ case
                  TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReT")) _) _) _) _ -> ReTApp
                  TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t1) t2                        -> Arrow t1 t2
                  TyApp _ (TyApp _ (TyCon _ (n2s -> "(,)")) t1) t2                       -> PairApp t1 t2
                  TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "StT")) _) _) _             -> StTApp
                  TyApp _ (TyCon _ (n2s -> "I")) _                                       -> IdApp
                  _                                                                      -> Pure

dstArrow :: MonadError AstError m => Ty -> m (Ty, Ty)
dstArrow = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t1) t2 -> pure (t1, t2)
      t                                               -> failAt (ann t) "Purify: expecting arrow, encountered non-arrow"

-- | This takes a type of the form
--    StT S1 (StT S2 (... (StT Sm I)))
-- and returns
--    [S1, ..., Sm]
dstStT :: Ty -> Maybe [Ty]
dstStT = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "StT")) s) m -> (s :) <$> dstStT m
      TyCon _ (n2s -> "I")                           -> pure []
      _                                              -> Nothing

-- | This takes a type of the form
--    m a
-- and returns
--    a
dstCompR :: Ty -> Maybe Ty
dstCompR = \ case
      TyApp _ _ a -> pure a
      _           -> Nothing

-- | This takes a type of the form
--    m a
-- and returns
--    m
dstCompM :: Ty -> Maybe Ty
dstCompM = \ case
      TyApp _ m _ -> pure m
      _           -> Nothing

-- | This takes a type of the form
--    ReT In Out (StT S1 (StT S2 (... (StT Sm I)))) T
-- and returns
--    (In, Out, [S1, ..., Sm], T)
dstReT :: Ty -> Maybe (Ty, Ty, [Ty], Ty)
dstReT = \ case
      TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReT")) i) o) m) a -> dstStT m >>= \ ms -> pure (i, o, ms, a)
      _                                                                      -> Nothing

---------------------------
-- Purifying State Monadic definitions
---------------------------

classifyCases :: (Fresh m, MonadError AstError m) => Exp -> m (Cases Annote Pat Exp Ty)
classifyCases = \ case
      Var an t g
            | n2s g == "get"    -> pure $ Get an t
            | otherwise         -> pure $ Apply an t g []
      App an (Var _ t x) e
            | n2s x == "return" -> pure $ Return an t e
            | n2s x == "put"    -> pure $ Put an e
            | n2s x == "lift"   -> pure $ Lift an e
      App an (App _ (Var _ t x) e) g
            | n2s x == ">>="    -> pure $ Bind an t e g
            | otherwise         -> pure $ Apply an t x [e, g]
      e@App {}                  -> do
            (n, t, es) <- dstApp e
            pure $ Apply (ann e) t n es
      Match an t e1 mp e2 es me -> pure $ SMatch an t e1 mp e2 es me
      d                         -> failAt (ann d) $ "Unclassifiable case: " ++ show d

data Cases an p e t = Get an t
                    | Return an t e
                    | Lift an e
                    | Put an e
                    | Bind an t e e
                    | Apply an t (Name Exp) [e]
                    | SMatch an t e MatchPat e [e] (Maybe e)

-- | purifyStateBody
--   rho  -- pure environment
--   stos -- state vars
--   stys -- state types
--   i    -- lifting "depth"
--   tm   -- term to be purified
purifyStateBody :: (Fresh m, MonadError AstError m, MonadIO m) =>
                     PureEnv -> [Exp] -> [Ty] -> Int -> [Ty] -> Exp -> m Exp
purifyStateBody rho stos stys i ms = classifyCases >=> \ case
      Get an _        -> do
            s <- liftMaybe an ("Purify: state-layer mismatch: length stos == " ++ show (length stos) ++ ", i == " ++ show i)
                  $ stos `atMay` i
            pure $ mkTuple $ s : stos

      Return _ _ e    -> pure $ mkTuple $ e : stos

      Lift _ e        -> purifyStateBody rho stos stys (i + 1) ms e

      Put _ e         -> pure $ mkTuple $ nil : replaceAtIndex (i - 1) e stos

      Apply an t n es -> do
            rator <- mkPureApp an rho (Var an t n) es
            pure $ mkApp rator stos

    -- e1 must be simply-typed, so don't purify it.
      SMatch an ty e1 mp e2 es Nothing -> do
            ty' <- rangeTy <$> purifyTy an ms ty
            e2' <- purifyStateBody rho stos stys i ms e2
            pure $ Match an ty' e1 mp e2' es Nothing

      SMatch an ty e1 mp e2 es (Just e3) -> do
            ty' <- rangeTy <$> purifyTy an ms ty
            e2' <- purifyStateBody rho stos stys i ms e2
            e3' <- purifyStateBody rho stos stys i ms e3
            pure $ Match an ty' e1 mp e2' es (Just e3')

      Bind an t e g -> do
            te         <- fst <$> dstArrow t
            a          <- liftMaybe (ann te) "Invalid type in bind" $ dstCompR te -- a is the return type of e
            e'         <- purifyStateBody rho stos stys i ms e
            ns         <- freshVars "st" $ a : stys
            let vs      = map (mkVar an) ns
            g_pure_app <- mkPureApp an rho g vs
            let p       = mkTuplePat ns
            -- g can, in general, be an application. That's causing the error when (var2Name g) is called.
            pure $ mkLet an p e' g_pure_app

---------------------------
-- Purifying Resumption Monadic definitions
---------------------------

-- It may be that the type for each subexpression occurring in here should be
-- part of the annotation here. That is partially accomplished right now.
data RCase an t p e = RReturn an e
                    | RLift an e
                    | RVar an t (Name e)
                    | Signal an e
                    | RBind an e e t
                    | SigK an t e (Name e) [(e, t)] -- (signal e >>= g e1 ... ek)
                    | Extrude an t (Name e) [e]     -- [(e, t)]
                    | RApp an t (Name e) [(e, t)]
                    | RMatch an t e MatchPat e [e] (Maybe e)

instance Show (RCase an t p e) where
      show RReturn {} = "RReturn"
      show RLift   {} = "RLift"
      show RVar    {} = "RVar"
      show Signal  {} = "Signal"
      show RBind   {} = "RBind"
      show SigK    {} = "SigK"
      show Extrude {} = "Extrude"
      show RApp    {} = "RApp"
      show RMatch  {} = "RMatch"

-- classifyApp classifies syntactic applications. Just seemed easier to
-- make it its own function.
classifyApp :: MonadError AstError m => Annote -> (Name Exp, Ty, [Exp]) -> m (RCase Annote Ty p Exp)
classifyApp an (x, t, [])  = pure $ RVar an t x
classifyApp an (x, t, [e])
      | n2s x == "return" = pure $ RReturn an e
      | n2s x == "lift"   = pure $ RLift an e
      | n2s x == "signal" = pure $ Signal an e
      | otherwise         = RApp an t x <$> mkBindings an t [e]
classifyApp an (x, t, [e, g])
      | n2s x == ">>="     = case isSignal e of
            Just s -> do
                  (g', t, es) <- dstApp g
                  bes         <- mkBindings an t es
                  pure $ SigK an t s g' bes
            Nothing -> do
                  tau <- snd <$> dstArrow t
                  RBind an e g . fst <$> dstArrow tau
      | n2s x == "extrude" = pure $ Extrude an t x [e, g]
      | otherwise          = RApp an t x <$> mkBindings an t [e, g]
      where isSignal :: Exp -> Maybe Exp
            isSignal = \ case
                  App _ (Var _ _ (n2s -> "signal")) e -> pure e
                  _                                   -> Nothing
classifyApp an (x, t, es)
      | n2s x == "extrude" = pure $ Extrude an t x es
      | otherwise          = do
            bes <- mkBindings an t es
            pure $ RApp an t x bes


classifyRCases :: (Fresh m, MonadError AstError m) => Exp -> m (RCase Annote Ty Pat Exp)
classifyRCases = \ case
     e@(App an _ _)            -> dstApp e >>= classifyApp an
     Match an t e1 mp e2 es me -> pure $ RMatch an t e1 mp e2 es me
     Var an t x                -> pure $ RVar an t x
     d                         -> failAt (ann d) $ "Unclassifiable R-case: " ++ show d

-- state for res-purification.
data PSto = PSto [DataCon] [(Pat, Exp)] (Name Exp) (Map Ty (Name DataConId))

addClause :: MonadState PSto m => DataCon -> m ()
addClause dc   = modify (\ (PSto dcs pes iv as) -> PSto (dc : dcs) pes iv as)

addEquation :: MonadState PSto m => (Pat, Exp) -> m ()
addEquation pe = modify (\ (PSto dcs pes iv as) -> PSto dcs (pe : pes) iv as)

getI :: MonadState PSto m => m (Name Exp)
getI = get >>= (\ (PSto _ _ iv _) -> pure iv)

getACtor :: (Fresh m, MonadState PSto m) => String -> Ty -> m (Name DataConId)
getACtor s t = do
      PSto dcs pes iv as <- get
      case Map.lookup (unAnn t) as of
            Nothing -> do
                  c <- freshVar $ "A_" ++ s
                  put $ PSto dcs pes iv $ Map.insert (unAnn t) c as
                  pure c
            Just c -> pure c

-- | purifyResBody
--  rho    -- pure environment
--  i      -- input type
--  o      -- output type
--  t      -- return type of monad (I'm assuming it's pure)
--  stys   -- state types [S1, ..., Sm]
--  stos   -- input states [s1, ..., sm]
--  tm     -- term to be purified
purifyResBody :: (Fresh m, MonadError AstError m, MonadIO m) =>
                   PureEnv -> Ty -> Ty -> Ty -> [Exp] -> [Ty] -> Exp -> StateT PSto m Exp
purifyResBody rho i o a stos ms = classifyRCases >=> \ case
      -- purifyResBody (return e)         = "(Left (e, (s1, (..., sm))))"
      RReturn _ e -> mkLeft "RReturn" e stos

      -- purifyResBody (signal e
      --        >>= g e1 ... ek) = "Right ((e, (s1, (..., sm))), R_g e1 ... ek)"
      --           Side effect:
      --     * add the clause "R_g T1 ... Tk" to R
      --     (where Ti is the type of ei)
      --     * add the eqn
      --         dispatch ((R_g e1 ... ek), (s1, ..., sm)) i
      --      = g_pure e1 ... ek i s1 ... sm
      -- to defn of dispatch
      SigK an _ e g bes -> do
            let (es, ts) = unzip bes
            ts' <- mapM (purifyTy an ms) ts

            r_g <- fresh $ s2n $ "R_" ++ prettyPrint g
            addClause (mkRDataCon an r_g ts')      -- "R_g T1 ... Tk"

            (p, xs) <- mkRPat an ts' r_g           -- Pattern (R_g e1 ... ek)
            ns      <- freshVars "s" ms
            let pairpat = if null ns then p else mkPairPat p (mkTuplePat ns)
            -- ^ Pattern (R_g e1 ... ek, (s1, ..., sn))

            let svars = map (\ (x, t) -> Var an t x) ns  -- [s1, ..., sn]
            let vars  = map (\ (x, t) -> Var an t x) (zip xs ts')   -- [x1, ..., xn]
            iv <- getI
            g_pure_app <- mkPureApp an rho (Var an a g) (vars ++ [Var an i iv] ++ svars)
            -- "dispatch (R_g e1 ... ek, (s1, ..., sn)) i = g_pure e1 ... ek i s1 ... sn"
            addEquation (pairpat, g_pure_app)

            let rGes    = mkApp (Con an (mkArrowTy ts' rTy) r_g) es
            let inner = case stos of
                  [] -> rGes                    --  R_g e1 ... ek :: R
                  _  -> mkPair rGes $ mkTuple stos
                                              -- ^ (R, (s1, ..., sm))
            let outer   = mkPair e inner      -- (e, (R_g e1 ... ek, (s1, (..., sm))))
            pure $ mkRight outer             -- Right ((e, R_g e1 ... ek), (s1, (..., sm)))

      -- purifyResBody (signal e)         = "(Right ((e, (s1, (..., sm))), R_ret))"
      Signal an e -> do
            let r_return     = s2n "R_return"
            let inner = case stos of
                  [] -> Con an rTy r_return
                  _  -> mkPair (Con an rTy r_return) $ mkTuple stos
            let outer   = mkPair e inner

            -- by default, must add the eqn: dispatch R_return i = Left i

            p_ret <- fst <$> mkRPat an [] r_return
            p_ret_ns <- freshVars "s" ms
            let p_ret' = if null p_ret_ns then p_ret else mkPairPat p_ret (mkTuplePat p_ret_ns)

            iv <- getI
            b_ret' <- mkLeft "Signal" (Var an i iv) $ map (uncurry $ flip $ Var an) p_ret_ns
            addClause $ mkRDataCon an r_return []
            addEquation (p_ret', b_ret')

            pure $ mkRight outer

      -- purifyResBody (e >>= g)          = "let
      --   -- N.B.: The irrefutable pattern here is
      --   -- sketchy, but it should be okay because
      --   -- of the restriction on occurrences of
      --   -- "signal"
      --               Left (v, (s1, (..., sm))) = [|purifyResBody e|]
      --             in
      --               g_pure v s1 ... sm"
      -- It appears to me that g can be an application (and not only a variable). In which case, I guess
      -- the right thing to do is just apply "v s1 ... sm" to g. And possibly add "_pure" to the function at the
      -- head of the application. The way it's written below assumes that g is a variable.
      RBind an e g tg -> do
            -- purify the types of e and g
            tg'         <- purifyTy an ms tg
            -- ert is the return type of e; ty is the type of the whole expression
            (ert, _)    <- dstArrow tg'
            e'          <- purifyResBody rho i o ert stos ms e

            -- start calculating pattern: p = (Left (v, (s1, (..., sm))))
            svars       <- freshVars "s" ms
            v           <- freshVar "v"
            p           <- mkLeftPat "RBind" (patVar (v, ert)) (map patVar svars)
            -- done calculating p = Left (v, (s1, (..., sm)))

            -- calculating g_pure_app = "g_pure v s1 ... sm"
            let vars     = map (\ (x, t) -> Var an t x) svars
            g_pure_app  <- mkPureApp an rho g (Var an ert v : vars)
            -- done calculating g_pure_app
            pure $ mkLet an p e' g_pure_app

          -- N.b., the pure env rho will have to contain *all* purified bindings
          --       or this will fail.
      RLift an e -> do
            e'    <- purifyStateBody rho stos ms 0 ms e -- was 1, which seems incorrect.
            s_i   <- freshVars "s" ms
            v     <- freshVar "v"
            -- the pattern "(v, (s1, (..., sm)))"
            let p       = mkTuplePat $ (v, a) : s_i
            body <- mkLeft "RLift" (Var an a v) stos

            pure $ mkLet an p e' body

      RVar an tx x   -> do
            tx' <- purifyTy an ms tx
            pure $ mkApp (Var an tx' x') stos
                  where x' = if isStart x then s2n "$Pure.start" else x

      -- extrude :: Monad m => ReT i o (StT s m) a -> s -> ReT i o m a
      --
      -- To purify e = (extrude ... (extrude phi s1) ... sn):
      -- 1. N.b., phi :: ReT i o (StT s m) a. We'll assume that e is "maximal". I.e.,
      --    that you have identified how many times extrude has been applied and its arguments s1, ..., sn.
      -- 2. phi' <- purifyResBody phi
      -- 3. make definition and addit to definitions: $LL = phi'
      -- 4. return $ (...($LL s1)...sn)
      Extrude an t rator rands -> case flattenExtr $ mkApp (Var an t rator) rands of
            (Var an t d, stos) -> do
                  t'  <- purifyTy an ms t
                  pure $ mkApp (Var an t' d) stos
            (e@App {}, stos)   -> do
                  (f, t, es) <- dstApp e
                  t'         <- purifyTy (ann e) ms t
                  pure $ mkApp (Var (ann e) t' f) $ es ++ stos
            (e, _)             -> failAt (ann e) $ "Extruded device is non-variable: " ++ show e

      RApp an rty rator rands -> do
            rator' <- purifyResBody rho i o a stos ms (Var an rty rator)
            -- N.b., don't think it's necessary to purify the rands because
            -- they're simply typed.
            f      <- mkPureApp an rho rator' (map fst rands)
            pure $ mkApp f stos

       -- e1 must be simply-typed, so don't purify it.
      RMatch an ty e1 mp e2 es Nothing   -> do
            ty' <- rangeTy <$> purifyTy an ms ty
            e2' <- purifyResBody rho i o a stos ms e2
            pure $ Match an ty' e1 mp e2' es Nothing

      RMatch an ty e1 mp e2 es (Just e3) -> do
            ty' <- rangeTy <$> purifyTy an ms ty
            e2' <- purifyResBody rho i o a stos ms e2
            e3' <- purifyResBody rho i o a stos ms e3
            pure $ Match an ty' e1 mp e2' es (Just e3')

      where mkLeft :: (Fresh m, MonadError AstError m) => String -> Exp -> [Exp] -> StateT PSto m Exp
            mkLeft s a stos = do
                  let t = typeOf a
                  c <- getACtor s t
                  let anode = App ( MsgAnnote "Purify: mkLeft")
                                  ( Con (MsgAnnote "Purify: mkLeft") (mkArrowTy [t] aTy) c)
                                  a
                  pure ( App ( MsgAnnote "Purify: mkLeft")
                             ( Con ( MsgAnnote "Purify: mkLeft")
                                   ( mkArrowTy [if null stos then aTy else mkPairTy aTy $ mkTupleTy ms] $ mkRangeTy o ms)
                                   ( s2n "Prelude.Left")
                             )
                             (if null stos then anode else mkPair anode (mkTuple stos))
                        )

            mkLeftPat :: (Fresh m, MonadError AstError m) => String -> Pat -> [Pat] -> StateT PSto m Pat
            mkLeftPat s a stos = do
                  let t = typeOf a
                  c <- getACtor s t
                  let anode =  PatCon (MsgAnnote "Purify: mkLeftPat") (Embed $ mkArrowTy [t] aTy) (Embed c) [a]
                  pure ( PatCon ( MsgAnnote "Purify: mkLeftPat")
                             ( Embed $ mkArrowTy [if null stos then aTy else mkPairTy aTy $ mkTupleTy ms] $ mkRangeTy o ms)
                             ( Embed $ s2n "Prelude.Left")
                             [ if null stos then anode else mkPairPat anode (mkTuplePat' stos)]
                       )

            mkRight :: Exp -> Exp
            mkRight e = App ( MsgAnnote "Purify: mkRight")
                            ( Con (MsgAnnote "Purify: mkRight") (mkArrowTy [typeOf e] $ mkRangeTy o ms) (s2n "Prelude.Right"))
                            e

---------------------------
-- A Compendium of Helper Functions
---------------------------

dispatchTy :: Ty -> Ty -> [Ty] -> Ty
dispatchTy i o ms = mkArrowTy [domTy, i] etor
    where etor  = mkRangeTy o ms
          domTy = if null ms then rTy else (mkPairTy rTy . mkTupleTy) ms

-- | Takes t1 and t2 and creates (t1, t2). I'm assuming (?!?) that the name of the
-- pairing constructor is "(,)". Correct me if I'm wrong.
mkPairTy :: Ty -> Ty -> Ty
mkPairTy t = TyApp (MsgAnnote "Purify: mkPairTy") (TyApp (MsgAnnote "Purify: mkPairTy") (TyCon (MsgAnnote "Purify: mkPairTy") $ s2n "(,)") t)

mkPair :: Exp -> Exp -> Exp
mkPair e1 e2 = App (MsgAnnote "Purify: mkPair") (App (MsgAnnote "Purify: mkPair") (Con (MsgAnnote "Purify: mkPair") t (s2n "(,)")) e1) e2
      where t = mkArrowTy [typeOf e1, typeOf e2] $ mkPairTy (typeOf e1) $ typeOf e2

-- global constant representation of R data type
rTy :: Ty
rTy = TyCon (MsgAnnote "Purify: rTy") (s2n "R_")

aTy :: Ty
aTy = TyCon (MsgAnnote "Purify: aTy") (s2n "A_")

nilTy :: Ty
nilTy = TyCon (MsgAnnote "Purify: nilTy") (s2n "()")

nil :: Exp
nil = Con (MsgAnnote "Purify: nil") nilTy (s2n "()")

nilPat :: Pat
nilPat = PatCon (MsgAnnote "Purify: nilPat") (Embed nilTy) (Embed $ s2n "()") []

mkTuple :: [Exp] -> Exp
mkTuple = \ case
      [] -> nil
      xs -> foldl1 mkPair xs

mkTupleTy :: [Ty] -> Ty
mkTupleTy = \ case
      [] -> nilTy
      ts -> foldl1 mkPairTy ts

mkTuplePat :: [(Name Exp, Ty)] -> Pat
mkTuplePat = mkTuplePat' . map patVar

mkTuplePat' :: [Pat] -> Pat
mkTuplePat' = \ case
      [] -> nilPat
      xs -> foldl1 mkPairPat xs

patVar :: (Name Exp, Ty) -> Pat
patVar (n, t) = PatVar (MsgAnnote "Purify: patVar") (Embed t) n

mkVar :: Annote -> (Name Exp, Ty) -> Exp
mkVar an (n, t) = Var an t n

mkRangeTy :: Ty -> [Ty] -> Ty
mkRangeTy o = \ case
      [] -> mkEitherTy aTy (mkPairTy o rTy)
      ts -> mkEitherTy (mkPairTy aTy $ mkTupleTy ts) (mkPairTy o $ mkPairTy rTy $ mkTupleTy ts)

-- | Takes [T1, ..., Tn-1] Tn and returns (T1 -> (T2 -> ... (T(n-1) -> Tn) ...))
mkArrowTy :: [Ty] -> Ty -> Ty
mkArrowTy ps = foldr1 arr . (ps ++) . (: [])

mkPairPat :: Pat -> Pat -> Pat
mkPairPat p1 p2 = PatCon (MsgAnnote "Purify: mkPairPat") (Embed $ mkPairTy (typeOf p1) (typeOf p2)) (Embed (s2n "(,)")) [p1, p2]

mkEitherTy :: Ty -> Ty -> Ty
mkEitherTy t1 = TyApp (MsgAnnote "Purify: mkEitherTy") (TyApp (MsgAnnote "Purify: mkEitherTy") (TyCon (MsgAnnote "Purify: mkEitherTy") $ s2n "Prelude.Either") t1)

mkRPat :: Fresh m => Annote -> [Ty] -> Name DataConId -> m (Pat, [Name Exp])
mkRPat an ts r_g = do
      xs <- freshVars "store" ts
      let varpats = map (\ (x, t) -> PatVar an (Embed t) x) xs
      pure (PatCon an (Embed $ TyCon an $ s2n "R_") (Embed r_g) varpats, map fst xs)

mkPureVar :: MonadError AstError m => Annote -> PureEnv -> Ty -> Name Exp -> m Exp
mkPureVar an rho t x = case n2s x of
      "extrude" -> pure $ Var an t x
      "unfold"  -> pure $ Var an t x
      _         -> Var an <$> lookupPure an x rho <*> pure x

mkApp :: Exp -> [Exp] -> Exp
mkApp = foldl' $ App (MsgAnnote "Purify: mkApp")

mkPureApp :: MonadError AstError m => Annote -> PureEnv -> Exp -> [Exp] -> m Exp
mkPureApp an rho e es = do
      (rator, t, rands) <- dstApp e
      ep                <- mkPureVar an rho t rator
      pure $ mkApp ep $ rands ++ es

dstApp :: MonadError AstError m => Exp -> m (Name Exp, Ty, [Exp])
dstApp = \ case
      App _ (Var _ t n) rand -> pure (n, t, [rand])
      App _ rator rand       -> do
            (n, t, es) <- dstApp rator
            pure (n, t, es ++ [rand])
      Var _ t n              -> pure (n, t, [])
      d                      -> failAt (ann d) "Tried to dst non-app"

-- > let p = e1 in e2
--     becomes
-- > case e1 of { p -> e2 }
-- N.b., use a Match here instead of a Case.
mkLet :: Annote -> Pat -> Exp -> Exp -> Exp
mkLet an p e1 e2 = Case an (typeOf e2) e1 (bind p e2) Nothing

mkBindings :: (Pretty t, MonadError AstError m) => Annote -> Ty -> [t] -> m [(t, Ty)]
mkBindings an t es = do
      let ps = paramTys t
      when (length ps < length es)
            $ failAt an
            $ "Purify: mkBindings: attempting to calculate argument types: "
           ++ "[" ++ intercalate ", " (map prettyPrint es) ++ "]"
           ++ " vs "
           ++ "[" ++ intercalate ", " (map prettyPrint ps) ++ "]"
      pure $ zip es ps
