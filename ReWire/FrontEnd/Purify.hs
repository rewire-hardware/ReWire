{-# LANGUAGE FlexibleContexts, LambdaCase, ViewPatterns, TupleSections #-}
{-# LANGUAGE Safe #-}

module ReWire.FrontEnd.Purify (purify) where

import ReWire.Annotation
import ReWire.FrontEnd.Unbound
      ( Fresh (..), s2n, n2s
      )
import ReWire.Error
import ReWire.FrontEnd.Syntax
import ReWire.Pretty

import Control.Arrow (first, second, (&&&))
import Control.Monad.State
import Control.Monad.Fail (MonadFail)
import Data.List (nubBy, foldl', find, intercalate)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)

atMay :: (Eq i, Num i) => [a] -> i -> Maybe a
atMay []       _ = Nothing
atMay (x : _)  0 = Just x
atMay (_ : xs) n = atMay xs $ n - 1

isPrim :: Name Exp -> Bool
isPrim = notElem '.' . n2s

isStart :: Name Exp -> Bool
isStart = (== "Main.start") . n2s

freshVar :: Fresh m => String -> m (Name a)
freshVar n = fresh (s2n $ "?X_" ++ n ++ "_")

freshVars :: Fresh m => String -> [b] -> m [(Name a, b)]
freshVars v = mapM (\ (i, b) -> (, b) <$> freshVar (v ++ show i)) . zip [0 :: Int ..]

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item : b)
      where (a, _ : b) = splitAt n ls

poly2Ty :: Fresh m => Poly -> m Ty
poly2Ty (Poly p) = snd <$> unbind p

purify :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) => FreeProgram -> m FreeProgram
purify (ts, ds) = do
      (smds, pds)         <- partitionEithers <$> mapM isStateMonadicDefn ds
      (rmds, ods)         <- partitionEithers <$> mapM isResMonadicDefn pds
      let (startd, rmds') =  partitionEithers  $  map  isStartDefn rmds
      let rmds = rmds' ++ startd -- TODO(chathhorn): kludge to make sure start is the last thing purified.

      let nameNpolyS = map (defnName &&& defnPolyTy) smds
          nameNpolyR = map (defnName &&& defnPolyTy) rmds

      imprhoR       <- mapM (\ (d, Embed p) -> (d, ) <$> poly2Ty p) nameNpolyR

      (_, tyStart)  <- liftMaybe noAnn "No start symbol!"
                     $ find (isStart . fst) imprhoR
      (i, o, _, t) <- liftMaybe (ann tyStart) ("dstReT applied to: " ++ show tyStart)
                     $ dstReT $ rangeTy tyStart

      -- if t = (a, s), need to set t := a and stos := s -- TODO(chathhorn): this is a huge kludge
      -- let t' :| ms = dstTupleTy t

      rhoS      <- mkPureEnv nameNpolyS
      rhoR      <- mkPureEnv nameNpolyR
      let rho = rhoS ++ rhoR

      pure_smds <- mapM (purifyStateDefn rho) smds
      iv        <- freshVar "i"

      -- TODO(chathhorn): mechanism for retrieving ms is really kludgy
      -- TODO(chathhorn): rmds and imprhoR seem to contain duplicate information
      (pure_rmds, PSto dcs pes _ ms) <- runStateT (mapM (purifyResDefn rho imprhoR iv) rmds) $ PSto [] [] [] []

      -- TODO(chathhorn): entirely to remove duplicate R_return.
      let dcs' = nubBy deq dcs
          pes' = nubBy peq pes
          deq (DataCon _ d1 _) (DataCon _ d2 _) = n2s d1 == n2s d2
          peq p1 p2 = patStr (fst p1) == patStr (fst p2)
          patStr (PatCon _ _ (Embed n) _) = n2s n
          patStr (PatVar _ _ n) = n2s n
      -- ^ TODO(chathhorn) hackity hack

      let ty   = dispatchTy i o t ms

      disp <- mkDispatch ty pes' iv

      tunfold <- getUnfoldTy ds

      pure ( ts ++ [mkRDatatype dcs']
           , mkStart i o tunfold t ms
           : disp
           : ods ++ pure_smds ++ pure_rmds)

      -- TODO(chathhorn): what if I retype extrude and unfold (et al.?) with concrete types and re-run type inference?
      where getUnfoldTy :: (Fresh m, MonadError AstError m) => [Defn] -> m Ty
            getUnfoldTy ds = case filter isUnfold ds of
                  [Defn { defnPolyTy = Embed t }] -> poly2Ty t
                  _                               -> failAt (MsgAnnote "Purify: getUnfoldTy") "Definition of unfold primitive not found"

            isUnfold :: Defn -> Bool
            isUnfold Defn { defnName = n } = n2s n == "unfold"

-- In addition to all the above, we re-tie the recursive knot by adding a new
--   "start" as follows
--         start :: ReT In Out I T
--         start = unfold dispatch start_pure
--           start :: In -> (Either T (O, R), ()) --- drop the "()"?

-- Correcting mkStart.
-- In the type of Main.start.
-- t (return type of Main.start) needs to be combined with ms (list of StT s, stores).
mkStart :: Ty -> Ty -> Ty -> Ty -> [Ty] -> Defn
mkStart i o tunfold t ms = Defn
      { defnAnnote = MsgAnnote "start function"
      , defnName   = s2n "Main.start"
      , defnPolyTy = [] |-> tyStart
      , defnInline = False
      , defnBody   = appl
      }
      where etor       = mkRangeTy t o ms
            extresTy :: Ty -> [Ty] -> Ty
            extresTy = foldl' mkPairTy

            reT i o a  = TyCon (MsgAnnote "Purify: reT") (s2n "ReT") `tyApp` i `tyApp` o `tyApp` a
            tyStart    = tycomp (MsgAnnote "Purify: tyStart")
                              (reT i o (TyCon (MsgAnnote "Purify: tyStart") (s2n "I")))
                              (extresTy t ms)

            unfold     = Var (MsgAnnote "Purify: unfold") tunfold (s2n "unfold")
            dispatch   = Var (MsgAnnote "Purify: dispatch") (dispatchTy i o t ms) (s2n "$Pure.dispatch")
            start_pure = Var (MsgAnnote "Purify: $Pure.start") etor (s2n "$Pure.start")
            appl       = Embed $ bind [] (App (MsgAnnote "Purify: appl") (App (MsgAnnote "Purify: appl") unfold dispatch) start_pure)

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
mkDispatch :: (MonadError AstError m, Fresh m) => Ty -> [(Pat, Exp)] -> Name Exp -> m Defn
mkDispatch _ [] _ = failAt NoAnnote "Empty dispatch: invalid ReWire (is recursion guarded by signal?)"
mkDispatch ty (p : pes) iv = do
      dsc     <- freshVar "dsc"
      let body  = Embed $ bind [dsc :: Name Exp, iv :: Name Exp] cases
          cases = mkCase an (Var an rTy dsc) p pes
          an    = MsgAnnote $ "Generated dispatch function: " ++ prettyPrint cases

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
       { dataAnnote = MsgAnnote "R Datatype"
       , dataName   = s2n "R"
       , dataKind   = KStar
       , dataCons   = dcs
       }

type PureEnv = [(Name Exp, Ty)]

mkPureEnv :: (Fresh m, MonadError AstError m) => [(Name Exp, Embed Poly)] -> m PureEnv
mkPureEnv []                     = pure []
mkPureEnv ((n, Embed phi) : nps) = do
      ty     <- poly2Ty phi
      let ms  = getStates ty
      purety <- purifyTy (ann ty) ms ty
      env    <- mkPureEnv nps
      pure $ (n, purety) : env

getStates :: Ty -> [Ty]
getStates t =
      let r               = rangeTy t
          sts             = fromMaybe [] (dstCompM r >>= dstStT)
      in case dstReT r of
          Just (_, _, sts', _) -> sts'
          _                    -> sts

lookupPure :: MonadError AstError m => Annote -> Name Exp -> PureEnv -> m Ty
lookupPure an x rho = case lookup x rho of
      Just ty -> pure ty
      Nothing -> failAt an $ "No pure binding for variable: " ++ n2s x

lookupImpure :: MonadError AstError m => Annote -> Name Exp -> PureEnv -> m Ty
lookupImpure an x rho = case lookup x rho of
      Just ty -> pure ty
      Nothing -> failAt an $ "No impure binding for variable: " ++ n2s x

isStateMonadicDefn :: Fresh m => Defn -> m (Either Defn Defn)
isStateMonadicDefn d
      | isPrim (defnName d) = pure $ Right d
      | otherwise           = do
            ty <- poly2Ty poly
            pure $ if isStateMonad ty then Left d else Right d
      where Embed poly = defnPolyTy d

            isStateMonad :: Ty -> Bool
            isStateMonad ty = case rangeTy ty of
                  TyApp an (TyApp _ (TyApp _ (TyCon _ (n2s -> "StT")) _) m) a -> isStateMonad (tycomp an m a)
                  TyApp _ (TyCon _ (n2s -> "I")) _                            -> True
                  _                                                           -> False

isResMonadicDefn :: Fresh m => Defn -> m (Either Defn Defn)
isResMonadicDefn d
      | isPrim (defnName d) = pure $ Right d
      | otherwise           = do
            ty <- poly2Ty poly
            pure $ if isResMonad ty then Left d else Right d
      where Embed poly = defnPolyTy d

            isResMonad :: Ty -> Bool
            isResMonad ty = case rangeTy ty of
                  TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReT")) _) _) _) _ -> True
                  _                                                                      -> False

isStartDefn :: Defn -> Either Defn Defn
isStartDefn = \ case
      d | isStart (defnName d) -> Left d
      d                        -> Right d

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
      let stos      = foldr (\ (n, t) nts -> Var (ann d) t n : nts) [] nstos
      e'           <- purifyStateBody rho stos stys 0 ms e
      let b_pure    = bind (args ++ map fst nstos) e'
      pure $ d { defnName = d_pure, defnPolyTy = [] |-> p_pure, defnBody = Embed b_pure }
      where
            Embed body = defnBody d
            Embed phi  = defnPolyTy d

liftMaybe :: MonadError AstError m => Annote -> String -> Maybe a -> m a
liftMaybe an msg = maybe (failAt an msg) pure

purifyResDefn :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) =>
                 PureEnv -> [(Name Exp, Ty)] -> Name Exp -> Defn -> StateT PSto m Defn
purifyResDefn rho imprhoR iv d = do
      ty           <- lookupImpure an dname imprhoR
      ms           <- addStates $ getStates ty
      pure_ty      <- purifyTy (ann d) ms ty
      (args, e)    <- unbind body

      (i, o, _, a) <- liftMaybe (ann d) "failed at purifyResDefn" $ dstReT $ rangeTy ty

      (nstos, mst) <- do
            nstos   <- freshVars "sto" ms
            let stos = map (\ (n, t) -> Var an t n) nstos
            pure (map fst nstos, (stos, ms))
      e'           <- purifyResBody rho i o a iv mst e

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
                                    $ purifyResTy ms t
      StTApp        -> liftMaybe an ( "Purify: failed to purify StT type: " ++ prettyPrint t)
                                    $ purifyStTTy t
      PairApp t1 t2 -> TyApp an <$> (TyApp an (TyCon an $ s2n "(,)") <$> purifyTy an ms t1) <*> purifyTy an ms t2
      Pure          -> pure t
      _             -> failAt an $ "Purify: failed to purify type: " ++ prettyPrint t

            -- This takes a Ty of the form
            --      T1 -> T2 -> ... -> Tn -> ReT In Out (StT S1 (StT S2 (... (StT Sm I)))) T
            -- and returns a Ty of the form
            --      T1 -> T2 -> ... -> Tn -> In -> S1 -> ... -> Sm -> (Either T (O, R), (S1, (..., Sm)))
      where purifyResTy :: [Ty] -> Ty -> Maybe Ty
            purifyResTy ms t = do
                  (_, o, _, a) <- dstReT $ rangeTy t
                  pure $ mkArrowTy (paramTys t ++ ms) $ mkRangeTy a o ms

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
      Case an t dsc bnds me     -> do
            (p, e) <- unbind bnds
            pure $ Switch an t dsc p e me
      Match an t e1 mp e2 es me -> pure $ SMatch an t e1 mp e2 es me
      d                         -> failAt (ann d) $ "Unclassifiable case: " ++ show d

data Cases an p e t = Get an t
                    | Return an t e
                    | Lift an e
                    | Put an e
                    | Bind an t e e
                    | Apply an t (Name Exp) [e]
                    | Switch an t e p e (Maybe e)
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

      Switch an t e1 p e2 (Just e) -> do
            e1' <- purifyStateBody rho stos stys i ms e1
            e2' <- purifyStateBody rho stos stys i ms e2
            e'  <- purifyStateBody rho stos stys i ms e
            pure $ Case an t e1' (bind p e2') (Just e')

      Switch an t e1 p e2 Nothing  -> do
            e1' <- purifyStateBody rho stos stys i ms e1
            e2' <- purifyStateBody rho stos stys i ms e2
            pure $ Case an t e1' (bind p e2') Nothing

    -- e1 must be simply-typed, so don't purify it.
      SMatch an ty e1 mp e2 es Nothing -> do
            ty' <- purifyTy an ms ty
            e2' <- purifyStateBody rho stos stys i ms e2
            pure $ Match an ty' e1 mp e2' es Nothing

      SMatch an ty e1 mp e2 es (Just e3) -> do
            ty' <- purifyTy an ms ty
            e2' <- purifyStateBody rho stos stys i ms e2
            e3' <- purifyStateBody rho stos stys i ms e3
            pure $ Match an ty' e1 mp e2' es (Just e3')

      Bind an t e g -> do
            (te, _)    <- dstArrow t
            a          <- liftMaybe (ann te) "Invalid type in bind" $ dstCompR te -- a is the return type of e
            e'         <- purifyStateBody rho stos stys i ms e
            ns         <- freshVars "st" $ a : stys
            let vs      = foldr (\ (n, t) nts -> Var an t n : nts) [] ns
            g_pure_app <- mkPureApp an rho g vs
            let p       = mkTuplePat ns
            -- g can, in general, be an application. That's causing the error when (var2Name g) is called.
            pure $ mkLet an p e' g_pure_app

---------------------------
-- Purifying Resumption Monadic definitions
---------------------------

-- It may be that the type for each subexpression occurring in here should be
-- part of the annotation here. That is partially accomplished right now.
data RCase an t p e = RReturn an e t
                    | RLift an e
                    | RVar an t (Name e)
                    | Signal an e
                    | RBind an e e t
                    | SigK an t e (Name e) [(e, t)] -- (signal e >>= g e1 ... ek)
                    | Extrude an t (Name e) [e]     -- [(e, t)]
                    | RApp an t (Name e) [(e, t)]
                    | RSwitch an t e p e (Maybe e)
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
      show RSwitch {} = "RSwitch"
      show RMatch  {} = "RMatch"

-- classifyApp classifies syntactic applications. Just seemed easier to
-- make it its own function.
classifyApp :: MonadError AstError m => Annote -> (Name Exp, Ty, [Exp]) -> m (RCase Annote Ty p Exp)
classifyApp an (x, t, [])  = pure $ RVar an t x
classifyApp an (x, t, [e])
      | n2s x == "return" = do
            (te, _) <- dstArrow t
            pure $ RReturn an e te
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
                  (_, tau) <- dstArrow t
                  (tg, _)  <- dstArrow tau
                  pure $ RBind an e g tg
      | n2s x == "extrude" = pure $ Extrude an t x [e, g]
      | otherwise          = do
            bes <- mkBindings an t [e, g]
            pure $ RApp an t x bes
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
     Case an t dsc bnds me     -> do (p, e) <- unbind bnds
                                     pure $ RSwitch an t dsc p e me
     Match an t e1 mp e2 es me -> pure $ RMatch an t e1 mp e2 es me
     Var an t x                -> pure $ RVar an t x
     d                         -> failAt (ann d) $ "Unclassifiable R-case: " ++ show d

-- state for res-purification.
data PSto = PSto [DataCon] [(Pat, Exp)] [Defn] [Ty]

addClause :: MonadState PSto m => DataCon -> m ()
addClause dc   = modify (\ (PSto dcs pes defs sts) -> PSto (dc : dcs) pes defs sts)

addEquation :: MonadState PSto m => (Pat, Exp) -> m ()
addEquation pe = modify (\ (PSto dcs pes defs sts) -> PSto dcs (pe : pes) defs sts)

addStates :: MonadState PSto m => [Ty] -> m [Ty]
addStates sts' = do
      modify $ \ case
            PSto dcs pes defs sts | length sts' > length sts -> PSto dcs pes defs sts'
            sto                                              -> sto
      PSto _ _ _ ms <- get
      pure ms

-- | purifyResBody
--  rho    -- pure environment
--  i      -- input type
--  o      -- output type
--  t      -- return type of monad (I'm assuming it's pure)
--  stys   -- state types [S1, ..., Sm]
--  stos   -- input states [s1, ..., sm]
--  iv     -- fresh var to use for input type
--  tm     -- term to be purified
purifyResBody :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) =>
                   PureEnv -> Ty -> Ty -> Ty -> Name Exp -> ([Exp], [Ty]) -> Exp -> StateT PSto m Exp
purifyResBody rho i o t iv mst = classifyRCases >=> \ case
      -- purifyResBody (return e)         = "(Left (e, (s1, (..., sm))))"
      RReturn an e te       -> do
            te' <- purifyTy an (snd mst) te
            let etor = mkRangeTy te' o (snd mst)
            let tup  = mkTuple $ e : fst mst
            pure $ mkLeft (mkArrowTy [typeOf tup] etor) tup

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
            ts' <- mapM (purifyTy an $ snd mst) ts

            r_g <- fresh $ s2n $ "R_" ++ prettyPrint g
            addClause (mkRDataCon an r_g ts')      -- "R_g T1 ... Tk"

            (p, xs) <- mkRPat an ts' r_g           -- Pattern (R_g e1 ... ek)
            ns      <- freshVars "s" $ snd mst
            let pairpat = if null ns then p else mkPairPat p (mkTuplePat ns)
            -- ^ Pattern (R_g e1 ... ek, (s1, ..., sn))

            let svars = map (\ (x, t) -> Var an t x) ns  -- [s1, ..., sn]
            let vars  = map (\ (x, t) -> Var an t x) (zip xs ts')   -- [x1, ..., xn]
            g_pure_app <- mkPureApp an rho (Var an t g) (vars ++ [Var an i iv] ++ svars)
            -- "dispatch (R_g e1 ... ek, (s1, ..., sn)) i = g_pure e1 ... ek i s1 ... sn"
            addEquation (pairpat, g_pure_app)

            let rGes    = mkApp (Con an (mkArrowTy ts' rTy) r_g) es
            let inner = case fst mst of
                  []   -> rGes                    --  R_g e1 ... ek :: R
                  stos -> mkPair rGes $ mkTuple stos
                                              -- ^ (R, (s1, ..., sm))
            let outer   = mkPair e inner      -- (e, (R_g e1 ... ek, (s1, (..., sm))))
            let etor    = mkRangeTy t o (snd mst)
            pure $ mkRight (mkArrowTy [typeOf outer] etor) outer             -- Right ((e, R_g e1 ... ek), (s1, (..., sm)))

      -- purifyResBody (signal e)         = "(Right ((e, (s1, (..., sm))), R_ret))"
      Signal an e -> do
            let r_return     = s2n "R_return"
            let inner = case fst mst of
                  []   -> Con an rTy r_return
                  stos -> mkPair (Con an rTy r_return) $ mkTuple stos
            let outer   = mkPair e inner
            let etor    = mkRangeTy t o (snd mst)

            -- by default, must add the eqn: dispatch R_return i = Left i
            let ty   = dispatchTy i o t $ snd mst

            p_ret <- fst <$> mkRPat an [] r_return
            p_ret_ns <- freshVars "s" $ snd mst
            let p_ret' = if null p_ret_ns then p_ret else mkPairPat p_ret (mkTuplePat p_ret_ns)

            let var_i = Var an i iv
            let b_ret = if null p_ret_ns then var_i else mkPair var_i (mkTuple $ map (uncurry $ flip $ Var an) p_ret_ns)
            let b_ret' = mkLeft (mkArrowTy [typeOf b_ret] $ rangeTy ty) b_ret
            addClause $ mkRDataCon an r_return []
            addEquation (p_ret', b_ret')

            pure $ mkRight (mkArrowTy [typeOf outer] etor) outer

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
            tg'         <- purifyTy an (snd mst) tg
            -- ert is the return type of e; ty is the type of the whole expression
            (ert, _)    <- dstArrow tg'
            e'          <- purifyResBody rho i o ert iv mst e

            -- start calculating pattern: p = (Left (v, (s1, (..., sm))))
            let etor     = mkRangeTy ert o (snd mst)
            svars       <- freshVars "state" $ snd mst
            v           <- freshVar "v"
            let stps     = mkTuplePat $ (v, ert) : svars
            let p        = PatCon an (Embed etor) (Embed $ s2n "Prelude.Left") [stps]
            -- done calculating p = Left (v, (s1, (..., sm)))

            -- calculating g_pure_app = "g_pure v s1 ... sm"
            let vars     = map (\ (x, t) -> Var an t x) svars
            g_pure_app  <- mkPureApp an rho g (Var an ert v : vars)
            -- done calculating g_pure_app
            pure $ mkLet an p e' g_pure_app

          -- N.b., the pure env rho will have to contain *all* purified bindings
          --       or this will fail.
      RLift an e -> do
            e'    <- uncurry (purifyStateBody rho) mst 0 (snd mst) e -- was 1, which seems incorrect.
            s_i   <- freshVars "State" $ snd mst
            v     <- freshVar "var"
            -- the pattern "(v, (s1, (..., sm)))"
            let p       = mkTuplePat $ (v, t) : s_i
                etor    = mkRangeTy t o (snd mst)
                tup     = mkTuple $ Var an t v : fst mst
                body    = mkLeft (mkArrowTy [typeOf tup] etor) tup

            pure $ mkLet an p e' body

      RVar an tx x   -> do
            tx' <- purifyTy an (snd mst) tx
            pure $ mkApp (Var an tx' x') (fst mst)
                  where x' = if isStart x then s2n "$Pure.start" else x

      -- extrude :: Monad m => ReT i o (StT s m) a -> s -> ReT i o m (a, s)
      --
      -- To purify e = (extrude ... (extrude phi s1) ... sn):
      -- 1. N.b., phi :: ReT i o (StT s m) a. We'll assume that e is "maximal". I.e.,
      --    that you have identified how many times extrude has been applied and its arguments s1, ..., sn.
      -- 2. phi' <- purifyResBody phi
      -- 3. make definition and addit to definitions: $LL = phi'
      -- 4. return $ (...($LL s1)...sn)
      Extrude an t rator rands -> case flattenExtr $ mkApp (Var an t rator) rands of
            (Var an t d, stos) -> do
                  t'  <- purifyTy an (snd mst) t
                  pure $ mkApp (Var an t' d) stos
            (e@App {}, stos)   -> do
                  (f, t, es) <- dstApp e
                  t'         <- purifyTy (ann e) (snd mst) t
                  pure $ mkApp (Var (ann e) t' f) $ es ++ stos
            (e, _)             -> failAt (ann e) $ "Extruded device is non-variable: " ++ show e

      RApp an rty rator rands -> do
            rator' <- purifyResBody rho i o t iv mst (Var an rty rator)
            -- N.b., don't think it's necessary to purify the rands because
            -- they're simply typed.
            f      <- mkPureApp an rho rator' (map fst rands)
            pure $ mkApp f $ fst mst

      RSwitch an ty dsc p e1 Nothing     -> do
            ty' <- purifyTy an (snd mst) ty
            e1' <- purifyResBody rho i o t iv mst e1
            pure $ Case an ty' dsc (bind p e1') Nothing

      RSwitch an ty dsc p e1 (Just e2)   -> do
            ty' <- purifyTy an (snd mst) ty
            e1' <- purifyResBody rho i o t iv mst e1
            e2' <- purifyResBody rho i o t iv mst e2
            pure $ Case an ty' dsc (bind p e1') (Just e2')

       -- e1 must be simply-typed, so don't purify it.
      RMatch an ty e1 mp e2 es Nothing   -> do
            ty' <- purifyTy an (snd mst) ty
            e2' <- purifyResBody rho i o t iv mst e2
            pure $ Match an ty' e1 mp e2' es Nothing

      RMatch an ty e1 mp e2 es (Just e3) -> do
            ty' <- purifyTy an (snd mst) ty
            e2' <- purifyResBody rho i o t iv mst e2
            e3' <- purifyResBody rho i o t iv mst e3
            pure $ Match an ty' e1 mp e2' es (Just e3')

---------------------------
-- A Compendium of Helper Functions
---------------------------

dispatchTy :: Ty -> Ty -> Ty -> [Ty] -> Ty
dispatchTy i o t ms = mkArrowTy [domTy, i] etor
    where etor  = mkRangeTy t o ms
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
rTy = TyCon (MsgAnnote "Purify: rTy") (s2n "R")

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
mkTuplePat = \ case
      [] -> nilPat
      xs -> foldl1 mkPairPat $ map patVar xs
      where patVar :: (Name Exp, Ty) -> Pat
            patVar (n, t) = PatVar (MsgAnnote "Purify: mkTuplePat") (Embed t) n

mkRangeTy :: Ty -> Ty -> [Ty] -> Ty
mkRangeTy a o = \ case
      [] -> mkEitherTy a (mkPairTy o rTy)
      ts -> mkEitherTy (mkPairTy a $ mkTupleTy ts) (mkPairTy o $ mkPairTy rTy $ mkTupleTy ts)

-- | Takes [T1, ..., Tn-1] Tn and returns (T1 -> (T2 -> ... (T(n-1) -> Tn) ...))
mkArrowTy :: [Ty] -> Ty -> Ty
mkArrowTy ps = foldr1 arr . (ps ++) . (: [])

mkPairPat :: Pat -> Pat -> Pat
mkPairPat p1 p2 = PatCon (MsgAnnote "Purify: mkPairPat") (Embed $ mkPairTy (typeOf p1) (typeOf p2)) (Embed (s2n "(,)")) [p1, p2]

-- Shouldn't mkLeft/mkRight change the type t to an Either?
mkLeft :: Ty -> Exp -> Exp
mkLeft t = App (MsgAnnote "Purify: mkLeft") (Con (MsgAnnote "Purify: mkLeft") t (s2n "Prelude.Left"))

mkRight :: Ty -> Exp -> Exp
mkRight t = App (MsgAnnote "Purify: mkRight") (Con (MsgAnnote "Purify: mkRight") t (s2n "Prelude.Right"))

mkEitherTy :: Ty -> Ty -> Ty
mkEitherTy t1 = TyApp (MsgAnnote "Purify: mkEitherTy") (TyApp (MsgAnnote "Purify: mkEitherTy") (TyCon (MsgAnnote "Purify: mkEitherTy") $ s2n "Prelude.Either") t1)

mkRDataCon :: Annote -> Name DataConId -> [Ty] -> DataCon
mkRDataCon an r_g ts = DataCon an r_g ([] |-> mkArrowTy ts rTy)

mkRPat :: Fresh m => Annote -> [Ty] -> Name DataConId -> m (Pat, [Name Exp])
mkRPat an ts r_g = do
      xs <- freshVars "store" ts
      let varpats = map (\ (x, t) -> PatVar an (Embed t) x) xs
      pure (PatCon an (Embed $ TyCon an $ s2n "R") (Embed r_g) varpats, map fst xs)

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
