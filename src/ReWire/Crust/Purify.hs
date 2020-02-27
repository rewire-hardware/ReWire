{-# LANGUAGE FlexibleContexts, LambdaCase, ViewPatterns, TupleSections #-}
{-# LANGUAGE Safe #-}

module ReWire.Crust.Purify (purify) where

import ReWire.Annotation
import ReWire.Unbound (Fresh (..), s2n, n2s)
import ReWire.Error
import ReWire.Crust.Syntax
import ReWire.Pretty

import Control.Arrow (first, second, (&&&))
import Control.Monad.State
import Control.Monad.Fail (MonadFail)
import Data.List (foldl', isSuffixOf)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Bool (bool)

import Data.Set (Set, singleton, insert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

atMay :: (Eq i, Num i) => [a] -> i -> Maybe a
atMay []       _ = Nothing
atMay (x : _)  0 = Just x
atMay (_ : xs) n = atMay xs $ n - 1

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

-- TODO(chathhorn): what if we retype extrude and unfold (et al.?) with
-- concrete types and re-run type inference? Much of the muck here is just
-- calculating types that could be inferred.

-- | Transforms functions in state and resumption monads into plain first-order functions.
purify :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) => FreeProgram -> m FreeProgram
purify (ts, ds) = do
      (smds, pds)         <- partitionEithers <$> mapM isStateMonadicDefn ds
      (rmds, ods)         <- partitionEithers <$> mapM isResMonadicDefn pds

      unless (any (isStart . defnName) rmds) $ failAt noAnn "No definition for Main.start found!"

      let nameNpolyS = map (defnName &&& defnPolyTy) smds
          nameNpolyR = map (defnName &&& defnPolyTy) rmds

      -- TODO(chathhorn): perhaps move to a type-checking phase?
      (i, o, ms, _) <- getCanonReTy rmds >>= liftMaybe noAnn "Inconsistent ReT types (i, o, and state layers must be the same for all ReT)."

      rhoS      <- mkPureEnv ms nameNpolyS
      rhoR      <- mkPureEnv ms nameNpolyR
      let rho = rhoS ++ rhoR

      pure_smds <- mapM (purifyStateDefn rho ms) smds

      iv <- freshVar "i"

      (pure_rmds, PSto _ dcs pes _ allAs) <- runStateT (mapM (purifyResDefn rho ms) rmds) $ PSto False [] [] iv mempty

      disp <- mkDispatch i o ms iv pes

      pure ( filter (not . isAorR) ts ++ [mkRDatatype dcs] ++ [mkADatatype $ Map.toList allAs]
           , mkStart i o ms
           : disp
           : ods ++ pure_smds ++ pure_rmds)

      where getCanonReTy :: Fresh m => [Defn] -> m (Maybe (Ty, Ty, [Ty], Set Ty))
            getCanonReTy = (getCanonReTy' . catMaybes <$>) . mapM (projDefnTy >=> pure . dstReT)

            getCanonReTy' :: [(Ty, Ty, [Ty], Ty)] -> Maybe (Ty, Ty, [Ty], Set Ty)
            getCanonReTy' = \ case
                  []                  -> Nothing
                  (i, o, ms, a) : rs  -> foldr mergeReTys (Just (i, o, ms, singleton a)) rs

            mergeReTys :: (Ty, Ty, [Ty], Ty) -> Maybe (Ty, Ty, [Ty], Set Ty) -> Maybe (Ty, Ty, [Ty], Set Ty)
            mergeReTys (i, o, ms, a) = \ case
                  Just (i', o', ms', a') | length ms >= length ms' && isSuffixOf ms' ms
                                                && i == i' && o == o' -> Just (i, o, ms, insert a a')
                                         | length ms < length ms'  && isSuffixOf ms ms'
                                                && i == i' && o == o' -> Just (i, o, ms', insert a a')
                  _                                                   -> Nothing

            isAorR :: DataDefn -> Bool
            isAorR = uncurry (||) . ((== "A_") &&& (== "R_")) . n2s . dataName

-- | In addition to all the above, we re-tie the recursive knot by adding a new
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
            resultTy   = mkTupleTy $ aTy : ms

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

-- | Converts
-- >  (extrude (extrude (... (extrude dev s1) ...) s(n-1)) sn)
-- into
-- >  (dev, [s1, ..., sn])
-- Won't work if called on a non-extrude application.
flattenExtr :: Exp -> (Exp, [Exp])
flattenExtr = \ case
      App _ (App _ (Var _ _ (n2s -> "extrude")) arg1) arg2 -> second (++ [arg2]) $ flattenExtr arg1
      e@(App _ _ rand)                                     -> first (const e) $ flattenExtr rand
      e                                                    -> (e, [])

-- | Generates the dispatch function.
-- > dispatch (R_g e1 ... ek) i = g_pure e1 ... ek i
mkDispatch :: (MonadError AstError m, Fresh m) => Ty -> Ty -> [Ty] -> Name Exp -> [(Pat, Exp)] -> m Defn
mkDispatch _ _ _  _  [] = failAt NoAnnote "Purify: empty dispatch: invalid ReWire (is recursion guarded by signal?)"
mkDispatch i o ms iv (p : pes) = do
      let ty    = dispatchTy i o ms
          domTy = mkTupleTy $ rTy : ms
      dsc     <- freshVar "dsc"
      let body  = Embed $ bind [dsc :: Name Exp, iv :: Name Exp] cases
          cases = mkCase an (Var an domTy dsc) p pes
          an    = MsgAnnote $ "Purify: generated dispatch function: " ++ prettyPrint cases

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
mkADataCon (t, n) = DataCon (MsgAnnote "Purify: generated A data ctor") n $ [] |-> mkArrowTy [t] aTy

type PureEnv = [(Name Exp, Ty)]

mkPureEnv :: (Fresh m, MonadError AstError m) => [Ty] -> [(Name Exp, Embed Poly)] -> m PureEnv
mkPureEnv _ []                     = pure []
mkPureEnv ms ((n, Embed phi) : nps) = do
      ty     <- poly2Ty phi
      purety <- purifyTy (ann ty) ms ty
      ((n, purety) :) <$> mkPureEnv ms nps

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

purifyStateDefn :: (Fresh m, MonadError AstError m, MonadIO m) =>
                   PureEnv -> [Ty] -> Defn -> m Defn
purifyStateDefn rho ms d = do
      ty           <- poly2Ty phi
      let ms'       = getStates ty
      p_pure       <- lookupPure (ann d) (defnName d) rho
      let d_pure    = defnName d
      (args, e)    <- unbind body
      nstos        <- freshVars "sigma" ms
      let stos      = map (mkVar $ ann d) nstos
      e'           <- purifyStateBody rho stos ms (length ms - length ms') e
      let b_pure    = bind (args ++ map fst nstos) e'
      pure $ d { defnName = d_pure, defnPolyTy = [] |-> p_pure, defnBody = Embed b_pure }
      where Embed body = defnBody d
            Embed phi  = defnPolyTy d

liftMaybe :: MonadError AstError m => Annote -> String -> Maybe a -> m a
liftMaybe an msg = maybe (failAt an msg) pure

purifyResDefn :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) => PureEnv -> [Ty] -> Defn -> StateT PSto m Defn
purifyResDefn rho ms d = do
      ty            <- projDefnTy d
      (i, o, _, a)  <- liftMaybe (ann d) "Purify: failed at purifyResDefn" $ dstReT $ rangeTy ty
      pure_ty       <- purifyTy (ann d) ms ty
      (args, e)     <- unbind body

      (nstos, stos) <- (map fst &&& map (mkVar an)) <$> freshVars "sto" ms
      e'            <- purifyResBody rho i o a stos ms e

      --
      -- Below is an egregious hack to compile the start symbol slightly differently.
      --
      let args'  = if isStart dname then [] else args
          nstos' = if isStart dname then [] else nstos
          b_pure = bind (args' ++ nstos') e'
          p_pure = if isStart dname then [] |-> rangeTy pure_ty else [] |-> pure_ty
          d_pure = if isStart dname then s2n "$Pure.start" else dname

      pure $ d { defnName = d_pure, defnPolyTy = p_pure, defnBody = Embed b_pure }
      where dname      = defnName d
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
                                    $ purifyStTTy ms t
      PairApp t1 t2 -> TyApp an <$> (TyApp an (TyCon an $ s2n "(,)") <$> purifyTy an ms t1) <*> purifyTy an ms t2
      Pure          -> pure t
      _             -> failAt an $ "Purify: failed to purify type: " ++ prettyPrint t

            -- This takes a Ty of the form
            -- >    T1 -> T2 -> ... -> Tn -> ReT In Out (StT S1 (StT S2 (... (StT Sm I)))) T
            -- and returns a Ty of the form
            -- >    T1 -> T2 -> ... -> Tn -> In -> S1 -> ... -> Sm -> (Either T (O, R), (S1, (..., Sm)))
      where purifyResTy :: Ty -> Maybe Ty
            purifyResTy t = do
                  (_, o, _, _) <- dstReT $ rangeTy t
                  pure $ mkArrowTy (paramTys t ++ ms) $ mkRangeTy o ms

            -- Takes
            -- >    T1 -> T2 -> ... -> Tn -> StT S1 (StT S2 (... (StT Sm I))) T
            -- and replaces it by
            -- >    T1 -> ... -> Tn -> S1 -> ... -> Sm -> (T, (S1, (..., Sm)))
            --
            -- I'm going to rewrite this to stick precisely to Adam's description.
            -- N.b., the product in the codomain associates to the right. The
            -- definition of purifyStTTy required a little hackery to get the result
            -- into that form.
            purifyStTTy :: [Ty] -> Ty -> Maybe Ty
            purifyStTTy ms t = do
                  let r        = rangeTy t
                  a           <- dstCompR r
                  pure $ mkArrowTy (paramTys t ++ ms) $ mkTupleTy (a : ms)

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
-- >  StT S1 (StT S2 (... (StT Sm I)))
-- and returns
-- >  [S1, ..., Sm]
dstStT :: Ty -> Maybe [Ty]
dstStT = \ case
      TyApp _ (TyApp _ (TyCon _ (n2s -> "StT")) s) m -> (s :) <$> dstStT m
      TyCon _ (n2s -> "I")                           -> pure []
      _                                              -> Nothing

-- | This takes a type of the form
-- >  m a
-- and returns
-- >  a
dstCompR :: Ty -> Maybe Ty
dstCompR = \ case
      TyApp _ _ a -> pure a
      _           -> Nothing

-- | This takes a type of the form
-- >  m a
-- and returns
-- >  m
dstCompM :: Ty -> Maybe Ty
dstCompM = \ case
      TyApp _ m _ -> pure m
      _           -> Nothing

-- | This takes a type of the form
-- >  ReT In Out (StT S1 (StT S2 (... (StT Sm I)))) T
-- and returns
-- >  (In, Out, [S1, ..., Sm], T)
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
            | n2s x == "rwReturn" -> pure $ Return an t e
            | n2s x == "put"    -> pure $ Put an e
            | n2s x == "lift"   -> pure $ Lift an e
      App an (App _ (Var _ t x) e) g
            | n2s x == "rwBind"    -> pure $ Bind an e g
            | otherwise         -> pure $ Apply an t x [e, g]
      e@App {}                  -> do
            (n, t, es) <- dstApp e
            pure $ Apply (ann e) t n es
      Match an _ e1 mp e2 es me -> pure $ SMatch an e1 mp e2 es me
      d                         -> failAt (ann d) $ "Purify: unclassifiable case: " ++ show d

data Cases an p e t = Get an t
                    | Return an t e
                    | Lift an e
                    | Put an e
                    | Bind an e e
                    | Apply an t (Name Exp) [e]
                    | SMatch an e MatchPat e [e] (Maybe e)

-- | purifyStateBody
--   rho  -- pure environment
--   stos -- state vars
--   stys -- state types
--   i    -- lifting "depth"
--   tm   -- term to be purified
purifyStateBody :: (Fresh m, MonadError AstError m, MonadIO m) =>
                     PureEnv -> [Exp] -> [Ty] -> Int -> Exp -> m Exp
purifyStateBody rho stos stys i = classifyCases >=> \ case
      Get an _        -> do
            s <- liftMaybe an ("Purify: state-layer mismatch: length stos == " ++ show (length stos) ++ ", i == " ++ show i)
                  $ stos `atMay` i
            pure $ mkTuple $ s : stos

      Return _ _ e    -> pure $ mkTuple $ e : stos

      Lift _ e        -> purifyStateBody rho stos stys (i + 1) e

      Put _ e         -> pure $ mkTuple $ nil : replaceAtIndex i e stos

      Apply an t n es -> mkPureApp an rho t n $ es ++ stos

      -- e1 must be simply-typed, so don't purify it.
      SMatch an e1 mp e2 es e3 -> do
            p <- transPat mp
            e2' <- purifyStateBody rho stos stys i $ mkApp an e2 $ es ++ map (mkVar an) (patVars p)
            Case an (typeOf e2') e1 (bind p e2')  <$> maybe' (purifyStateBody rho stos stys i <$> e3)

      Bind an e g -> do
            a           <- liftMaybe (ann e) "Purify: invalid type in bind" $ dstCompR $ typeOf e
            ns          <- freshVars "st" $ a : stys
            (f, tf, es) <- dstApp g
            g_pure_app  <- mkPureApp an rho tf f $ es ++ map (mkVar an) ns
            let p        = mkTuplePat an $ map patVar ns
            e'          <- purifyStateBody rho stos stys i e
            mkLet an p e' g_pure_app

maybe' :: Monad m => Maybe (m a) -> m (Maybe a)
maybe' = maybe (pure Nothing) (Just <$>)

transPat :: Fresh m => MatchPat -> m Pat
transPat = \ case
      MatchPatCon an t c ps -> PatCon an (Embed t) (Embed c) <$> mapM transPat ps
      MatchPatVar an t      -> PatVar an (Embed t) <$> freshVar "m2c"

patVars :: Pat -> [(Name Exp, Ty)]
patVars = \ case
      PatCon _ _ _ ps      -> concatMap patVars ps
      PatVar _ (Embed t) x -> [(x, t)]

---------------------------
-- Purifying Resumption Monadic definitions
---------------------------

-- It may be that the type for each subexpression occurring in here should be
-- part of the annotation here. That is partially accomplished right now.
data RCase an t p e = RReturn an e
                    | RLift an e
                    | RVar an t (Name e)
                    | Signal an e
                    | RBind an e e
                    | SigK an t e (Name e) [(e, t)] -- (signal e >>= g e1 ... ek)
                    | Extrude an t (Name e) [e]     -- [(e, t)]
                    | RApp an t (Name e) [e]
                    | RMatch an e MatchPat e [e] (Maybe e)

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

classifyRCases :: (Fresh m, MonadError AstError m) => Exp -> m (RCase Annote Ty Pat Exp)
classifyRCases = \ case
     e@(App an _ _)            -> dstApp e >>= classifyApp an
     Match an _ e1 mp e2 es me -> pure $ RMatch an e1 mp e2 es me
     Var an t x                -> pure $ RVar an t x
     d                         -> failAt (ann d) $ "Purify: unclassifiable R-case: " ++ show d

-- | Classifies syntactic applications. Just seemed easier to
-- make it its own function.
classifyApp :: MonadError AstError m => Annote -> (Name Exp, Ty, [Exp]) -> m (RCase Annote Ty p Exp)
classifyApp an (x, t, [])  = pure $ RVar an t x
classifyApp an (x, _, [e])
      | n2s x == "rwReturn" = pure $ RReturn an e
      | n2s x == "lift"   = pure $ RLift an e
      | n2s x == "signal" = pure $ Signal an e
classifyApp an (x, t, [e, g])
      | n2s x == "rwBind"    = case isSignal e of
            Just s -> do
                  (g', t, es) <- dstApp g
                  pure $ SigK an t s g' $ zip es $ paramTys t
            Nothing -> pure $ RBind an e g
      | n2s x == "extrude" = pure $ Extrude an t x [e, g]
      where isSignal :: Exp -> Maybe Exp
            isSignal = \ case
                  App _ (Var _ _ (n2s -> "signal")) e -> pure e
                  _                                   -> Nothing
classifyApp an (x, t, es)
      | n2s x == "extrude" = pure $ Extrude an t x es
      | otherwise          = pure $ RApp an t x es

-- state for res-purification.
data PSto = PSto Bool [DataCon] [(Pat, Exp)] (Name Exp) (Map Ty (Name DataConId))


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

            r_g <- freshVar $ "R_" ++ prettyPrint g
            addClause $ mkRDataCon an r_g ts'      -- "R_g T1 ... Tk"

            (p, xs) <- mkRPat an ts' r_g           -- Pattern (R_g e1 ... ek)
            ns      <- freshVars "s" ms
            let pairpat = mkTuplePat an $ p : map patVar ns -- Pattern (R_g e1 ... ek, (s1, ..., sn))

            let svars = map (\ (x, t) -> Var an t x) ns  -- [s1, ..., sn]
            let vars  = map (\ (x, t) -> Var an t x) (zip xs ts')   -- [x1, ..., xn]
            iv <- getI
            g_pure_app <- mkPureApp an rho a g $ vars ++ Var an i iv : svars
            -- "dispatch (R_g e1 ... ek, (s1, ..., sn)) i = g_pure e1 ... ek i s1 ... sn"
            addEquation (pairpat, g_pure_app)

            let rGes  = mkApp an (Con an (mkArrowTy ts' rTy) r_g) es
            let outer = mkTuple $ [e, rGes] ++ stos
            pure $ mkRight outer             -- Right ((e, R_g e1 ... ek), (s1, (..., sm)))

      -- purifyResBody (signal e)         = "(Right ((e, (s1, (..., sm))), R_ret))"
      Signal an e -> do
            addNakedSignal an
            pure $ mkRight $ mkTuple $ [e, Con an rTy $ s2n "R_return"] ++ stos

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
      RBind an e g -> do
            -- purify the types of e and g
            tg'         <- purifyTy an ms $ typeOf g
            -- ert is the return type of e; ty is the type of the whole expression
            (ert, _)    <- dstArrow tg'
            e'          <- purifyResBody rho i o ert stos ms e

            -- start calculating pattern: p = (Left (v, (s1, (..., sm))))
            svars       <- freshVars "s" ms
            v           <- freshVar "v"
            p           <- mkLeftPat "RBind" (patVar (v, ert)) (map patVar svars)
            -- done calculating p = Left (v, (s1, (..., sm)))

            -- calculating g_pure_app = "g_pure v s1 ... sm"
            let vars     = map (mkVar an) svars
            (f, t, es)  <- dstApp g
            g_pure_app  <- mkPureApp an rho t f $ es ++ Var an ert v : vars
            -- done calculating g_pure_app
            mkLet an p e' g_pure_app

          -- N.b., the pure env rho will have to contain *all* purified bindings
          --       or this will fail.
      RLift an e -> do
            e'    <- purifyStateBody rho stos ms 0 e
            s_i   <- freshVars "s" ms
            v     <- freshVar "v"
            -- the pattern "(v, (s1, (..., sm)))"
            let p       = mkTuplePat an $ map patVar $ (v, a) : s_i
            body  <- mkLeft "RLift" (Var an a v) stos

            mkLet an p e' body

      RVar an tx x   -> do
            tx' <- purifyTy an ms tx
            pure $ mkApp' an tx' x' stos
                  where x' = if isStart x then s2n "$Pure.start" else x

      -- extrude :: Monad m => ReT i o (StT s m) a -> s -> ReT i o m a
      --
      -- To purify e = (extrude ... (extrude phi s1) ... sn):
      -- 1. N.b., phi :: ReT i o (StT s m) a. We'll assume that e is "maximal". I.e.,
      --    that you have identified how many times extrude has been applied and its arguments s1, ..., sn.
      -- 2. phi' <- purifyResBody phi
      -- 3. make definition and addit to definitions: $LL = phi'
      -- 4. return $ (...($LL s1)...sn)
      Extrude an t rator rands -> case flattenExtr $ mkApp' an t rator rands of
            (Var an t d, stos) -> do
                  t'  <- purifyTy an ms t
                  pure $ mkApp' an t' d stos
            (e@App {}, stos)   -> do
                  (f, t, es) <- dstApp e
                  t'         <- purifyTy (ann e) ms t
                  pure $ mkApp' (ann e) t' f $ es ++ stos
            (e, _)             -> failAt (ann e) $ "Purify: extruded device is non-variable: " ++ show e

      RApp an rty rator rands -> do
            rator' <- purifyResBody rho i o a stos ms (Var an rty rator)
            -- N.b., don't think it's necessary to purify the rands because
            -- they're simply typed.
            (f, t, stos') <- dstApp rator'
            mkPureApp an rho t f $ rands ++ stos'

       -- e1 must be simply-typed, so don't purify it.
      RMatch an e1 mp e2 es e3 -> do
            p <- transPat mp
            e2' <- purifyResBody rho i o a stos ms $ mkApp an e2 $ es ++ map (mkVar an) (patVars p)
            Case an (typeOf e2') e1 (bind p e2') <$> maybe' (purifyResBody rho i o a stos ms <$> e3)

      where mkLeft :: (Fresh m, MonadError AstError m, MonadState PSto m) => String -> Exp -> [Exp] -> m Exp
            mkLeft s a stos = do
                  let an = ann a
                  let t = typeOf a
                  c <- getACtor s t
                  let anode = App an (Con an (mkArrowTy [t] aTy) c) a
                  pure $ App an (Con an (mkArrowTy [mkTupleTy $ aTy : ms] $ mkRangeTy o ms) (s2n "Done"))
                              (mkTuple $ anode : stos)

            mkLeftPat :: (Fresh m, MonadError AstError m) => String -> Pat -> [Pat] -> StateT PSto m Pat
            mkLeftPat s a stos = do
                  let an = ann a
                  let t = typeOf a
                  c <- getACtor s t
                  let anode =  PatCon an (Embed $ mkArrowTy [t] aTy) (Embed c) [a]
                  pure $ PatCon an (Embed $ mkArrowTy [mkTupleTy $ aTy : ms] $ mkRangeTy o ms) (Embed $ s2n "Done")
                              [mkTuplePat an $ anode : stos]

            mkRight :: Exp -> Exp
            mkRight e = App (ann e) (Con (ann e) (mkArrowTy [typeOf e] $ mkRangeTy o ms) (s2n "Pause")) e

            addNakedSignal :: (Fresh m, MonadError AstError m, MonadState PSto m) => Annote -> m ()
            addNakedSignal an = do
                  PSto hasSig _ _ _ _ <- get
                  unless hasSig $ do
                        -- by default, must add the eqn: dispatch R_return i = Left i
                        let r_return = s2n "R_return"

                        p_ret     <- fst <$> mkRPat an [] r_return
                        p_ret_ns  <- freshVars "s" ms
                        let p_ret' = mkTuplePat an $ p_ret : map patVar p_ret_ns

                        iv        <- getI
                        b_ret'    <- mkLeft "Signal" (Var an i iv) $ map (uncurry $ flip $ Var an) p_ret_ns
                        addClause $ mkRDataCon an r_return []
                        addEquation (p_ret', b_ret')
                        modify (\ (PSto _ dcs pes iv as) -> PSto True dcs pes iv as)

            getI :: MonadState PSto m => m (Name Exp)
            getI = get >>= (\ (PSto _ _ _ iv _) -> pure iv)

            addClause :: MonadState PSto m => DataCon -> m ()
            addClause dc   = modify (\ (PSto hasSig dcs pes iv as) -> PSto hasSig (dc : dcs) pes iv as)

            addEquation :: MonadState PSto m => (Pat, Exp) -> m ()
            addEquation pe = modify (\ (PSto hasSig dcs pes iv as) -> PSto hasSig dcs (pe : pes) iv as)

            getACtor :: (Fresh m, MonadState PSto m) => String -> Ty -> m (Name DataConId)
            getACtor s t = do
                  PSto _ _ _ _ as <- get
                  case Map.lookup (unAnn t) as of
                        Nothing -> do
                              c <- freshVar $ "A_" ++ s ++ show (Map.size as)
                              -- TODO(chathhorn): the num postfixed by Fresh is dropped in toCore because ctors assumed unique.
                              modify (\ (PSto hasSig dcs pes iv as) -> PSto hasSig dcs pes iv $ Map.insert (unAnn t) c as)
                              pure c
                        Just c -> pure c


dispatchTy :: Ty -> Ty -> [Ty] -> Ty
dispatchTy i o ms = mkArrowTy [domTy, i] etor
    where etor  = mkRangeTy o ms
          domTy = mkTupleTy $ rTy : ms

-- | Takes t1 and t2 and creates (t1, t2). I'm assuming (?!?) that the name of the
-- pairing constructor is "(,)". Correct me if I'm wrong.
mkPairTy :: Ty -> Ty -> Ty
mkPairTy t = TyApp (MsgAnnote "Purify: mkPairTy") (TyApp (MsgAnnote "Purify: mkPairTy") (TyCon (MsgAnnote "Purify: mkPairTy") $ s2n "(,)") t)

mkPair :: Exp -> Exp -> Exp
mkPair e1 e2 = App (MsgAnnote "Purify: mkPair") (App (MsgAnnote "Purify: mkPair") (Con (MsgAnnote "Purify: mkPair") t (s2n "(,)")) e1) e2
      where t = mkArrowTy [typeOf e1, typeOf e2] $ mkPairTy (typeOf e1) $ typeOf e2

mkPairPat :: Annote -> Pat -> Pat -> Pat
mkPairPat an p1 p2 = PatCon an (Embed $ mkPairTy (typeOf p1) (typeOf p2)) (Embed (s2n "(,)")) [p1, p2]

-- | Global constant representation of R data type.
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
mkTuple = foldr mkPair nil

mkTupleTy :: [Ty] -> Ty
mkTupleTy = foldr mkPairTy nilTy

mkTuplePat :: Annote -> [Pat] -> Pat
mkTuplePat an = foldr (mkPairPat an) nilPat

patVar :: (Name Exp, Ty) -> Pat
patVar (n, t) = PatVar (MsgAnnote "Purify: patVar") (Embed t) n

mkVar :: Annote -> (Name Exp, Ty) -> Exp
mkVar an (n, t) = Var an t n

mkRangeTy :: Ty -> [Ty] -> Ty
mkRangeTy o ts = mkEitherTy (mkTupleTy ts) o

-- | Takes [T1, ..., Tn-1] Tn and returns (T1 -> (T2 -> ... (T(n-1) -> Tn) ...))
mkArrowTy :: [Ty] -> Ty -> Ty
mkArrowTy ps = foldr1 arr . (ps ++) . (: [])

mkEitherTy :: Ty -> Ty -> Ty
mkEitherTy t1 = TyApp (MsgAnnote "Purify: mkEitherTy") (TyApp (MsgAnnote "Purify: mkEitherTy") (TyCon (MsgAnnote "Purify: mkEitherTy") $ s2n "PuRe") t1)

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

mkApp :: Annote -> Exp -> [Exp] -> Exp
mkApp an = foldl' $ App an

mkApp' :: Annote -> Ty -> Name Exp -> [Exp] -> Exp
mkApp' an t n = mkApp an (Var an t n)

mkPureApp :: MonadError AstError m => Annote -> PureEnv -> Ty -> Name Exp -> [Exp] -> m Exp
mkPureApp an rho t rator es = flip (mkApp an) es <$> mkPureVar an rho t rator

dstApp :: MonadError AstError m => Exp -> m (Name Exp, Ty, [Exp])
dstApp = \ case
      App _ (Var _ t n) rand -> pure (n, t, [rand])
      App _ rator rand       -> do
            (n, t, es) <- dstApp rator
            pure (n, t, es ++ [rand])
      Var _ t n              -> pure (n, t, [])
      d                      -> failAt (ann d) $ "Purify: tried to dst non-app: " ++ show (unAnn d)

-- | Lets are desugared already, so use a case instead (with lifted discriminator).
mkLet :: Fresh m => Annote -> Pat -> Exp -> Exp -> m Exp
mkLet an p e1 e2 = do
      v <- freshVar "disc"
      -- Need to lift the discriminator.
      pure $ App an (Lam an (typeOf e1) $ bind v $ Case an (typeOf e2) (Var an (typeOf e1) v) (bind p e2) Nothing) e1
