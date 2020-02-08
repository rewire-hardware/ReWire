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
import Data.List (foldl', find, intercalate)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Either (partitionEithers)

-- Append to a non-empty list.
(|:) :: [a] -> a -> NonEmpty a
xs |: x = NE.reverse $ x :| reverse xs

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
freshVars v = \ case
      []       -> pure []
      (x : xs) -> NE.toList <$> freshVars' v (x :| xs)

freshVars' :: Fresh m => String -> NonEmpty b -> m (NonEmpty (Name a, b))
freshVars' n = mapM (\ (i, b) -> (, b) <$> freshVar (((n ++) . show) i)) . NE.zip (NE.iterate (+ 1) (0 :: Int))

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item : b)
      where (a, _ : b) = splitAt n ls

poly2Ty :: Fresh m => Poly -> m Ty
poly2Ty (Poly p) = snd <$> unbind p

purify :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) => FreeProgram -> m FreeProgram
purify (ts, ds) = do
      (smds, pds) <- partitionEithers <$> mapM isStateMonadicDefn ds
      (rmds, ods) <- partitionEithers <$> mapM isResMonadicDefn pds

      let nameNpolyS = map (defnName &&& defnPolyTy) smds
          nameNpolyR = map (defnName &&& defnPolyTy) rmds

      imprhoR   <- mapM (\ (d, Embed p) -> poly2Ty p >>= \ t -> pure (d, t)) nameNpolyR

      (_, tyStart) <- liftMaybe noAnn "No start symbol!"
                        $ find (isStart . fst) imprhoR
      (_, i, o, t) <- liftMaybe (ann tyStart) ("dstResTy applied to: " ++ show tyStart)
                        $ dstResTy tyStart

      --- if t = (a, s), need to set t := a and stos := s
      let tups = dstTupleTy t

      rhoS      <- mkPureEnv nameNpolyS $ NE.tail tups
      rhoR      <- mkPureEnv nameNpolyR $ NE.tail tups
      let rho = rhoS ++ rhoR

      pure_smds <- mapM (purifyStateDefn rho $ NE.tail tups) smds
      iv        <- freshVar "i"

      -- by default, must add the eqn: dispatch R_return i = Left i
      let ty   = dispatchTy i o tups
          pdef = PatCon (MsgAnnote "Purify: pdef") (Embed (TyCon (MsgAnnote "Purify: pdef") (s2n "R"))) (Embed $ s2n "R_return") []
          bdef = mkLeft (rangeTy ty) (Var (MsgAnnote "Purify: bdef") i iv)

      (pure_rmds, PSto dcs pes _) <- runStateT
            (mapM (purifyResDefn rho imprhoR iv $ NE.tail tups) rmds)
            $ PSto [] ((pdef, bdef) :| []) []

      disp <- mkDispatch ty pes iv

      pure ( ts ++ [mkRDatatype dcs]
           , mkStart i o tups
           : disp
           : ods ++ pure_smds ++ pure_rmds)

-- In addition to all the above, we re-tie the recursive knot by adding a new
--   "start" as follows
--         start :: ReT In Out I T
--         start = unfold dispatch start_pure
--           start :: In -> (Either T (O, R), ()) --- drop the "()"?

-- Correcting mkStart.
-- In the type of Main.start.
-- t (return type of Main.start) needs to be combined with ms (list of StT s, stores).
mkStart :: Ty -> Ty -> NonEmpty Ty -> Defn
mkStart i o (t :| ms) = Defn
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

            unfold     = Var (MsgAnnote "Purify: unfold") (TyBlank $ MsgAnnote "stub type for unfold") (s2n "unfold")
            dispatch   = Var (MsgAnnote "Purify: dispatch") (dispatchTy i o $ t :| ms) (s2n "$Pure.dispatch")
            start_pure = Var (MsgAnnote "Purify: start_pure") etor (s2n "$Pure.start")
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
mkDispatch :: Fresh m => Ty -> NonEmpty (Pat, Exp) -> Name Exp -> m Defn
mkDispatch ty pes iv = do
      dsc     <- freshVar "dsc"
      let body  = Embed $ bind [dsc :: Name Exp, iv :: Name Exp] cases
          cases = mkCase an (rangeTy ty) (Var an rTy dsc) pes
          an    = MsgAnnote $ "Generated dispatch function: " ++ prettyPrint cases

      pure Defn
            { defnAnnote = an
            , defnName   = s2n "$Pure.dispatch"
            , defnPolyTy = [] |-> ty
            , defnInline = False
            , defnBody   = body
            }

mkCase :: Annote -> Ty -> Exp -> NonEmpty (Pat, Exp) -> Exp
mkCase an ty dsc = \ case
      (p, e) :| []         -> Case an ty dsc (bind p e) Nothing
      (p, e) :| (pe : pes) -> Case an ty dsc (bind p e) . Just $ mkCase an ty dsc $ pe :| pes

mkRDatatype :: [DataCon] -> DataDefn
mkRDatatype dcs = DataDefn
       { dataAnnote = MsgAnnote "R Datatype"
       , dataName   = s2n "R"
       , dataKind   = KStar
       , dataCons   = mkRDataCon (MsgAnnote "Purify: mkRDatatype") (s2n "R_return") [] : dcs
       }

-- global constant representation of R data type
rTy :: Ty
rTy = TyCon (MsgAnnote "Purify: rTy") (s2n "R")

type PureEnv = [(Name Exp, Ty)]

mkPureEnv :: (Fresh m, MonadError AstError m) => [(Name Exp, Embed Poly)] -> [Ty] -> m PureEnv
mkPureEnv [] _                      = pure []
mkPureEnv ((n, Embed phi) : nps) ms = do
      ty     <- poly2Ty phi
      purety <- purifyTy (ann ty) ms ty
      env    <- mkPureEnv nps ms
      pure $ (n, purety) : env

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

purifyStateDefn :: (Fresh m, MonadError AstError m, MonadIO m) =>
                   PureEnv -> [Ty] -> Defn -> m Defn
purifyStateDefn rho ms d = do
      ty           <- poly2Ty phi
      p_pure       <- lookupPure (ann d) (defnName d) rho
      let d_pure    = defnName d
      (args, e)    <- unbind body
      (_, stys, _) <- liftMaybe (ann d) "failed at purifyStateDefn" $ dstStT ty
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
                 PureEnv -> [(Name Exp, Ty)] -> Name Exp -> [Ty] -> Defn -> StateT PSto m Defn
purifyResDefn rho imprhoR iv ms d = do
      ty           <- lookupImpure an dname imprhoR
      pure_ty      <- purifyTy (ann d) ms ty
      (args, e)    <- unbind body

      (_, i, o, a) <- liftMaybe (ann d) "failed at purifyResDefn" $ dstResTy ty

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
                  (ts, _, o, a) <- dstResTy t
                  pure $ mkArrowTy $ (ts ++ ms) |: mkRangeTy a o ms

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
                  (ts, stos, a) <- dstStT t
                  pure $ mkArrowTy $ (ts ++ stos) |: mkTupleTy (a : stos)

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

paramTys :: Ty -> [Ty]
paramTys = paramTys' []
      where paramTys' :: [Ty] -> Ty -> [Ty]
            paramTys' acc = \ case
                  TyApp    _ (TyApp _ (TyCon _ (n2s -> "->")) t1) t2  -> paramTys' (t1 : acc) t2
                  _                                                   -> reverse acc

-- This takes a type of the form
--    T1 -> T2 -> ... -> Tnd -> StT S1 (StT S2 (... (StT Sm I))) T
-- and returns
--    ([T1, ..., Tn], [S1, ..., Sm], T)
dstStT :: Ty -> Maybe ([Ty], [Ty], Ty)
dstStT ty = do
      (a, stos) <- dstComp $ rangeTy ty
      pure (paramTys ty, stos, a)
      where dstComp :: Ty -> Maybe (Ty, [Ty])
            dstComp = \ case
                  TyApp _ c@(TyApp _ (TyApp _ (TyCon _ (n2s -> "StT")) _) _) a -> (a, ) <$> dstStT c
                  TyApp _ (TyCon _ (n2s -> "I")) a                             -> pure (a, [])
                  _                                                            -> Nothing

            dstStT :: Ty -> Maybe [Ty]
            dstStT = \ case
                  TyApp _ (TyApp _ (TyCon _ (n2s -> "StT")) s) m -> (s :) <$> dstStT m
                  TyCon _ (n2s -> "I")                           -> pure []
                  _                                              -> Nothing

-- This takes a type of the form
--    T1 -> T2 -> ... -> Tn -> ReT In Out (StT S1 (StT S2 (... (StT Sm I)))) T
-- and returns
--    ([T1, ..., Tn], In, Out, T)
dstResTy :: Ty -> Maybe ([Ty], Ty, Ty, Ty)
dstResTy ty = do
      (i, o, a)   <- dstReT $ rangeTy ty
      pure (paramTys ty, i, o, a)
      where dstReT :: Ty -> Maybe (Ty, Ty, Ty)
            dstReT = \ case
                  TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReT")) i) o) _) a -> Just (i, o, a)
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

-- purifyStateBody
--   rho  -- pure environment
--   stos -- state vars
--   stys -- state types
--   i    -- lifting "depth"
--   tm   -- term to be purified
purifyStateBody :: (Fresh m, MonadError AstError m, MonadIO m) =>
                     PureEnv -> [Exp] -> [Ty] -> Int -> [Ty] -> Exp -> m Exp
purifyStateBody rho stos stys i ms = classifyCases >=> \ case
      Get an _      -> do
            s <- liftMaybe an ("Purify: state-layer mismatch: length stos == " ++ show (length stos) ++ ", i == " ++ show i)
                  $ stos `atMay` i
            pure $ mkTuple $ s : stos

      Return _ _ e -> pure $ mkTuple $ e : stos

      Lift _ e     -> purifyStateBody rho stos stys (i + 1) ms e

      Put an e     -> pure $ mkTuple $ nil : stos'
            where nil   = Con an nilTy (s2n "()")
                  nilTy = TyCon an (s2n "()")
                  stos' = replaceAtIndex (i-1) e stos

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
            (_, _, a)  <- liftMaybe (ann te) "Non-StT Type passed to dstStT" $ dstStT te -- a is the return type of e
            e'         <- purifyStateBody rho stos stys i ms e
            ns         <- freshVars "st" $ a : stys
            let vs      = foldr (\ (n, t) nts -> Var an t n : nts) [] ns
            g_pure_app <- mkPureApp an rho g vs
            let p       = mkTuplePat ns
            -- g can, in general, be an application. That's causing the error when (var2Name g) is called.
            (gn, _, _) <- dstApp g
            ptg        <- lookupPure an gn rho
            let ty      = rangeTy ptg
            pure $ mkLet an ty p e' g_pure_app

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
data PSto = PSto [DataCon] (NonEmpty (Pat, Exp)) [Defn]

addClause :: MonadState PSto m => DataCon -> m ()
addClause dc   = modify (\ (PSto dcs pes defs) -> PSto (dc : dcs) pes defs)

addEquation :: MonadState PSto m => (Pat, Exp) -> m ()
addEquation pe = modify (\ (PSto dcs pes defs) -> PSto dcs (pe <| pes) defs)

-- purifyResBody
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
            pure $ mkLeft etor $ mkTuple $ e : fst mst

      -- purifyResBody (signal e
      --        >>= g e1 ... ek) = "Right ((e, (s1, (..., sm))), R_g e1 ... ek)"
      --           Side effect:
      --     * add the clause "R_g T1 ... Tk" to R
      --     (where Ti is the type of ei)
      --     * add the eqn
      --         dispatch ((R_g e1 ... ek), (s1, ..., sm)) i
      --      = g_pure e1 ... ek i s1 ... sm
      -- to defn of dispatch
      SigK an t e g bes -> do
            let (es, ts) = unzip bes
            ts' <- mapM (purifyTy an $ snd mst) ts

            r_g <- fresh $ s2n $ "R_" ++ prettyPrint g
            let rt      = mkArrowTy $ ts' |: rTy
            let rcon    = Con an rt r_g
            let rGes    = mkApp rcon es
            addClause (mkRDataCon an r_g ts')      -- "R_g T1 ... Tk"

            ns     <- freshVars "s" $ snd mst
            let stopat = mkTuplePat ns             -- Pattern (s1, ..., sn)
            (p, xs) <- mkRPat an ts' r_g           -- Pattern (R_g e1 ... ek)
            let pairpat = case snd mst of          -- Pattern (R_g e1 ... ek, (s1, ..., sn))
                  []-> p
                  _ -> mkPairPat p stopat

            let svars = map (\ (x, t) -> Var an t x) ns  -- [s1, ..., sn]
            let vars  = map (\ (x, t) -> Var an t x) (zip xs ts')   -- [x1, ..., xn]
            g_pure_app <- mkPureApp an rho (Var an t g) (vars ++ [Var an i iv] ++ svars)
            -- "dispatch (R_g e1 ... ek, (s1, ..., sn)) i = g_pure e1 ... ek i s1 ... sn"
            addEquation (pairpat, g_pure_app)

            let etor        = mkRangeTy t o (snd mst)
            let inner = case fst mst of
                  []   -> rGes                    --  R_g e1 ... ek :: R
                  stos -> mkTuple [rGes, mkTuple stos]
                                                  -- ^ (R, (s1, ..., sm))
            let outer   = mkTuple [e, inner]      -- (e, (R_g e1 ... ek, (s1, (..., sm))))
            pure $ mkRight etor outer             -- Right ((e, R_g e1 ... ek), (s1, (..., sm)))

      -- purifyResBody (signal e)         = "(Right ((e, (s1, (..., sm))), R_ret))"
      Signal an e -> do
            let r_return     = Con an rTy (s2n "R_return")
            let inner = case fst mst of
                  []   -> r_return
                  stos -> mkTuple [r_return, mkTuple stos]
            let outer   = mkTuple [e, inner]
            let etor    = mkRangeTy t o (snd mst)
            pure $ mkRight etor outer

      -- purifyResBody (e >>= g)          = "let
      --   -- N.B.: The irrefutable pattern here is
      --   -- sketchy, but it should be okay because
      --   -- of the restriction on occurrences of
      --   -- "signal"
      --               (Left v, (s1, (..., sm))) = [|purifyResBody e|]
      --             in
      --               g_pure v s1 ... sm"
      -- It appears to me that g can be an application (and not only a variable). In which case, I guess
      -- the right thing to do is just apply "v s1 ... sm" to g. And possibly add "_pure" to the function at the
      -- head of the application. The way it's written below assumes that g is a variable.
      RBind an e g tg -> do
            -- purify the types of e and g
            tg'         <- purifyTy an (snd mst) tg
            -- ert is the return type of e; ty is the type of the whole expression
            (ert, ty)   <- dstArrow tg'
            e'          <- purifyResBody rho i o ert iv mst e
            g'          <- purifyResBody rho i o t iv mst g

            -- start calculating pattern: p = (Left (v, (s1, (..., sm))))
            let etor     = mkRangeTy ert o (snd mst)
            svars       <- freshVars "state" $ snd mst
            v           <- freshVar "v"
            let stps     = mkTuplePat $ (v, ert) : svars
            let p        = PatCon an (Embed etor) (Embed $ s2n "Prelude.Left") [stps]
            -- done calculating p = Left (v, (s1, (..., sm)))

            -- calculating g_pure_app = "g_pure v s1 ... sm"
            let vars     = map (\ (x, t) -> Var an t x) svars
            g_pure_app  <- mkPureApp an rho g' (Var an ert v : vars)
            -- done calculating g_pure_app
            pure $ mkLet an ty p e' g_pure_app

          -- N.b., the pure env rho will have to contain *all* purified bindings
          --       or this will fail.
      RLift an e -> do
            e'    <- uncurry (purifyStateBody rho) mst 0 (snd mst) e -- was 1, which seems incorrect.
            s_i   <- freshVars "State" $ snd mst
            v     <- freshVar "var"
            -- the pattern "(v, (s1, (..., sm)))"
            let p       = mkTuplePat $ (v, t) : s_i
                etor    = mkRangeTy t o (snd mst)
                leftv   = mkLeft etor (Var an t v)
                body    = mkTuple $ leftv : fst mst
                body_ty = mkTupleTy (etor : snd mst)
            pure $ mkLet an body_ty p e' body

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

dispatchTy :: Ty -> Ty -> NonEmpty Ty -> Ty
dispatchTy i o (t :| ms) = mkArrowTy $ domTy :| [i, etor]
    where etor  = mkRangeTy t o ms
          domTy = if null ms then rTy else (mkPairTy rTy . mkTupleTy) ms

-- | Takes t1 and t2 and creates (t1, t2). I'm assuming (?!?) that the name of the
-- pairing constructor is "(,)". Correct me if I'm wrong.
mkPairTy :: Ty -> Ty -> Ty
mkPairTy t = TyApp (MsgAnnote "Purify: mkPairTy") (TyApp (MsgAnnote "Purify: mkPairTy") (TyCon (MsgAnnote "Purify: mkPairTy") $ s2n "(,)") t)

mkPair :: Exp -> Exp -> Exp
mkPair e1 e2 = App (MsgAnnote "Purify: mkPair") (App (MsgAnnote "Purify: mkPair") (Con (MsgAnnote "Purify: mkPair") t (s2n "(,)")) e1) e2
      where t = mkArrowTy $ typeOf e1 :| [typeOf e2, mkPairTy (typeOf e1) $ typeOf e2]

mkTuple :: [Exp] -> Exp
mkTuple = \ case
      []     -> Con (MsgAnnote "Purify: mkTuple") (mkTupleTy []) $ s2n "()"
      x : xs -> foldl1 mkPair $ x :| xs

mkTupleTy :: [Ty] -> Ty
mkTupleTy = \ case
      []     -> TyCon (MsgAnnote "Purify: mkTupleTy") $ s2n "()"
      t : ts -> foldl1 mkPairTy $ t :| ts

dstTupleTy :: Ty -> NonEmpty Ty
dstTupleTy = \ case
      -- TyCon _ (n2s -> "()")                         -> [] -- TODO(chathhorn): wish this was the inverse of mkTupleTy
      TyApp _ (TyApp _ (TyCon _ (n2s -> "(,)")) t1) t2 -> dstTupleTy t1 <> (t2 :| [])
      t                                                -> t :| []

mkRangeTy :: Ty -> Ty -> [Ty] -> Ty
mkRangeTy a o = \ case
      [] -> mkEitherTy a (mkPairTy o rTy)
      ts -> mkEitherTy (mkPairTy a $ mkTupleTy ts) (mkPairTy o $ mkPairTy rTy $ mkTupleTy ts)

-- | Takes [T1, ..., Tn] and returns (T1 -> (T2 -> ... (T(n-1) -> Tn) ...))
mkArrowTy :: NonEmpty Ty -> Ty
mkArrowTy = foldr1 arr

mkPairPat :: Pat -> Pat -> Pat
mkPairPat p1 p2 = PatCon (MsgAnnote "Purify: mkPairPat") (Embed $ mkPairTy (typeOf p1) (typeOf p2)) (Embed (s2n "(,)")) [p1, p2]

mkTuplePat :: [(Name Exp, Ty)] -> Pat
mkTuplePat = \ case
      []     -> PatCon (MsgAnnote "Purify: mkTuplePat") (Embed $ TyCon (MsgAnnote "Purify: mkTuplePat") $ s2n "()") (Embed $ s2n "()") []
      x : xs -> foldl1 mkPairPat $ NE.map patVar $ x :| xs
      where patVar :: (Name Exp, Ty) -> Pat
            patVar (n, t) = PatVar (MsgAnnote "Purify: mkTuplePat") (Embed t) n

-- Shouldn't mkLeft/mkRight change the type t to an Either?
mkLeft :: Ty -> Exp -> Exp
mkLeft t = App (MsgAnnote "Purify: mkLeft") (Con (MsgAnnote "Purify: mkLeft") t (s2n "Prelude.Left"))

mkRight :: Ty -> Exp -> Exp
mkRight t = App (MsgAnnote "Purify: mkRight") (Con (MsgAnnote "Purify: mkRight") t (s2n "Prelude.Right"))

mkEitherTy :: Ty -> Ty -> Ty
mkEitherTy t1 = TyApp (MsgAnnote "Purify: mkEitherTy") (TyApp (MsgAnnote "Purify: mkEitherTy") (TyCon (MsgAnnote "Purify: mkEitherTy") $ s2n "Prelude.Either") t1)

mkRDataCon :: Annote -> Name DataConId -> [Ty] -> DataCon
mkRDataCon an r_g ts = DataCon an r_g ([] |-> mkArrowTy (ts |: rTy))

mkRPat :: Fresh m => Annote -> [Ty] -> Name DataConId -> m (Pat, [Name Exp])
mkRPat an ts r_g = do
      xs <- freshVars "store" ts
      let varpats = map (\ (x, t) -> PatVar an (Embed t) x) xs
      pure (PatCon an (Embed $ TyCon an $ s2n "R") (Embed r_g) varpats, map fst xs)

mkPureVar :: MonadError AstError m => Annote -> PureEnv -> Name Exp -> Ty -> m Exp
mkPureVar an rho x t
      | n2s x == "extrude" = pure $ Var an t x
      | n2s x == "unfold"  = pure $ Var an t x
      | otherwise          = do
            t' <- lookupPure an x rho
            pure $ Var an t' x

mkApp :: Exp -> [Exp] -> Exp
mkApp = foldl' (App (MsgAnnote "Purify: mkApp"))

mkPureApp :: MonadError AstError m => Annote -> PureEnv -> Exp -> [Exp] -> m Exp
mkPureApp an rho e es = do
      (rator, t, rands) <- dstApp e
      ep                <- mkPureVar an rho rator t
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
mkLet :: Annote -> Ty -> Pat -> Exp -> Exp -> Exp
mkLet an ty p e1 e2 = Case an ty e1 (bind p e2) Nothing

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
