{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.Crust.Purify (purify) where

import ReWire.Annotation (Annote (MsgAnnote, NoAnnote), ann, unAnn, noAnn)
import ReWire.Crust.Syntax (Exp (..), Kind (..), Ty (..), Pat (..), MatchPat (..), DefnAttr (..), DataConId, DataCon (..), Builtin (..), Defn (..), Poly (..), DataDefn (..), FreeProgram, flattenApp)
import ReWire.Crust.TypeCheck (unify')
import ReWire.Crust.Types (tupleTy, mkArrowTy, typeOf, arrowLeft, paramTys, isReacT, codomTy, (|->), isStateT, dstArrow, dstStateT, dstTyApp, dstReacT, nilTy)
import ReWire.Crust.Util (mkApp, mkTuplePat, mkTuple, nil, isPrim, patVars, toVar, toPatVar, transPat, transMPat, mkLam)
import ReWire.Error (failAt, failAtWith, failInternal, relocatingTo, MonadError, AstError)
import ReWire.Pretty (TextShow (showb, showt), fromText, prettyPrint)
import ReWire.Unbound (freshVar, Fresh, s2n, n2s, bind, Bind, Name, Embed (Embed), unbind)

import Control.Arrow (second, (&&&))
import Control.Monad (unless, zipWithM, (>=>))
import Control.Monad.State (MonadState, StateT (runStateT), modify, get)
import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.List (find)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.Text (Text, isPrefixOf)

import qualified Data.HashMap.Strict as Map

atMay :: (Eq i, Num i) => [a] -> i -> Maybe a
atMay []       _ = Nothing
atMay (x : _)  0 = Just x
atMay (_ : xs) n = atMay xs $ n - 1

freshVars :: Fresh m => Text -> [b] -> m [(b, Name a)]
freshVars v = mapM (\ (i, b) -> (b, ) <$> freshVar (v <> showt i)) . zip [0 :: Int ..]

poly2Ty :: Fresh m => Poly -> m Ty
poly2Ty (Poly p) = snd <$> unbind p

projDefnTy :: Fresh m => Defn -> m Ty
projDefnTy Defn { defnPolyTy = Embed t } = poly2Ty t

-- TODO(chathhorn): what if we retype extrude and unfold (et al.?) with
-- concrete types and re-run type inference? Much of the muck here is just
-- calculating types that could be inferred.

-- | Transforms functions in state and resumption monads into plain first-order functions.
purify :: (Fresh m, MonadError AstError m, MonadFail m) => Name Exp -> FreeProgram -> m FreeProgram
purify start (ts, syns, ds) = do
      (smds, notSmds)     <- partitionEithers <$> mapM isStateMonadicDefn ds
      (rmds, ods)         <- partitionEithers <$> mapM isReacMonadicDefn notSmds

      maybe (failAtWith noAnn ("No definition for start function (" <> prettyPrint start <> ") found!")
                  [] ["Define a function named " <> prettyPrint start <> ", or select a different entry point with --start=<name>."])
            (projDefnTy >=> checkStartType)
            $ find ((== start) . defnName) rmds

      let nameNpolyS = map (defnName &&& defnPolyTy) smds
          nameNpolyR = map (defnName &&& defnPolyTy) rmds

      -- TODO(chathhorn): perhaps move to a type-checking phase?
      (i, o, ms) <- getCanonReacTy rmds >>= liftMaybe noAnn "Inconsistent ReacT types (i, o, and state layers must be the same for all ReacT)."

      rhoS      <- mkPureEnv ms nameNpolyS
      rhoR      <- mkPureEnv ms nameNpolyR
      let rho = rhoS <> rhoR

      pure_smds <- mapM (purifyStateDefn rho ms) smds

      iv <- freshVar "i"

      (pure_rmds, PSto points _ allAs) <- runStateT (mapM (purifyResDefn start rho ms) rmds) $ PSto mempty iv mempty

      let (dcs, pes) = unzip $ Map.elems points

      disp <- mkDispatch i o ms iv pes

      pure ( filter (not . isAorR) ts <> [mkRDatatype dcs] <> [mkADatatype $ Map.toList allAs]
           , syns
           , mkStart start i o ms : disp : ods <> pure_smds <> pure_rmds
           )

      where getCanonReacTy :: Fresh m => [Defn] -> m (Maybe (Ty, Ty, [Ty]))
            getCanonReacTy = (getCanonReacTy' . catMaybes <$>) . mapM (projDefnTy >=> pure . dstReacT . codomTy)

            getCanonReacTy' :: [(Ty, Ty, [Ty], Ty)] -> Maybe (Ty, Ty, [Ty])
            getCanonReacTy' = \ case
                  []                 -> Nothing
                  (i, o, ms, _) : rs -> foldr mergeReacTys (Just (i, o, ms)) rs

            mergeReacTys :: (Ty, Ty, [Ty], Ty) -> Maybe (Ty, Ty, [Ty]) -> Maybe (Ty, Ty, [Ty])
            mergeReacTys (i, o, ms, _) = \ case
                  Just (i', o', ms')
                        | Just ms'' <- unifyStateStacks ms ms'
                        , Just ui   <- unify' i i', Just uo <- unify' o o'
                              -> Just (ui, uo, ms'')
                  _           -> Nothing

            isAorR :: DataDefn -> Bool
            isAorR = uncurry (||) . ((== "A_") &&& (== "R_")) . n2s . dataName

            checkStartType :: MonadError AstError m => Ty -> m ()
            checkStartType t = case dstReacT t of
                  Just (_, _, [], _) -> pure ()
                  Just (_, _,  _, _) -> failAt (ann t) "Start definition must have type ReacT i o I (). Use the extrude function to remove state layers."
                  _                  -> failAt (ann t) "Start definition must have type ReacT i o I ()."

-- | Re-ties the recursive knot by adding a new "start":
--         start :: ReacT In Out I ()
--         start = unfold dispatch start_pure
-- where start_pure :: (S1, (..., Sm)) -> PuRe (S1, (..., Sm)) Out supplies the
-- initial register values (ToHyle evaluates it with a zero argument, which is
-- dead unless start pauses before extruding).
mkStart :: Name Exp -> Ty -> Ty -> [Ty] -> Defn
mkStart start i o ms = Defn
      { defnAnnote = MsgAnnote "start function"
      , defnName   = start
      , defnPolyTy = [] |-> startTy
      , defnAttr   = Just NoInline
      , defnBody   = appl
      }
      where etor       = mkRangeTy o ms
            state0Ty   = mkArrowTy [tupleTy (MsgAnnote "Purify: state0Ty") ms] etor

            reacT i o a = TyCon (MsgAnnote "Purify: reacT") (s2n "ReacT") `tyApp` i `tyApp` o `tyApp` a
            startTy     = TyApp (MsgAnnote "Purify: startTy")
                              (reacT i o (TyCon (MsgAnnote "Purify: startTy") (s2n "Identity")))
                              nilTy

            unfold     = Builtin (MsgAnnote "Purify: unfold") Nothing (Just $ mkArrowTy [dispatchTy i o ms, state0Ty] startTy) Unfold
            dispatch   = Var (MsgAnnote "Purify: dispatch") Nothing (Just $ dispatchTy i o ms) $ s2n "$Pure.dispatch"
            start_pure = Var (MsgAnnote "Purify: $Pure.start") Nothing (Just state0Ty) $ s2n "$Pure.start"
            appl       = Embed $ bind [] $ mkApp (MsgAnnote "Purify: appl") unfold [dispatch, start_pure]

            tyApp :: Ty -> Ty -> Ty
            tyApp = TyApp $ MsgAnnote "reacT"

-- | Converts
-- >  (extrude (extrude (... (extrude dev s1) ...) s(n-1)) sn)
-- into
-- >  (dev, [s1, ..., sn])
flattenExtr :: Exp -> (Exp, [Exp])
flattenExtr = \ case
      App _ _ _ (App _ _ _ (Builtin _ _ _ Extrude) arg1) arg2 -> second (<> [arg2]) $ flattenExtr arg1
      e                                                       -> (e, [])

-- | Generates the dispatch function.
-- > dispatch (R_g e1 ... ek) i = g_pure e1 ... ek i
mkDispatch :: (MonadError AstError m, Fresh m) => Ty -> Ty -> [Ty] -> Name Exp -> [(Pat, Exp)] -> m Defn
mkDispatch i o ms iv = \ case
      []        -> failAt NoAnnote "empty dispatch: invalid ReWire (is recursion guarded by signal?)"
      (p : pes) -> do
            disc     <- freshVar "disc"
            let ty    = dispatchTy i o ms
                domTy = tupleTy (MsgAnnote "Purify: mkDispatch: domTy") $ rTy : ms
                body  = Embed $ bind [disc :: Name Exp, iv :: Name Exp] cases
                cases = mkCases an (Var an Nothing (Just domTy) disc) p pes
                an    = MsgAnnote "Purify: generated dispatch function"

            pure Defn
                  { defnAnnote = an
                  , defnName   = s2n "$Pure.dispatch"
                  , defnPolyTy = [] |-> ty
                  , defnAttr   = Just NoInline
                  , defnBody   = body
                  }

mkCases :: Annote -> Exp -> (Pat, Exp) -> [(Pat, Exp)] -> Exp
mkCases an disc = foldr mkCase' . flip (mkCase an disc) Nothing
      where mkCase' :: (Pat, Exp) -> Exp -> Exp
            mkCase' (p, e) = mkCase an disc (p, e) . Just

mkCase :: Annote -> Exp -> (Pat, Exp) -> Maybe Exp -> Exp
mkCase an disc (p, e) = Match an Nothing (typeOf e) disc (transPat p) $ mkLam' (p, e)
      where mkLam' :: (Pat, Exp) -> Exp
            mkLam' (p, e) = mkLam an (patVars p) e

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

-- | Aligns the shorter state stack against the tail of the longer one and
--   unifies pointwise (state types must agree, but possibly only up to
--   normalization of type-level Nats, e.g. @Vec (1 + 1)@ vs @Vec 2@);
--   returns the longer stack with the overlap unified.
unifyStateStacks :: [Ty] -> [Ty] -> Maybe [Ty]
unifyStateStacks ms ms'
      | length ms < length ms' = unifyStateStacks ms' ms
      | otherwise              = (pre <>) <$> zipWithM unify' suf ms'
      where (pre, suf) = splitAt (length ms - length ms') ms

type PureEnv = HashMap (Name Exp) Ty

mkPureEnv :: (Fresh m, MonadError AstError m) => [Ty] -> [(Name Exp, Embed Poly)] -> m PureEnv
mkPureEnv ms = fmap Map.fromList . mapM (\ (n, Embed phi) -> do
      ty     <- poly2Ty phi
      purety <- purifyTy (ann ty) ms $ Just ty
      pure (n, purety))

-- | The state layers of a state-monadic definition's type.
getStates :: Ty -> [Ty]
getStates t = fromMaybe [] $ dstTyApp (codomTy t) >>= dstStateT . fst

lookupPure :: MonadError AstError m => Annote -> Name Exp -> PureEnv -> m Ty
lookupPure an x = maybe (failInternal an $ "No pure binding for variable: " <> n2s x) pure . Map.lookup x

isStateMonadicDefn :: Fresh m => Defn -> m (Either Defn Defn)
isStateMonadicDefn = \ case
      d@Defn { defnName = n } | isPrim n -> pure $ Right d
      d@Defn { defnPolyTy = Embed poly } -> bool (Right d) (Left d) . isStateT <$> poly2Ty poly

isReacMonadicDefn :: Fresh m => Defn -> m (Either Defn Defn)
isReacMonadicDefn = \ case
      d@Defn { defnName = n } | isPrim n -> pure $ Right d
      d@Defn { defnPolyTy = Embed poly } -> bool (Right d) (Left d) . isReacT <$> poly2Ty poly

purifyStateDefn :: (Fresh m, MonadError AstError m) =>
                   PureEnv -> [Ty] -> Defn -> m Defn
purifyStateDefn rho ms d = do
      ty           <- poly2Ty phi
      let ms'       = getStates ty
      unless (length ms' <= length ms && isJust (unifyStateStacks ms ms')) $ failInternal (ann d)
            $ "state layers of " <> n2s (defnName d) <> " are not a suffix of the canonical state stack (rwc bug)."
      p_pure       <- lookupPure (ann d) (defnName d) rho
      (args, e)    <- unbind body
      nstos        <- freshVars "sigma" ms
      let stos      = toVar (ann d) <$> nstos
      e'           <- relocatingToDefn d $ purifyStateBody rho stos ms (length ms - length ms') e
      pure $ d { defnPolyTy = [] |-> p_pure, defnBody = Embed $ bind (args <> (snd <$> nstos)) e' }
      where Embed body = defnBody d
            Embed phi  = defnPolyTy d

liftMaybe :: MonadError AstError m => Annote -> Text -> Maybe a -> m a
liftMaybe an msg = maybe (failAt an msg) pure

-- | Relocate errors raised while purifying a definition's body to that
--   definition — but only for genuine user definitions. Lambda-lifted and
--   other synthesized defns ($-prefixed) carry an annotation inherited from
--   inlined code, so relocating to them would move a good location to a worse
--   one; leave those errors where they already point.
relocatingToDefn :: MonadError AstError m => Defn -> m a -> m a
relocatingToDefn d
      | "$" `isPrefixOf` n2s (defnName d) = id
      | otherwise                         = relocatingTo $ ann d

purifyResDefn :: (Fresh m, MonadError AstError m, MonadFail m) => Name Exp -> PureEnv -> [Ty] -> Defn -> StateT PSto m Defn
purifyResDefn start rho ms d = do
      ty            <- projDefnTy d
      (i, o, _, a)  <- maybe (failInternal (ann d) "failed at purifyResDefn") pure $ dstReacT $ codomTy ty
      pure_ty       <- purifyTy (ann d) ms $ Just ty
      (args, e)     <- unbind body

      nstos         <- freshVars "sto" ms
      let stos       = toVar an <$> nstos
      e'            <- relocatingToDefn d $ purifyResBody start rho i o a stos ms e

      if defnName d /= start
            then pure $ d { defnPolyTy = [] |-> pure_ty, defnBody = Embed $ bind (args <> (snd <$> nstos)) e' }
            else do
                  -- start (which per checkStartType has no parameters of its
                  -- own) becomes $Pure.start, re-tied via unfold in mkStart.
                  -- To match unfold's type it takes the initial stores as a
                  -- single tuple, which ToHyle evaluates with a zero argument:
                  -- those stores are dead unless start pauses before
                  -- extruding, since stateless code can't inspect them.
                  s0 <- freshVar "s0"
                  let s0Ty = tupleTy an ms
                  b  <- mkLet an (mkTuplePat an $ patVar <$> nstos) (Var an Nothing (Just s0Ty) s0) e'
                  pure $ d { defnName   = s2n "$Pure.start"
                           , defnPolyTy = [] |-> mkArrowTy [s0Ty] (codomTy pure_ty)
                           , defnBody   = Embed $ bind (args <> [s0]) b
                           }
      where Embed body = defnBody d
            an         = defnAnnote d

---------------------------
-- Purifying Types
---------------------------

data TyVariety = Arrow !Ty !Ty | ReacTApp | StateTApp | IdApp | PairApp !Ty !Ty | Pure

purifyTy :: MonadError AstError m => Annote -> [Ty] -> Maybe Ty -> m Ty
purifyTy an _ Nothing   = failInternal an "purifyTy: encountered untyped expression (rwc bug)."
purifyTy an ms (Just t) = case classifyTy t of
      Arrow t1 t2   -> TyApp an <$> (TyApp an (TyCon an $ s2n "->") <$> purifyTy an ms (Just t1)) <*> purifyTy an ms (Just t2)
      ReacTApp      -> maybe (failInternal an $ "failed to purify ReacT type: " <> prettyPrint t) pure
                                    $ purifyResTy t
      StateTApp     -> maybe (failInternal an $ "failed to purify StateT type: " <> prettyPrint t) pure
                                    $ purifyStateTTy ms t
      PairApp t1 t2 -> TyApp an <$> (TyApp an (TyCon an $ s2n "(,)") <$> purifyTy an ms (Just t1)) <*> purifyTy an ms (Just t2)
      Pure          -> pure t
      IdApp         -> failInternal an "purifyTy: encountered Identity."

            -- This takes a Ty of the form
            -- >    T1 -> T2 -> ... -> Tn -> ReacT In Out (StateT S1 (StateT S2 (... (StateT Sm I)))) T
            -- and returns a Ty of the form
            -- >    T1 -> T2 -> ... -> Tn -> S1 -> ... -> Sm -> PuRe (S1, (..., Sm)) Out
      where purifyResTy :: Ty -> Maybe Ty
            purifyResTy t = do
                  (_, o, _, _) <- dstReacT $ codomTy t
                  pure $ mkArrowTy (paramTys t <> ms) $ mkRangeTy o ms

            -- Takes
            -- >    T1 -> T2 -> ... -> Tn -> StateT S1 (StateT S2 (... (StateT Sm I))) T
            -- and replaces it by
            -- >    T1 -> ... -> Tn -> S1 -> ... -> Sm -> (T, (S1, (..., Sm)))
            --
            -- I'm going to rewrite this to stick precisely to Adam's description.
            -- N.b., the product in the codomain associates to the right. The
            -- definition of purifyStateTTy required a little hackery to get the result
            -- into that form.
            purifyStateTTy :: [Ty] -> Ty -> Maybe Ty
            purifyStateTTy ms t = do
                  a <- snd <$> dstTyApp (codomTy t)
                  pure $ mkArrowTy (paramTys t <> ms) $ tupleTy (MsgAnnote "Purify: purifyStateTTy") $ a : ms

            classifyTy :: Ty -> TyVariety
            classifyTy = \ case
                  t | isReacT t                                                  -> ReacTApp
                  TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t1) t2                -> Arrow t1 t2
                  TyApp _ (TyApp _ (TyCon _ (n2s -> "(,)")) t1) t2               -> PairApp t1 t2
                  TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "StateT")) _) _) _  -> StateTApp
                  TyApp _ (TyCon _ (n2s -> "Identity")) _                        -> IdApp
                  _                                                              -> Pure

---------------------------
-- Purifying State Monadic definitions
---------------------------

classifyCases :: (Fresh m, MonadError AstError m) => Exp -> m Cases
classifyCases ex = case flattenApp ex of
      (Builtin an _ t Get       , [])     -> pure $ CGet an t
      (Builtin an _ t Return    , [e])    -> pure $ CReturn an t e
      (Builtin an _ _ Put       , [e])    -> pure $ CPut an e
      (Builtin an _ _ Lift      , [e])    -> pure $ CLift an e
      (Builtin an _ _ Bind      , [e, g]) -> pure $ CBind an e g
      (Match an _ _ disc p e els, [])     -> pure $ CMatch an disc p e els
      (Case an _ _ disc bnd els , [])     -> pure $ CCase an disc bnd els
      (Builtin an _ _ Error     , _)      -> failAt an "encountered unsynthesizable definition."
      (Var an _ t g             , es)     -> pure $ CApply an t g es
      d                                   -> failInternal (ann ex) $ "unclassifiable case: " <> prettyPrint d

data Cases = CGet Annote !(Maybe Ty)
           | CReturn Annote !(Maybe Ty) !Exp
           | CLift Annote !Exp
           | CPut Annote !Exp
           | CBind Annote !Exp !Exp
           | CApply Annote !(Maybe Ty) !(Name Exp) ![Exp]
           | CMatch Annote !Exp !MatchPat !Exp !(Maybe Exp)
           | CCase Annote !Exp !(Bind Pat Exp) !(Maybe Exp)

-- | purifyStateBody
--   rho  -- pure environment
--   stos -- state vars
--   stys -- state types
--   i    -- lifting "depth"
--   tm   -- term to be purified
purifyStateBody :: (Fresh m, MonadError AstError m) =>
                     PureEnv -> [Exp] -> [Ty] -> Int -> Exp -> m Exp
purifyStateBody rho stos stys i = classifyCases >=> \ case
      CGet an _        -> do
            s <- maybe (failInternal an $ "state-layer mismatch: length stos == " <> showt (length stos) <> ", i == " <> showt i) pure
                  $ stos `atMay` i
            pure $ mkTuple an $ s : stos

      CReturn an _ e   -> pure $ mkTuple an $ e : stos

      CLift _ e        -> purifyStateBody rho stos stys (i + 1) e

      CPut an e        -> mkTuple an . (nil :) <$> replaceAtIndex an i e stos

      CApply an _ n es -> mkPureApp an rho n $ es <> stos

      -- disc must be simply-typed, so don't purify it.
      CMatch an disc mp e els -> do
            p  <- transMPat mp
            e' <- purifyStateBody rho stos stys i $ mkApp an e $ toVar an <$> patVars p
            mkCase an disc (p, e') <$> mapM (purifyStateBody rho stos stys i) els

      -- disc must be simply-typed, so don't purify it.
      CCase an disc bnd els -> do
            (p, e) <- unbind bnd
            e'     <- purifyStateBody rho stos stys i e
            els'   <- mapM (purifyStateBody rho stos stys i) els
            pure $ Case an Nothing (typeOf e') disc (bind p e') els'

      CBind an e g -> do
            a           <- maybe (failInternal (ann e) "invalid type in bind") pure $ typeOf e >>= (fmap snd . dstTyApp)
            ns          <- freshVars "st" $ a : stys
            (f, es)     <- dstApp g
            g_pure_app  <- mkPureApp an rho f $ es <> (toVar an <$> ns)
            e'          <- purifyStateBody rho stos stys i e
            let p        = mkTuplePat an $ patVar <$> ns
            mkLet an p e' g_pure_app

      where replaceAtIndex :: MonadError AstError m => Annote -> Int -> a -> [a] -> m [a]
            replaceAtIndex an n item ls = case splitAt n ls of
                  (a, _ : b) | n >= 0 -> pure $ a <> (item : b)
                  _                   -> failInternal an "replaceAtIndex: invalid index (this should never happen)"

---------------------------
-- Purifying Resumption Monadic definitions
---------------------------

-- It may be that the type for each subexpression occurring in here should be
-- part of the annotation here. That is partially accomplished right now.
data RCase = RReturn Annote !Exp
           | RLift Annote !Exp
           | RVar Annote !(Maybe Ty) !(Name Exp)
           | RSignal Annote !Exp
           | RBind Annote !Exp !Exp
           | RSigK Annote !Ty !Exp !(Name Exp) ![(Exp, Ty)] -- (signal e >>= g e1 ... ek)
           | RExtrude Annote !(Maybe Ty) ![Exp]                     -- [(e, t)]
           | RApp Annote !(Maybe Ty) !(Name Exp) ![Exp]
           | RMatch Annote !Exp !MatchPat !Exp !(Maybe Exp)
           | RCaseE Annote !Exp !(Bind Pat Exp) !(Maybe Exp)

instance TextShow RCase where
      showb = \ case
            RReturn  {} -> fromText "RReturn"
            RLift    {} -> fromText "RLift"
            RVar     {} -> fromText "RVar"
            RSignal  {} -> fromText "RSignal"
            RBind    {} -> fromText "RBind"
            RSigK    {} -> fromText "RSigK"
            RExtrude {} -> fromText "RExtrude"
            RApp     {} -> fromText "RApp"
            RMatch   {} -> fromText "RMatch"
            RCaseE   {} -> fromText "RCaseE"

classifyRCases :: (Fresh m, MonadError AstError m) => Exp -> m RCase
classifyRCases ex = case flattenApp ex of
      (Builtin an _ _ Bind       , [sig -> Just s, flattenApp -> (Var _ _ (Just t) g', es)])
                                           -> pure $ RSigK an t s g' $ zip es $ paramTys t
      (Builtin an _ _ Bind       , [e, g]) -> pure $ RBind an e g
      (Builtin an _ _ Return     , [e])    -> pure $ RReturn an e
      (Builtin an _ _ Lift       , [e])    -> pure $ RLift an e
      (Builtin an _ _ Signal     , [e])    -> pure $ RSignal an e
      (Builtin an _ t Extrude    , es)     -> pure $ RExtrude an t es
      (Builtin an _ _ Error      , _)      -> failAt an "encountered unsynthesizable definition."
      (Match an _ _ disc p e els , [])     -> pure $ RMatch an disc p e els
      (Case an _ _ disc bnd els  , [])     -> pure $ RCaseE an disc bnd els
      (Var an _ t x              , [])     -> pure $ RVar an t x
      (Var an _ t x              , es)     -> pure $ RApp an t x es
      d                                  -> failInternal (ann ex) $ "unclassifiable R-case: " <> prettyPrint d
      where sig :: Exp -> Maybe Exp
            sig ex = case flattenApp ex of
                  (Builtin _ _ _ Signal, [arg]) -> pure arg
                  _                             -> Nothing

-- state for res-purification.
data PSto = PSto !(HashMap (Name Exp) ResPoint) !(Name Exp) !(HashMap Ty (Name DataConId))
type ResPoint = (DataCon, (Pat, Exp))

-- | purifyResBody
--  rho    -- pure environment
--  i      -- input type
--  o      -- output type
--  t      -- return type of monad (I'm assuming it's pure)
--  stys   -- state types [S1, ..., Sm]
--  stos   -- input states [s1, ..., sm]
--  tm     -- term to be purified
purifyResBody :: (Fresh m, MonadError AstError m)
              => Name Exp -> PureEnv -> Ty -> Ty -> Ty -> [Exp] -> [Ty] -> Exp -> StateT PSto m Exp
purifyResBody start rho i o a stos ms = classifyRCases >=> \ case
      -- purifyResBody (return e)         = "Done (A_ e, (s1, (..., sm)))"
      RReturn _ e | Just t <- typeOf e -> mkLeft "RReturn" e t stos
      RReturn an _                     -> failInternal an "purifyResBody: untyped expression encountered (rwc bug)."

      -- purifyResBody (signal e
      --        >>= g e1 ... ek) = "Pause (e, (R_g e1 ... ek, (s1, (..., sm))))"
      --           Side effect:
      --     * add the clause "R_g T1 ... Tk" to R
      --     (where Ti is the type of ei)
      --     * add the eqn
      --         dispatch ((R_g e1 ... ek), (s1, ..., sm)) i
      --      = g_pure e1 ... ek i s1 ... sm
      -- to defn of dispatch
      RSigK an _ e g bes -> do
            let (es, ts) = unzip bes
                r_g      = s2n $ "R_" <> prettyPrint g

            ts' <- mapM (purifyTy an ms . Just) ts

            (p, xs) <- mkRPat an ts' r_g           -- Pattern (R_g e1 ... ek)
            ns      <- freshVars "s" ms
            let pairpat = mkTuplePat an $ p : (patVar <$> ns) -- Pattern (R_g e1 ... ek, (s1, ..., sn))

            let svars = uncurry (Var an Nothing . Just) <$> ns  -- [s1, ..., sn]
            let vars  = zipWith (flip $ Var an Nothing . Just) xs ts'   -- [x1, ..., xn]
            iv <- getI
            g_pure_app <- mkPureApp an rho g $ vars <> (Var an Nothing (Just i) iv : svars)
            -- "dispatch (R_g e1 ... ek, (s1, ..., sn)) i = g_pure e1 ... ek i s1 ... sn"
            addResPoint g (mkRDataCon an r_g ts') (pairpat, g_pure_app) -- "R_g T1 ... Tk"

            let rGes  = mkApp an (Con an Nothing (Just $ mkArrowTy ts' rTy) r_g) es
            let outer = mkTuple an $ [e, rGes] <> stos
            pure $ mkRight outer             -- Pause (e, (R_g e1 ... ek, (s1, (..., sm))))

      -- purifyResBody (signal e)         = "Pause (e, (R_return, (s1, (..., sm))))"
      RSignal an e -> do
            addNakedSignal an
            pure $ mkRight $ mkTuple an $ [e, Con an Nothing (Just rTy) $ s2n "R_return"] <> stos

      -- purifyResBody (e >>= g)          = "let
      --               Done (A_ v, (s1, (..., sm))) = [|purifyResBody e|]
      --             in
      --               g_pure v s1 ... sm"
      RBind an e g -> do
            -- The Done-only pattern below is sound only if e cannot pause.
            -- normalizeBind establishes this by inlining reaction calls on
            -- bind left-hand sides; check it here rather than assume it.
            classifyRCases e >>= \ case
                  RReturn {} -> pure ()
                  RLift {}   -> pure ()
                  c          -> failAt (ann e)
                        $ "the left-hand side of this bind (" <> showt c
                        <> ") might pause: a signal result may only be consumed by a function call, and loops and extrude must be in tail position."
            -- purify the types of e and g
            tg'         <- purifyTy an ms $ typeOf g
            -- ert is the return type of e; ty is the type of the whole expression
            (ert, _)    <- maybe (failInternal (ann tg') "expecting arrow, encountered non-arrow") pure
                              $ dstArrow tg'
            e'          <- purifyResBody start rho i o ert stos ms e

            -- start calculating pattern: p = Done (A_ v, (s1, (..., sm)))
            svars       <- freshVars "s" ms
            v           <- freshVar "v"
            p           <- mkLeftPat "RBind" (patVar (ert, v)) ert (patVar <$> svars)
            -- done calculating p = Done (A_ v, (s1, (..., sm)))

            -- calculating g_pure_app = "g_pure v s1 ... sm"
            let vars     = toVar an <$> svars
            (f, es)     <- dstApp g
            g_pure_app  <- mkPureApp an rho f $ es <> (Var an Nothing (Just ert) v : vars)
            -- done calculating g_pure_app
            mkLet an p e' g_pure_app

          -- N.b., the pure env rho will have to contain *all* purified bindings
          --       or this will fail.
      RLift an e -> do
            e'    <- purifyStateBody rho stos ms 0 e
            s_i   <- freshVars "s" ms
            v     <- freshVar "v"
            -- the pattern "(v, (s1, (..., sm)))"
            let p  = mkTuplePat an $ patVar <$> (a, v) : s_i
            body  <- mkLeft "RLift" (Var an Nothing (Just a) v) a $ toVar an <$> s_i

            mkLet an p e' body

      -- A tail call to start becomes a call to $Pure.start, which takes its
      -- stores as a single tuple (see purifyResDefn).
      RVar an _ x | x == start -> pure $ mkApp an (Var an Nothing (Just $ mkArrowTy [tupleTy an ms] $ mkRangeTy o ms) $ s2n "$Pure.start") [mkTuple an stos]
      RVar an tx x -> do
            tx' <- purifyTy an ms tx
            pure $ mkApp an (Var an Nothing (Just tx') x) stos

      -- extrude :: Monad m => ReacT i o (StateT s m) a -> s -> ReacT i o m a
      --
      -- To purify e = (extrude ... (extrude phi s1) ... sn):
      -- 1. N.b., phi :: ReacT i o (StateT s m) a. We'll assume that e is "maximal". I.e.,
      --    that you have identified how many times extrude has been applied and its arguments s1, ..., sn.
      -- 2. phi' <- purifyResBody phi
      -- 3. make definition and add it to definitions: $LL = phi'
      -- 4. return $ (...($LL s1)...sn)
      RExtrude an t rands -> case flattenExtr $ mkApp an (Builtin an Nothing t Extrude) rands of
            (Var an' tan tx d, sts) -> do
                  checkFullExtrusion an' sts
                  tx' <- purifyTy an' ms tx
                  pure $ mkApp an' (Var an' tan (Just tx') d) sts
            (e@App {}, sts)         -> do
                  checkFullExtrusion (ann e) sts
                  (f, es) <- dstApp e
                  mkPureApp (ann e) rho f $ es <> sts
            (e, _)                  -> failInternal (ann e) $ "extruded device is non-variable: " <> prettyPrint e

      RApp an rty rator rands -> do
            rator' <- purifyResBody start rho i o a stos ms (Var an Nothing rty rator)
            -- N.b., don't think it's necessary to purify the rands because
            -- they're simply typed.
            (f, stos') <- dstApp rator'
            mkPureApp an rho f $ rands <> stos'

       -- disc must be simply-typed, so don't purify it.
      RMatch an disc mp e els -> do
            p  <- transMPat mp
            e' <- purifyResBody start rho i o a stos ms $ mkApp an e $ toVar an <$> patVars p
            mkCase an disc (p, e') <$> mapM (purifyResBody start rho i o a stos ms) els

       -- disc must be simply-typed, so don't purify it.
      RCaseE an disc bnd els -> do
            (p, e) <- unbind bnd
            e'     <- purifyResBody start rho i o a stos ms e
            els'   <- mapM (purifyResBody start rho i o a stos ms) els
            pure $ Case an Nothing (typeOf e') disc (bind p e') els'

      where checkFullExtrusion :: MonadError AstError m => Annote -> [Exp] -> m ()
            checkFullExtrusion an sts = unless (length sts == length ms) $ failAt an
                  $ "partial extrusion: extrude must supply initial values for all "
                  <> showt (length ms) <> " state layer(s) of the device (got " <> showt (length sts) <> ")."

            mkLeft :: (Fresh m, MonadError AstError m, MonadState PSto m) => Text -> Exp -> Ty -> [Exp] -> m Exp
            mkLeft tag e t sts = do
                  let an = ann e
                  c <- getACtor tag t
                  let anode = mkApp an (Con an Nothing (Just $ mkArrowTy [t] aTy) c) [e]
                  pure $ mkApp an (Con an Nothing (Just $ mkArrowTy [tupleTy an $ aTy : ms] $ mkRangeTy o ms) (s2n "Done")) [mkTuple an $ anode : sts]

            mkLeftPat :: (Fresh m, MonadError AstError m) => Text -> Pat -> Ty -> [Pat] -> StateT PSto m Pat
            mkLeftPat tag p t sts = do
                  let an = ann p
                  c <- getACtor tag t
                  let anode =  PatCon an (Embed Nothing) (Embed $ Just aTy) (Embed c) [p]
                  pure $ PatCon an (Embed Nothing) (Embed $ Just $ mkRangeTy o ms) (Embed $ s2n "Done") [mkTuplePat an $ anode : sts]

            mkRight :: Exp -> Exp
            mkRight e = mkApp (ann e) (Con (ann e) Nothing (mkArrowTy <$> (pure <$> typeOf e) <*> pure (mkRangeTy o ms)) (s2n "Pause")) [e]

            addNakedSignal :: (Fresh m, MonadError AstError m, MonadState PSto m) => Annote -> m ()
            addNakedSignal an = do
                  -- by default, must add the eqn: dispatch (R_return, ss) i = Done (A_ i, ss)
                  let r_return = s2n "R_return"

                  p_ret     <- fst <$> mkRPat an [] r_return
                  p_ret_ns  <- freshVars "s" ms
                  let p_ret' = mkTuplePat an $ p_ret : (patVar <$> p_ret_ns)

                  iv        <- getI
                  b_ret'    <- mkLeft "Signal" (Var an Nothing (Just i) iv) i $ uncurry (Var an Nothing . Just) <$> p_ret_ns
                  addResPoint r_return (mkRDataCon an r_return []) (p_ret', b_ret')

            getI :: MonadState PSto m => m (Name Exp)
            getI = get >>= (\ (PSto _ iv _) -> pure iv)

            addResPoint :: MonadState PSto m => Name Exp -> DataCon -> (Pat, Exp) -> m ()
            addResPoint gid dc ps = modify (\ (PSto pts iv as) -> PSto (Map.insert gid (dc, ps) pts) iv as)

            getACtor :: (Fresh m, MonadState PSto m) => Text -> Ty -> m (Name DataConId)
            getACtor s t = do
                  PSto _ _ as <- get
                  case Map.lookup (unAnn t) as of
                        Nothing -> do
                              c <- freshVar $ "A_" <> s <> showt (Map.size as)
                              -- TODO(chathhorn): the num postfixed by Fresh is dropped in toCore because ctors assumed unique.
                              modify (\ (PSto pes iv as) -> PSto pes iv $ Map.insert (unAnn t) c as)
                              pure c
                        Just c -> pure c

dispatchTy :: Ty -> Ty -> [Ty] -> Ty
dispatchTy i o ms = mkArrowTy [domTy, i] etor
    where etor  = mkRangeTy o ms
          domTy = tupleTy (MsgAnnote "Purify: dispatchTy") $ rTy : ms

-- | Global constant representation of R data type.
rTy :: Ty
rTy = TyCon (MsgAnnote "Purify: rTy") (s2n "R_")

aTy :: Ty
aTy = TyCon (MsgAnnote "Purify: aTy") (s2n "A_")

patVar :: (Ty, Name Exp) -> Pat
patVar = toPatVar (MsgAnnote "Purify: patVar")

mkRangeTy :: Ty -> [Ty] -> Ty
mkRangeTy o ts = mkEitherTy (tupleTy (MsgAnnote "Purify: mkRangeTy") ts) o

mkEitherTy :: Ty -> Ty -> Ty
mkEitherTy t1 = TyApp (MsgAnnote "Purify: mkEitherTy") (TyApp (MsgAnnote "Purify: mkEitherTy") (TyCon (MsgAnnote "Purify: mkEitherTy") $ s2n "PuRe") t1)

mkRPat :: Fresh m => Annote -> [Ty] -> Name DataConId -> m (Pat, [Name Exp])
mkRPat an ts r_g = do
      xs <- freshVars "store" ts
      let varpats = toPatVar an <$> xs
      pure (PatCon an (Embed Nothing) (Embed $ Just $ TyCon an $ s2n "R_") (Embed r_g) varpats, snd <$> xs)

mkPureVar :: MonadError AstError m => Annote -> PureEnv -> Name Exp -> m Exp
mkPureVar an rho x = Var an Nothing <$> (Just <$> lookupPure an x rho) <*> pure x

mkPureApp :: MonadError AstError m => Annote -> PureEnv -> Name Exp -> [Exp] -> m Exp
mkPureApp an rho rator es = flip (mkApp an) es <$> mkPureVar an rho rator

dstApp :: MonadError AstError m => Exp -> m (Name Exp, [Exp])
dstApp e = case flattenApp e of
      (Var _ _ _ n, es) -> pure (n, es)
      _                 -> failInternal (ann e) $ "tried to dst non-app: " <> showt (unAnn e)

-- | Lets are desugared already, so use a case instead (with lifted discriminator).
mkLet :: Fresh m => Annote -> Pat -> Exp -> Exp -> m Exp
mkLet an p e1 e2 = do
      v <- freshVar "disc"
      -- Need to lift the discriminator.
      pure $ mkApp an (Lam an Nothing (arrowLeft <$> typeOf e1) $ bind v $ mkCase an (Var an Nothing (typeOf e1) v) (p, e2) Nothing) [e1]
