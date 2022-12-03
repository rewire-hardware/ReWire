{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module ReWire.Crust.Purify (purify) where

import safe ReWire.Annotation
import safe ReWire.Crust.Syntax
import safe ReWire.Crust.TypeCheck (unify')
import safe ReWire.Error
import safe ReWire.Pretty
import safe ReWire.Unbound (Fresh (..), s2n, n2s)

import safe Control.Arrow (first, second, (&&&))
import safe Control.Monad.State
import safe Data.List (find, foldl', isSuffixOf)
import safe Data.Either (partitionEithers)
import safe Data.Maybe (fromMaybe, catMaybes)
import safe Data.Bool (bool)
import safe Data.Text (Text)

import safe Data.Set (Set, singleton, insert)
import safe Data.Map.Strict (Map)
import safe qualified Data.Map.Strict     as Map

import TextShow (TextShow (..), fromText)

atMay :: (Eq i, Num i) => [a] -> i -> Maybe a
atMay []       _ = Nothing
atMay (x : _)  0 = Just x
atMay (_ : xs) n = atMay xs $ n - 1

freshVar :: Fresh m => Text -> m (Name a)
freshVar = fresh . s2n

freshVars :: Fresh m => Text -> [b] -> m [(Name a, b)]
freshVars v = mapM (\ (i, b) -> (, b) <$> freshVar (v <> showt i)) . zip [0 :: Int ..]

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
purify :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) => Text -> FreeProgram -> m FreeProgram
purify start (ts, syns, ds) = do
      (smds, notSmds)     <- partitionEithers <$> mapM isStateMonadicDefn ds
      (rmds, ods)         <- partitionEithers <$> mapM isResMonadicDefn notSmds

      maybe (failAt noAnn $ "No definition for start function (" <> start <> ") found!")
            (projDefnTy >=> checkStartType)
            $ find (isStart . defnName) rmds

      let nameNpolyS = map (defnName &&& defnPolyTy) smds
          nameNpolyR = map (defnName &&& defnPolyTy) rmds

      -- TODO(chathhorn): perhaps move to a type-checking phase?
      (i, o, ms, _) <- getCanonReTy rmds >>= liftMaybe noAnn "Inconsistent ReT types (i, o, and state layers must be the same for all ReT)."

      rhoS      <- mkPureEnv ms nameNpolyS
      rhoR      <- mkPureEnv ms nameNpolyR
      let rho = rhoS ++ rhoR

      pure_smds <- mapM (purifyStateDefn rho ms) smds

      iv <- freshVar "i"

      (pure_rmds, PSto points _ allAs) <- runStateT (mapM (purifyResDefn start rho ms) rmds) $ PSto mempty iv mempty

      let (dcs, pes) = unzip $ Map.elems points

      disp <- mkDispatch i o ms iv pes

      pure ( filter (not . isAorR) ts ++ [mkRDatatype dcs] ++ [mkADatatype $ Map.toList allAs]
           , syns
           , mkStart start i o ms : disp : ods ++ pure_smds ++ pure_rmds
           )

      where getCanonReTy :: Fresh m => [Defn] -> m (Maybe (Ty, Ty, [Ty], Set Ty))
            getCanonReTy = (getCanonReTy' . catMaybes <$>) . mapM (projDefnTy >=> pure . dstReT . rangeTy)

            getCanonReTy' :: [(Ty, Ty, [Ty], Ty)] -> Maybe (Ty, Ty, [Ty], Set Ty)
            getCanonReTy' = \ case
                  []                  -> Nothing
                  (i, o, ms, a) : rs  -> foldr mergeReTys (Just (i, o, ms, singleton a)) rs

            mergeReTys :: (Ty, Ty, [Ty], Ty) -> Maybe (Ty, Ty, [Ty], Set Ty) -> Maybe (Ty, Ty, [Ty], Set Ty)
            mergeReTys (i, o, ms, a) = \ case
                  Just (i', o', ms', a')
                        | length ms >= length ms'
                        , isSuffixOf ms' ms
                        , Just ui <- unify' i i', Just uo <- unify' o o'
                              -> Just (ui, uo, ms, insert a a')
                        | length ms < length ms'
                        , isSuffixOf ms ms'
                        , Just ui <- unify' i i', Just uo <- unify' o o'
                              -> Just (ui, uo, ms', insert a a')
                  _           -> Nothing

            isAorR :: DataDefn -> Bool
            isAorR = uncurry (||) . ((== "A_") &&& (== "R_")) . n2s . dataName

            isStart :: Name Exp -> Bool
            isStart = (== start) . n2s

            checkStartType :: MonadError AstError m => Ty -> m ()
            checkStartType t = case dstReT t of
                  Just (_, _, [], _) -> pure ()
                  Just (_, _,  _, _) -> failAt (ann t) "Start definition must have type ReT i o I (). Use the extrude function to remove state layers."
                  _                  -> failAt (ann t) "Start definition must have type ReT i o I ()."

-- | In addition to all the above, we re-tie the recursive knot by adding a new
--   "start" as follows
--         start :: ReT In Out I T
--         start = unfold dispatch start_pure
--           start :: In -> (Either T (O, R), ()) --- drop the "()"?

-- Correcting mkStart.
-- In the type of Main.start.
-- t (return type of Main.start) needs to be combined with ms (list of StT s, stores).
mkStart :: Text -> Ty -> Ty -> [Ty] -> Defn
mkStart start i o ms = Defn
      { defnAnnote = MsgAnnote "start function"
      , defnName   = s2n start
      , defnPolyTy = [] |-> startTy
      , defnAttr   = Just NoInline
      , defnBody   = appl
      }
      where etor       = mkRangeTy o ms
            resultTy   = tupleTy (MsgAnnote "Purify: mkStart: resultTy") $ aTy : ms

            reT i o a  = TyCon (MsgAnnote "Purify: reT") (s2n "ReT") `tyApp` i `tyApp` o `tyApp` a
            startTy    = tycomp (MsgAnnote "Purify: startTy")
                              (reT i o (TyCon (MsgAnnote "Purify: startTy") (s2n "I")))
                              resultTy

            unfold     = Builtin (MsgAnnote "Purify: unfold") (mkArrowTy [dispatchTy i o ms, etor] startTy) Unfold
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
      App _ (App _ (Builtin _ _ Extrude) arg1) arg2 -> second (++ [arg2]) $ flattenExtr arg1
      e@(App _ _ rand)                              -> first (const e) $ flattenExtr rand
      e                                             -> (e, [])

-- | Generates the dispatch function.
-- > dispatch (R_g e1 ... ek) i = g_pure e1 ... ek i
mkDispatch :: (MonadError AstError m, Fresh m) => Ty -> Ty -> [Ty] -> Name Exp -> [(Pat, Exp)] -> m Defn
mkDispatch _ _ _  _  [] = failAt NoAnnote "Purify: empty dispatch: invalid ReWire (is recursion guarded by signal?)"
mkDispatch i o ms iv (p : pes) = do
      let ty    = dispatchTy i o ms
          domTy = tupleTy (MsgAnnote "Purify: mkDispatch: domTy") $ rTy : ms
      dsc      <- freshVar "dsc"
      let body  = Embed $ bind [dsc :: Name Exp, iv :: Name Exp] cases
          cases = mkCase an (Var an domTy dsc) p pes
          an    = MsgAnnote $ "Purify: generated dispatch function: " <> prettyPrint cases

      pure Defn
            { defnAnnote = an
            , defnName   = s2n "$Pure.dispatch"
            , defnPolyTy = [] |-> ty
            , defnAttr   = Just NoInline
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
lookupPure an x = maybe (failAt an $ "No pure binding for variable: " <> n2s x) pure . lookup x

isStateMonadicDefn :: Fresh m => Defn -> m (Either Defn Defn)
isStateMonadicDefn = \ case
      d@Defn { defnName = n } | isPrim n -> pure $ Right d
      d@Defn { defnPolyTy = Embed poly } -> bool (Right d) (Left d) . isStateMonad <$> poly2Ty poly

isResMonadicDefn :: Fresh m => Defn -> m (Either Defn Defn)
isResMonadicDefn = \ case
      d@Defn { defnName = n } | isPrim n -> pure $ Right d
      d@Defn { defnPolyTy = Embed poly } -> bool (Right d) (Left d) . isResMonad <$> poly2Ty poly

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

liftMaybe :: MonadError AstError m => Annote -> Text -> Maybe a -> m a
liftMaybe an msg = maybe (failAt an msg) pure

purifyResDefn :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) => Text -> PureEnv -> [Ty] -> Defn -> StateT PSto m Defn
purifyResDefn start rho ms d = do
      ty            <- projDefnTy d
      (i, o, _, a)  <- liftMaybe (ann d) "Purify: failed at purifyResDefn" $ dstReT $ rangeTy ty
      pure_ty       <- purifyTy (ann d) ms ty
      (args, e)     <- unbind body

      (nstos, stos) <- (map fst &&& map (mkVar an)) <$> freshVars "sto" ms
      e'            <- purifyResBody start rho i o a stos ms e

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

            isStart :: Name Exp -> Bool
            isStart = (== start) . n2s

---------------------------
-- Purifying Types
---------------------------

data TyVariety = Arrow !Ty !Ty | ReTApp | StTApp | IdApp | PairApp !Ty !Ty | Pure

purifyTy :: MonadError AstError m => Annote -> [Ty] -> Ty -> m Ty
purifyTy an ms t = case classifyTy t of
      Arrow t1 t2   -> TyApp an <$> (TyApp an (TyCon an $ s2n "->") <$> purifyTy an ms t1) <*> purifyTy an ms t2
      ReTApp        -> liftMaybe an ( "Purify: failed to purify ResT type: " <> prettyPrint t)
                                    $ purifyResTy t
      StTApp        -> liftMaybe an ( "Purify: failed to purify StT type: " <> prettyPrint t)
                                    $ purifyStTTy ms t
      PairApp t1 t2 -> TyApp an <$> (TyApp an (TyCon an $ s2n "(,)") <$> purifyTy an ms t1) <*> purifyTy an ms t2
      Pure          -> pure t
      _             -> failAt an $ "Purify: failed to purify type: " <> prettyPrint t

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
                  pure $ mkArrowTy (paramTys t ++ ms) $ tupleTy (MsgAnnote "Purify: purifyStTTy") $ a : ms

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
classifyCases ex = case flattenApp ex of
      [Builtin an t Get]        -> pure $ CGet an t
      [Builtin an t Return, e]  -> pure $ CReturn an t e
      [Builtin an _ Put, e]     -> pure $ CPut an e
      [Builtin an _ Lift, e]    -> pure $ CLift an e
      [Builtin an _ Bind, e, g] -> pure $ CBind an e g
      [Match an _ e1 mp e2 me]  -> pure $ CMatch an e1 mp e2 me
      [Error a _ _]             -> failAt a "Purify: encountered unsynthesizable definition."
      Var an t g : es           -> pure $ CApply an t g es
      d                         -> failAt (ann ex) $ "Purify: unclassifiable case: " <> showt d

data Cases an p e t = CGet an !t
                    | CReturn an !t !e
                    | CLift an !e
                    | CPut an !e
                    | CBind an !e !e
                    | CApply an !t !(Name Exp) ![e]
                    | CMatch an !e !MatchPat !e !(Maybe e)

-- | purifyStateBody
--   rho  -- pure environment
--   stos -- state vars
--   stys -- state types
--   i    -- lifting "depth"
--   tm   -- term to be purified
purifyStateBody :: (Fresh m, MonadError AstError m, MonadIO m) =>
                     PureEnv -> [Exp] -> [Ty] -> Int -> Exp -> m Exp
purifyStateBody rho stos stys i = classifyCases >=> \ case
      CGet an _        -> do
            s <- liftMaybe an ("Purify: state-layer mismatch: length stos == " <> showt (length stos) <> ", i == " <> showt i)
                  $ stos `atMay` i
            pure $ mkTuple an $ s : stos

      CReturn an _ e   -> pure $ mkTuple an $ e : stos

      CLift _ e        -> purifyStateBody rho stos stys (i + 1) e

      CPut an e        -> pure $ mkTuple an $ nil : replaceAtIndex i e stos

      CApply an t n es -> mkPureApp an rho t n $ es ++ stos

      -- e1 must be simply-typed, so don't purify it.
      CMatch an e1 mp e2 e3 -> do
            p <- transPat mp
            e2' <- purifyStateBody rho stos stys i $ mkApp an e2 $ map (mkVar an) (patVars p)
            Case an (typeOf e2') e1 (bind p e2')  <$> maybe' (purifyStateBody rho stos stys i <$> e3)

      CBind an e g -> do
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
      MatchPatWildCard an t -> pure $ PatWildCard an (Embed t)

patVars :: Pat -> [(Name Exp, Ty)]
patVars = \ case
      PatCon _ _ _ ps      -> concatMap patVars ps
      PatVar _ (Embed t) x -> [(x, t)]
      PatWildCard _ _      -> []

---------------------------
-- Purifying Resumption Monadic definitions
---------------------------

-- It may be that the type for each subexpression occurring in here should be
-- part of the annotation here. That is partially accomplished right now.
data RCase an t p e = RReturn an !e
                    | RLift an !e
                    | RVar an !t !(Name e)
                    | RSignal an !e
                    | RBind an !e !e
                    | RSigK an !t !e !(Name e) ![(e, t)] -- (signal e >>= g e1 ... ek)
                    | RExtrude an !t ![e]                -- [(e, t)]
                    | RApp an !t !(Name e) ![e]
                    | RMatch an !e !MatchPat !e !(Maybe e)

instance TextShow (RCase an t p e) where
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

classifyRCases :: (Fresh m, MonadError AstError m) => Exp -> m (RCase Annote Ty Pat Exp)
classifyRCases ex = case flattenApp ex of
      [Match an _ e1 mp e2 me]      -> pure $ RMatch an e1 mp e2 me
      [TypeAnn _ _ e]               -> classifyRCases e
      [Error a _ _]                 -> failAt a "Purify: encountered unsynthesizable definition."
      Builtin an _ Return  : [e]    -> pure $ RReturn an e
      Builtin an _ Lift    : [e]    -> pure $ RLift an e
      Builtin an _ Signal  : [e]    -> pure $ RSignal an e
      Builtin an _ Bind    : [sig -> Just s, flattenApp -> Var _ t g' : es]
                                    -> pure $ RSigK an t s g' $ zip es $ paramTys t
      Builtin an _ Bind    : [e, g] -> pure $ RBind an e g
      Builtin an t Extrude : es     -> pure $ RExtrude an t es
      [Var an t x]                  -> pure $ RVar an t x
      Var an t x           : es     -> pure $ RApp an t x es
      d                             -> failAt (ann ex) $ "Purify: unclassifiable R-case: " <> showt d
      where sig :: Exp -> Maybe Exp
            sig ex = case flattenApp ex of
                  [Builtin _ _ Signal, arg] -> pure arg
                  _                         -> Nothing

-- state for res-purification.
data PSto = PSto !(Map (Name Exp) ResPoint) !(Name Exp) !(Map Ty (Name DataConId))
type ResPoint = (DataCon, (Pat, Exp))

-- | purifyResBody
--  rho    -- pure environment
--  i      -- input type
--  o      -- output type
--  t      -- return type of monad (I'm assuming it's pure)
--  stys   -- state types [S1, ..., Sm]
--  stos   -- input states [s1, ..., sm]
--  tm     -- term to be purified
purifyResBody :: (Fresh m, MonadError AstError m, MonadIO m)
              => Text -> PureEnv -> Ty -> Ty -> Ty -> [Exp] -> [Ty] -> Exp -> StateT PSto m Exp
purifyResBody start rho i o a stos ms = classifyRCases >=> \ case
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
      RSigK an _ e g bes -> do
            let (es, ts) = unzip bes
            ts' <- mapM (purifyTy an ms) ts

            r_g <- freshVar $ "R_" <> prettyPrint g

            (p, xs) <- mkRPat an ts' r_g           -- Pattern (R_g e1 ... ek)
            ns      <- freshVars "s" ms
            let pairpat = mkTuplePat an $ p : map patVar ns -- Pattern (R_g e1 ... ek, (s1, ..., sn))

            let svars = map (uncurry $ flip $ Var an) ns  -- [s1, ..., sn]
            let vars  = zipWith (flip $ Var an) xs ts'   -- [x1, ..., xn]
            iv <- getI
            g_pure_app <- mkPureApp an rho a g $ vars ++ Var an i iv : svars
            -- "dispatch (R_g e1 ... ek, (s1, ..., sn)) i = g_pure e1 ... ek i s1 ... sn"
            addResPoint g (mkRDataCon an r_g ts') (pairpat, g_pure_app) -- "R_g T1 ... Tk"

            let rGes  = mkApp an (Con an (mkArrowTy ts' rTy) r_g) es
            let outer = mkTuple an $ [e, rGes] ++ stos
            pure $ mkRight outer             -- Right ((e, R_g e1 ... ek), (s1, (..., sm)))

      -- purifyResBody (signal e)         = "(Right ((e, (s1, (..., sm))), R_ret))"
      RSignal an e -> do
            addNakedSignal an
            pure $ mkRight $ mkTuple an $ [e, Con an rTy $ s2n "R_return"] ++ stos

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
            e'          <- purifyResBody start rho i o ert stos ms e

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
            body  <- mkLeft "RLift" (Var an a v) $ map (mkVar an) s_i

            mkLet an p e' body

      RVar an tx x   -> do
            tx' <- purifyTy an ms tx
            pure $ mkApp an (Var an tx' x') stos
                  where x' = if isStart x then s2n "$Pure.start" else x

      -- extrude :: Monad m => ReT i o (StT s m) a -> s -> ReT i o m a
      --
      -- To purify e = (extrude ... (extrude phi s1) ... sn):
      -- 1. N.b., phi :: ReT i o (StT s m) a. We'll assume that e is "maximal". I.e.,
      --    that you have identified how many times extrude has been applied and its arguments s1, ..., sn.
      -- 2. phi' <- purifyResBody phi
      -- 3. make definition and addit to definitions: $LL = phi'
      -- 4. return $ (...($LL s1)...sn)
      RExtrude an t rands -> case flattenExtr $ mkApp an (Builtin an t Extrude) rands of
            (Var an t d, stos) -> do
                  t'  <- purifyTy an ms t
                  pure $ mkApp an (Var an t' d) stos
            (e@App {}, stos)   -> do
                  (f, t, es) <- dstApp e
                  t'         <- purifyTy (ann e) ms t
                  pure $ mkApp (ann e) (Var (ann e) t' f) $ es ++ stos
            (e, _)             -> failAt (ann e) $ "Purify: extruded device is non-variable: " <> showt e

      RApp an rty rator rands -> do
            rator' <- purifyResBody start rho i o a stos ms (Var an rty rator)
            -- N.b., don't think it's necessary to purify the rands because
            -- they're simply typed.
            (f, t, stos') <- dstApp rator'
            mkPureApp an rho t f $ rands ++ stos'

       -- e1 must be simply-typed, so don't purify it.
      RMatch an e1 mp e2 e3 -> do
            p <- transPat mp
            e2' <- purifyResBody start rho i o a stos ms $ mkApp an e2 $ map (mkVar an) (patVars p)
            Case an (typeOf e2') e1 (bind p e2') <$> maybe' (purifyResBody start rho i o a stos ms <$> e3)

      where mkLeft :: (Fresh m, MonadError AstError m, MonadState PSto m) => Text -> Exp -> [Exp] -> m Exp
            mkLeft s a stos = do
                  let an = ann a
                  let t = typeOf a
                  c <- getACtor s t
                  let anode = App an (Con an (mkArrowTy [t] aTy) c) a
                  pure $ App an (Con an (mkArrowTy [tupleTy an $ aTy : ms] $ mkRangeTy o ms) (s2n "Done")) (mkTuple an $ anode : stos)

            mkLeftPat :: (Fresh m, MonadError AstError m) => Text -> Pat -> [Pat] -> StateT PSto m Pat
            mkLeftPat s a stos = do
                  let an = ann a
                  let t = typeOf a
                  c <- getACtor s t
                  let anode =  PatCon an (Embed aTy) (Embed c) [a]
                  pure $ PatCon an (Embed $ mkRangeTy o ms) (Embed $ s2n "Done") [mkTuplePat an $ anode : stos]

            mkRight :: Exp -> Exp
            mkRight e = App (ann e) (Con (ann e) (mkArrowTy [typeOf e] $ mkRangeTy o ms) (s2n "Pause")) e

            addNakedSignal :: (Fresh m, MonadError AstError m, MonadState PSto m) => Annote -> m ()
            addNakedSignal an = do
                  -- by default, must add the eqn: dispatch R_return i = Left i
                  let r_return = s2n "R_return"

                  p_ret     <- fst <$> mkRPat an [] r_return
                  p_ret_ns  <- freshVars "s" ms
                  let p_ret' = mkTuplePat an $ p_ret : map patVar p_ret_ns

                  iv        <- getI
                  b_ret'    <- mkLeft "Signal" (Var an i iv) $ map (uncurry $ flip $ Var an) p_ret_ns
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

            isStart :: Name Exp -> Bool
            isStart = (== start) . n2s

dispatchTy :: Ty -> Ty -> [Ty] -> Ty
dispatchTy i o ms = mkArrowTy [domTy, i] etor
    where etor  = mkRangeTy o ms
          domTy = tupleTy (MsgAnnote "Purify: dispatchTy") $ rTy : ms

-- | Global constant representation of R data type.
rTy :: Ty
rTy = TyCon (MsgAnnote "Purify: rTy") (s2n "R_")

aTy :: Ty
aTy = TyCon (MsgAnnote "Purify: aTy") (s2n "A_")

patVar :: (Name Exp, Ty) -> Pat
patVar (n, t) = PatVar (MsgAnnote "Purify: patVar") (Embed t) n

mkVar :: Annote -> (Name Exp, Ty) -> Exp
mkVar an (n, t) = Var an t n

mkRangeTy :: Ty -> [Ty] -> Ty
mkRangeTy o ts = mkEitherTy (tupleTy (MsgAnnote "Purify: mkRangeTy") ts) o

mkEitherTy :: Ty -> Ty -> Ty
mkEitherTy t1 = TyApp (MsgAnnote "Purify: mkEitherTy") (TyApp (MsgAnnote "Purify: mkEitherTy") (TyCon (MsgAnnote "Purify: mkEitherTy") $ s2n "PuRe") t1)

mkRPat :: Fresh m => Annote -> [Ty] -> Name DataConId -> m (Pat, [Name Exp])
mkRPat an ts r_g = do
      xs <- freshVars "store" ts
      let varpats = map (\ (x, t) -> PatVar an (Embed t) x) xs
      pure (PatCon an (Embed $ TyCon an $ s2n "R_") (Embed r_g) varpats, map fst xs)

mkPureVar :: MonadError AstError m => Annote -> PureEnv -> Ty -> Name Exp -> m Exp
mkPureVar an rho _ x = Var an <$> lookupPure an x rho <*> pure x

mkApp :: Annote -> Exp -> [Exp] -> Exp
mkApp an = foldl' $ App an

mkPureApp :: MonadError AstError m => Annote -> PureEnv -> Ty -> Name Exp -> [Exp] -> m Exp
mkPureApp an rho t rator es = flip (mkApp an) es <$> mkPureVar an rho t rator

dstApp :: MonadError AstError m => Exp -> m (Name Exp, Ty, [Exp])
dstApp e = case flattenApp e of
      Var _ t n : es -> pure (n, t, es)
      d              -> failAt (ann e) $ "Purify: tried to dst non-app: " <> showt (unAnn d)

-- | Lets are desugared already, so use a case instead (with lifted discriminator).
mkLet :: Fresh m => Annote -> Pat -> Exp -> Exp -> m Exp
mkLet an p e1 e2 = do
      v <- freshVar "disc"
      -- Need to lift the discriminator.
      pure $ App an (Lam an (typeOf e1) $ bind v $ Case an (typeOf e2) (Var an (typeOf e1) v) (bind p e2) Nothing) e1
