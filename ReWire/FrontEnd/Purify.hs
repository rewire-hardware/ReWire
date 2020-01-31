{-# LANGUAGE FlexibleContexts, LambdaCase, ViewPatterns, TupleSections #-}
{-# LANGUAGE Safe #-}

-- TODO(chathhorn): some of the 5 results of dstResTy seem to be unused

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
import Data.List (foldl', find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Either (partitionEithers)

-- Append to a non-empty list.
(|:) :: [a] -> a -> NonEmpty a
xs |: x = NE.reverse $ x :| reverse xs

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

-- is stuff like this kosher?
poly2Ty :: Fresh m => Poly -> m Ty
poly2Ty (Poly p) = snd <$> unbind p

purify :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) => FreeProgram -> m FreeProgram
purify (ts, ds) = do
      (smds, pds) <- partitionEithers <$> mapM isStateMonadicDefn ds
      (rmds, ods) <- partitionEithers <$> mapM isResMonadicDefn pds

      let nameNpolyS = map (defnName &&& defnPolyTy) smds
          nameNpolyR = map (defnName &&& defnPolyTy) rmds

      imprhoR   <- mapM (\ (d, Embed p) -> poly2Ty p >>= \ t -> return (d, t)) nameNpolyR

      (_, tyStart) <- liftMaybe noAnn "No start symbol!" $ find (isStart . fst) imprhoR
      (_, i, o, t) <- liftMaybe (ann tyStart) ("dstResTy applied to: " ++ show tyStart) $ dstResTy tyStart

      --- if t = (a, s), need to set t := a and stos := s
      let tupts = dstTuple t

      rhoS      <- mkPureEnv nameNpolyS $ tail tupts -- TODO(chathhorn): remove tail
      rhoR      <- mkPureEnv nameNpolyR $ tail tupts
      let rho = rhoS ++ rhoR

      pure_smds <- mapM (purifyStateDefn rho $ tail tupts) smds
      iv        <- freshVar "i"
      --
      -- The problem is that the start symbol is being passed (tail tupts) /= [] below:
      --
      (pure_rmds, PSto dcs pes _) <- runStateT (mapM (purifyResDefn rho imprhoR iv (tail tupts)) rmds) (PSto [] [] [])

      let r  = mkRDatatype dcs

      let dt        = dispatchTy i o (tail tupts) (head tupts)
          start_def = mkStart i o (tail tupts) (head tupts)

      df        <- mkDispatch dt pes iv i

      return (ts ++ [r], start_def : df : ods ++ pure_smds ++ pure_rmds)

   where dstTuple :: Ty -> [Ty] -- TODO(chathhorn): I wish this was the inverse of mkTupleTy
         dstTuple = \ case
            TyApp _ (TyApp _ (TyCon _ (n2s -> "(,)")) t1) t2 -> dstTuple t1 ++ [t2]
            -- TyCon _ (n2s -> "()")                            -> [] -- TODO(chathhorn)
            t                                                -> [t]

{-
In addition to all the above, we re-tie the recursive knot by adding a new
  "start" as follows:

        start :: ReT In Out I T
        start = unfold dispatch start_pure

          start :: In -> (Either T (O, R), ()) --- drop the "()"?
-}

-- Correcting mkStart.
--
-- In the type of Main.start.
-- t (return type of Main.start) needs to be combined with ms (list of StT s, stores).
--
--

mkStart :: Ty -> Ty -> [Ty] -> Ty -> Defn
mkStart i o ms t = Defn
      { defnAnnote = MsgAnnote "start function"
      , defnName   = s2n "Main.start"
      , defnPolyTy = [] |-> tyStart
      , defnInline = False
      , defnBody   = appl
      }
      where etor       = mkRangeTy t o ms
            ranStart   = mkPairTy etor (TyCon noAnn (s2n "()"))
            pureStart  = i `arr0` ranStart  -- seems irrelevant to the generated code.

            extresTy :: Ty -> [Ty] -> Ty
            extresTy = foldl' mkPairTy

            reT i o a  = TyCon noAnn (s2n "ReT") `tyApp` i `tyApp` o `tyApp` a
            tyStart    = tycomp noAnn (reT i o (TyCon noAnn (s2n "I"))) (extresTy t ms)

            unfold     = Var noAnn (TyBlank $ MsgAnnote "stub type for unfold") (s2n "unfold")
            dispatch   = Var noAnn (dispatchTy i o ms t) (s2n "$Pure.dispatch")
            start_pure = Var noAnn pureStart (s2n "$Pure.start")
            appl       = Embed $ bind [] (App noAnn (App noAnn unfold dispatch) start_pure)

{-
Converts
   (extrude (extrude (... (extrude dev s1) ...) s(n-1)) sn)
Into
   (dev, [s1, ..., sn])

Won't work if called on a non-extrude application.
-}
flattenExtr :: Exp -> (Exp, [Exp])
flattenExtr = \ case
      App _ (App _ (Var _ _ v) arg1) arg2
            | n2s v == "extrude" -> second (++ [arg2]) $ flattenExtr arg1
      e@(App _ _ rand)           -> first (const e) $ flattenExtr rand
      e                          -> (e, [])

tyApp :: Ty -> Ty -> Ty
tyApp = TyApp noAnn

-- dispatch (R_g e1 ... ek) i = g_pure e1 ... ek i
-- by default, must add the eqn: dispatch R_return i = Left i
mkDispatch :: (Fresh m, MonadError AstError m) => Ty -> [(Pat, Exp)] -> Name Exp -> Ty -> m Defn
mkDispatch ty pes iv _ = do
      dsc <- freshVar "dsc"
      let dscv = Var an rTy dsc
      body <- mkCase an ty dscv pes
      let dispatch_body = Embed (bind [dsc :: Name Exp, iv :: Name Exp] body)
      return Defn
            { defnAnnote = an
            , defnName   = s2n "$Pure.dispatch"
            , defnPolyTy = [] |-> ty
            , defnInline = False
            , defnBody = dispatch_body
            }

      where an :: Annote
            an = MsgAnnote "Generated dispatch function"

mkCase :: MonadError AstError m => Annote -> Ty -> Exp -> [(Pat, Exp)] -> m Exp
mkCase an ty dsc = \ case
      []           -> pure $ Error an (TyBlank an) "Something went wrong during purification while constructing the dispatch function"
      [(p, e)]     -> pure $ Case an ty dsc (bind p e) Nothing
      ((p, e):pes) -> Case an ty dsc (bind p e) . Just <$> mkCase an ty dsc pes

mkRDatatype :: [DataCon] -> DataDefn
mkRDatatype dcs = DataDefn
       { dataAnnote = MsgAnnote "R Datatype"
       , dataName   = s2n "R"
       , dataKind   = KStar
       , dataCons   = mkRDataCon noAnn (s2n "R_return") [] : dcs
       }

-- global constant representation of R data type
rTy :: Ty
rTy = TyCon noAnn (s2n "R")

type PureEnv = [(Name Exp, Ty)]

mkPureEnv :: (Fresh m, MonadError AstError m) => [(Name Exp, Embed Poly)] -> [Ty] -> m PureEnv
mkPureEnv [] _                   = return []
mkPureEnv ((n, Embed phi) : nps) ms = do
      ty     <- poly2Ty phi
      purety <- purifyTyM ty ms
      env    <- mkPureEnv nps ms
      return $ (n, purety) : env

lookupPure :: MonadError AstError m => Annote -> Name Exp -> PureEnv -> m Ty
lookupPure an x rho = case lookup x rho of
                           Just ty -> return ty
                           Nothing -> failAt an $ "No pure binding for variable: " ++ n2s x

lookupImpure :: MonadError AstError m => Annote -> Name Exp -> PureEnv -> m Ty
lookupImpure an x rho = case lookup x rho of
                           Just ty -> return ty
                           Nothing -> failAt an $ "No impure binding for variable: " ++ n2s x

isStateMonadicDefn :: Fresh m => Defn -> m (Either Defn Defn)
isStateMonadicDefn d
      | isPrim (defnName d) = return $ Right d
      | otherwise           = do
            ty <- poly2Ty poly
            return $ if isStateMonad ty then Left d else Right d
      where Embed poly = defnPolyTy d

            isStateMonad :: Ty -> Bool
            isStateMonad ty = case rangeTy ty of
                  TyApp an (TyApp _ (TyApp _ (TyCon _ (n2s -> "StT")) _) m) a -> isStateMonad (tycomp an m a)
                  TyApp _ (TyCon _ (n2s -> "I")) _                            -> True
                  _                                                           -> False

isResMonadicDefn :: Fresh m => Defn -> m (Either Defn Defn)
isResMonadicDefn d
      | isPrim (defnName d) = return $ Right d
      | otherwise           = do
            ty <- poly2Ty poly
            return $ if isResMonad ty then Left d else Right d
      where Embed poly = defnPolyTy d

            isResMonad :: Ty -> Bool
            isResMonad ty = case rangeTy ty of
                  TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReT")) _) _) _) _ -> True
                  _                                                                      -> False

purifyStateDefn :: (Fresh m, MonadError AstError m, MonadIO m) =>
                   PureEnv -> [Ty] -> Defn -> m Defn
purifyStateDefn rho ms d = do
  ty         <- poly2Ty phi
  p_pure     <- lookupPure (ann d) (defnName d) rho
  let d_pure = defnName d
  (args, e)   <- unbind body
  (_, stys, _) <- liftMaybe (ann d) "failed at purifyStateDefn" $ dstStT ty
  nstos      <- freshVars "sigma" stys
  let stos = foldr (\ (n, t) nts -> Var (ann d) t n : nts) [] nstos
  e'         <- purifyStateBody rho stos stys 0 ms e
  let b_pure = bind (args ++ map fst nstos) e'
  return $ d { defnName = d_pure, defnPolyTy = [] |-> p_pure, defnBody = Embed b_pure }
    where
      Embed body = defnBody d
      Embed phi  = defnPolyTy d

liftMaybe :: MonadError AstError m => Annote -> String -> Maybe a -> m a
liftMaybe an msg = maybe (failAt an msg) pure

purifyResDefn :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) =>
                 PureEnv -> [(Name Exp, Ty)] -> Name Exp -> [Ty] -> Defn -> StateT PSto m Defn
purifyResDefn rho imprhoR iv ms d = do
  ty         <- lookupImpure an dname imprhoR
  pure_ty    <- liftMaybe (ann d) "Failed to create pure type"
                     $ purifyResTy ty ms
  (args, e)   <- unbind body

  (_, i, o, a) <- liftMaybe (ann d) "failed at purifyResDefn" $ dstResTy ty

  -- let stys = fromMaybe [] ms

-- TODO TODO TODO
  (nstos, mst) <- do
            nstos <- freshVars "sto" ms
            let stos   = map (\ (n, t) -> Var an t n) nstos
            return (map fst nstos, (stos, ms))

  
--   mst <- do
--       stys <- ms
--             nstos <- freshVars "sto" $ length stys
--             let stos   = foldr (\ (n, t) nts -> Var an t n : nts) [] $ zip nstos $ NE.toList stys
--       return (stos, stys)
--             
--       
--       
--       
--   
--   nstos      <- freshVars "sto" (length stys)
--   let stos   = foldr (\ (n, t) nts -> Var an t n : nts) [] (zip nstos stys)
--   let mst    = case stos of
--                     [] -> Nothing
--                     _  -> Just (stos, stys)

  e'         <- purifyResBody rho i o a iv mst e

--
-- Below is an egregious hack to compile the start symbol slightly differently.
--
  let args'  = if isStart dname then [] else args
  let nstos' = if isStart dname then [] else nstos

  let b_pure = bind (args' ++ nstos') e'

  let p_pure = if isStart dname then [] |-> rangeTy pure_ty else [] |-> pure_ty
  let d_pure = if isStart dname then s2n "$Pure.start" else dname

  return $ d { defnName = d_pure, defnPolyTy = p_pure, defnBody = Embed b_pure }
    where
      dname      = defnName d
      Embed body = defnBody d
      an         = defnAnnote d

---------------------------
-- Purifying Types
---------------------------

data TyVariety t = Arrow Annote t t
                 | ReTApp Annote t t t t
                 | StTApp Annote t t t
                 | IdApp Annote t
                 | PairApp Annote t t
                 | Pure Annote t

instance Annotated (TyVariety t) where
      ann = \ case
            Arrow a _ _      -> a
            ReTApp a _ _ _ _ -> a
            StTApp a _ _ _   -> a
            IdApp a _        -> a
            PairApp a _ _    -> a --- do we need more tuples?
            Pure a _         -> a

-- TyApp an
--       (TyApp _ (TyCon _ ->) (TyVar _ (KVar ?K_o85) o33))
--(TyApp an (TyApp _ (TyApp _ (TyApp _ (TyCon _ ReT) (TyVar _ (KVar ?K_i86) i32)) (TyVar _ (KVar ?K_o87) o33)) (TyVar _ (KVar ?K_m88) m30)) (TyVar _ (KVar ?K_i89) i32))
classifyTy :: Ty -> Maybe (TyVariety Ty)
classifyTy = \ case
      TyApp an (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReT")) i) o) m) a -> pure $ ReTApp an i o m a
      TyApp an (TyApp _ (TyCon _ (n2s -> "->")) t1) t2                        -> pure $ Arrow an t1 t2
      TyApp an (TyApp _ (TyCon _ (n2s -> "(,)")) t1) t2                       -> pure $ PairApp an t1 t2
      TyApp an (TyApp _ (TyApp _ (TyCon _ (n2s -> "StT")) s) m) a             -> pure $ StTApp an s m a
      TyApp an (TyCon _ (n2s -> "I")) a                                       -> pure $ IdApp an a
      TyApp {}                                                                -> Nothing
      t                                                                       -> pure $ Pure (ann t) t

purifyTy :: Ty -> [Ty] -> Maybe Ty
purifyTy t ms = classifyTy t >>= \ case
      Arrow an t1 t2   -> TyApp an <$> (TyApp an (TyCon an $ s2n "->") <$> purifyTy t1 ms) <*> purifyTy t2 ms
      ReTApp {}        -> purifyResTy t ms
      StTApp {}        -> purifyStTTy t
      IdApp {}         -> Nothing
      PairApp a t1 t2  -> TyApp a <$> (TyApp a (TyCon a $ s2n "(,)") <$> purifyTy t1 ms) <*> purifyTy t2 ms
      Pure _ t         -> return t

purifyTyM :: MonadError AstError m => Ty -> [Ty] -> m Ty
purifyTyM t = liftMaybe (ann t) ("failed to purifyTy: " ++ prettyPrint (unAnn t)) . purifyTy t

dstTyApp :: [Ty] -> Ty -> ([Ty], Ty)
dstTyApp acc = \ case
      TyApp    _ (TyApp _ (TyCon _ (n2s -> "->")) t1) t2  -> dstTyApp (t1 : acc) t2
      t                                                   -> (reverse acc, t)

{-
   This takes a type of the form
      T1 -> T2 -> ... -> Tnd -> StT S1 (StT S2 (... (StT Sm I))) T
   and returns
      ([T1, ..., Tn], [S1, ..., Sm], T)
-}
dstStT :: Ty -> Maybe ([Ty], [Ty], Ty)
dstStT ty = do
      let (ts, smt) = dstTyApp [] ty
      (a, stos) <- dstComp smt
      return (ts, stos, a)
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

{-
   This takes a Ty of the form
        T1 -> T2 -> ... -> Tn -> ReT In Out (StT S1 (StT S2 (... (StT Sm I)))) T
   and returns a Ty of the form
        T1 -> T2 -> ... -> Tn -> In -> S1 -> ... -> Sm -> (Either T (O, R), (S1, (..., Sm)))
-}
purifyResTy :: Ty -> [Ty] -> Maybe Ty
purifyResTy t ms = do
      (ts, _, o, a) <- dstResTy t
      let ranTy        = mkRangeTy a o ms
      let purified     = mkArrowTy $ (ts ++ ms) |: ranTy
      return purified

--
-- Takes
--      T1 -> T2 -> ... -> Tn -> StT S1 (StT S2 (... (StT Sm I))) T
-- and replaces it by
--      T1 -> ... -> Tn -> S1 -> ... -> Sm -> (T, (S1, (..., Sm)))
--
-- I'm going to rewrite this to stick precisely to Adam's description.
-- N.b., the product in the codomain associates to the right. The
-- definition of purifyStTTy required a little hackery to get the result
-- into that form.
--
purifyStTTy :: Ty -> Maybe Ty
purifyStTTy t = do
      (ts, stos, a) <- dstStT t
      pure $ mkArrowTy $ (ts ++ stos) |: mkTupleTy (a : stos)

{-
   This takes a type of the form
      T1 -> T2 -> ... -> Tnd -> ReT In Out (StT S1 (StT S2 (... (StT Sm I)))) T
   and returns
      ([T1, ..., Tn], In, Out, ..., Sm], T)
-}
dstResTy :: Ty -> Maybe ([Ty], Ty, Ty, Ty)
dstResTy ty = do
      (i, o, _, a) <- dstReT rt
      return (ts, i, o, a)
      where (ts, rt) = dstTyApp [] ty

            dstReT :: Ty -> Maybe (Ty, Ty, Ty, Ty)
            dstReT = \ case
                  TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (n2s -> "ReT")) i) o) m) a -> Just (i, o, m, a)
                  _                                                                      -> Nothing

---------------------------
-- Purifying State Monadic definitions
---------------------------

classifyCases :: (Fresh m, MonadError AstError m) => Exp -> m (Cases Annote Pat Exp Ty)
classifyCases = \ case
     Var an t g | n2s g == "get"                       -> return $ Get an t
                | otherwise                            -> return $ Apply an (Var an t g) []
     App an (Var _ t x) e | n2s x == "return"          -> return $ Return an t e
                          | n2s x == "put"             -> return $ Put an e
                          | n2s x == "lift"            -> return $ Lift an e
     App an (App _ f@(Var _ t x) e) g | n2s x == ">>=" -> return $ Bind an t e g
                                      | otherwise      -> return $ Apply an f [e, g]
     t@(App an _ _)                                    -> do (f, _, es) <- dstApp t
                                                             return $ Apply an f es
     Case an t dsc bnds me                             -> do (p, e) <- unbind bnds
                                                             return (Switch an t dsc p e me)
     Match an t e1 mp e2 es me                         -> return $ SMatch an t e1 mp e2 es me
     d                                                 -> failAt (ann d) $ "Unclassifiable case: " ++ show d

data Cases an p e t = Get an t
                    | Return an t e
                    | Lift an e
                    | Put an e
                    | Bind an t e e
                    | Apply an e [e]
                    | Switch an t e p e (Maybe e)
                    | SMatch an t e MatchPat e [e] (Maybe e)

{-
   purifyStateBody
                  rho  -- pure environment
                  stos -- state vars
                  stys -- state types
                  i    -- lifting "depth"
                  tm   -- term to be purified
-}

--
-- Make sure to check for fencepost counting problems.
--
purifyStateBody :: (Fresh m, MonadError AstError m, MonadIO m) =>
                     PureEnv -> [Exp] -> [Ty] -> Int -> [Ty] -> Exp -> m Exp
purifyStateBody rho stos stys i ms tm = do
  exp <- classifyCases tm
  case exp of
    Get _ _      -> pure $ mkTuple $ (stos !! i) : stos

    Return _ _ e -> pure $ mkTuple $ e : stos

    Lift _ e     -> purifyStateBody rho stos stys (i+1) ms e

    Put an e      -> pure $ mkTuple $ nil : stos'
        where nil   = Con an nilTy (s2n "()")
              nilTy = TyCon an (s2n "()")
              stos' = replaceAtIndex (i-1) e stos

    Apply an e es -> do rator <- mkPureApp an rho e es
                        pure $ mkApp rator stos

    Switch an t e1 p e2 (Just e) -> do e1' <- purifyStateBody rho stos stys i ms e1
                                       e2' <- purifyStateBody rho stos stys i ms e2
                                       e'  <- purifyStateBody rho stos stys i ms e
                                       pure $ Case an t e1' (bind p e2') (Just e')

    Switch an t e1 p e2 Nothing  -> do e1' <- purifyStateBody rho stos stys i ms e1
                                       e2' <- purifyStateBody rho stos stys i ms e2
                                       pure $ Case an t e1' (bind p e2') Nothing

    -- e1 must be simply-typed, so don't purify it.
    SMatch an ty e1 mp e2 es Nothing -> do
         ty' <- purifyTyM ty ms
         e2' <- purifyStateBody rho stos stys i ms e2
         pure $ Match an ty' e1 mp e2' es Nothing

    SMatch an ty e1 mp e2 es (Just e3) -> do
         ty' <- purifyTyM ty ms
         e2' <- purifyStateBody rho stos stys i ms e2
         e3' <- purifyStateBody rho stos stys i ms e3
         pure $ Match an ty' e1 mp e2' es (Just e3')

    Bind an t e g -> do
      (te, _)  <- dstArrow t
      (_, _, a) <- liftMaybe (ann te) "Non-StT Type passed to dstStT" $ dstStT te -- a is the return type of e
      e'      <- purifyStateBody rho stos stys i ms e
      ns      <- freshVars "st" $ a : stys
      let vs = foldr (\ (n, t) nts -> Var an t n : nts) [] ns
      g_pure_app <- mkPureApp an rho g vs
      let p  = mkTuplePat ns
--
-- g can, in general, be an application. That's causing the error when (var2name g) is called.
--
      (f, _, _) <- dstApp g
      gn      <- var2name f -- var2name g
      ptg     <- lookupPure an gn rho
      let ty = rangeTy ptg
      pure $ mkLet an ty p e' g_pure_app

---------------------------
-- Purifying Resumption Monadic definitions
---------------------------

--
-- It may be that the type for each subexpression occurring in here should be
-- part of the annotation here. That is partially accomplished right now.
--
data RCase an t p e = RReturn an e t
                    | RLift an e t
                    | RVar an e
                    | Signal an e t
                    | RBind an e e t
                    | SigK an e e [(e, t)]   -- (signal e >>= g e1 ... ek)
                    | Extrude t e [e] -- [(e, t)]
                    | RApp an t e [(e, t)]
                    | RSwitch an t e p e (Maybe e)
                    | RMatch an t e MatchPat e [e] (Maybe e)

instance Show (RCase an t p e) where
      show RReturn {} = "RReturn"
      show RLift {}   = "RLift"
      show RVar {}    = "RVar"
      show Signal {}  = "Signal"
      show RBind {}   = "RBind"
      show SigK {}    = "SigK"
      show Extrude {} = "Extrude"
      show RApp {}    = "RApp"
      show RSwitch {} = "RSwitch"
      show RMatch {}  = "RMatch"

--
-- classifyApp classifies syntactic applications. Just seemed easier to
-- make it its own function.
--
classifyApp :: MonadError AstError m => Annote -> (Exp, Ty, [Exp]) -> m (RCase Annote Ty p Exp)
classifyApp _ (v@(Var an _ _), _, [])  = return $ RVar an v
classifyApp an (f, t, [e]) = do
  (te, _) <- dstArrow t
  case f of
    Var _ _ x | n2s x == "return" -> return $ RReturn an e te
              | n2s x == "lift"   -> return $ RLift an e te
              | n2s x == "signal" -> return $ Signal an e te
              | otherwise         -> do bes <- mkBindings t [e]
                                        return $ RApp an t f bes
    _ -> undefined -- TODO(chathhorn): impossible because of dstArrow?
classifyApp an (f, t, [e, g]) = case f of
  Var _ tb x | n2s x == ">>=" -> case issignal e of
                                      Just s -> do (g', tg', es) <- dstApp g
                                                   bes           <- mkBindings tg' es
                                                   return $ SigK an s g' bes
                                      Nothing -> do (_, tau) <- dstArrow tb
                                                    (tg, _)  <- dstArrow tau
                                                    return $ RBind an e g tg
             | n2s x == "extrude" -> return $ Extrude t f [e, g]
             | otherwise          -> do bes <- mkBindings t [e, g]
                                        return $ RApp an t f bes
  _ -> undefined -- TODO(chathhorn): is this impossible?
classifyApp an (f, t, es) = case f of
  Var _ _ x | n2s x == "extrude" -> return $ Extrude t f es
            | otherwise          -> do bes <- mkBindings t es
                                       return $ RApp an t f bes
  _ -> undefined -- TODO(chathhorn): is this impossible?

classifyRCases :: (Fresh m, MonadError AstError m) => Exp -> m (RCase Annote Ty Pat Exp)
classifyRCases = \ case
     t@(App an _ _)            -> do (f, t, es) <- dstApp t
                                     classifyApp an (f, t, es)
     Case an t dsc bnds me     -> do (p, e) <- unbind bnds
                                     return (RSwitch an t dsc p e me)
     Match an t e1 mp e2 es me -> return $ RMatch an t e1 mp e2 es me
     v@(Var an _ _)            -> return $ RVar an v
     d                         -> failAt (ann d) $ "Unclassifiable R-case: " ++ show d

-- state for res-purification.
data PSto = PSto [DataCon] [(Pat, Exp)] [Defn]

addClause :: MonadState PSto m => DataCon -> m ()
addClause dc   = modify (\ (PSto dcs pes defs) -> PSto (dc:dcs) pes defs)

addEquation :: MonadState PSto m => (Pat, Exp) -> m ()
addEquation pe = modify (\ (PSto dcs pes defs) -> PSto dcs (pe:pes) defs)

{-
   purifyResBody
                 rho    -- pure environment
                 i      -- input type
                 o      -- output type
                 t      -- return type of monad (I'm assuming it's pure)
                 stys   -- state types [S1, ..., Sm]
                 stos   -- input states [s1, ..., sm]
                 iv     -- fresh var to use for input type
                 tm     -- term to be purified
-}
purifyResBody :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) =>
                   PureEnv -> Ty -> Ty -> Ty -> Name Exp -> ([Exp], [Ty]) -> Exp -> StateT PSto m Exp
purifyResBody rho i o t iv mst tm = do
  cls <- classifyRCases tm
  case cls of

--     purifyResBody (return e)         = "(Left (e, (s1, (..., sm))))"
       RReturn _ e te       -> do
         te' <- purifyTyM te (snd mst)
         let etor = mkRangeTy te' o (snd mst)
         return $ mkLeft etor $ mkTuple $ e : fst mst

--   purifyResBody (signal e
--                     >>= g e1 ... ek) = "Right ((e, (s1, (..., sm))), R_g e1 ... ek)"
--                        Side effect:
--                  * add the clause "R_g T1 ... Tk" to R
--                  (where Ti is the type of ei)
--                  * add the eqn
--                      dispatch ((R_g e1 ... ek), (s1, ..., sm)) i
--                   = g_pure e1 ... ek i s1 ... sm
--              to defn of dispatch

       SigK an e g bes -> do
         let (es, ts) = unzip bes
         ts' <- mapM (\ t -> purifyTyM t $ snd mst) ts

         r_g <- fresh $ s2n $ "R_" ++ prettyPrint g
         let rt      = mkArrowTy $ ts' |: rTy
         let rcon    = Con an rt r_g
         let rGes    = mkApp rcon es
         addClause (mkRDataCon an r_g ts')                      -- "R_g T1 ... Tk"

         ns     <- freshVars "s" $ snd mst
         let stopat = mkTuplePat ns                 -- Pattern (s1, ..., sn)
         (p, xs) <- mkRPat an ts' r_g                            -- Pattern (R_g e1 ... ek)
         let pairpat = case snd mst of                        -- Pattern (R_g e1 ... ek, (s1, ..., sn))
               []-> p
               _ -> mkPairPat p stopat

         let svars = map (\ (x, t) -> Var an t x) ns  -- [s1, ..., sn]
         let vars  = map (\ (x, t) -> Var an t x) (zip xs ts')   -- [x1, ..., xn]
         g_pure_app <- mkPureApp an rho g (vars ++ [Var an i iv] ++ svars)
         -- "dispatch (R_g e1 ... ek, (s1, ..., sn)) i = g_pure e1 ... ek i s1 ... sn"
         addEquation (pairpat, g_pure_app)

         let etor        = mkRangeTy t o (snd mst)
         let inner = case fst mst of
               []   -> rGes              --  R_g e1 ... ek :: R
               stos -> mkTuple [rGes, mkTuple stos]
                                                              -- ^ (R, (s1, ..., sm))
         let outer   = mkTuple [e, inner]   -- (e, (R_g e1 ... ek, (s1, (..., sm))))
         return $ mkRight etor outer                          -- Right ((e, R_g e1 ... ek), (s1, (..., sm)))

      -- purifyResBody (signal e)         = "(Right ((e, (s1, (..., sm))), R_ret))"
       Signal an e _ -> do
         let r_return     = Con an rTy (s2n "R_return")
         let inner = case fst mst of
               []   -> r_return
               stos -> mkTuple [r_return, mkTuple stos]
         let outer   = mkTuple [e, inner]
         let etor         = mkRangeTy t o (snd mst)
         return $ mkRight etor outer

{-
      purifyResBody (e >>= g)          = "let
            -- N.B.: The irrefutable pattern here is
            -- sketchy, but it should be okay because
            -- of the restriction on occurrences of
            -- "signal"
                        (Left v, (s1, (..., sm))) = [|purifyResBody e|]
                      in
                        g_pure v s1 ... sm"
It appears to me that g can be an application (and not only a variable). In which case, I guess
the right thing to do is just apply "v s1 ... sm" to g. And possibly add "_pure" to the function at the
head of the application. The way it's written below assumes that g is a variable.
-}
       RBind an e g tg -> do
         -- purify the types of e and g
         tg'         <- purifyTyM tg (snd mst)
         -- ert is the return type of e; ty is the type of the whole expression
         (ert, ty)   <- dstArrow tg'
         e'          <- purifyResBody rho i o ert iv mst e
         g'          <- purifyResBody rho i o t iv mst g

         -- start calculating pattern: p = (Left (v, (s1, (..., sm))))
         let etor     = mkRangeTy ert o (snd mst)
         svars       <- freshVars "state" $ snd mst
         v           <- freshVar "v"
         let stps    = mkTuplePat $ (v, ert) : svars
         let p        = PatCon an (Embed etor) (Embed $ s2n "Prelude.Left") [stps]
         -- done calculating p = Left (v, (s1, (..., sm)))

         -- calculating g_pure_app = "g_pure v s1 ... sm"
         let vars     = map (\ (x, t) -> Var an t x) svars
         g_pure_app  <- mkPureApp an rho g' (Var an ert v : vars)
         -- done calculating g_pure_app
         return $ mkLet an ty p e' g_pure_app

       -- N.b., the pure env rho will have to contain *all* purified bindings
       --       or this will fail.
       RLift an e _ -> do
         e'    <- uncurry (purifyStateBody rho) mst 0 (snd mst) e -- was 1, which seems incorrect.
         s_i   <- freshVars "State" $ snd mst
         v     <- freshVar "var"
         -- the pattern "(v, (s1, (..., sm)))"
         let p       = mkTuplePat $ (v, t) : s_i
             etor    = mkRangeTy t o (snd mst)
             leftv   = mkLeft etor (Var an t v)
             body    = mkTuple $ leftv : fst mst
             body_ty = mkTupleTy (etor : snd mst)
         return $ mkLet an body_ty p e' body

       RVar an (Var _ tx x)   -> do
         tx' <- purifyTyM tx (snd mst)
         return $ mkApp (Var an tx' x') (fst mst)
           where x' = if isStart x then s2n "$Pure.start" else x


{-
extrude :: Monad m => ReT i o (StT s m) a -> s -> ReT i o m (a, s)

To purify e = (extrude ... (extrude phi s1) ... sn):
1. N.b., phi :: ReT i o (StT s m) a. We'll assume that e is "maximal". I.e.,
   that you have identified how many times extrude has been applied and its arguments s1, ..., sn.
2. phi' <- purifyResBody phi
3. make definition and addit to definitions: $LL = phi'
4. return $ (...($LL s1)...sn)
-}

       Extrude _ rator rands -> do
         let (dev, stos) = flattenExtr $ mkApp rator rands
         case dev of
              Var an t d -> do
                t'  <- liftMaybe an "purifying extruded device" $ purifyTy t (snd mst)
                return $ mkApp (Var an t' d) stos

              App an _ _  -> do
                (Var an' t f, _, es) <- dstApp dev
                t'                <- liftMaybe an "purifying extruded device" $ purifyTy t (snd mst)
                let f' = Var an' t' f
                return $ mkApp f' (es ++ stos)
              e          -> failAt (ann e) $ "Extruded device is non-variable: " ++ show dev

       RApp an _ rator rands -> do
         rator' <- purifyResBody rho i o t iv mst rator
     --    N.b., don't think it's necessary to purify the rands because they're simply typed.
         f <- mkPureApp an rho rator' (map fst rands)
         return $ mkApp f $ fst mst

       RSwitch an ty dsc p e1 Nothing     -> do
         ty' <- purifyTyM ty (snd mst)
         e1' <- purifyResBody rho i o t iv mst e1
         return $ Case an ty' dsc (bind p e1') Nothing

       RSwitch an ty dsc p e1 (Just e2)   -> do
         ty' <- purifyTyM ty (snd mst)
         e1' <- purifyResBody rho i o t iv mst e1
         e2' <- purifyResBody rho i o t iv mst e2
         return $ Case an ty' dsc (bind p e1') (Just e2')

       -- e1 must be simply-typed, so don't purify it.
       RMatch an ty e1 mp e2 es Nothing   -> do
         ty' <- purifyTyM ty (snd mst)
         e2' <- purifyResBody rho i o t iv mst e2
         return $ Match an ty' e1 mp e2' es Nothing

       RMatch an ty e1 mp e2 es (Just e3) -> do
         ty' <- purifyTyM ty (snd mst)
         e2' <- purifyResBody rho i o t iv mst e2
         e3' <- purifyResBody rho i o t iv mst e3
         return $ Match an ty' e1 mp e2' es (Just e3')

       _ -> undefined -- TODO(chathhorn): missing some RVar cases.

---------------------------
-- A Compendium of Helper Functions
---------------------------

dispatchTy :: Ty -> Ty -> [Ty] -> Ty -> Ty
dispatchTy i o ms t = mkArrowTy $ domTy :| [i, etor]
    where etor  = mkRangeTy t o ms
          domTy = if null ms then rTy else (mkPairTy rTy . mkTupleTy) ms

var2name :: MonadError AstError m => Exp -> m (Name Exp)
var2name (Var _ _ x) = return x
var2name d           = failAt (ann d) $ "var2string applied to non-variable: " ++ show d

--
-- Takes t1 and t2 and creates (t1, t2). I'm assuming (?!?) that the name of the
-- pairing constructor is "(,)". Correct me if I'm wrong.
--
mkPairTy :: Ty -> Ty -> Ty
mkPairTy t1 = TyApp noAnn (TyApp noAnn (TyCon noAnn $ s2n "(,)") t1)

mkPair :: Exp -> Exp -> Exp -- TODO(chathhorn): find the real (,) ctor
mkPair e1 e2 = App noAnn (App noAnn (Con noAnn t (s2n "(,)")) e1) e2
      where t = mkArrowTy $ typeof e1 :| [typeof e2, mkPairTy (typeof e1) $ typeof e2]

mkTuple :: [Exp] -> Exp
mkTuple = \ case
      []     -> Con noAnn (mkTupleTy []) $ s2n "()" -- TODO(chathhorn): find the real unit
      x : xs -> foldl1 mkPair $ x :| xs

mkTupleTy :: [Ty] -> Ty
mkTupleTy = \ case
      []     -> TyCon noAnn $ s2n "()"
      t : ts -> foldl1 mkPairTy $ t :| ts

mkRangeTy :: Ty -> Ty -> [Ty] -> Ty
mkRangeTy a o = \ case
      [] -> mkEitherTy a (mkPairTy o rTy)
      ts -> mkEitherTy (mkPairTy a $ mkTupleTy ts) (mkPairTy o $ mkPairTy rTy $ mkTupleTy ts)

{-
  Takes [T1, ..., Tn] and returns (T1 -> (T2 -> ... (T(n-1) -> Tn) ...))
-}
mkArrowTy :: NonEmpty Ty -> Ty
mkArrowTy = foldr1 arr0

mkPairPat :: Pat -> Pat -> Pat
mkPairPat p1 p2 = PatCon noAnn (Embed $ mkPairTy (typeof p1) (typeof p2)) (Embed (s2n "(,)")) [p1, p2]

mkTuplePat :: [(Name Exp, Ty)] -> Pat
mkTuplePat = \ case
      []          -> PatCon noAnn (Embed $ TyCon noAnn $ s2n "()") (Embed $ s2n "()") []
      x : xs      -> foldl1 mkPairPat $ NE.map patVar $ x :| xs
            where patVar :: (Name Exp, Ty) -> Pat
                  patVar (n, t) = PatVar noAnn (Embed t) n

-- TODO(chathhorn): remove
--                  f :: (Pat, Ty) -> (Pat, Ty) -> (Pat, Ty)
--
--                  t :: Ty
--                  t = foldl1' mkPairTy $ map snd xs
--                  p :: Pat
--                  p foldl1' ( \ (n, t) -> 
--      [(n, t)]    -> (PatVar noAnn (Embed t) n, t)
--      ((n, t):ns) -> (p1p2, mkPairTy t t2)
--            where pn       = PatVar noAnn (Embed t) n
--                  (p2, t2) = mkTuplePat ns
--                  p1p2     = PatCon noAnn (Embed $ mkPairTy t t2) (Embed (s2n "(,)")) [pn, p2]

-- Shouldn't mkLeft/mkRight change the type t to an Either?
mkLeft :: Ty -> Exp -> Exp
mkLeft t = App noAnn (Con noAnn t (s2n "Prelude.Left"))

mkRight :: Ty -> Exp -> Exp
mkRight t = App noAnn (Con noAnn t (s2n "Prelude.Right"))

mkEitherTy :: Ty -> Ty -> Ty
mkEitherTy t1 = TyApp noAnn (TyApp noAnn (TyCon noAnn $ s2n "Prelude.Either") t1)

mkRDataCon :: Annote -> Name DataConId -> [Ty] -> DataCon
mkRDataCon an r_g ts = DataCon an r_g ([] |-> mkArrowTy (ts |: rTy))

mkRPat :: Fresh m => Annote -> [Ty] -> Name DataConId -> m (Pat, [Name Exp])
mkRPat an ts r_g = do
      xs <- freshVars "store" ts
      let varpats = map (\ (x, t) -> PatVar an (Embed t) x) xs
      return (PatCon an (Embed ty) (Embed r_g) varpats, map fst xs)
      where ty = foldr (TyApp an) (TyCon an $ s2n "R") ts

mkPureVar :: MonadError AstError m => PureEnv -> Exp -> m Exp
mkPureVar rho = \ case
  v@(Var an _ x) | n2s x == "extrude" -> return v
                 | n2s x == "unfold"  -> return v
                 | otherwise          -> do
                     t' <- lookupPure an x rho
                     return $ Var an t' x
  d                       -> failAt (ann d) $ "Can't make a pure variable out of a non-variable" ++ show d

mkApp :: Exp -> [Exp] -> Exp
mkApp = foldl' (App noAnn)

mkPureApp :: MonadError AstError m => Annote -> PureEnv -> Exp -> [Exp] -> m Exp
mkPureApp _ rho e es = do (rator, _, rands) <- dstApp e
                          ep                <- mkPureVar rho rator
                          return $ mkApp ep $ rands ++ es

dstApp :: MonadError AstError m => Exp -> m (Exp, Ty, [Exp])
dstApp (App _ f@(Var _ t _) rand) = return (f, t, [rand])
dstApp (App _ rator rand)         = do (f, t, es) <- dstApp rator
                                       return (f, t, es++[rand])
dstApp x@(Var _ t _)              = return (x, t, [])
dstApp d                          = failAt (ann d) "Tried to dst non-app"

-- > let p = e1 in e2
--     becomes
-- > case e1 of { p -> e2 }
-- N.b., use a Match here instead of a Case.
mkLet :: Annote -> Ty -> Pat -> Exp -> Exp -> Exp
mkLet an ty p e1 e2 = Case an ty e1 (bind p e2) Nothing

mkBindings :: MonadError AstError m => Ty -> [t] -> m [(t, Ty)]
mkBindings _ []     = return []
mkBindings t (e:es) = do (dom, ran) <- dstArrow t
                         bes       <- mkBindings ran es
                         return $ (e, dom) : bes

issignal :: Exp -> Maybe Exp
issignal (App _ (Var _ _ (n2s -> "signal")) e) = pure e
issignal _                                     = Nothing

dstArrow :: MonadError AstError m => Ty -> m (Ty, Ty)
dstArrow (TyApp _ (TyApp _ (TyCon _ (n2s -> "->")) t1) t2) = return (t1, t2)
dstArrow t                                                 = failAt (ann t) "Non-arrow type(4)"
