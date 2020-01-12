{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{-# LANGUAGE Safe #-}

module ReWire.FrontEnd.Purify (purify,
                               classifyTy,
                               TyVariety(Arrow,ReTApp,StTApp,IdApp,PairApp,Pure),
                               liftMaybe) where

import ReWire.Annotation
import ReWire.FrontEnd.Unbound
      ( Fresh (..), name2String, string2Name, runFreshMT
      )
import ReWire.Error
import ReWire.FrontEnd.Syntax
import Control.Monad.State
import Control.Monad.Fail (MonadFail)
import Data.List (find)

freshVar :: Fresh m => String -> m (Name a)
freshVar n = fresh (string2Name $ "?X_" ++ n ++ "_")

freshVars :: (Enum a1, Num a1, Show a1, Fresh m) => String -> a1 -> m [Name a]
freshVars n m = mapM (freshVar . (n++) . show) [0..m-1]

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

-- is stuff like this kosher?
poly2Ty :: Fresh m => Poly -> m Ty
poly2Ty (Poly p) = do (_,ty) <- unbind p
                      return ty

purify :: (MonadError AstError m, MonadIO m, MonadFail m) => FreeProgram -> m FreeProgram
purify (ts, ds) = runFreshMT $ do
      mds       <- mapM isStateMonadicDefn ds
      let (smds,pds) = partition mds
      rds       <- mapM isResMonadicDefn pds
      let (rmds,ods) = partition rds

      let nameNpolyS = map (\ d -> (defnName d, defnPolyTy d)) smds
      let nameNpolyR = map (\ d -> (defnName d, defnPolyTy d)) rmds
      imprhoR   <- mapM (\ (d,Embed p) -> poly2Ty p >>= \ t -> return (d,t)) nameNpolyR

      (_,tyStart) <- liftMaybe "No start symbol!" $ find (isStart . n2s . fst) imprhoR
      (_,i,o,_,t) <- liftMaybe ("dstResTy applied to: " ++ show tyStart) {- $ trace ("tyStart = " ++ show tyStart) -} $ dstResTy tyStart

      --- if t = (a,s), need to set t:=a and stos:=s
      let (ty,stys) = case dstTuple t of
            [] -> undefined -- TODO(chathhorn): impossible
            [t]    -> (t,[])
            (t:ts) -> (t,ts)
      let ms   = case stys of         --- these will contain states if they exist!!!!!
                      [] -> Nothing
                      _  -> Just stys

      rhoS      <- mkPureEnv nameNpolyS ms
      rhoR      <- mkPureEnv nameNpolyR ms
      let rho = rhoS ++ rhoR

      pure_smds <- mapM (purifyStateDefn rho ms) smds
      iv        <- freshVar "i"
      --
      -- The problem is that the start symbol is being passed ms/=[] below:
      --
      (pure_rmds,PSto dcs pes _) <- runStateT (mapM (purifyResDefn rho imprhoR iv ms) rmds) (PSto [] [] [])

      let r  = mkR_Datatype dcs

      let dt = dispatchTy i o ms ty
      df        <- mkDispatch dt pes iv i

      start_def <- mkStart i o ms ty

      return (ts++[r], start_def : df : ods ++ pure_smds ++ pure_rmds)
   where dstTuple :: Ty -> [Ty]
         dstTuple t = case t of
            TyApp _ (TyApp _ (TyCon _ con) t1) t2 | n2s con == "(,)" -> dstTuple t1 ++ [t2]
            _                                                         -> [t]


--
-- Converts
--    (extrude (extrude (... (extrude ((Var _ t f) e1...ek) s1) ...) s(n-1)) sn)
-- Into
--    rangeTy t

{-
-- We need to calculate the "un-extruded type" of the start symbol, so that we can
-- gather all the state types (if any).
--
-- Caution: assuming that every start symbol has a l.h.s. of the form above.
--
range_type_start :: MonadError AstError m => Exp -> m Ty
range_type_start e = do
  let e' = stripExtrude e
  (_,t,_) <- dstApp e'
  return (rangeTy t)
   where
     stripExtrude :: Exp -> Exp
     stripExtrude e = case e of
       (App _ (App _ h@(Var _ _ v) arg1) arg2) | n2s v == "extrude" -> dev
                                                      where (dev,_) = flattenExtr arg1
       _                                                            -> e
-}

isStart :: String -> Bool
isStart n = take 10 n == "Main.start" && null nonNumsInSuffix
  where nonNumsInSuffix = [ c | c <- drop 10 n, not $ c `elem` ['0'..'9']]
--        s               = n2s n

{-
In addition to all the above, we re-tie the recursive knot by adding a new
  "start" as follows:

  	  start :: ReT In Out I T
	  start = unfold dispatch start_pure

          start :: In -> (Either T (O,R),()) --- drop the "()"?
-}

-- Debugging.
-- grunt i o ms t = "i = " ++ show i ++ "\n" ++
--                  "o = " ++ show o ++ "\n" ++
--                  "ms = " ++ show ms ++ "\n" ++
--                  "t = " ++ show t

-- Correcting mkStart.
--
-- In the type of Main.start.
-- t (return type of Main.start) needs to be combined with ms (list of StT s, stores).
--
--

mkStart :: Monad m => Ty -> Ty -> Maybe [Ty] -> Ty -> m Defn
mkStart i o ms t = {- trace ("MKSTART: tyStart = " ++ show tyStart ++ ":::" ++ grunt i o ms t) $-} do
  return $ Defn {
      defnAnnote = MsgAnnote "start function"
    , defnName   = s2n "Main.start"
    , defnPolyTy = [] |-> tyStart
    , defnInline = False
    , defnBody   = appl
    }
   where na         = NoAnnote
         etor       = mkRangeTy t o ms
         ranStart   = mkPairTy etor (TyCon na (s2n "()"))
         pureStart  = i `arr0` ranStart  -- seems irrelevant to the generated code.

         extresTy :: Ty -> Maybe [Ty] -> Ty
         extresTy t (Just stos) = foldl mkPairTy t stos
         extresTy t _           = t

         reT i o a  = c "ReT" `tyApp` i `tyApp` o `tyApp` a
         tyStart    = TyComp noAnn (reT i o (c "I")) (extresTy t ms)

         unfold     = Var na (TyBlank $ MsgAnnote "stub type for unfold") (s2n "unfold")
         dispatch   = Var na (dispatchTy i o ms t) (s2n "$Pure.dispatch")
         start_pure = Var na pureStart (s2n "$Pure.start")
         appl       = Embed $ bind [] (App na (App na unfold dispatch) start_pure)


s2n :: String -> Name a
{-# INLINE s2n #-}
s2n = string2Name

n2s :: Name a -> String
{-# INLINE n2s #-}
n2s = name2String

{-
Converts
   (extrude (extrude (... (extrude dev s1) ...) s(n-1)) sn)
Into
   (dev,[s1,...,sn])

Won't work if called on a non-extrude application.
-}
flattenExtr :: Exp -> (Exp, [Exp])
flattenExtr e = case e of
  (App _ (App _ (Var _ _ v) arg1) arg2) | n2s v == "extrude" -> (dev,args++[arg2])
      where (dev,args) = flattenExtr arg1
  t@(App _ _ rand)                                       -> (t,args)
      where (_,args) = flattenExtr rand
  _                                                          -> (e,[])

mkId :: String -> Name b
mkId = string2Name

c :: String -> Ty
c = TyCon noAnn . mkId

tyApp :: Ty -> Ty -> Ty
tyApp = TyApp noAnn

-- dispatch (R_g e1 ... ek) i = g_pure e1 ... ek i
mkDispatch :: (Fresh m, MonadError AstError m) => Ty -> [(Pat,Exp)] -> Name Exp -> Ty -> m Defn
mkDispatch ty pes iv _ = do

  -- by default, must add the eqn: dispatch R_return i = Left i
--  let pdef = PatCon NoAnnote (Embed (TyCon NoAnnote (s2n "R"))) (Embed $ s2n "R_return") []
--  let bdef = mkLeft NoAnnote (rangeTy ty) (Var NoAnnote it iv)

  dsc <- freshVar "dsc"
  let dscv = Var an rTy dsc
  let body = mkCase an ty dscv (pes {- ++ [(pdef,bdef)]-})
  let dispatch_body = Embed (bind [dsc :: Name Exp, iv :: Name Exp] body)
  let dispatch = seed_dispatch { defnBody = dispatch_body }
  return dispatch
    where seed_dispatch = Defn {
                            defnAnnote = an
                          , defnName   = s2n "$Pure.dispatch"
                          , defnPolyTy = [] |-> ty
                          , defnInline = False
                          , defnBody   = undefined
                          }
          an            = MsgAnnote "Defn of dispatch function"


mkCase :: Annote -> Ty -> Exp -> [(Pat,Exp)] -> Exp
mkCase _ _ _ []              = error "Shouldn't ever happen."
mkCase an ty dsc ((p,e):[])  = Case an ty dsc (bind p e) Nothing
mkCase an ty dsc ((p,e):pes) = Case an ty dsc (bind p e) (Just rest)
   where rest = mkCase an ty dsc pes

mkR_Datatype :: [DataCon] -> DataDefn
mkR_Datatype dcs = seedR { dataCons = {- r_return : -} dcs }
   where
     seedR = DataDefn
       { dataAnnote = MsgAnnote "R Datatype"
       , dataName   = s2n "R"
       , dataKind   = KStar
       , dataCons   = []
       }

-- global constant representation of R data type
rTy :: Ty
rTy = TyCon NoAnnote (s2n "R")

type PureEnv = [(Name Exp, Ty)]

mkPureEnv :: (Fresh m,MonadError AstError m) => [(Name Exp, Embed Poly)] -> Maybe [Ty] -> m PureEnv
mkPureEnv [] _                   = return []
mkPureEnv ((n,Embed phi):nps) ms = do
  ty  <- poly2Ty phi
  purety <- purifyTyM ty ms -- liftMaybe "failed to purify type" (purifyResTy ty)
  env <- mkPureEnv nps ms
  return $ (n, purety) : env

lookupPure :: MonadError AstError m => Annote -> Name Exp -> PureEnv -> m Ty
lookupPure an x rho = case lookup x rho of
                           Just ty -> return ty
                           Nothing -> failAt an $ "No pure binding for variable: " ++ n2s x

lookupImpure :: MonadError AstError m => Annote -> Name Exp -> PureEnv -> m Ty
lookupImpure an x rho = case lookup x rho of
                           Just ty -> return ty
                           Nothing -> failAt an $ "No impure binding for variable: " ++ n2s x

partition :: [Either a b] -> ([a], [b])
partition = foldl step  ([],[])
   where
     step (smds,pds) (Left d)  = (d : smds, pds)
     step (smds,pds) (Right d) = (smds, d : pds)

isStateMonadicDefn :: Fresh m => Defn -> m (Either Defn Defn)
isStateMonadicDefn d = do ty <- poly2Ty poly
                          if isStateMonad . rangeTy $ ty
                             then return $ Left d
                             else return $ Right d
  where Embed poly = defnPolyTy d
        isStateMonad :: Ty -> Bool
        isStateMonad = \ case
              TyComp an (TyApp _ (TyApp _ (TyCon _ n) _) m) a | n2s n == "StT" ->
                                                                  isStateMonad (TyComp an m a)
                                                              | otherwise              -> False
              TyComp _ (TyCon _ n) _                          | n2s n == "I"   -> True
                                                              | otherwise              -> False
              _                                                                        -> False

isResMonadicDefn :: Fresh m => Defn -> m (Either Defn Defn)
isResMonadicDefn d = do ty <- poly2Ty poly
                        if isResMonad ty
                           then return $ Left d
                           else return $ Right d
  where Embed poly = defnPolyTy d
        isResMonad :: Ty -> Bool
        isResMonad ty = case rangeTy ty of
                         TyComp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ n) _) _) _) _
                           | n2s n == "ReT" -> True
                           | otherwise      -> False
                         _                  -> False


purifyStateDefn :: (Fresh m, MonadError AstError m, MonadIO m) =>
                   PureEnv -> Maybe [Ty] -> Defn -> m Defn
purifyStateDefn rho ms d = do
  ty         <- poly2Ty phi
--  liftIO $ putStrLn $ "purify_state_defn"
  p_pure     <- lookupPure an dname rho
--  let p_pure = [] |-> ty
  let d_pure = dname -- s2n $ dname ++ "_pure"
  (args,e)   <- unbind body
  (_,stys,_) <- liftMaybe "failed at purifyStateDefn" $ dstStT ty
  nstos      <- freshVars "sigma" (length stys)
  let stos = foldr (\ (n,t) nts -> Var an t n : nts) [] (zip nstos stys)
  e'         <- purify_state_body rho stos stys 0 ms e
  let b_pure = bind (args++nstos) e'
  return $ d { defnName=d_pure, defnPolyTy=([] |-> p_pure), defnBody=(Embed b_pure) }
    where
      dname      = defnName d
      Embed body = defnBody d
      an         = defnAnnote d
      Embed phi  = defnPolyTy d

liftMaybe :: MonadError AstError m => String -> Maybe a -> m a
liftMaybe _ (Just v)   = return v
liftMaybe msg Nothing = failAt NoAnnote msg

purifyResDefn :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) =>
                 PureEnv -> [(Name Exp,Ty)] -> Name Exp -> Maybe [Ty] -> Defn -> StateT PSto m Defn
purifyResDefn rho imprhoR iv ms d = do
  ty         <- lift $ lookupImpure an dname imprhoR
  pure_ty    <- lift $ liftMaybe "Failed to create pure type"
                     {- $ trace ("ARG purifyResTy " ++ show ty ++ " " ++ show ms) -}
                     $ purifyResTy ty ms
  (args,e)   <- lift $ unbind body

  (_,i,o,_,a) <- lift $ liftMaybe "failed at purifyResDefn" $ dstResTy ty

  let stys = case ms of
        Nothing  -> []
        Just sts -> sts

  nstos      <- lift $ freshVars "sto" (length stys)
  let stos   = foldr (\ (n,t) nts -> Var an t n : nts) [] (zip nstos stys)
  let mst    = case stos of
                    [] -> Nothing
                    _  -> Just (stos,stys)

  e'         <- purify_res_body rho i o a {- stys stos -} iv mst e

--
-- Below is an egregious hack to compile the start symbol slightly differently.
--
--  let p_pure = [] |-> pure_ty
  let p_pure = if n2s dname == "Main.start" then [] |-> rangeTy pure_ty else [] |-> pure_ty
  let d_pure = if n2s dname == "Main.start" then s2n "$Pure.start" else dname
  let args'  = if n2s dname == "Main.start" then [] else args
  let nstos'  = if n2s dname == "Main.start" then [] else nstos

  let b_pure = bind (args' ++ nstos') e'
  return {- $ trace ("HEY " ++ show d_pure ++ " :: " ++ show p_pure) -} $ d { defnName=d_pure, defnPolyTy=p_pure, defnBody=(Embed b_pure) }
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

classifyTy :: Ty -> Maybe (TyVariety Ty)
classifyTy t = case t of
   TyComp an (TyApp _ (TyApp _ (TyApp _ (TyCon _ n) i) o) m) a
                                                   | n2s n == "ReT"   -> return $ ReTApp an i o m a
   TyApp an (TyApp _ (TyCon _ con) t1) t2          | n2s con == "->"  -> return $ Arrow an t1 t2
                                                   | n2s con == "(,)" -> return $ PairApp an t1 t2
   TyComp an (TyApp _ (TyApp _ (TyCon _ n) s) m) a | n2s n == "StT"   -> return $ StTApp an s m a
   TyComp an (TyCon _ n) a                         | n2s n == "I"     -> return $ IdApp an a
   TyComp _ _ _                                                       -> Nothing
   _                                                                  -> return $ Pure (ann t) t

purifyTy :: Ty -> Maybe [Ty] -> Maybe Ty
purifyTy t ms = do
    tc <- classifyTy t
    case tc of
         Arrow an t1 t2      -> do
           t1' <- purifyTy t1 ms
           t2' <- purifyTy t2 ms
           return $ TyApp an ((TyApp an (TyCon an $ s2n "->")) t1') t2'
         ReTApp _ _ _ _ _ -> purifyResTy t ms
         StTApp _ _ _ _   -> purifyStTTy t
         IdApp _ _        -> Nothing
         PairApp a t1 t2    -> do
           t1' <- purifyTy t1 ms
           t2' <- purifyTy t2 ms
           return $ TyApp a (TyApp a (TyCon a $ s2n "(,)") t1') t2'
         Pure _ t         -> return t

purifyTyM :: MonadError AstError m => Ty -> Maybe [Ty] -> m Ty
purifyTyM t ms = liftMaybe ("failed to purifyTy: " ++ show t) $ purifyTy t ms

dstTyApp :: [Ty] -> Ty -> ([Ty],Ty)
dstTyApp acc t@(TyApp _ (TyApp _ (TyCon _ arr) t1) t2) = if n2s arr == "->"
                                                             then dstTyApp (t1:acc) t2
                                                             else (reverse acc,t)
dstTyApp acc t               = (reverse acc,t)

{-
   This takes a type of the form
      T1 -> T2 -> ... -> Tnd -> StT S1 (StT S2 (... (StT Sm I))) T
   and returns
      ([T1,...,Tn],[S1,...,Sm],T)
-}
dstStT :: Ty -> Maybe ([Ty],[Ty],Ty)
dstStT ty = {- trace ("dstStT: " ++ show ty) $ -} do
  let (ts,smt) = dstTyApp [] ty
  (a,stos) <- dstComp smt
  return (ts,stos,a)
   where
         ---
     dstComp :: Ty -> Maybe (Ty,[Ty])
     dstComp = \ case
              TyComp _ c@(TyApp _ (TyApp _ (TyCon _ n) _) _) a | n2s n == "StT" ->
                                                                    do sts <- dstStT c
                                                                       Just (a,sts)
                                                                | otherwise              -> Nothing
              TyComp _ (TyCon _ n) a                           | n2s n == "I"           -> Just(a,[])
                                                                | otherwise              -> Nothing
              _                                                                          -> Nothing

         ---
     dstStT :: Ty -> Maybe [Ty]
     dstStT = \ case
           TyApp _ (TyApp _ (TyCon _ n) s) m | n2s n == "StT" -> do stos <- dstStT m
                                                                    return (s:stos)
                                             | otherwise      -> Nothing
           TyCon _ n                         | n2s n == "I"   -> return []
           _                                                  -> Nothing



{-
   This takes a Ty of the form
        T1 -> T2 -> ... -> Tn -> ReT In Out (StT S1 (StT S2 (... (StT Sm I)))) T
   and returns a Ty of the form
        T1 -> T2 -> ... -> Tn -> In -> S1 -> ... -> Sm -> (Either T (O,R),(S1,(...,Sm)))
-}
purifyResTy :: Ty -> Maybe [Ty] -> Maybe Ty
purifyResTy t ms = do
  (ts,_,o,_,a) <- dstResTy t
  let stos = case ms of
        Nothing -> []
        Just sl -> sl
  let ranTy    = mkRangeTy a o ms {- $ mkTupleTy stos  case stos of
                      [] -> mkEitherTy a (mkPairTy o rTy)
                      _  -> mkRangeTy a o $ mkTupleTy stos -}
  let purified = foldr arr0 ranTy (ts ++ stos)
  return {- $ trace ("purifyResTy: t = " ++ show t ++ "\n" ++ "purifyResTy: ms = " ++ show ms ++ "\n" ++ "purifyResTy: a = " ++ show a ++ "\n" ++ "purifyResTy: ts = " ++ show ts ++ "\n" ++ "purifyResTy: purified = " ++ show purified) -} $ purified

--
-- Takes
--      T1 -> T2 -> ... -> Tn -> StT S1 (StT S2 (... (StT Sm I))) T
-- and replaces it by
--      T1 -> ... -> Tn -> S1 -> ... -> Sm -> (T,(S1,(...,Sm)))
--
-- I'm going to rewrite this to stick precisely to Adam's description.
-- N.b., the product in the codomain associates to the right. The
-- definition of purifyStTTy required a little hackery to get the result
-- into that form.
--
purifyStTTy :: Ty -> Maybe Ty
purifyStTTy t = do
  (ts,stos,a) <- dstStT t
  let rang     = mkTupleTy (a : stos)
  let purified = mkArrowTy (ts ++ stos ++ [rang])
  return purified


{-
   This takes a type of the form
      T1 -> T2 -> ... -> Tnd -> ReT In Out (StT S1 (StT S2 (... (StT Sm I)))) T
   and returns
      ([T1,...,Tn],In,Out,[S1,...,Sm],T)
-}
dstResTy :: Ty -> Maybe ([Ty],Ty,Ty,[Ty],Ty)
dstResTy ty = do (i,o,m,a) <- dstReT rt
                 stos      <- dstStT m
                 return (ts,i,o,stos,a)
   where (ts,rt) = dstTyApp [] ty
         ---
         dstReT :: Ty -> Maybe (Ty, Ty, Ty, Ty)
         dstReT = \ case
           TyComp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ n) i) o) m) a
             | n2s n == "ReT" -> Just (i,o,m,a)

             | otherwise              -> Nothing
           _                          -> Nothing
         ---
         dstStT :: Ty -> Maybe [Ty]
         dstStT = \ case
           TyApp _ (TyApp _ (TyCon _ n) s) m | n2s n == "StT" -> do stos <- dstStT m
                                                                    return (s:stos)
                                             | otherwise      -> Nothing
           TyCon _ n                         | n2s n == "I"   -> return []
           _                                                  -> Nothing


---------------------------
-- Purifying State Monadic definitions
---------------------------

classifyCases :: (Fresh m, MonadError AstError m) => Exp -> m (Cases Annote Pat Exp Ty)
classifyCases = \ case
     Var an t g | n2s g == "get"                    -> return $ Get an t
                | otherwise                         -> return $ Apply an (Var an t g) []
     (App an (Var _ t x) e) | xs == "return"        -> return $ Return an t e
                            | xs == "put"           -> return $ Put an e
                            | xs == "lift"          -> return $ Lift an e
            where xs = n2s x
     App an (App _ f@(Var _ t x) e) g | xs == ">>=" -> return $ Bind an t e g
                                      | otherwise   -> return $ Apply an f [e,g]
            where xs = n2s x
     t@(App an _ _)                                 -> do (f,_,es) <- dstApp t
                                                          return $ Apply an f es
     Case an t dsc bnds me                          -> do (p,e) <- unbind bnds
                                                          return (Switch an t dsc p e me)
     Match an t e1 mp e2 es me                      -> return $ SMatch an t e1 mp e2 es me
     d                                              -> failAt (ann d) $ "Unclassifiable case: " ++ show d

data Cases an p e t = Get an t
                    | Return an t e
                    | Lift an e
                    | Put an e
                    | Bind an t e e
                    | Apply an e [e]
                    | Switch an t e p e (Maybe e)
                    | SMatch an t e MatchPat e [e] (Maybe e)


{-
   purify_state_body
                  rho  -- pure environment
                  stos -- state vars
                  stys -- state types
                  i    -- lifting "depth"
                  tm   -- term to be purified
-}

--
-- Make sure to check for fencepost counting problems.
--
purify_state_body :: (Fresh m, MonadError AstError m, MonadIO m) =>
                     PureEnv -> [Exp] -> [Ty] -> Int -> Maybe [Ty] -> Exp -> m Exp
purify_state_body rho stos stys i ms tm = do
--  liftIO $ putStrLn $ "purify_state_body"
  exp <- classifyCases tm
  case exp of
    Get _ _      -> return $ mkTupleExp $ (stos !! i, stys !! i) : zip stos stys

    Return _ t e -> do (te,_) <- dstArrow t
                       return $ mkTupleExp $ (e,te) : zip stos stys

    Lift _ e     -> purify_state_body rho stos stys (i+1) ms e

    Put an e      -> return $ mkTupleExp $ (nil,nilTy) : zip stos' stys
        where nil   = Con an nilTy (s2n "()")
              nilTy = TyCon an (s2n "()")
              stos' = replaceAtIndex (i-1) e stos

    Apply an e es -> do rator <- mkPureApp an rho e es
                        return $ mkApp rator stos

    Switch an t e1 p e2 (Just e) -> do e1' <- purify_state_body rho stos stys i ms e1
                                       e2' <- purify_state_body rho stos stys i ms e2
                                       e'  <- purify_state_body rho stos stys i ms e
                                       return $ Case an t e1' (bind p e2') (Just e')

    Switch an t e1 p e2 Nothing  -> do e1' <- purify_state_body rho stos stys i ms e1
                                       e2' <- purify_state_body rho stos stys i ms e2
                                       return $ Case an t e1' (bind p e2') Nothing

    -- e1 must be simply-typed, so don't purify it.
    SMatch an ty e1 mp e2 es Nothing -> do
         ty' <- purifyTyM ty ms
         e2' <- purify_state_body rho stos stys i ms e2
         return $ Match an ty' e1 mp e2' es Nothing

    SMatch an ty e1 mp e2 es (Just e3) -> do
         ty' <- purifyTyM ty ms
         e2' <- purify_state_body rho stos stys i ms e2
         e3' <- purify_state_body rho stos stys i ms e3
         return $ Match an ty' e1 mp e2' es (Just e3')

    Bind an t e g -> do
      (te,_)  <- dstArrow t
      (_,_,a) <- liftMaybe "Non-StT Type passed to dstStT" $ dstStT te -- a is the return type of e
      e'      <- purify_state_body rho stos stys i ms e
      ns      <- freshVars "st" (length stos + 1)
      let vs = foldr (\ (n,t) nts -> Var an t n : nts) [] (zip ns (a:stys))
      g_pure_app <- mkPureApp an rho g vs
      (p,_)   <- {- trace ("ns*stys = " ++ show (zip ns (a:stys))) $ -} mkTuplePat (zip ns (a:stys))
--
-- g can, in general, be an application. That's causing the error when (var2name g) is called.
--
      (f,_,_) <- dstApp g
      gn      <- var2name f -- var2name g
      ptg     <- lookupPure an gn rho
      let ty = rangeTy ptg
      return $ mkLet an ty p e' g_pure_app

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
                    | RBind an e t e t
                    | SigK an e t e t [(e,t)]   -- (signal e >>= g e1 ... ek)
                    | Extrude t e [e] -- [(e,t)]
                    | RApp an t e [(e,t)]
                    | RSwitch an t e p e (Maybe e)
                    | RMatch an t e MatchPat e [e] (Maybe e)

instance Show (RCase an t p e) where
  show (RReturn _ _ _)        = "RReturn"
  show (RLift _ _ _)          = "RLift"
  show (RVar _ _)             = "RVar"
  show (Signal _ _ _)         = "Signal"
  show (RBind _ _ _ _ _)      = "RBind"
  show (SigK _ _ _ _ _ _)     = "SigK"
  show (Extrude _ _ _)        = "Extrude"
  show (RApp _ _ _ _)         = "RApp"
  show (RSwitch _ _ _ _ _ _)  = "RSwitch"
  show (RMatch _ _ _ _ _ _ _) = "RMatch"

--
-- classifyApp classifies syntactic applications. Just seemed easier to
-- make it its own function.
--
classifyApp :: MonadError AstError m => Annote -> (Exp, Ty, [Exp]) -> m (RCase Annote Ty p Exp)
classifyApp _ (v@(Var an _ _),_,[])  = return $ RVar an v
classifyApp an (f,t,[e]) = do
  (te, _) <- dstArrow t
  case f of
    Var _ _ x | xs == "return" -> return $ RReturn an e te
              | xs == "lift"   -> return $ RLift an e te
              | xs == "signal" -> return $ Signal an e te
              | otherwise      -> do bes <- mkBindings t [e]
                                     return $ RApp an t f bes
                  where xs = n2s x
    _ -> undefined -- TODO(chathhorn): impossible because of dstArrow?
classifyApp an (f,t,[e,g]) = case f of
  Var _ tb x | xs == ">>=" -> do sig <- issignal e
                                 case sig of
                                   Just (s,ts) -> do (g',tg',es) <- dstApp g
                                                     bes         <- mkBindings tg' es
                                                     return $ SigK an s ts g' tg' bes
                                   Nothing -> do (te,tau) <- dstArrow tb
                                                 (tg,_)   <- dstArrow tau
                                                 return $ RBind an e te g tg
             | xs == "extrude" -> return $ Extrude t f [e,g]
             | otherwise   -> do bes <- mkBindings t [e,g]
                                 return $ RApp an t f bes
                 where xs = n2s x
  _ -> undefined -- TODO(chathhorn): is this impossible?
classifyApp an (f,t,es) = case f of
  Var _ _ x | xs == "extrude" -> return $ Extrude t f es
            | otherwise       -> do bes <- mkBindings t es
                                    return $ RApp an t f bes
                 where xs = n2s x
  _ -> undefined -- TODO(chathhorn): is this impossible?

classifyRCases :: (Fresh m, MonadError AstError m) => Exp -> m (RCase Annote Ty Pat Exp)
classifyRCases = \ case
     t@(App an _ _)            -> do (f,t,es) <- dstApp t
                                     classifyApp an (f,t,es)
     Case an t dsc bnds me     -> do (p,e) <- unbind bnds
                                     return (RSwitch an t dsc p e me)
     Match an t e1 mp e2 es me -> return $ RMatch an t e1 mp e2 es me
     v@(Var an _ _)            -> return $ RVar an v
     d                         -> failAt (ann d) $ "Unclassifiable R-case: " ++ show d

-- state for res-purification.
data PSto = PSto [DataCon] [(Pat,Exp)] [Defn]

addClause :: MonadState PSto m => DataCon -> m ()
addClause dc   = modify (\ (PSto dcs pes defs) -> PSto (dc:dcs) pes defs)

addEquation :: MonadState PSto m => (Pat, Exp) -> m ()
addEquation pe = modify (\ (PSto dcs pes defs) -> PSto dcs (pe:pes) defs)

{-
   purify_res_body
                 rho    -- pure environment
                 i      -- input type
                 o      -- output type
                 t      -- return type of monad (I'm assuming it's pure)
                 stys   -- state types [S1,...,Sm]
                 stos   -- input states [s1,...,sm]
                 iv     -- fresh var to use for input type
                 tm     -- term to be purified
-}
purify_res_body :: (Fresh m, MonadError AstError m, MonadIO m, MonadFail m) =>
                   PureEnv -> Ty -> Ty -> Ty -> {- [Ty] -> [Exp] -> -} Name Exp -> Maybe ([Exp],[Ty]) -> Exp -> StateT PSto m Exp
purify_res_body rho i o t {- stys stos -} iv mst tm = do
  let stateType = case mst of
        Nothing      -> Nothing
        Just (_,sts) -> Just $ mkTupleTy sts
  let (stos,stys) = case mst of
        Nothing -> ([],[])
        Just (svs,sts) -> (svs,sts)

  cls <- lift $ classifyRCases tm
  case cls of

--     purify_res_body (return e)         = "(Left (e,(s1,(...,sm))))"
       RReturn _ e te       -> do
         te' <- lift $ purifyTyM te (Just stys)
         let etor = mkRangeTy te' o (sndM mst)
         return $ mkLeft etor $ mkTupleExp $ (e,etor) : zip stos stys

--   purify_res_body (signal e
--                     >>= g e1 ... ek) = "Right ((e, (s1,(...,sm))), R_g e1 ... ek)"
--                        Side effect:
--                  * add the clause "R_g T1 ... Tk" to R
--                  (where Ti is the type of ei)
--                  * add the eqn
--                      dispatch ((R_g e1 ... ek),(s1,...,sm)) i
--                   = g_pure e1 ... ek i s1 ... sm
--              to defn of dispatch

       SigK an e te g _ bes -> do
         let (es,ts) = unzip bes
         ts' <- lift $ mapM (\ t -> purifyTyM t (Just stys)) ts
         r_g <- sub g
         let rt      = mkArrowTy $ ts' ++ [rTy]
         let rcon    = Con an rt (s2n r_g)
         let rGes    = mkApp rcon es
         addClause (mkRDataCon an r_g ts')                      -- "R_g T1 ... Tk"

         ns     <- freshVars "s" (length stos)
         (stopat,_) <- mkTuplePat $ zip ns stys                 -- Pattern (s1,...,sn)
         (p,xs) <- mkRPat an ts' r_g                            -- Pattern (R_g e1 ... ek)
         let pairpat = case stateType of                        -- Pattern (R_g e1 ... ek,(s1,...,sn))
               Nothing    -> p
               Just stoTy -> mkPairPat rTy stoTy p stopat

         let svars = map (\ (x,t) -> Var an t x) $ zip ns stys  -- [s1,...,sn]
         let vars  = map (\ (x,t) -> Var an t x) (zip xs ts')   -- [x1,...,xn]
         g_pure_app <- mkPureApp an rho g (vars ++ [Var an i iv] ++ svars)
         --"dispatch (R_g e1 ... ek,(s1,...,sn)) i = g_pure e1 ... ek i s1 ... sn"
         addEquation (pairpat,g_pure_app)

         let etor        = mkRangeTy t o (sndM mst)
         let (inner,tin) = case mst of
               Nothing -> (rGes,rTy)                               --  R_g e1 ... ek :: R
               Just (stos,stys)  -> mkTuple [(rGes,rTy),(s1_sn,stotys)] --  (R,(s1,...,sm))
                   where s1_sn  = mkTupleExp $ zip stos stys
                         stotys = mkTupleTy stys
         let (outer,_)   = mkTuple [(e,te),(inner,tin)]        -- (e, (R_g e1 ... ek, (s1,(...,sm))))
         return $ mkRight etor outer                          -- Right ((e, R_g e1 ... ek), (s1,(...,sm)))


--        purify_res_body (signal e)         = "(Right ((e,(s1,(...,sm))),R_ret))"
       Signal an e tau -> do
         let r_return     = Con an rTy (s2n "R_return")
         let (inner,tin)  = case mst of
               Nothing          -> (r_return,rTy)
               Just (stos,stys) -> mkTuple [(r_return,rTy),(s1_sn,stotys)]
                   where s1_sn  = mkTupleExp $ zip stos stys
                         stotys = mkTupleTy stys
         let (outer,_)    = mkTuple [(e,tau),(inner,tin)]
         let etor         = mkRangeTy t o (sndM mst)
         return $ mkRight etor outer

{-
      purify_res_body (e >>= g)          = "let
            -- N.B.: The irrefutable pattern here is
            -- sketchy, but it should be okay because
            -- of the restriction on occurrences of
            -- "signal"
                        (Left v,(s1,(...,sm))) = [|purify_res_body e|]
                      in
                        g_pure v s1 ... sm"
It appears to me that g can be an application (and not only a variable). In which case, I guess
the right thing to do is just apply "v s1 ... sm" to g. And possibly add "_pure" to the function at the
head of the application. The way it's written below assumes that g is a variable.
-}
       RBind an e _ g tg -> do
         -- purify the types of e and g
         tg'           <- purifyTyM tg (Just stys)
         -- ert is the return type of e; ty is the type of the whole expression
         (ert,ty)      <- dstArrow tg'
         e'            <- purify_res_body rho i o ert {- stys stos -} iv mst e
         g'            <- purify_res_body rho i o t {- stys stos -} iv mst g

         -- start calculating pattern: p = (Left (v,(s1,(...,sm))))
         let etor  = mkRangeTy ert o (sndM mst)
         svars         <- freshVars "state" (length stys)
         v             <- freshVar "v"
         (stps,_)      <- mkTuplePat $ (v,ert) : (zip svars stys)
         let p = PatCon an (Embed etor) (Embed $ s2n "Prelude.Left") [stps]
         -- done calculating p = Left (v,(s1,(...,sm)))

         -- calculating g_pure_app = "g_pure v s1 ... sm"
         let vars       = map (\ (x,t) -> Var an t x) (zip svars stys)
         g_pure_app <-  mkPureApp an rho g' (Var an ert v : vars)
         -- done calculating g_pure_app
         return $ mkLet an ty p e' g_pure_app

       -- N.b., the pure env rho will have to contain *all* purified bindings
       --       or this will fail.
       RLift an e _ -> do
         let ms = case mst of
               Nothing     -> Nothing
               Just (_, t) -> Just t
         e'    <- lift $ purify_state_body rho stos stys 0 ms e -- was 1, which seems incorrect.
         s_i   <- freshVars "State" (length stys)
         v     <- freshVar "var"
         -- the pattern "(v,(s1,(...,sm)))"
         (p,_) <- {- trace "there" $ -} mkTuplePat $ (v,t) : zip s_i stys

         let etor    = mkRangeTy t o (sndM mst)
         let leftv   = mkLeft etor (Var an t v)
         let body    = mkTupleExp $ (leftv,etor) : zip stos stys
         let body_ty = mkTupleTy (etor : stys)
         return $ mkLet an body_ty p e' body

       RVar an (Var _ tx x)   -> do
--         lift $ liftIO $ putStrLn $ "***** RVar is " ++ n2s x
         tx' <- purifyTyM tx (Just stys)
         return {-$ trace ("BLAHHH::: tx = " ++ show tx ++ "," ++ "tx' = " ++ show tx' ++ ", stys = " ++ show stys )-} $ mkApp (Var an tx' x') stos
           where x' = if n2s x == "Main.start" then (s2n "$Pure.start") else x


{-
extrude :: Monad m => ReT i o (StT s m) a -> s -> ReT i o m (a,s)

To purify e = (extrude ... (extrude phi s1) ... sn):
1. N.b., phi :: ReT i o (StT s m) a. We'll assume that e is "maximal". I.e.,
   that you have identified how many times extrude has been applied and its arguments s1, ..., sn.
2. phi' <- purify_res_body phi
3. make definition and addit to definitions: $LL = phi'
4. return $ (...($LL s1)...sn)
-}

       Extrude _ rator rands -> do
         let (dev,stos) = flattenExtr $ mkApp rator rands
         case dev of
              Var an t d -> do
                t'  <- liftMaybe "purifying extruded device" $ purifyTy t (Just stys)
--                tau <- lookupPure NoAnnote d rho
--                trace ("HEY " ++ show d ++ " :: " ++ show t') $
                return $ mkApp (Var an t' d) stos

              App _ _ _  -> do
                (Var an t f,_,es) <- dstApp dev
                t'                <- liftMaybe "purifying extruded device" $ purifyTy t (Just stys)
                let f' = Var an t' f
                {- trace (show f' ++ " :: " ++ show t') $ -}
                return $ mkApp f' (es++stos)
              _          -> failAt NoAnnote $ "Extruded device is non-variable: " ++ show dev

       RApp an _ rator rands -> do
         rator' <- purify_res_body rho i o t iv mst rator
     --    N.b., don't think it's necessary to purify the rands because they're simply typed.
     --    rands' <- mapM (purify_res_body rho i o t stys stos iv . fst) rands
         f <- mkPureApp an rho rator' (map fst rands)
         return $ mkApp f stos

       RSwitch an ty dsc p e1 Nothing     -> do
         ty' <- lift $ purifyTyM ty (Just stys)
         e1' <- purify_res_body rho i o t iv mst e1
         return $ Case an ty' dsc (bind p e1') Nothing

       RSwitch an ty dsc p e1 (Just e2)   -> do
         ty' <- lift $ purifyTyM ty (Just stys)
         e1' <- purify_res_body rho i o t iv mst e1
         e2' <- purify_res_body rho i o t iv mst e2
         return $ Case an ty' dsc (bind p e1') (Just e2')

       -- e1 must be simply-typed, so don't purify it.
       RMatch an ty e1 mp e2 es Nothing   -> do
         ty' <- lift $ purifyTyM ty (Just stys)
         e2' <- purify_res_body rho i o t iv mst e2
         return $ Match an ty' e1 mp e2' es Nothing

       RMatch an ty e1 mp e2 es (Just e3) -> do
         ty' <- lift $ purifyTyM ty (Just stys)
         e2' <- purify_res_body rho i o t iv mst e2
         e3' <- purify_res_body rho i o t iv mst e3
         return $ Match an ty' e1 mp e2' es (Just e3')

       _ -> undefined -- TODO(chathhorn): missing some RVar cases.
---------------------------
-- A Compendium of Helper Functions
---------------------------

dispatchTy :: Ty -> Ty -> Maybe [Ty] -> Ty -> Ty
dispatchTy i o ms t = {- trace ("Input to dispatchTy: " ++ show (i,o,ms,t) ++ "\n" ++ "Output from mkRangeTy: " ++ show etor) $ -} mkArrowTy [domTy, i, etor]
    where
          etor = mkRangeTy t o ms
          domTy = case ms of
            Just s  -> mkPairTy rTy (mkTupleTy s)
            Nothing -> rTy

var2name :: MonadError AstError m => Exp -> m (Name Exp)
var2name (Var _ _ x) = return x
var2name d           = failAt (ann d) $ "var2string applied to non-variable: " ++ show d

--
-- Takes t1 and t2 and creates (t1,t2). I'm assuming (?!?) that the name of the
-- pairing constructor is "(,)". Correct me if I'm wrong.
--
mkPairTy :: Ty -> Ty -> Ty
mkPairTy t1 t2 = TyApp NoAnnote (TyApp NoAnnote (TyCon NoAnnote pair) t1) t2
    where pair = s2n "(,)"

mkPair :: (Exp, Ty) -> (Exp, Ty) -> (Exp, Ty)
mkPair (e1,t1) (e2,t2) = (App NoAnnote (App NoAnnote paircon e1) e2, t1xt2)
   where t1xt2   = mkPairTy t1 t2
         paircon = Con NoAnnote t1xt2 (s2n "(,)")

mkTuple :: [(Exp, Ty)] -> (Exp, Ty)
mkTuple [] = error "one"
mkTuple bs = foldr1 mkPair bs

mkTupleTy :: [Ty] -> Ty
mkTupleTy [] = TyCon NoAnnote (s2n "()") -- error "two"
mkTupleTy bs = foldr1 mkPairTy bs

mkTupleExp :: [(Exp, Ty)] -> Exp
mkTupleExp [] = error "three"
mkTupleExp bs = fst (mkTuple bs)

mkRangeTy :: Ty -> Ty -> Maybe [Ty] -> Ty
mkRangeTy a o ms = case ms of
                      Nothing -> mkEitherTy a (mkPairTy o rTy)
                      Just _  -> mkEitherTy (mkPairTy a s) (mkPairTy o (mkPairTy rTy s))
                                       where s = case ms of
                                              Nothing   -> error "mmRangeTy: state type empty"
                                              Just stos -> mkTupleTy stos

{-
  Takes [T1,...,Tn] and returns (T1 -> (T2 -> ... (T(n-1) -> Tn) ...))
-}
mkArrowTy :: [Ty] -> Ty
mkArrowTy []  = error "four"
mkArrowTy tys = foldr1 arr0 tys

mkPairPat :: Ty -> Ty -> Pat -> Pat -> Pat
mkPairPat ty1 ty2 p1 p2 = PatCon NoAnnote (Embed $ mkPairTy ty1 ty2) (Embed (s2n "(,)")) [p1,p2]

mkTuplePat :: MonadError AstError m => [(Name Exp, Ty)] -> m (Pat, Ty)
mkTuplePat []         = return (nilPat,nilTy)
  where nilPat = PatCon NoAnnote (Embed nilTy) (Embed (s2n "()")) []
        nilTy  = TyCon NoAnnote (s2n "()")
mkTuplePat ((n,t):[]) = return (PatVar NoAnnote (Embed t) n,t)
mkTuplePat ((n,t):ns) =
  do (p2,t2) <- mkTuplePat ns
     let p1p2 = PatCon NoAnnote (Embed $ mkPairTy t t2) (Embed (s2n "(,)")) [pn,p2]
     return $ (p1p2, mkPairTy t t2)
         where pn = PatVar NoAnnote (Embed t) n

-- Shouldn't mkLeft/mkRight change the type t to an Either?
mkLeft :: Ty -> Exp -> Exp
mkLeft t e = App NoAnnote (Con NoAnnote t (s2n "Prelude.Left")) e

mkRight :: Ty -> Exp -> Exp
mkRight t e = App NoAnnote (Con NoAnnote t (s2n "Prelude.Right")) e

mkEitherTy :: Ty -> Ty -> Ty
mkEitherTy t1 t2 = TyApp NoAnnote (TyApp NoAnnote (TyCon NoAnnote either) t1) t2
    where either = s2n "Prelude.Either"

sub :: MonadError AstError m => Exp -> m String
sub g = case g of
  (Var _ _ x) -> return $ "R_" ++ gn
      where gn = n2s x
  d            -> failAt (ann d) "sub failed"

mkRDataCon :: Annote -> String -> [Ty] -> DataCon
mkRDataCon an r_g ts = DataCon an (s2n r_g) ([] |-> ty)
     where ty = mkArrowTy $ ts ++ [rTy]

mkRPat :: Fresh m => Annote -> [Ty] -> String -> m (Pat,[Name Exp])
mkRPat an ts r_g = do
  xs <- freshVars "store" (length ts) -- fencepost counting problem?
  let varpats = map (\ (x,t) -> PatVar an (Embed t) x) (zip xs ts)
  return (PatCon an (Embed ty) (Embed $ s2n r_g) varpats, xs)
   where ty = foldr (TyApp an) (TyCon an $ s2n r_g) ts

mkPureVar :: MonadError AstError m => PureEnv -> Exp -> m Exp
mkPureVar rho = \ case
  v@(Var an _ x) | xs == "extrude" -> return v
                 | xs == "unfold"  -> return v
                 | otherwise       -> do
                     t' <- lookupPure an x rho
                     return $ Var an t' (x {- ++ "_pure" -})
      where xs = n2s x
  d                       -> failAt (ann d) $ "Can't make a pure variable out of a non-variable" ++ show d

mkApp :: Exp -> [Exp] -> Exp
mkApp rator = foldl step rator
  where step ap rand = App NoAnnote ap rand

mkPureApp :: MonadError AstError m => Annote -> PureEnv -> Exp -> [Exp] -> m Exp
mkPureApp _ rho e es = do (rator, _, rands) <- dstApp e
                          ep                <- mkPureVar rho rator
                          return $ mkApp ep $ rands ++ es

dstApp :: MonadError AstError m => Exp -> m (Exp, Ty, [Exp])
dstApp (App _ f@(Var _ t _) rand) = return (f,t,[rand])
dstApp (App _ rator rand)         = do (f,t,es) <- dstApp rator
                                       return (f,t,es++[rand])
dstApp x@(Var _ t _)              = return (x,t,[])
dstApp d                          = failAt (ann d) "Tried to dst non-app"

-- > let p = e1 in e2
--     becomes
-- > case e1 of { p -> e2 }
-- N.b., use a Match here instead of a Case.
mkLet :: Annote -> Ty -> Pat -> Exp -> Exp -> Exp
mkLet an ty p e1 e2 = Case an ty e1 (bind p e2) Nothing

mkBindings :: MonadError AstError m => Ty -> [t] -> m [(t, Ty)]
mkBindings _ []     = return []
mkBindings t (e:es) = do (dom,ran) <- dstArrow t
                         bes       <- mkBindings ran es
                         return $ (e,dom) : bes

issignal :: MonadError AstError m => Exp -> m (Maybe (Exp,Ty))
issignal (App _ (Var _ t x) e) | xs == "signal" = do (te,_) <- dstArrow t
                                                     return (Just (e,te))
                               | otherwise      = return Nothing
            where xs = n2s x
issignal _                                      = return Nothing

dstArrow :: MonadError AstError m => Ty -> m (Ty,Ty)
dstArrow (TyApp an (TyApp _ (TyCon _ con) t1) t2) = case n2s con of
                                                           "->" -> return (t1,t2)
                                                           _    -> failAt an "Non-arrow type(3)"
dstArrow t                                          = failAt (ann t) "Non-arrow type(4)"

sndM :: Maybe (a,b) -> Maybe b
sndM Nothing      = Nothing
sndM (Just (_,y)) = Just y
