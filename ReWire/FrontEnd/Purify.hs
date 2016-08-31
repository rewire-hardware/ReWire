{-# LANGUAGE FlexibleContexts, LambdaCase #-}
-- {-# LANGUAGE Safe, FlexibleContexts, LambdaCase #-}
module ReWire.FrontEnd.Purify (purify) where

import ReWire.Annotation
import ReWire.FrontEnd.Unbound
      ( Fresh (..), name2String, string2Name
      )
import ReWire.Error
import ReWire.FrontEnd.Syntax
import ReWire.FrontEnd.Records (freshVar, freshVars, replaceAtIndex, poly2Ty)
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.List (nub)
import Debug.Trace

purify :: (Fresh m, MonadError AstError m) => Program -> m Program
purify (Program p) = do
      (ts, ds)  <- untrec p
      mds       <- mapM isStateMonadicDefn ds
      let (smds,pds) = partition mds
      rds       <- mapM isResMonadicDefn pds
      let (rmds,ods) = partition rds

      let nameNpolyS = map (\ d -> (defnName d, defnPolyTy d)) smds
      rhoS      <- mkPureEnv nameNpolyS
      let nameNpolyR = map (\ d -> (defnName d, defnPolyTy d)) rmds
      imprhoR   <- mapM (\ (d,Embed p) -> poly2Ty p >>= \ t -> return (n2s d,t)) nameNpolyR
      rhoR      <- mkPureEnv nameNpolyR
      (i,o,t)   <- checkIOT (map ((\ (Embed p) -> p) . snd) nameNpolyR)

      let rho = rhoS ++ rhoR
      pure_smds <- mapM (purifyStateDefn rho) smds
      iv        <- freshVar "i"
      (pure_rmds,PSto dcs pes) <- runStateT (mapM (purifyResDefn rho imprhoR iv) rmds) (PSto [] [])

      let r  = mkR_Datatype dcs

      let dt = dispatchTy i t
      df        <- mkDispatch dt pes iv

      return $ Program $ trec (ts++[r], pds ++ pure_smds ++ pure_rmds) 
   where f Nothing ms  = ms
         f (Just a) ms = a : ms
         errmsg        = "Non-unique input, output or return type in specification"
         dispatchTy :: Ty -> Ty -> Ty
         dispatchTy i t = mkArrowTy [TyCon an $ s2n "R", i, etor]
             where etor = mkEitherTy an t (mkPairTy an (TyCon an $ s2n "W8") (TyCon an (s2n "R")))
                   an   = MsgAnnote "Type of dispatch function"


s2n :: String -> Name a
{-# INLINE s2n #-}
s2n = string2Name 

n2s :: Name a -> String
{-# INLINE n2s #-}
n2s = name2String

--
-- N.b., this is my attempt to rough out the purification pass described in Adam's note.
-- I'm putting this in a separate file (i.e., not "Purify.hs") because I'm old fashioned.
--

checkIOT :: (Fresh m, MonadError AstError m) => [Poly] -> m (Ty,Ty,Ty)
checkIOT []   = failAt NoAnnote "checkIOT assumes non-empty input"
checkIOT phis = do
  tys <- mapM (poly2Ty >=> (liftMaybe NoAnnote "whatever" . destroyResTy) >=> return . proj) phis
  let iots = nub tys
  case iots of
       [(i,o,t)] -> return (i,o,t)
       _         -> failAt NoAnnote "checkIOT: Non-unique input, output, or result types"
     where proj (_,i,o,_,t) = (i,o,t)

{-
{-
   (extractIOT tys) takes a list of res-monadic types and returns
        Just (i,o,t)
   if each ty in tys has the form
        ... -> ResT i o (...) t
   and otherwise
        Nothing
So, basically, all these types should have one and only one of each of In, Out, and T types.
-}
extractIOT :: [Ty] -> Maybe (Ty,Ty,Ty)
extractIOT []  = error "extractIOT assumes non-empty input"
extractIOT tys = let
                   proj (_,i,o,_,t) = (i,o,t)
                   iots             = nub $ myfromJust $ mapM (trace "1" destroyResTy >=> return . proj) tys
                 in
                  case iots of
                       [(i,o,t)] -> Just (i,o,t)
                       _         -> Nothing

-}

{-
[
 ((-> Main.In) (((ReT Main.In) Main.Out) I){()}),
 (((ReT Main.In) Main.Out) I){()},
 (((ReT Main.In) Main.Out) I){()},
 ((-> Main.In) (((ReT Main.In) Main.Out) I){()}),
 ((-> o) (((ReT i) o) m){i})
]
data Ty = TyApp Annote Ty Ty
        | TyCon Annote (Name TyConId)
        | TyVar Annote Kind (Name Ty)
        | TyComp Annote Ty Ty -- application of a monad
        | TyBlank Annote
           deriving (Eq, Generic, Typeable, Data)
-}

varfree (TyApp _ t1 t2)  = varfree t1 && varfree t2
varfree (TyCon _ _)      = True
varfree (TyVar _ _ _)    = False
varfree (TyComp _ t1 t2) = varfree t1 && varfree t2
varfree (TyBlank _)      = True


foo :: [Ty] -> Maybe [([Ty], Ty, Ty, [Ty], Ty)]
foo =  mapM destroyResTy

tys = [inrioi, rioi]

inrioi = arr0 (TyCon NoAnnote (s2n "In")) rioi

strioi = TyComp NoAnnote (reT (c "In") (c "Out") (stT (c "S1") (stT (c "S2") (c "I")))) nilTy
        where nilTy = TyCon NoAnnote (s2n "()")

crud = c "T1" `arr0` c "T2" `arr0` strioi

rioi = TyComp an (TyApp an (TyApp an (TyApp an ret inty) out) i) nilTy
        where nilTy = TyCon an (s2n "()")
              an    = NoAnnote
              out   = TyCon an (s2n "Out")
              inty  = TyCon an (s2n "In")
              ret   = TyCon an (s2n "ReT")
              i     = TyCon an (s2n "I")


mkId :: String -> Name b
mkId = string2Name

c :: String -> Ty
c = TyCon noAnn . mkId

tyApp :: Ty -> Ty -> Ty
tyApp = TyApp noAnn

reT :: Ty -> Ty -> Ty -> Ty
reT i o m = c "ReT" `tyApp` i `tyApp` o `tyApp` m

stT :: Ty -> Ty -> Ty
stT s m   = c "StT" `tyApp` s `tyApp` m


myfromJust (Just a) = a
myfromJust Nothing  = error "hey stupid"
proj (_,i,o,_,t) = (i,o,t)
            
-- dispatch (R_g e1 ... ek) i = g_pure e1 ... ek i
-- by default, must add the eqn: dispatch R_return i = Left i
mkDispatch :: (Fresh m, MonadError AstError m) => Ty -> [(Pat,Exp)] -> String -> m Defn
mkDispatch ty pes iv = do
  dsc <- freshVar "dsc"
  let dscv = Var an (TyCon an (s2n "R")) (s2n dsc)
  let body = mkCase an ty dscv pes
  let dispatch_body = Embed (bind [s2n dsc :: Name Exp, s2n iv :: Name Exp] body)
  let dispatch = seed_dispatch { defnBody = dispatch_body }
  return dispatch
    where seed_dispatch = Defn {
                            defnAnnote = an
                          , defnName   = s2n "R"
                          , defnPolyTy = [] |-> ty
                          , defnInline = False
                          , defnBody   = undefined
                          }
          an            = MsgAnnote "Defn of dispatch function"

mkCase :: Annote -> Ty -> Exp -> [(Pat,Exp)] -> Exp
mkCase an ty dsc []          = error "Shouldn't ever happen."
mkCase an ty dsc ((p,e):[])  = Case an ty dsc (bind p e) Nothing
mkCase an ty dsc ((p,e):pes) = Case an ty dsc (bind p e) (Just rest)
   where rest = mkCase an ty dsc pes

mkR_Datatype :: [DataCon] -> DataDefn
mkR_Datatype dcs = seedR { dataCons = r_return : dcs }
   where
     seedR = DataDefn 
       { dataAnnote = MsgAnnote "R Datatype"
       , dataName   = s2n "R"
       , dataKind   = KStar
       , dataCons   = []
       }
     r_return = DataCon NoAnnote (s2n "R_return") ([] |-> TyCon NoAnnote (s2n "R"))

type TySkel  = ([Ty],Ty,Ty,[Ty],Ty)
type PureEnv = [(String, Ty)]

mkPureEnv :: (Fresh m,MonadError AstError m) => [(Name a, Embed Poly)] -> m PureEnv
mkPureEnv []                  = return []
mkPureEnv ((n,Embed phi):nps) = do
  ty  <- poly2Ty phi
  purety <- liftMaybe NoAnnote "failed to purify type" (purifyResTy ty)
  env <- mkPureEnv nps
  return $ (n2s n, purety) : env

lookupPure :: MonadError AstError m => Annote -> String -> PureEnv -> m Ty
lookupPure an x rho = case lookup x rho of
                           Just ty -> return ty
                           Nothing -> failAt an $ "No pure binding for variable: " ++ x

lookupImpure :: MonadError AstError m => Annote -> String -> PureEnv -> m Ty
lookupImpure an x rho = case lookup x rho of
                           Just ty -> return ty
                           Nothing -> failAt an $ "No impure binding for variable: " ++ x

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
              TyComp an (TyApp _ (TyApp _ (TyCon _ n) s) m) a | n2s n == "StT" ->
                                                                  isStateMonad (TyComp an m a)
                                                              | otherwise              -> False
              TyComp an (TyCon _ n) a                         | n2s n == "I"   -> True
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
                         TyComp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ n) i) o) m) a
                           | n2s n == "ReT" -> True
                           | otherwise      -> False
                         _                  -> False


purifyStateDefn :: (Fresh m, MonadError AstError m) => PureEnv -> Defn -> m Defn
purifyStateDefn rho d = do
  ty         <- lookupPure an dname rho
  let p_pure = [] |-> ty
  let d_pure = s2n $ dname ++ "_pure"
  (args,e)   <- unbind body
  (_,stys,_) <- liftMaybe an "failed at purifyStateDefn" $ destroyStT ty
  nstos      <- freshVars (length stys)
  let stos = foldr (\ (n,t) nts -> Var an t (s2n n) : nts) [] (zip nstos stys)
  e'         <- purify_state_body rho stos stys 0 e
  let b_pure = bind args e'
  return $ d { defnName=d_pure, defnPolyTy=p_pure, defnBody=(Embed b_pure) }
    where
      dname      = n2s $ defnName d
      Embed body = defnBody d
      an         = defnAnnote d

liftMaybe :: MonadError AstError m => Annote -> String -> Maybe a -> m a
liftMaybe _ _ (Just v)   = return v
liftMaybe an msg Nothing = failAt an msg

purifyResDefn :: (Fresh m, MonadError AstError m) => PureEnv -> [(String,Ty)] -> String -> Defn -> StateT PSto m Defn
purifyResDefn rho imprhoR iv d = do
  ty         <- lift $ lookupImpure an dname imprhoR
  pure_ty    <- lift $ liftMaybe an "Failed to create pure type" $ purifyResTy ty
  let p_pure = [] |-> pure_ty
  let d_pure = s2n $ dname ++ "_pure"
  (args,e)   <- lift $ unbind body
  (ts,i,o,stys,a) <- lift $ liftMaybe an "failed at purifyResDefn" $ trace "2" $ destroyResTy ty

  nstos      <- lift $ freshVars (length stys)
  let stos = foldr (\ (n,t) nts -> Var an t (s2n n) : nts) [] (zip nstos stys)
  e'         <- purify_res_body rho ts i o a stys stos ty iv e
  let b_pure = bind args e'
  trace ("HEY: "++ n2s d_pure ++ " :: " ++ show p_pure) $
     return $ d { defnName=d_pure, defnPolyTy=p_pure, defnBody=(Embed b_pure) }
    where
      dname      = n2s $ defnName d
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
   TyComp an _ _                                                      -> Nothing 
   _                                                                  -> return $ Pure (ann t) t

purifyTy :: Ty -> Maybe Ty
purifyTy t = do
    tc <- classifyTy t
    case tc of
         Arrow an t1 t2      -> do
           t1' <- purifyTy t1
           t2' <- purifyTy t2
           return $ TyApp an ((TyApp an (TyCon an $ s2n "->")) t1') t2'
         ReTApp _ _ _ _ _ -> purifyResTy t
         StTApp _ _ _ _   -> purifyStTTy t
         IdApp a _        -> Nothing
         PairApp a t1 t2    -> do
           t1' <- purifyTy t1
           t2' <- purifyTy t2
           return $ TyApp a (TyApp a (TyCon a $ s2n "(,)") t1') t2'
         Pure a t         -> return t


dstrTyApp :: [Ty] -> Ty -> ([Ty],Ty)
dstrTyApp acc t@(TyApp _ (TyApp _ (TyCon _ arr) t1) t2) = if n2s arr == "->"
                                                             then dstrTyApp (t1:acc) t2
                                                             else (reverse acc,t)
dstrTyApp acc t               = (reverse acc,t)

{-
   This takes a type of the form
      T1 -> T2 -> ... -> Tnd -> StT S1 (StT S2 (... (StT Sm I))) T
   and returns
      ([T1,...,Tn],[S1,...,Sm],T)
-}
destroyStT :: Ty -> Maybe ([Ty],[Ty],Ty)
destroyStT ty = do let (ts,smt) = dstrTyApp [] ty
                   (a,stos) <- dstrComp smt
                   return (ts,stos,a)
   where 
         ---
     dstrComp :: Ty -> Maybe (Ty,[Ty])          
     dstrComp = \ case
              TyComp an c@(TyApp _ (TyApp _ (TyCon _ n) _) _) a | n2s n == "StT" ->
                                                                    do sts <- dstrStT c
                                                                       Just (a,sts)
                                                                | otherwise              -> Nothing
              TyComp an (TyCon _ n) a                           | n2s n == "I"           -> Just(a,[])
                                                                | otherwise              -> Nothing
              _                                                                          -> Nothing
           
         ---
     dstrStT :: Ty -> Maybe [Ty]
     dstrStT = \ case
           TyApp _ (TyApp _ (TyCon _ n) s) m | n2s n == "StT" -> do stos <- dstrStT m
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
purifyResTy :: Ty -> Maybe Ty
purifyResTy t = do
  (ts,i,o,stos,a) <- destroyResTy t
  let etor     = mkEitherTy NoAnnote a (mkPairTy NoAnnote o (TyCon NoAnnote (s2n "R")))
  let ranTy    = case stos of
                      [] -> etor
                      _  -> mkPairTy NoAnnote etor (mkTupleTy stos)
  let purified = foldr arr0 ranTy (ts ++ stos)
  return purified

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
  (ts,stos,a) <- destroyStT t
  let rang     = mkTupleTy (a : stos)
  let purified = mkArrowTy (ts ++ stos ++ [rang])
  return purified


{-
   This takes a type of the form
      T1 -> T2 -> ... -> Tnd -> ReT In Out (StT S1 (StT S2 (... (StT Sm I)))) T
   and returns
      ([T1,...,Tn],In,Out,[S1,...,Sm],T)
-}
destroyResTy :: Ty -> Maybe ([Ty],Ty,Ty,[Ty],Ty)
destroyResTy ty = -- trace ("destroyResTy called with: " ++ show ty) $
                  do (i,o,m,a) <- dstrReT rt
                     stos      <- dstrStT m
                     return (ts,i,o,stos,a)
   where (ts,rt) = dstrTyApp [] ty
         ---
         dstrReT :: Ty -> Maybe (Ty, Ty, Ty, Ty)
         dstrReT = \ case
           TyComp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ n) i) o) m) a
             | n2s n == "ReT" -> Just (i,o,m,a)

             | otherwise              -> Nothing
           _                          -> Nothing      
         ---
         dstrStT :: Ty -> Maybe [Ty]
         dstrStT = \ case
           TyApp _ (TyApp _ (TyCon _ n) s) m | n2s n == "StT" -> do stos <- dstrStT m
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
     (App an (Var _ t x) e) | xs == "return"        -> return $ Return an t e
                            | xs == "put"           -> return $ Put an e
                            | xs == "lift"          -> return $ Lift an e
            where xs = n2s x
     App an (App _ f@(Var _ t x) e) g | xs == ">>=" -> return $ Bind an t e g
                                      | otherwise   -> return $ Apply an f [e,g]
            where xs = n2s x
     t@(App an _ _)                                 -> do (f,_,es) <- destructApp t
                                                          return $ Apply an f es
     Case an t dsc bnds me                          -> do (p,e) <- unbind bnds
                                                          return (Switch an t dsc p e me)
     d                                              -> failAt (ann d) "Unclassifiable case"

data Cases an p e t = Get an t
                    | Return an t e
                    | Lift an e
                    | Put an e
                    | Bind an t e e
                    | Apply an e [e]
                    | Switch an t e p e (Maybe e)



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
purify_state_body :: (Fresh m, MonadError AstError m) => PureEnv -> [Exp] -> [Ty] -> Int -> Exp -> m Exp
purify_state_body rho stos stys i tm = do
  exp <- classifyCases tm
  case exp of
    Get an _      -> return $ mkTupleExp an $ (stos !! i, stys !! i) : zip stos stys
    Return an t e -> do (te,_) <- destructArrow t
                        return $ mkTupleExp an $ (e,te) : zip stos stys
    Lift an e     -> purify_state_body rho stos stys (i+1) e
    Put an e      -> return $ mkTupleExp an $ (nil,nilTy) : zip stos' stys
        where nil   = Con an nilTy (s2n "()")
              nilTy = TyCon an (s2n "()")
              stos' = replaceAtIndex i e stos
    Apply an e es -> mkPureApp an rho e es
    Switch an t e1 p e2 (Just e) -> do e1' <- purify_state_body rho stos stys i e1
                                       e2' <- purify_state_body rho stos stys i e2
                                       e'  <- purify_state_body rho stos stys i e
                                       return $ Case an t e1' (bind p e2') (Just e')
    Switch an t e1 p e2 Nothing  -> do e1' <- purify_state_body rho stos stys i e1
                                       e2' <- purify_state_body rho stos stys i e2
                                       return $ Case an t e1' (bind p e2') Nothing
    Bind an t e g -> do
      e'    <- purify_state_body rho stos stys i e
      ns    <- freshVars (length stos + 1)
      let vs = foldr (\ (n,t) nts -> Var an t (s2n n) : nts) [] (zip ns stys)
      g_pure_app <- mkPureApp an rho g vs
      (p,_) <- mkTuplePat an (zip ns stys)
      gn    <- var2string g
      ptg   <- lookupPure an gn rho
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
                    | Signal an e t
                    | RBind an e t e t
                    | SigK an e t e t [(e,t)]   -- (signal e >>= g e1 ... ek)
                    | RApp an t e [(e,t)]
                    | RSwitch an t e p e (Maybe e)

--
-- classifyApp classifies syntactic applications. Just seemed easier to
-- make it its own function.
--
classifyApp :: MonadError AstError m => Annote -> (Exp, Ty, [Exp]) -> m (RCase Annote Ty p Exp)
classifyApp an (f,t,[e]) = do
  (te,_) <- destructArrow t
  case f of
   Var _ _ x | xs == "return" -> return $ RReturn an e te
             | xs == "lift"   -> return $ RLift an e te
             | xs == "signal" -> return $ Signal an e te
             | otherwise      -> do bes <- mkBindings t [e]
                                    return $ RApp an t f bes
     where xs = n2s x
classifyApp an (f,t,[e,g]) = case f of
  Var _ tb x | xs == ">>=" -> do sig <- issignal e
                                 case sig of
                                  Just te -> do (g',tg',es) <- destructApp g
                                                bes         <- mkBindings tg' es
                                                return $ SigK an e te g' tg' bes
                                  Nothing -> do (_,tg,_) <- destructApp g
                                                (te,_)   <- destructArrow tb
                                                return $ RBind an e te g tg
             | otherwise   -> do bes <- mkBindings t [e,g]
                                 return $ RApp an t f bes
                 where xs = n2s x
classifyApp an (f,t,es)    = do bes <- mkBindings t es
                                return $ RApp an t f bes

classifyRCases :: (Fresh m, MonadError AstError m) => Exp -> m (RCase Annote Ty Pat Exp)
classifyRCases = \ case 
     t@(App an _ _)        -> do (f,t,es) <- destructApp t
                                 classifyApp an (f,t,es)
     Case an t dsc bnds me -> do (p,e) <- unbind bnds
                                 return (RSwitch an t dsc p e me)
     v@(Var an t _)        -> return $ RApp an t v []
     d                     -> failAt (ann d) "Unclassifiable R-case"
      
-- state for res-purification.
data PSto = PSto [DataCon] [(Pat,Exp)]
addClause dc   = modify (\ (PSto dcs pes) -> PSto (dc:dcs) pes)
addEquation pe = modify (\ (PSto dcs pes) -> PSto dcs (pe:pes))

{-
   purify_res_body
                 rho    -- pure environment
                 ts     -- [T1,...,Tn]
                 i      -- input type
                 o      -- output type
                 t      -- return type of monad
                 stys   -- state types [S1,...,Sm]
                 stos   -- input states [s1,...,sm]
                 tmty   -- type of tm
                 iv     -- fresh var to use for input type
                 tm     -- term to be purified
-}
purify_res_body :: (Fresh m, MonadError AstError m) =>
                   PureEnv -> [Ty] -> Ty -> Ty -> Ty -> [Ty] -> [Exp] -> Ty -> String -> Exp -> StateT PSto m Exp
purify_res_body rho ts i o t stys stos tmty iv tm = do
  cls <- lift $ classifyRCases tm
  case cls of
                               
       RReturn an e t        -> return $ mkTupleExp an $ (mkLeft an etor e, etor) : zip stos stys
          where etor = mkEitherTy an t (mkPairTy an o (TyCon an (s2n "R"))) 
 
       SigK an e te g tg bes -> do
         r_g <- sub g
         let rt   = mkArrowTy $ ts ++ [TyCon an (s2n "R")]
         let rcon = Con an rt (s2n r_g)
         let rGes = mkApp an rcon es 
         addClause (mkRDataCon an r_g ts) -- "R_g T1 ... Tk"
         (p,xs) <- mkRPat an ts r_g       -- Pattern (R_g e1 ... ek)
         let vars = map (\ (x,t) -> Var an t (s2n x)) (zip xs ts)
         g_pure_app <- mkPureApp an rho g (vars ++ [Var an i (s2n iv)])
         addEquation (p,g_pure_app) --"dispatch (R_g e1 ... ek) i = g_pure e1 ... ek i" 
         let (eRg,_) = mkTuple an [(e,te),(rGes,t)]
         -- Calculate Either T (O,R)
         let etor = mkEitherTy an t (mkPairTy an o (TyCon an (s2n "R")))
         return $ mkTupleExp an $ (mkRight an etor eRg, etor) : zip stos stys
              where (es,ts) = unzip bes

       Signal an e tau -> do
         let r_return  = Con an (TyCon an (s2n "R")) (s2n "R_return")
         let (eRret,_) = mkTuple an [(e,tau),(r_return,TyCon an (s2n "R"))]
         let etor      = mkEitherTy an t (mkPairTy an o (TyCon an (s2n "R")))
         return $ mkTupleExp an $ (mkRight an etor eRret,etor) : zip stos stys

       RBind an e te g tg -> do
         -- ert is the return type of e; ty is the type of the whole expression
         (ert,ty)     <- destructArrow tg
         e'           <- purify_res_body rho ts i o ert stys stos ty iv e
         -- start calculating pattern: p = (Left v,(s1,(...,sm)))
         let etor  = mkEitherTy an ert (mkPairTy an o (TyCon an (s2n "R")))
         svars        <- freshVars (length stys)
         v            <- freshVar "v"
         (stps,stoTy) <- mkTuplePat an (zip svars stys)
         let leftv = PatCon an (Embed etor) (Embed $ s2n "Left") [PatVar an (Embed t) (s2n v)]
         let p     =  mkPairPat an etor stoTy leftv stps
         -- done calculating p = (Left v,(s1,(...,sm)))
         -- calculating g_pure_app = "g_pure v s1 ... sm"
         let vars       = map (\ (x,t) -> Var an t (s2n x)) (zip svars stys)
         g_pure_app <- mkPureApp an rho g (Var an ert (s2n v) : vars)
         -- done calculating g_pure_app
         return $ mkLet an ty p e' g_pure_app

       -- N.b., the pure env rho will have to contain *all* purified bindings
       --       or this will fail.
       RLift an e te -> do 
         e' <- lift $ purify_state_body rho stos stys 1 e
         s_i <- freshVars (length stys)
         v   <- freshVar "v"
         -- the pattern "(v,(s1,(...,sm)))"
         p <- mkTuplePat an $ (v,t) : zip s_i stys         
         let etor  = mkEitherTy an t (mkPairTy an o (TyCon an (s2n "R")))
         let leftv = mkLeft an etor (Var an t (s2n v))
         return $ mkTupleExp an $ (leftv,etor) : zip stos stys

       RApp an ty rator rands -> mkPureApp an rho rator (map fst rands)

       RSwitch an ty dsc p e1 Nothing   -> do
         e1' <- purify_res_body rho ts i o t stys stos tmty iv e1
         return $ Case an ty dsc (bind p e1') Nothing

       RSwitch an ty dsc p e1 (Just e2) -> do
         e1' <- purify_res_body rho ts i o t stys stos tmty iv e1
         e2' <- purify_res_body rho ts i o t stys stos tmty iv e2
         return $ Case an ty dsc (bind p e1') (Just e2')

---------------------------
-- A Compendium of Helper Functions
---------------------------

var2string :: MonadError AstError m => Exp -> m String
var2string (Var _ _ x) = return $ n2s x
var2string d           = failAt (ann d) "var2string applied to non-variable"

--
-- Takes t1 and t2 and creates (t1,t2). I'm assuming (?!?) that the name of the
-- pairing constructor is "(,)". Correct me if I'm wrong.                  
--                  
mkPairTy :: Annote -> Ty -> Ty -> Ty
mkPairTy an t1 t2 = TyApp an (TyApp an (TyCon an pair) t1) t2
    where pair = s2n "(,)"

mkPair :: Annote -> (Exp, Ty) -> (Exp, Ty) -> (Exp, Ty)
mkPair an (e1,t1) (e2,t2) = (App an (App an paircon e1) e2, t1xt2)
   where t1xt2   = mkPairTy an t1 t2
         paircon = Con an t1xt2 (s2n "(,)")

mkTuple :: Annote -> [(Exp, Ty)] -> (Exp, Ty)
mkTuple an [] = error "one"
mkTuple an bs = foldr1 (mkPair an) bs

mkTupleTy :: [Ty] -> Ty
mkTupleTy [] = error "two"
mkTupleTy bs = foldr1 (mkPairTy NoAnnote) bs

mkTupleExp :: Annote -> [(Exp, Ty)] -> Exp
mkTupleExp an [] = error "three"
mkTupleExp an bs = fst (mkTuple an bs)

{-
  Takes [T1,...,Tn] and returns (T1 -> (T2 -> ... (T(n-1) -> Tn) ...))
-}
mkArrowTy :: [Ty] -> Ty
mkArrowTy []  = error "four"
mkArrowTy tys = foldr1 arr0 tys

mkPairPat :: Annote -> Ty -> Ty -> Pat -> Pat -> Pat
mkPairPat an ty1 ty2 p1 p2 = PatCon an (Embed $ mkPairTy an ty1 ty2) (Embed (s2n "(,)")) [p1,p2]

mkTuplePat :: MonadError AstError m => Annote -> [(String, Ty)] -> m (Pat, Ty)
mkTuplePat an []         = failAt an "Empty product patterns don't exist"
mkTuplePat an ((n,t):[]) = return (PatVar an (Embed t) (s2n n),t)
mkTuplePat an ((n,t):ns) =
  do (p2,t2) <- mkTuplePat an ns
     let p1p2 = PatCon an (Embed $ mkPairTy an t t2) (Embed (s2n "(,)")) [pn,p2]
     return $ (p1p2, mkPairTy an t t2)
         where pn = PatVar an (Embed t) (s2n n)

-- Shouldn't mkLeft/mkRight change the type t to an Either?
mkLeft :: Annote -> Ty -> Exp -> Exp
mkLeft an t e = App an (Con an t (s2n "Left")) e
mkRight :: Annote -> Ty -> Exp -> Exp
mkRight an t e = App an (Con an t (s2n "Right")) e

mkEitherTy :: Annote -> Ty -> Ty -> Ty
mkEitherTy an t1 t2 = TyApp an (TyApp an (TyCon an either) t1) t2
    where either = s2n "Either"

sub :: MonadError AstError m => Exp -> m String
sub g = case g of
  (Var an t x) -> return $ "R_" ++ gn
      where gn = n2s x
  d            -> failAt (ann d) "sub failed"            

{-
rtype :: Annote -> String -> [Ty] -> Ty
rtype an r_g tes = foldr arr0 (TyCon an (s2n r_g)) tes
-}

mkRDataCon :: Annote -> String -> [Ty] -> DataCon
mkRDataCon an r_g ts = DataCon an (s2n r_g) ([] |-> ty)
   where ty = foldr (TyApp an) (TyCon an (s2n "R")) ts

mkRPat :: Fresh m => Annote -> [Ty] -> String -> m (Pat,[String])
mkRPat an ts r_g = do
  xs <- freshVars (length ts) -- fencepost counting problem?
  let varpats = map (\ (x,t) -> PatVar an (Embed t) (s2n x)) (zip xs ts)
  return (PatCon an (Embed ty) (Embed $ s2n r_g) varpats, xs)
   where ty = foldr (TyApp an) (TyCon an (s2n r_g)) ts

mkPureVar :: MonadError AstError m => PureEnv -> Exp -> m Exp
mkPureVar rho = \ case
  (Var an t x) -> do t' <- lookupPure an xs rho
                     return $ Var an t' (s2n $ xs ++ "_pure")
      where xs = n2s x
  d            -> failAt (ann d) "Can't make a pure variable out of a non-variable"            

mkApp :: Annote -> Exp -> [Exp] -> Exp
mkApp an rator = foldr (App an) rator

mkPureApp :: MonadError AstError m => Annote -> PureEnv -> Exp -> [Exp] -> m Exp
mkPureApp an rho e es = do ep <- mkPureVar rho e
                           return $ mkApp an e es

destructApp :: MonadError AstError m => Exp -> m (Exp, Ty, [Exp])
destructApp (App _ f@(Var _ t _) rand) = return (f,t,[rand])
destructApp (App _ rator rand)         = do (f,t,es) <- destructApp rator
                                            return (f,t,es++[rand])
destructApp x@(Var _ t _)              = return (x,t,[])
destructApp d                          = failAt (ann d) "Tried to destruct non-app"

-- > let p = e1 in e2
--     becomes
-- > case e1 of { p -> e2 }
mkLet :: Annote -> Ty -> Pat -> Exp -> Exp -> Exp
mkLet an ty p e1 e2 = Case an ty e1 (bind p e2) Nothing

mkBindings :: MonadError AstError m => Ty -> [t] -> m [(t, Ty)]
mkBindings t []     = return []
mkBindings t (e:es) = do (dom,ran) <- destructArrow t
                         bes       <- mkBindings ran es
                         return $ (e,dom) : bes

issignal :: MonadError AstError m => Exp -> m (Maybe Ty)
issignal (App _ (Var _ t x) e) | xs == "signal" = do (te,_) <- destructArrow t
                                                     return (Just te)
                               | otherwise      = return Nothing
            where xs = n2s x
issignal _                                      = return Nothing


destructArrow :: MonadError AstError m => Ty -> m (Ty,Ty)
destructArrow t@(TyApp an (TyApp _ (TyCon _ con) t1) t2) = case n2s con of
                                                           "->" -> return (t1,t2)
                                                           _    -> failAt an "Non-arrow type(3)"
destructArrow t                                          = failAt (ann t) "Non-arrow type(4)"



