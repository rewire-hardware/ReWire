{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module ReWire.FrontEnd.Purification where

import ReWire.Annotation
import ReWire.FrontEnd.Unbound
      ( Fresh (..), name2String, string2Name
      )
import ReWire.Error
import ReWire.FrontEnd.Syntax
import ReWire.FrontEnd.Records (freshVar, freshVars, replaceAtIndex, poly2Ty)
import Control.Monad.State

--
-- N.b., this is my attempt to rough out the purification pass described in Adam's note.
-- I'm putting this in a separate file (i.e., not "Purify.hs") because I'm old fashioned.
--



{-
- [State-monadic functions]

    Suppose

      f :: T1 -> T2 -> ... -> Tn -> StT S1 (StT S2 (... (StT Sm I))) T
      f x1 ... xn = e.

    Replace this with

      f_pure :: T1 -> ... -> Tn -> S1 -> ... -> Sm -> (T,(S1,(...,Sm)))
      f_pure x1 ... xn s1 ... sm = "[|purify_state_body 1 e|]"

    where s1 ... sm are fresh variables
-}

{-
records :: (Fresh m, MonadError AstError m) => Program -> m Program
records (Program p) = do
      (ts, ds) <- untrec p
      ds'      <- desugarDefns ts ds
      newdefs  <- mapM desugarRec ts
      let ts' = map desugarRecData ts
      return $ Program $ trec (ts', ds' ++ concat newdefs)
-}

purify :: (Fresh m, MonadError AstError m) => Program -> m Program
purify (Program p) = do
      (ts, ds) <- untrec p
      mds      <- mapM isStateMonadicDefn ds
      let (smds,pds) = partition mds
      let nameNpoly  = map (\ d -> (defnName d, defnPolyTy d)) smds
      rho      <- mkPureEnv nameNpoly
      pure_ds  <- mapM (purifyDefn rho) smds
      return $ Program $ trec (ts,pds++pure_ds) 
   where f Nothing ms  = ms
         f (Just a) ms = a : ms

partition :: [Either a b] -> ([a], [b])
partition = foldl step  ([],[])
   where
     step (smds,pds) (Left d)  = (d : smds, pds)
     step (smds,pds) (Right d) = (smds, d : pds)

type PureEnv = [(String, Ty)]

mkPureEnv :: Fresh m => [(Name a, Embed Poly)] -> m PureEnv
mkPureEnv []                  = return []
mkPureEnv ((n,Embed phi):nps) = do ty  <- poly2Ty phi 
                                   env <- mkPureEnv nps
                                   return $ (name2String n, ty) : env

--
-- The main work is filling this in
--
purifyDefn :: (Fresh m, MonadError AstError m) => PureEnv -> Defn -> m Defn
purifyDefn rho d = do ty <- lookupPure (defnAnnote d) dname rho
                      let p_pure = [] |-> ty
                      let d_pure = string2Name $ dname ++ "_pure"
                      (args,e) <- unbind body
                      e' <- purify_state_body rho undefined undefined 0 e
                      let b_pure = bind args e'
                      return $ d { defnName=d_pure, defnPolyTy=p_pure, defnBody=(Embed b_pure) }
  where
    dname      = name2String $ defnName d
    Embed body = defnBody d

--purify_state_body :: (Fresh m, MonadError AstError m) => PureEnv -> [Exp] -> [Ty] -> Int -> Exp -> m Exp

isStateMonadicDefn :: Fresh m => Defn -> m (Either Defn Defn)
isStateMonadicDefn d = do ty <- poly2Ty poly
                          if isStateMonadic ty
                             then return $ Left d
                             else return $ Right d
  where Embed poly = defnPolyTy d
        nd         = name2String (defnName d)
        isStateMonadic :: Ty -> Bool
        isStateMonadic = isStateMonad . rangeTy
          where
            isStateMonad :: Ty -> Bool
            isStateMonad = \ case
              TyComp an (TyApp _ (TyApp _ (TyCon _ n) s) m) a | name2String n == "StT" ->
                                                                  isStateMonad (TyComp an m a)
                                                              | otherwise              -> False
              TyComp an (TyCon _ n) a                         | name2String n == "I"   -> True
                                                              | otherwise              -> False
              _                                                                        -> False

data TyVariety t = Arrow Annote t t
                 | StTApp Annote t t t
                 | IdApp Annote t
                 | Pure Annote t

instance Annotated (TyVariety t) where
      ann = \ case
            Arrow a _ _     -> a
            StTApp a _ _ _  -> a
            IdApp a _       -> a
            Pure a _        -> a

classifyTy :: MonadError AstError m => Ty -> m (TyVariety Ty)
classifyTy t = case t of
   TyApp an (TyApp _ (TyCon _ con) t1) t2          | name2String con == "->" -> return $ Arrow an t1 t2
                                                   | otherwise               -> return $ Pure an t
   TyComp an (TyApp _ (TyApp _ (TyCon _ n) s) m) a | name2String n == "StT"  -> return $ StTApp an s m a
                                                   | otherwise               ->
                                             failAt an $ "Unclassifiable monad transformer: " ++ show n
   TyComp an (TyCon _ n) a                         | name2String n == "I"    -> return $ IdApp an a
                                                   | otherwise               ->
                                             failAt an $ "Unclassifiable monad: " ++ show n
   _                                                                         -> return $ Pure (ann t) t
   
--
-- Takes
--      T1 -> T2 -> ... -> Tn -> StT S1 (StT S2 (... (StT Sm I))) T
-- and replaces it by
--      T1 -> ... -> Tn -> S1 -> ... -> Sm -> (T,(S1,(...,Sm)))
--
-- I'm going to rewrite this to stick precisely to Adam's description.
-- N.b., the product in the codomain associates to the right. The
-- definition of purifyTy required a little hackery to get the result
-- into that form.
--
purifyTy :: MonadError AstError m => Ty -> m Ty
purifyTy t = do c <- classifyTy t 
                case c of
                  Arrow _ t1 t2     -> do t2' <- purifyTy t2
                                          return $ arr0 t1 t2'
                  StTApp an s m a   -> do (v,stos) <- flattenStT t
                                          purifyLayered an (v,stos)
--                  StTApp an s m a   -> do t' <- purifyTy (TyComp an m (mkPairTy an a s))
--                                          return $ arr0 s t'
                  IdApp _ a         -> return a
                  Pure _ ty         -> return ty

purifyLayered :: MonadError AstError m => Annote -> (Ty, [Ty]) -> m Ty
purifyLayered an (v,stos) = do xty <- productTy an (v:stos)
                               return $ foldl arr0 xty stos
                               
flattenStT :: MonadError AstError m => Ty -> m (Ty, [Ty])
flattenStT t = do c <- classifyTy t
                  case c of
                   StTApp an s m a -> do (v,stos) <- flattenStT (TyComp an m a)
                                         return (v,s:stos)
                   IdApp _ a       -> return (a,[])
                   _               -> failAt (ann c) "Can't flatten monadic type"


--
-- Takes t1 and t2 and creates (t1,t2). I'm assuming (?!?) that the name of the
-- pairing constructor is "(,)". Correct me if I'm wrong.                  
--                  
mkPairTy :: Annote -> Ty -> Ty -> Ty
mkPairTy an t1 t2 = TyApp an (TyApp an (TyCon an pair) t1) t2
    where pair = string2Name "(,)"

productTy :: MonadError AstError m => Annote -> [Ty] -> m Ty
productTy an []     = failAt an "Product of no types should not occur."
productTy an (t:[]) = return t
productTy an (t:ts) = do xts <- productTy an ts
                         return $ mkPairTy an t xts

mkTuple :: MonadError AstError m => Annote -> [Exp] -> m Exp
mkTuple an []     = failAt an "Empty product expressions don't exist"
mkTuple an (e:[]) = return e
mkTuple an (e:es) = do tes <- mkTuple an es
                       return $ App an e tes

mkPairPat :: Annote -> Ty -> Ty -> String -> String -> Pat
mkPairPat an ty1 ty2 n1 n2 = PatCon an (Embed $ mkPairTy an ty1 ty2) (Embed (string2Name "(,)")) [p1,p2]
   where p1        = PatVar an (Embed ty1) (string2Name n1)
         p2        = PatVar an (Embed ty2) (string2Name n2)

mkTuplePat :: MonadError AstError m => Annote -> [(String, Ty)] -> m (Pat, Ty)
mkTuplePat an []         = failAt an "Empty product patterns don't exist"
mkTuplePat an ((n,t):[]) = return (PatVar an (Embed t) (string2Name n),t)
mkTuplePat an ((n,t):ns) =
  do (p2,t2) <- mkTuplePat an ns
     let p1p2 = PatCon an (Embed $ mkPairTy an t t2) (Embed (string2Name "(,)")) [pn,p2]
     return $ (p1p2, mkPairTy an t t2)
         where pn = PatVar an (Embed t) (string2Name n)

classifyCases :: (Fresh m, MonadError AstError m) => Exp -> m (Cases Annote Pat Exp)
classifyCases = \ case 
     Var an _ g | name2String g == "get"            -> return $ Get an
     (App an (Var _ _ x) e) | xs == "return"        -> return $ Return an e
                            | xs == "put"           -> return $ Put an e
                            | xs == "lift"          -> return $ Lift an e
            where xs = name2String x
     App an (App _ f@(Var _ _ x) e) g | xs == ">>=" -> return $ Bind an e g
                                      | otherwise   -> return $ Apply an f [e,g]
            where xs = name2String x
     t@(App an _ _)                                  -> do (f,_,es) <- destructApp t
                                                           return $ Apply an f es
                                                        where 
     Case an _ dsc bnds me                          -> do (p,e) <- unbind bnds
                                                          return (Switch an dsc p e me)
     d                                              -> failAt (ann d) "Unclassifiable case"

destructApp (App _ f@(Var _ t _) rand) = return (f,t,[rand])
destructApp (App _ rator rand)         = do (f,t,es) <- destructApp rator
                                            return (f,t,es++[rand])
destructApp d                          = failAt (ann d) "Tried to destruct non-app"

-- > let p = e1 in e2
--     becomes
-- > case e1 of { p -> e2 }
mkLet :: Annote -> Ty -> Pat -> Exp -> Exp -> Exp
mkLet an ty p e1 e2 = Case an ty e1 (bind p e2) Nothing

data Cases an p e = Get an
                  | Return an e
                  | Lift an e
                  | Put an e
                  | Bind an e e
                  | Apply an e [e]
                  | Switch an e p e (Maybe e)

lookupPure :: MonadError AstError m => Annote -> String -> PureEnv -> m Ty
lookupPure an x rho = case lookup x rho of
                           Just ty -> return ty
                           Nothing -> failAt an $ "No pure binding for variable: " ++ x

mkPureVar :: MonadError AstError m => PureEnv -> Exp -> m Exp
mkPureVar rho = \ case
  (Var an t x) -> do t' <- lookupPure an xs rho
                     return $ Var an t' (string2Name $ xs ++ "_pure")
      where xs = name2String x
  d            -> failAt (ann d) "Can't make a pure variable out of a non-variable"            

mkApply :: MonadError AstError m => Annote -> PureEnv -> Exp -> [Exp] -> m Exp
mkApply an rho e es = do ep <- mkPureVar rho e
                         return $ foldl (App an) e es


--
-- Make sure to check for fencepost counting problems.
--
purify_state_body :: (Fresh m, MonadError AstError m) => PureEnv -> [Exp] -> [Ty] -> Int -> Exp -> m Exp
purify_state_body rho stos tys i tm = do
  exp <- classifyCases tm
  case exp of
    Get an        -> mkTuple an (stos !! i : stos)
    Return an e   -> mkTuple an (e:stos)
    Lift an e     -> purify_state_body rho stos tys (i+1) e
    Put an e      -> mkTuple an (nil : stos')
        where nil   = Con an (TyCon an (string2Name "()")) (string2Name "()")
              stos' = replaceAtIndex i e stos
    Apply an e es -> mkApply an rho e es
    Switch an e1 p e2 (Just e) -> do e1' <- purify_state_body rho stos tys i e1
                                     e2' <- purify_state_body rho stos tys i e2
                                     e'  <- purify_state_body rho stos tys i e
                                     return $ Case an (TyBlank an) e1' (bind p e2') (Just e')
    Switch an e1 p e2 Nothing  -> do e1' <- purify_state_body rho stos tys i e1
                                     e2' <- purify_state_body rho stos tys i e2
                                     return $ Case an (TyBlank an) e1' (bind p e2') Nothing
    Bind an e g -> do e' <- purify_state_body rho stos tys i e
                      ns <- freshVars (length stos + 1)
                      let vs = foldr (\ (n,t) nts -> Var an t (string2Name n) : nts) [] (zip ns tys)
                      g_pure_app <- mkApply an rho g vs
                      (p,_)  <- mkTuplePat an (zip ns tys)
                      return $ mkLet an ty p e' g_pure_app
                         where ty         = TyBlank an -- Possible to calculate this type, but it's a
                                                       -- pain in the ass. Is it necessary?


{-  
      purify_state_body i (e >>= g)     = "let
                                             (v,(s1,(...,sm))) = [|purify_state_body i e|]
					   in
					     g_pure v s1 ... sm"
-}

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

destructArrow :: MonadError AstError m => Ty -> m (Ty,Ty)
destructArrow t@(TyApp an (TyApp _ (TyCon _ con) t1) t2) = case name2String con of
                                                           "->" -> return (t1,t2)
                                                           _    -> failAt an "Non-arrow type(3)"
destructArrow t                                          = failAt (ann t) "Non-arrow type(4)"

--
-- shinola classifies syntactic applications. Just seemed easier to
-- make it its own function. It distinguishes shit from shinola.
--
shinola :: MonadError AstError m => Annote -> (Exp, Ty, [Exp]) -> m (RCase Annote Ty p Exp)
shinola an (f,t,[e]) = do
  (te,_) <- destructArrow t
  case f of
   Var _ _ x | xs == "return" -> return $ RReturn an e te
             | xs == "lift"   -> return $ RLift an e te
             | xs == "signal" -> return $ Signal an e te
     where xs = name2String x
shinola an (f,t,[e,g]) = case f of
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
                 where xs = name2String x
shinola an (f,t,es)    = do bes <- mkBindings t es
                            return $ RApp an t f bes

mkBindings :: MonadError AstError m => Ty -> [t] -> m [(t, Ty)]
mkBindings t []     = return []
mkBindings t (e:es) = do (dom,ran) <- destructArrow t
                         bes       <- mkBindings ran es
                         return $ (e,dom) : bes

issignal :: MonadError AstError m => Exp -> m (Maybe Ty)
issignal (App _ (Var _ t x) e) | xs == "signal" = do (te,_) <- destructArrow t
                                                     return (Just te)
                               | otherwise      = return Nothing
            where xs = name2String x
issignal _                                      = return Nothing

classifyRCases :: (Fresh m, MonadError AstError m) => Exp -> m (RCase Annote Ty Pat Exp)
classifyRCases = \ case 
     t@(App an _ _)        -> do (f,t,es) <- destructApp t
                                 shinola an (f,t,es)
     Case an t dsc bnds me -> do (p,e) <- unbind bnds
                                 return (RSwitch an t dsc p e me)
     d                     -> failAt (ann d) "Unclassifiable R-case"

-- Shouldn't mkLeft/mkRight change the type t to an Either?
mkLeft :: Annote -> Ty -> Exp -> Exp
mkLeft an t e = App an (Con an t (string2Name "Left")) e
mkRight :: Annote -> Ty -> Exp -> Exp
mkRight an t e = App an (Con an t (string2Name "Right")) e

mkEitherTy :: Annote -> Ty -> Ty -> Ty
mkEitherTy an t1 t2 = TyApp an (TyApp an (TyCon an either) t1) t2
    where either = string2Name "Either"

sub :: MonadError AstError m => Exp -> m String
sub g = case g of
  (Var an t x) -> return $ "R_" ++ gn
      where gn = name2String x
  d            -> failAt (ann d) "sub failed"            

rtype :: Annote -> String -> [Ty] -> Ty
rtype an r_g tes = foldr arr0 (TyCon an (string2Name r_g)) tes
      
-- Just a stub for the clause and equation state
data Bogus  = Bogus
addClause   = undefined
addEquation = undefined


purify_res_body :: (Fresh m, MonadError AstError m) => Ty -> Ty -> Ty -> [Exp] -> Exp -> StateT Bogus m Exp
purify_res_body i o t stos tm = do
  cls <- lift $ classifyRCases tm
  case cls of
--
--     purify_res_body (return e)         = "(Left e,(s1,(...,sm)))"
--
       RReturn an e t        -> lift $ mkTuple an (mkLeft an t e : stos)

       SigK an e te g tg bes -> do
         r_g <- sub g
         let rt   = rtype an r_g ts
         let rcon = Con an rt (string2Name r_g)
         let rGes = foldr (App an) rcon es
         addClause "R_g T1 ... Tk"                                     -- TBD
         addEquation "dispatch (R_g e1 ... ek) i = g_pure e1 ... ek i" -- TBD
         eRg <- lift $ mkTuple an [e,rGes]
-- Calculate Either T (O,R)
         let etor = mkEitherTy an t (mkPairTy an o (TyCon an (string2Name "R")))
         lift $ mkTuple an (mkRight an undefined eRg : stos) -- not sure about the type to pass
          where (es,ts) = unzip bes

{-
	purify_res_body (signal e
	                  >>= g e1 ... ek) = "(Right (e,R_g e1 ... ek),(s1,(...,sm)))"
			          	     Side effect:
					       * add the clause "R_g T1 ... Tk" to R
						    (where Ti is the type of ei)
					       * add the eqn
					           dispatch (R_g e1 ... ek) i
						      = g_pure e1 ... ek i
						 to defn of dispatch

        purify_res_body (signal e)         = "(Right (e,R_ret),(s1,(...,sm)))"
			          	     Side effect:
					       * add the clause "R_return" to R
					       * add the eqn
					           dispatch R_return i = Left i
						 to defn of dispatch

	purify_res_body (e >>= g)          = "let
			       		        -- N.B.: The irrefutable pattern here is
						-- sketchy, but it should be okay because
						-- of the restriction on occurrences of
						-- "signal"
                                                (Left v,(s1,(...,sm))) = [|purify_res_body e|]
                                              in
                                                 g_pure v s1 ... sm"

        purify_res_body (lift e)           = "let
                                                 (v,(s1,(...,sm))) = [|purify_state_body 1 e|]
                                              in
                                                 (Left v,(s1,(...,sm)))"

        purify_res_body (f e1 ... ek)      = "f_pure e1 .... ek"

        purify_res_body (case e of
                           P1 -> e1
                           ...
                           Pk -> ek)       = "case e of
                                                P1 -> [|purify_res_body i e1|]
                                                ...
                                                Pk -> [|purify_res_body i ek|]"
-}
