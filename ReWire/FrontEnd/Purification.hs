{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module ReWire.FrontEnd.Purification where

import ReWire.Annotation
import ReWire.FrontEnd.Unbound
      ( Fresh (..), name2String, string2Name
      )
import ReWire.Error
import ReWire.FrontEnd.Syntax
import ReWire.FrontEnd.Records (freshVar, freshVars, replaceAtIndex, poly2Ty)

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
    --- THINK THIS STUFF BELOW THROUGH AGAIN. Not sure (flattenApp g) returns the right stuff.
     App an (App _ f@(Var _ _ x) e) g | xs == ">>=" -> return $ Bind an e g
                                      | otherwise   -> return $ Apply an f (e : flattenApp g)
            where xs = name2String x
     Case an _ dsc bnds me                          -> do (p,e) <- unbind bnds
                                                          return (Switch an dsc p e me)
     d                                              -> failAt (ann d) "Unclassifiable case"

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
-- Junkyard below                               
--

{-
stateMonadic :: Defn -> m (Ty,Defn)
stateMonadic d = undefined
  where name         = defnName d
        (Embed poly) = defnPolyTy d

isStateMonad :: [Ty] -> Ty -> Maybe (Ty,[Ty])
isStateMonad stos = \ case
    TyComp an (TyApp _ (TyApp _ (TyCon _ n) s) m) a | name2String n == "StT" ->
                                                        isStateMonad (s:stos) (TyComp an m a)
                                                    | otherwise              -> Nothing
    TyComp an (TyCon _ n) a                         | name2String n == "I"   -> Just (a,reverse stos)
                                                    | otherwise              -> Nothing
    _                                                                        -> Nothing

--ranTy :: Ty -> Ty
ranTy :: [Ty] -> Ty -> (Ty, [Ty])
ranTy taus t@(TyApp _ (TyApp _ (TyCon _ con) t1) t2) = case name2String con of
                                                              "->" -> ranTy (t1:taus) t2
                                                              _    -> (t,taus)
ranTy taus t                                         = (t,taus)
-}
