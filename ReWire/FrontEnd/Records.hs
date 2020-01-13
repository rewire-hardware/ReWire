{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
module ReWire.FrontEnd.Records
      ( desugarDefns
      , desugarRecData
      , desugarRec
      ) where

import ReWire.Annotation
import ReWire.FrontEnd.Unbound
      ( Fresh (..), name2String, string2Name
      )
import ReWire.Error
import ReWire.FrontEnd.Syntax

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

flatten :: [([a],b)] -> [(a,b)]
flatten = foldr (\ (as,b) asbs -> cross b as ++ asbs) []
      where cross :: b -> [a] -> [(a, b)]
            cross b = foldr (\ a as -> (a,b) : as) [] 

mkApp :: Annote -> Exp -> [Exp] -> Exp
mkApp _ rator []          = rator
mkApp an rator (arg:args) = mkApp an (App an rator arg) args

rmdups :: Eq a => [(a,b)] -> [(a,b)]
rmdups []         = []
rmdups ((x,e):xs) = case lookup x xs of
                         Just _  -> rmdups xs
                         Nothing -> (x,e) : rmdups xs

----------------
-- Desugaring variable and constructor record updates
----------------

findIndex :: Name FieldId -> Int -> [(Name FieldId, a)] -> Maybe Int
findIndex _ _ []            = Nothing
findIndex f i ((nf,_):rest) = if name2String f == name2String nf
                               then Just i
                               else findIndex f (i+1) rest

lookUpF :: (Eq a, Annotation an, MonadError AstError m) =>
               an -> a -> [(t, [(a, b)])] -> m (t, [(a, b)])
lookUpF an _ []           = failAt an "Unrecognized Field Name"
lookUpF an f ((d,fs):dfs) = case lookup f fs of
                              Just _  -> return (d,fs)
                              Nothing -> lookUpF an f dfs

{-
lookUpF :: Eq a => a -> [(t, [(a, b)])] -> Maybe (t, [(a, b)])
lookUpF _ []           = Nothing
lookUpF f ((d,fs):dfs) = case lookup f fs of
                              Just _  -> Just (d,fs)
                              Nothing -> lookUpF f dfs
-}
----------------
-- Desugaring data declarations with a single constructor that is also in record form                      
----------------

desugarDefns :: (Fresh m, MonadError AstError m) =>
                [DataDefn] ->
                [Defn]     ->
                m [Defn]
desugarDefns ts ds = mapM (desugarDefn rho) ds
   where rho = mkRecEnv ts

desugarDefn :: (Fresh m, MonadError AstError m) =>
               DataEnv ->
               Defn    ->
               m Defn
desugarDefn rho d = do
  let Embed b = defnBody d
  (ns,e) <- unbind b
  e'     <- transRecUpdate rho e
  let b' = bind ns e'
  return  $ d { defnBody = Embed b' }

-- transRecUpdate is written under the assumption that the 'e' below is either a Var or a Con.
-- I think that it's possible (have to check the parser) that it's a RecUp. I'm going to assume
-- away the RecUp case for the time being.                      

transRecUpdate :: (MonadError AstError m, Fresh m) =>
                  [(Embed (Name DataConId), [(Name FieldId, Embed Poly)])] ->
                  Exp                                                      ->
                  m Exp
transRecUpdate _ (RecUp an _ _ [])              = failAt an "Ill-formed Record Update."
transRecUpdate rho (RecUp an t e ups@((f,_):_)) = case e of
  Var _ _ _  -> do (c,fs) <- lookUpF an f rho
                   varrecupdate an (Embed t) c fs ups e
  Con _ t _  -> do (Embed c,fs) <- lookUpF an f rho
                   let ups'  = rmdups ups
                   let l_ups = length ups'
                   if length fs /= l_ups
                   then 
                     failAt an "Constructor record applications must be fully saturated in ReWire."
                   else
                     do exps <- arrange an fs ups'
                        return $ mkApp an (Con an t c) exps
  d          -> failAt (ann d) "Ill-formed record update."
transRecUpdate _ (RecConApp an _ _ [])            = failAt an "Ill-formed Record Construction."

transRecUpdate rho (RecConApp an t c ups@((f,_):_)) = do
                   (Embed _,fs) <- lookUpF an f rho
                   let ups'  = rmdups ups
                   let l_ups = length ups'
                   if length fs /= l_ups
                   then 
                     failAt an "Constructor record applications must be fully saturated in ReWire."
                   else
                     do exps <- arrange an fs ups'
                        return $ mkApp an (Con an t c) exps

transRecUpdate _ e              = return e

-- PU, this code stinks.
arrange :: (MonadError AstError m, Eq a) => Annote -> [(a,b)] -> [(a,c)] -> m [c]
arrange _ [] _            = return []
arrange an ((f,_):fs) ups = case lookup f ups of
                                 Just e -> do es <- arrange an fs ups'
                                              return (e:es)
                                     where ups' = filter (\ (f',_) -> f /= f') ups
                                 Nothing -> failAt an "Failure processing constructor record application."

varrecupdate :: Fresh m =>
                Annote                       ->
                Embed Ty                     ->
                Embed (Name DataConId)       ->
                [(Name FieldId, Embed Poly)] ->
                [(Name FieldId, Exp)]        ->
                Exp                          ->
                m Exp
varrecupdate _ _ _ _ [] e             = return e
varrecupdate an t c fs ((f,e'):ups) e = do e'' <- recupdate an t c (f,e') fs e 
                                           varrecupdate an t c fs ups e''

recupdate :: Fresh m =>
             Annote                       ->
             Embed Ty                     ->
             Embed (Name DataConId)       ->
             (Name FieldId,Exp)           ->
             [(Name FieldId, Embed Poly)] ->
             Exp                          ->
             m Exp
recupdate an ty@(Embed t) c (f,new) fds dscr = do
  (p,e) <- mkRecPatExp an ty c f fds new
  return (Case an t dscr (bind p e) Nothing)

type DataEnv = [(Embed (Name DataConId), [(Name FieldId, Embed Poly)])]
mkRecEnv :: [DataDefn] -> DataEnv
mkRecEnv = foldr f []
  where f d ds = case dataCons d of
                      [RecCon _ n _ fds] -> (Embed n,flatten fds) : ds
                      _                  -> ds

-- Given the declaration:  data T = C {f1,f2 :: Int}  
-- Record Variable Update: x {f1 = 9}
-- ...is desugared as:     case x of C d f2    -> C 9 f2
mkRecPatExp
  :: Fresh m =>
     Annote                       ->
     Embed Ty                     ->
     Embed (Name DataConId)       ->
     Name FieldId                 ->
     [(Name FieldId, Embed Poly)] ->
     Exp                          ->
     m (Pat, Exp)
mkRecPatExp an typ@(Embed ty) c@(Embed cstr) f fns e = do
  fvs <- freshVars "rpat" m
  let fns'   = map ((\ (Embed x) -> x) . snd) fns
  fns'' <- mapM poly2Ty fns'
  let foobar = zip fvs fns''
-- constructing the pattern (e.g., "C d f2")
  let pvs    = map (\ (v,t) -> PatVar an (Embed t) v) foobar
  let pat    = PatCon an typ c pvs
-- constructing the expression (e.g., "C 9 f2")
  let Just i = findIndex f 0 fns
  let cvs    = map (\ (v,t) -> Var an t v) foobar
  let cvs'   = replaceAtIndex i e cvs
  let exp    = mkApp an (Con an ty cstr) cvs'
  return (pat,exp)
     where m = length fns

----------------
-- Desugaring data declarations with a single constructor that is also in record form                      
----------------

desugarRecData :: DataDefn -> DataDefn
desugarRecData d = case dataCons d of
                        [RecCon _ n t _] -> d { dataCons=[DataCon (MsgAnnote "arglebargle") n t] }
                        _                 -> d

desugarRec :: (Fresh m, MonadError AstError m) => DataDefn -> m [Defn]
desugarRec d = case dataCons d of
                    [RecCon an n t fds] -> recordDefs an n t fds
                    _ | recfree d       -> return []
                      | otherwise       -> failAt (ann d) "Unsupported record syntax"
    where recfree :: DataDefn -> Bool
          recfree = foldr f True . dataCons
          f d ds = case d of
                      RecCon _ _ _ _ -> False
                      DataCon _ _ _  -> ds

{-
recordDefs converts the guts of a RecCon (i.e., its arguments) into a list of Defn's. Each of
these Defn purports to be a desugaring of a field name.
-}
recordDefs :: (Fresh m, MonadError AstError m) =>
              Annote -> Name DataConId -> Embed Poly -> [([Name FieldId], Embed Poly)] -> m [Defn]
recordDefs an n (Embed (Poly phi)) flds = do
                   (_,ty) <- unbind phi
                   tys'   <- mapM poly2Ty polys
                   let targ = rangeTy ty
                   let t1t2 = foldr (\ b bs -> (targ,b) : bs) [] tys'
                   let pats = mkpats an (Embed ty) (map (\ (t1,t2) -> Embed (arr0 t1 t2)) t1t2) n m
                   return $ map (\ (p,f,(t1,t2)) -> mkdefn an f t1 t2 p) (zip3 pats fs t1t2)
  where 
        fdecs = flatten flds
        fs    = map (string2Name . name2String . fst) fdecs
        polys = map ((\ (Embed x) -> x) . snd) fdecs
        m     = length fdecs

{-
(mkdefn an f ty t p) creates abstract syntax for the following definition:
   f :: t1 -> t2
   f = \ (e::t1) -> case e of
                         p -> (y::t2)
-}
mkdefn :: Annote -> Name Exp -> Ty -> Ty -> Pat -> Defn
mkdefn an f t1 t2 p = Defn 
                        { defnAnnote = an
                        , defnName   = f
                        , defnPolyTy = ty
                        , defnInline = False -- True -- or whatever
                        , defnBody   = body
                        }
  where
    ty      = [] |-> arr0 t1 t2
    body    = Embed (bind [string2Name "e"] caseexp)
    caseexp = Case an t2 (Var an t1 (string2Name "e")) (bind p (Var an t2 (string2Name "y"))) Nothing

mkpats
  :: (Enum a, Eq a, Num a) =>
     Annote -> Embed Ty -> [Embed Ty] -> Name DataConId -> a -> [Pat]
mkpats an t ts n m = map (mkpi an t ts n m) [0..m-1]
  where mkpi an t ts n m i = PatCon an t (Embed n) pats
          where pats          = map tweek (zip [0..m-1] ts)
                tweek (j,tau) = PatVar an tau (if i==j then string2Name "y" else string2Name "x")
  

{-
-----------------------------------------------------------------------
-- What follows are some notes about what I don't implement and why. --
-----------------------------------------------------------------------

data C = Car {company, boo :: String, model :: String, year :: Int} | Boat { etc :: Int } deriving (Show) 
data S = S1 { x :: Int } | S2 { x :: Int }   -- OK  
--data S' = S1' { x' :: Int } | S2' { x' :: Char }   -- Not OK since x' has different types.
--data F = F { x :: Int }                            -- Not OK since x already field of S.


--
-- This is the definition arising from desugaring the year field of the Car constructor of the C type.
-- 
yeardef :: C -> Int
yeardef c = case c of
                 Car _ _ _ y -> y
                 Boat _      -> undefined


--
-- This is the definition arising from desugaring the x field of the S1 and S2 constructors of the S type.
-- 
xdef :: S -> Int
xdef s = case s of
              S1 y -> y
              S2 y -> y

{-
Some things to note. 

Because C has two record constructors and year is not a field of the second constructor, Boat,
the function yeardef may generate an undefined. That's a no-no in ReWire.

1. If we have a data type with one and only one constructor, and it's a record constructor, then we can desugar it in ReWire.
2. We cannot have a data type with a record constructor and a normal constructor, because it will introduce unedfined's.
3. If we have a data type with multiple constructors and one of them is a record constructor, then they must all be record constructors and, furthermore, each record constructor must have identical sets of fields. Yet even furthermore, each field in each respective constructor must have the same type (as in S above).

Upshot: I'm going to assume that case (1) holds --- i.e., that record constructors are the one and only constructor in their respective data types.

-}

-}
