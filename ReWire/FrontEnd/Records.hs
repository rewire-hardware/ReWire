{-# LANGUAGE FlexibleContexts #-}
module ReWire.FrontEnd.Records where

import ReWire.Annotation
import ReWire.FrontEnd.Unbound
      ( Fresh (..), FreshMT (..), runFreshM
      , Embed (..)
      , TRec (..), trec, untrec
      , Name (..), AnyName (..), SubstName (..)
      , Bind (..), bind, unbind
      , Alpha, aeq, Subst (..)
      , toListOf
      , name2String, string2Name
      )
import ReWire.Error
import ReWire.FrontEnd.Syntax

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
                   tys'   <- mapM splitArrow tys
                   let pats = mkpats an (Embed ty) (map (\ (ty,t) -> Embed (arr0 ty t)) tys') n m
                   return $ map (\ (p,f,(t1,t2)) -> mkdefn an f t1 t2 p) (zip3 pats fs tys')
  where 
        fdecs = flatten flds
        fs    = map (string2Name . name2String . fst) fdecs
        tys   = map ((\ (Embed x) -> x) . snd) fdecs
        m     = length fdecs
        flatten :: [([a],b)] -> [(a,b)]
        flatten = foldr (\ (as,b) asbs -> cross b as ++ asbs) []
            where cross :: b -> [a] -> [(a, b)]
                  cross b = foldr (\ a as -> (a,b) : as) [] 

splitArrow (Poly phi) = do
  (_,ty) <- unbind phi
  case ty of
       (TyApp _ (TyApp _ (TyCon _ arr) t1) t2) -> return (t1,t2)
       d                                       -> failAt (ann d) "record field non-arrow type"


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
                        , defnInline = True -- or whatever
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
  


-----------------------------------------------------------------------
-- What follows are some notes about what I don't implement and why. --
-----------------------------------------------------------------------

data C = Car {company, boo :: String, model :: String, year :: Int} | Boat { etc :: Int } deriving (Show) 
data S = S1 { x :: Int } | S2 { x :: Int }   -- OK  
--data S' = S1' { x' :: Int } | S2' { x' :: Char }   -- Not OK since x' has different types.

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

