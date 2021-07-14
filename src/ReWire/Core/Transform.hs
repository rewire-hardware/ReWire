module ReWire.Core.Transform ( removeEmpty ) where

import ReWire.Annotation (Annote, noAnn)
import ReWire.Core.Syntax
import Data.Maybe (catMaybes)
import Data.List (foldl')

removeEmpty :: Monad m => Program -> m Program
removeEmpty (Program cs ds) = return $ Program (map reCon $ filter ((>0) . sizeof . typeof) cs) $ map reDefn $ filter ((>0) . sizeof . typeof) ds

reCon :: DataCon -> DataCon
reCon (DataCon a d i nctors t) = DataCon a d i nctors (reTy t)

reDefn :: Defn -> Defn
reDefn d = d { defnTy = reTy (defnTy d), defnBody = reExp (renumLVars (defnTy d)) (defnBody d) }

renumLVars :: Ty -> LId -> Maybe LId
renumLVars t x = if x < length szVec && szVec !! x > 0 then Just (x - preZeros) else Nothing
      where szVec :: [Int]
            szVec = case flattenArrow t of
                  (as, _) -> map sizeof as

            preZeros :: Int
            preZeros = preZeros' szVec 0 x

            preZeros' :: [Int] -> Int -> Int -> Int
            preZeros' _ n 0  = n
            preZeros' [] n _ = n
            preZeros' (0 : szs) n m = preZeros' szs (n + 1) (m - 1)
            preZeros' (_ : szs) n m = preZeros' szs n (m - 1)

reExp :: (LId -> Maybe LId) -> Exp -> Exp
reExp rn = \ case
      App _ e1 e2 | sizeof (typeof e2) <= 0 -> reExp rn e1
      App a e1 e2                           -> App a (reExp rn e1) (reExp rn e2)
      Prim a t g                            -> Prim a (reTy t) g
      GVar a t g                            -> GVar a (reTy t) g
      LVar a t l                            -> case rn l of
            Just l' -> LVar a (reTy t) l'
            Nothing -> LVar a (reTy t) l
      Con a t s d                           -> Con a (reTy t) s d
      Match a t e _ g ls _  | sizeof (typeof e) == 0
                                            -> reExp rn $ toApp a (reTy t) g (catMaybes $ map rn ls)
      Match a t e p g ls (Just e')          -> Match a (reTy t) (reExp rn e) (rePat p) g (catMaybes $ map rn ls) (Just $ reExp rn e')
      Match a t e p g ls Nothing            -> Match a (reTy t) (reExp rn e) (rePat p) g (catMaybes $ map rn ls) Nothing
      NativeVHDL a t txt                    -> NativeVHDL a (reTy t) txt
      where toApp :: Annote -> Ty -> GId -> [LId] -> Exp -- TODO(chathhorn): type of lvars
            toApp a t g ls = foldl' (App a) (GVar a dummyTy g) (map (LVar noAnn dummyTy) ls)

reTy :: Ty -> Ty
reTy = \ case
      TyApp _ _ t1 t2 | sizeof t2 <= 0 -> reTy t1
      TyApp a s t1 t2                  -> TyApp a s (reTy t1) (reTy t2)
      t                                -> t

rePat :: Pat -> Pat
rePat = \ case
      PatCon a t dc ps -> PatCon a (reTy t) dc (filter ((>0) . sizeof . typeof) (map rePat ps))
      PatVar a t -> PatVar a (reTy t)
