module ReWire.Core.Transform ( removeEmpty ) where

import ReWire.Annotation (Annote, noAnn)
import ReWire.Core.Syntax
import Data.Maybe (catMaybes)
import Data.List (foldl')

removeEmpty :: Monad m => Program -> m Program
removeEmpty (Program start ds) = return $ Program start $ map reDefn $ filter ((>0) . sizeOf . typeOf) ds

reDefn :: Defn -> Defn
reDefn d = d { defnTy = reTy (defnTy d), defnBody = reExp (renumLVars $ defnTy d) (defnBody d) }

renumLVars :: Ty -> LId -> Maybe (LId, Int)
renumLVars (Ty an szVec _) x = if x >= 0 && x < length szVec && szVec !! x > 0
      then Just (x - preZeros, szVec !! x)
      else Nothing
      where preZeros :: Int
            preZeros = preZeros' szVec 0 x

            preZeros' :: [Int] -> Int -> Int -> Int
            preZeros' _ n 0  = n
            preZeros' [] n _ = n
            preZeros' (0 : szs) n m = preZeros' szs (n + 1) (m - 1)
            preZeros' (_ : szs) n m = preZeros' szs n (m - 1)

reExp :: (LId -> Maybe (LId, Int)) -> Exp -> Exp
reExp rn = \ case
      Call a t n args                       -> Call a t n $ map (reExp rn) $ filter ((> 0) . sizeOf . typeOf) args
      LVar a t l                            -> case rn l of
            Just (l', _) -> LVar a t l'
            Nothing      -> LVar a t l
      Con a t d args                        -> Con a t d $ map (reExp rn) $ filter ((> 0) . sizeOf . typeOf) args
      Match a t e _ g ls _  | sizeOf (typeOf e) == 0
                                            -> reExp rn $ toCall a t g (catMaybes $ map rn ls)
      Match a t e p g ls (Just e')          -> Match a t (reExp rn e) (rePat p) g (map fst $ catMaybes $ map rn ls) (Just $ reExp rn e')
      Match a t e p g ls Nothing            -> Match a t (reExp rn e) (rePat p) g (map fst $ catMaybes $ map rn ls) Nothing
      NativeVHDL a t txt args               -> NativeVHDL a t txt $ map (reExp rn) $ filter ((> 0) . sizeOf . typeOf) args
      where toCall :: Annote -> Ty -> GId -> [(LId, Int)] -> Exp
            toCall a t g ls = Call a t g $ map (\ (x, sz) -> LVar a (Ty a [] sz) x) ls

reTy :: Ty -> Ty
reTy (Ty an ps r) = Ty an (filter (> 0) ps) r

rePat :: Pat -> Pat
rePat = \ case
      PatCon a t dc ps -> PatCon a t dc $ map rePat $ filter ((> 0) . sizeOf . typeOf) ps
      PatVar a t       -> PatVar a t

