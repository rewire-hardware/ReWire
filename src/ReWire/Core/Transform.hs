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
      Con a s v w args                      -> Con a s v w $ map (reExp rn) $ filter ((> 0) . sizeOf . typeOf) args
      Match a t e _ g _  | sizeOf (typeOf e) == 0
                                            -> reExp rn $ Call a t g []
      Match a t e p g (Just e')             -> Match a t (reExp rn e) (rePat p) g $ Just $ reExp rn e'
      Match a t e p g Nothing               -> Match a t (reExp rn e) (rePat p) g Nothing
      NativeVHDL a t txt args               -> NativeVHDL a t txt $ map (reExp rn) $ filter ((> 0) . sizeOf . typeOf) args

reTy :: Ty -> Ty
reTy (Ty an ps r) = Ty an (filter (> 0) ps) r

rePat :: Pat -> Pat
rePat = \ case
      PatCon a s v w ps -> PatCon a s v w $ map rePat $ filter ((> 0) . sizeOf . typeOf) ps
      PatVar a t        -> PatVar a t

