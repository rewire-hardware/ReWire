module ReWire.Core.Transform ( removeEmpty ) where

import ReWire.Annotation (Annote, noAnn)
import ReWire.Core.Syntax
import Data.Maybe (catMaybes)
import Data.List (foldl')

removeEmpty :: Monad m => Program -> m Program
removeEmpty (Program start ds) = return $ Program start $ map reDefn $ filter ((> 0) . sizeOf) ds

reDefn :: Defn -> Defn
reDefn d = d { defnSig = reSig (defnSig d), defnBody = reExp (renumLVars $ defnSig d) (defnBody d) }

renumLVars :: Sig -> LId -> Maybe (LId, Int)
renumLVars (Sig an szVec _) x = if x >= 0 && x < length szVec && szVec !! x > 0
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
      Call a t n args                       -> Call a t n $ map (reExp rn) $ filter ((> 0) . sizeOf) args
      LVar a s l                            -> case rn l of
            Just (l', _) -> LVar a s l'
            Nothing      -> LVar a s l
      Con a s v w args                      -> Con a s v w $ map (reExp rn) $ filter ((> 0) . sizeOf) args
      Lit a s v                             -> Lit a s v
      Slice a args                          -> Slice a $ map (reExp rn) $ filter ((> 0) . sizeOf) args
      Match a s g e _ _  | sizeOf e == 0
                                            -> reExp rn $ Call a (Sig a [] s) g []
      Match a s g e p (Just e')             -> Match a s g (reExp rn e) (rePat p) $ Just $ reExp rn e'
      Match a s g e p Nothing               -> Match a s g (reExp rn e) (rePat p) Nothing
      NativeVHDL a s txt args               -> NativeVHDL a s txt $ map (reExp rn) $ filter ((> 0) . sizeOf) args

reSig :: Sig -> Sig
reSig (Sig an ps r) = Sig an (filter (> 0) ps) r

rePat :: Pat -> Pat
rePat = \ case
      PatCon a s v w ps -> PatCon a s v w $ map rePat $ filter ((> 0) . sizeOf) ps
      PatSlice a ps     -> PatSlice a $ map rePat $ filter ((> 0) . sizeOf) ps
      p                 -> p

