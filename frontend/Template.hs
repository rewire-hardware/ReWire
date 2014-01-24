{-#LANGUAGE TemplateHaskell#-}
module Template where

import Language.Haskell.TH

sel :: Int -> Int -> ExpQ
sel i n = [| \x -> $(caseE [| x |] [alt]) |]
    where alt :: MatchQ
          alt = match pat (normalB rhs) []
 
          pat :: Q Pat
          pat = tupP (map varP as)
 
          rhs :: ExpQ
          rhs = varE(as !! (i -1)) -- !! is 0 based
 
          as :: [Name]
          as = [mkName ("a" ++ show i) | i <- [1..n] ]
