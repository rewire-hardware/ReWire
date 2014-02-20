{-
 -
 -
 - This implementation of a regular expression matching
 - device is from Sidhu and Prasanna's "Fast Regular 
 - Expression Matching using FPGAs".  
 -
 -}


module RegExp where

import Control.Monad.Resumption.Reactive
import Control.Monad.Identity


{- Here we define the abstract syntax for regular expressions -}

data RegExp a =   Bar   (RegExp a) (RegExp a)
                | Star  (RegExp a)
                | Cons  (RegExp a) (RegExp a)
                | Paren (RegExp a)
                | Atom a


type Machine input = ReacT (Bool,input) Bool Identity ()


match :: Eq a => a -> Machine a
match a = let match' = \(prev,i) -> ReacT (return (Right (prev && i == a, match')))
           in ReacT (return (Right (False,match')))
