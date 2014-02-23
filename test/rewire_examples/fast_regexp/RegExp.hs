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
import Control.Monad.State
import Control.Monad.Identity


{- Here we define the abstract syntax for regular expressions -}

data RegExp a =   Bar   (RegExp a) (RegExp a)
                | Star  (RegExp a)
                | Cons  (RegExp a) (RegExp a)
                | Paren (RegExp a)
                | Atom a


type Output = Maybe Bool
type Input input = (Bool,input)

type Machine input = ReacT (Input input) Output Identity ()
type WMachine input = (Output,input) -> Machine input


match :: Eq a => a -> Machine a
match a = ReacT (return (Right (Nothing, match' Nothing)))
    where
      match' prev (ff_input,i) = case prev of 
                                        Nothing    -> ReacT 
                                                            (return 
                                                              (Right 
                                                                (Nothing, match' (Just ff_input))))
                                        Just prev' -> ReacT 
                                                            (return 
                                                              (Right 
                                                                  (Just ((i == a) && prev'), match' (Just ff_input))))


(<|>) :: Eq a => Machine a -> Machine a -> Machine a 
r1 <|> r2 = ReacT $ do
                      Right (r1_result,r1_resume) <- deReacT r1
                      Right (r2_result,r2_resume) <- deReacT r2
                      case (r1_result,r2_result) of
                                (Nothing,Nothing)    -> (return
                                                                (Right
                                                                   (Nothing, \x -> (r1_resume x) <|> (r2_resume x))))
                                (Just r1r,Nothing)   -> (return 
                                                                (Right 
                                                                   (Nothing, barcont (r1_result,r1_resume) r2_resume)))
                                (Nothing,Just r2r)   -> (return
                                                                (Right
                                                                   (Nothing, barcont (r2_result,r2_resume) r1_resume)))
                                (Just r1r, Just r2r) -> (return
                                                                (Right
                                                                  (Just (r1r || r2r), \x -> (r1_resume x) <|> (r2_resume x))))
    where
      barcont h@(Just held_res,held_cont) resume input = ReacT $ do
                                                             Right (res, cont) <- deReacT $ resume input
                                                             case res of
                                                                  Nothing -> (return
                                                                                (Right
                                                                                  (Nothing, barcont h cont)))
                                                                  Just res' -> (return
                                                                                (Right
                                                                                  (Just (res' || held_res), \x -> (held_cont x) <|> (cont x))))


seq :: Eq a => Machine a -> Machine a -> Machine a
seq m1 m2 = ReacT $ do
                      Right (r1_result, r1_resume) <- deReacT m1
                      Right (_, r2_resume) <- deReacT m2 
                      case r1_result of
                          Just r1  -> undefined
  where
    runLeft react = ReacT $ do
                              Right (result,resume) <- deReacT react
                              case result of
                                    Nothing -> (return
                                                 (Right
                                                  (Nothing, \x -> runLeft (resume x))))
                                    Just res -> do
                                                  Right (rresult, rresume) <- deReacT m2
                                                  case rresult of
                                                        Nothing -> runRight (result,resume)
    runRight m@(mresult,mresume) react input = undefined
                                                
                                               
    
