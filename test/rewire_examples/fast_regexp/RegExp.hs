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

type Output = Maybe Bool
type FFVal  = Output
type Input input = (Output,input)

data Machine input = Machine {deMachine :: (Input input) -> ReacT (Input input) 
                                              Output Identity (Output,Machine input)}

data RegExp a =   Bar   (RegExp a) (RegExp a)
                | Star  (RegExp a)
                | Cons  (RegExp a) (RegExp a)
                | Paren (RegExp a)
                | Atom a


compMachine :: Eq a => RegExp a -> Machine a
compMachine (Bar r1 r2)  = (compMachine r1) <|> (compMachine r2)
compMachine (Star r1)    = star (compMachine r1)
compMachine (Cons r1 r2) = rseq (compMachine r1) (compMachine r2)
compMachine (Paren r1)   = compMachine r1
compMachine (Atom a)     = match a Nothing -- The flipflop is primed with Nothing


runMachine :: Machine input -> (Input input) -> ReacT (Input input) Output Identity (Output,Machine input)
runMachine m1 input = ReacT $ do
                                (output, m1') <- stepMachine input m1
                                return $ Right (output, \input' -> runMachine m1' input')

stepMachine :: Input input -> Machine input -> Identity (Output,Machine input)
stepMachine input m = case ((deReacT ((deMachine m) input))) of
                                  Identity (Left v) -> Identity v
                              

match :: Eq a => a -> (FFVal -> Machine a)
match a = \flipflop -> Machine (\(prev_output, char) -> case flipflop of
                                                              Nothing -> return (Nothing,match a prev_output)
                                                              Just ff -> return (Just ((a == char) && ff),(match a) prev_output))

(<|>) :: Eq a => Machine a -> Machine a -> Machine a 
m1 <|> m2 = Machine (\input -> case fst input of
                                    Nothing    -> return (Nothing, m1 <|> m2)
                                    Just _     -> ReacT $ do
                                                            (output1,resume1) <- stepMachine input m1
                                                            (output2,resume2) <- stepMachine input m2
                                                            case (output1,output2) of
                                                                   (Nothing,Nothing)  -> return $ Left (Nothing, resume1 <|> resume2)
                                                                   (Nothing,_)        -> return $ Left (Nothing, resume1 <|> m2)
                                                                   (_,Nothing)        -> return $ Left (Nothing, m1 <|> resume2)
                                                                   (Just r1, Just r2) -> return $ Left (Just (r1 || r2), resume1 <|> resume2))
                                                                   
        

rseq :: Eq a => Machine a -> Machine a -> Machine a
rseq m1 m2 = Machine (\input -> case fst input of
                                    Nothing -> return (Nothing, rseq m1 m2)
                                    Just _  -> ReacT $ do
                                                          (output1,resume1) <- stepMachine input m1
                                                          case output1 of
                                                                  Nothing -> return $ Left (Nothing, rseq resume1 m2)
                                                                  Just _  -> do
                                                                               (output2, resume2) <- stepMachine (output1,snd input) m2 
                                                                               case output2 of
                                                                                      Nothing  -> return $ Left (Nothing, rseq m1 resume2)
                                                                                      Just _   -> return $ Left (output2, rseq resume1 resume2))

star :: Eq a => Machine a -> Machine a
star m1 = star' (Just False) m1

star' :: Eq a => Output -> Machine a -> Machine a
star' output m1 = Machine (\input -> case fst input of
                                            Nothing -> return (Nothing, star' output m1)
                                            Just inval  -> ReacT $ do
                                                                    (inner_output, inner_resume) <- case output of
                                                                                                           Nothing     -> stepMachine input m1
                                                                                                           Just outval -> stepMachine (Just (inval || outval), snd input) m1
                                                                    return $ Left (inner_output, star' inner_output inner_resume))
                                                                                            
                                                
                                               
--Testing Routines and expressions.  This stuff doesn't need to be converted.
test_expr  = Atom 'a' -- 'a'
test_expr2 = Bar (Atom 'a') (Atom 'b') -- "a|b"
test_expr3 = Star test_expr -- "a*"
test_expr4 = Cons test_expr2 test_expr3 -- "(a|b)a*"

machine1 = compMachine test_expr
machine2 = compMachine test_expr2
machine3 = compMachine test_expr3
machine4 = compMachine test_expr4

reactivate x = ReacT $ return $ Right (Nothing, x)

r1 = reactivate (runMachine machine1)
r2 = reactivate (runMachine machine2)
r3 = reactivate (runMachine machine3)
r4 = reactivate (runMachine machine4)

runReact :: [input] -> ReacT (Input input) Output Identity (Output,Machine input) -> IO ()
runReact []  (ReacT m)  = do
                           putStrLn "Input complete."
                           let (Right (result,_)) = runIdentity m
                           putStrLn $ "Ending on: " ++ (show result)

runReact (i:is) (ReacT m) = do
                          let (Right (output,resume)) = runIdentity m
                          putStrLn $ "Current output reads: " ++ (show output)
                          putStrLn $ "<Enter> to continue"
                          _ <- getLine
                          case output of
                                Nothing -> runReact (i:is) (resume (Just True,i)) --According to S&P outer input should always be true
                                Just r  -> runReact is (resume (Just True, i))

