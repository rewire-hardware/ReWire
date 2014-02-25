{-
 -
 -
 - This implementation of a regular expression matching
 - device is from Sidhu and Prasanna's "Fast Regular 
 - Expression Matching using FPGAs".  
 -
 -}


module RegExpStandalone where

--import Control.Monad.Resumption.Reactive
--import Control.Monad.Identity
import Prelude ()

{- Here we define the abstract syntax for regular expressions -}

--"Prelude Functions" and Data Decls

data Maybe a = Just a | Nothing
data Bool = True | False
data Either a b = Left a | Right b
data Pair a b = Pair a b

data Identity a = Identity a
runIdentity :: Identity a -> a
runIdentity a = case a of
                  Identity a' -> a'

returnI :: a -> Identity a
returnI a = Identity a

bindI :: Identity a -> (a -> Identity b) -> Identity b
bindI i r = case i of
             (Identity a) -> r a

data ReacT input output a = ReacT (Identity (Either a (Pair output (input -> ReacT input output a))))

deReacT :: ReacT input output a -> Identity (Either a (Pair output (input -> ReacT input output a)))
deReacT r = case r of
              ReacT a -> a

returnRe :: a -> ReacT input output a
returnRe a = ReacT (Identity (Left a))


fst :: Pair a b -> a
fst p = case p of
          Pair a b -> a 

snd :: Pair a b -> b 
snd p = case p of
          Pair a b -> b

beq :: Bool -> Bool -> Bool
beq b1 b2 = case (Pair b1 b2) of
                  Pair True True   -> True
                  Pair False False -> True
                  x                -> False

and :: Bool -> Bool -> Bool
and b1 b2 = case (Pair b1 b2) of
                   Pair True True -> True
                   x              -> False

or :: Bool -> Bool -> Bool
or b1 b2 = case (Pair b1 b2) of
                  Pair True x -> True
                  Pair x True -> True
                  x           -> False


data Char = Char Bool Bool Bool Bool Bool Bool Bool Bool

ceq :: Char -> Char -> Bool
ceq c1 c2 = case Pair c1 c2 of
                Pair (Char c7 c6 c5 c4 c3 c2 c1 c0) (Char d7 d6 d5 d4 d3 d2 d1 d0) -> and (beq c7 d7)  (and (beq c6 d6) (and (beq c5 d5)
                                                                                      (and (beq c4 d4) (and (beq c3 d3) (and (beq c2 d2)
                                                                                      (and (beq c1 d1) (beq c0 d0)))))))
                  

data Machine input = Machine {deMachine :: ((Pair (Maybe Bool) input)) -> 
                                            ReacT ((Pair (Maybe Bool) input)) (Maybe Bool) (Pair (Maybe Bool) (Machine input))}

data RegExp a =   Bar   (RegExp a) (RegExp a)
                | Star  (RegExp a)
                | Cons  (RegExp a) (RegExp a)
                | Paren (RegExp a)
                | Atom a

compMachine :: RegExp Char -> Machine Char
compMachine b  = case b of 

                    (Bar r1 r2)  -> bar (compMachine r1) (compMachine r2)
                    (Star r1)    -> star (compMachine r1)
                    (Cons r1 r2) -> rseq (compMachine r1) (compMachine r2)
                    (Paren r1)   -> compMachine r1
                    (Atom a)     -> match a Nothing --Flipflop is primed with nothing

runMachine :: Machine input -> ((Pair (Maybe Bool) input)) -> ReacT ((Pair (Maybe Bool) input)) (Maybe Bool) (Pair (Maybe Bool) (Machine input))
runMachine m1 input = ReacT (bindI (stepMachine input m1) (\p -> case p of 
                                                                      (Pair output m1') -> returnI (Right (Pair output (\input' -> runMachine m1' input')))))

stepMachine :: (Pair (Maybe Bool) input) -> Machine input -> Identity (Pair (Maybe Bool) (Machine input))
stepMachine input m = case ((deReacT ((deMachine m) input))) of
                                  Identity (Left v) -> Identity v
                              
match :: Char -> ((Maybe Bool) -> Machine Char)
match a = \flipflop -> Machine (\(Pair prev_output char) -> case flipflop of
                                                              Nothing -> returnRe (Pair Nothing (match a prev_output))
                                                              Just ff -> returnRe (Pair (Just (and (ceq a char) ff)) ((match a) prev_output)))

bar :: Machine Char -> Machine Char -> Machine Char
bar m1 m2 = Machine (\input -> case fst input of
                                    Nothing    -> returnRe (Pair Nothing (bar m1 m2))
                                    Just zd    -> ReacT (bindI (stepMachine input m1) (\(Pair output1 resume1) ->
                                                         bindI (stepMachine input m2) (\(Pair output2 resume2) ->
                                                         case (Pair output1 output2) of
                                                                   (Pair Nothing Nothing)     -> returnI (Left (Pair Nothing (bar resume1 resume2)))
                                                                   (Pair Nothing zd)          -> returnI (Left (Pair Nothing (bar resume1 m2)))
                                                                   (Pair zd Nothing)          -> returnI (Left (Pair Nothing (bar m1 resume2)))
                                                                   (Pair (Just r1) (Just r2)) -> returnI (Left (Pair (Just (or r1 r2)) (bar resume1 resume2)))))))
                                                                   
        

rseq :: Machine Char -> Machine Char -> Machine Char 
rseq m1 m2 = Machine (\input -> case fst input of
                                    Nothing -> returnRe (Pair Nothing (rseq m1 m2))
                                    Just zd -> ReacT ( bindI (stepMachine input m1) (\(Pair output1 resume1) ->
                                                       case output1 of
                                                                  Nothing -> returnI (Left (Pair Nothing (rseq resume1 m2)))
                                                                  Just zd -> (bindI (stepMachine (Pair output1 (snd input)) m2) (\(Pair output2 resume2) ->
                                                                               case output2 of
                                                                                      Nothing  -> returnI (Left (Pair Nothing (rseq m1 resume2)))
                                                                                      Just zd  -> returnI (Left (Pair output2 (rseq resume1 resume2))))))))

star :: Machine Char -> Machine Char
star m1 = star' (Just False) m1

star' :: (Maybe Bool) -> Machine Char -> Machine Char
star' output m1 = Machine (\input -> case fst input of
                                            Nothing -> returnRe (Pair Nothing (star' output m1))
                                            Just inval  -> ReacT ( bindI (case output of
                                                                               Nothing     -> stepMachine input m1
                                                                               Just outval -> stepMachine (Pair (Just (or inval outval)) (snd input)) m1) (\(Pair inner_output inner_resume) ->
                                                                    returnI (Left (Pair inner_output (star' inner_output inner_resume))))))
                                                                                            
                                               
--Testing Routines and expressions.  This stuff doesn't need to be converted.
a_char = Char False True True False False False False True
b_char = Char False True True False False False True  False

test_expr  = Atom a_char -- 'a'
test_expr2 = Bar (Atom a_char) (Atom b_char) -- "a|b"
test_expr3 = Star test_expr -- "a*"
test_expr4 = Cons test_expr2 test_expr3 -- "(a|b)a*"

machine1 = compMachine test_expr
machine2 = compMachine test_expr2
machine3 = compMachine test_expr3
machine4 = compMachine test_expr4

reactivate x = ReacT (returnI (Right (Pair Nothing x)))

r1 = reactivate (runMachine machine1)
r2 = reactivate (runMachine machine2)
r3 = reactivate (runMachine machine3)
r4 = reactivate (runMachine machine4)

