data Maybe a is Nothing | Just a end
data Bool is True | False end
data Either a b is Left a | Right b end
data ReacT input output a is ReacT ((Either a (Pair output (input -> ReacT input output a)))) end
data Pair a b is Pair a b end
data Char is Char Bool Bool Bool Bool Bool Bool Bool Bool end

data Machine input is Machine ((Pair (Maybe Bool) input) -> ReacT ((Pair (Maybe Bool) input)) (Maybe Bool) (Pair (Maybe Bool) (Machine input))) end

data RegExp a is Bar (RegExp a) (RegExp a)
                 | Star (RegExp a)
                 | Cons (RegExp a) (RegExp a)
                 | Paren (RegExp a)
                 | Atom a
              end


returnRe :: a -> ReacT input output a
is
  \a -> ReacT ( (Left a))
end

fst :: Pair a b -> a
is
  \p -> case p of
         {
           Pair a b -> a
         }
end

snd :: Pair a b -> b
is
  \p -> case p of
         {
           Pair a b -> b
         }
end

beq :: Bool -> Bool -> Bool
is
  \ba -> \bb -> case (Pair ba bb) of
                  {
                      Pair True  True  -> True
                    ; Pair False False -> True 
                    ; x                -> False
                  }
end

and :: Bool -> Bool -> Bool
is
  \ba -> \bb -> case (Pair ba bb) of
                  {
                    Pair True True -> True
                    ; x            -> False
                  }
end

or :: Bool -> Bool -> Bool
is
  \ba -> \bb -> case (Pair ba bb) of
                  {
                    Pair True x -> True
                    ; Pair x True -> True
                    ; x           -> False
                  }
end

ceq :: Char -> Char -> Bool
is
  \ca -> \cb -> case (Pair ca cb) of
                 {
                  Pair (Char ca cb cc cd ce cf cg ch) (Char da db dc dd de df dg dh) -> and (beq ca da)  (and (beq cb db) (and (beq cc dc)
                                                                                        (and (beq cd dd) (and (beq ce de) (and (beq cf df)
                                                                                        (and (beq cg dg) (beq ch dh)))))))
                 }
end

match :: Char -> (Pair (Maybe Bool) Char) -> ReacT (Pair (Maybe Bool) Char) (Maybe Bool) (Pair (Maybe Bool) Char)
is
  \a -> \b -> matchP Nothing a b 
end

matchP :: (Maybe Bool) -> Char -> (Pair (Maybe Bool) Char) -> ReacT (Pair (Maybe Bool) Char) (Maybe Bool) (Pair (Maybe Bool) Char)
is
  \state -> \cmatch -> \input -> case input of
                                      {
                                        (Pair Nothing  c) -> ReacT (Right (Pair Nothing (matchP state cmatch)))
                                        ; (Pair (Just i) c) -> case state of
                                                                  {
                                                                    Nothing   -> ReacT (Right (Pair Nothing (matchP (Just i) cmatch)))
                                                                    ; Just s  -> ReacT (Right (Pair (Just (and s (ceq cmatch c))) (matchP (Just i) cmatch)))
                                                                  }
                                      }

end

rseq :: ReacT (Pair (Maybe Bool) Char) (Maybe Bool) (Pair (Maybe Bool) Char) ->
        ReacT (Pair (Maybe Bool) Char) (Maybe Bool) (Pair (Maybe Bool) Char) ->
        ReacT (Pair (Maybe Bool) Char) (Maybe Bool) (Pair (Maybe Bool) Char)
is
    \left -> \right -> case left of
                          {
                           ReacT (Right (Pair Nothing res)) -> returnRe (Pair Nothing a_char)
                          }
end


{-

bar :: Machine Char -> Machine Char -> Machine Char
is
  \mi -> \mii -> Machine (\input -> case fst input of
                                    {
                                      Nothing -> returnRe (Pair Nothing (bar mi mii))
                                      ; Just zd -> ReacT (apply (stepMachine input mi) (\p -> case p of
                                                                                            {
                                                                                              Pair outputi resumei -> apply (stepMachine input mii) (\pp -> case pp of
                                                                                                                                                                 {
                                                                                                                                                                   Pair outputii resumeii -> case (Pair outputi outputii) of
                                                                                                                                                                                                  {
                                                                                                                                                                                                    (Pair Nothing Nothing) ->  (Left (Pair Nothing (bar resumei resumeii)))
                                                                                                                                                                                                    ; (Pair Nothing zd)          ->  (Left (Pair Nothing (bar resumei mii)))
                                                                                                                                                                                                    ; (Pair zd Nothing)          ->  (Left (Pair Nothing (bar mi resumeii)))
                                                                                                                                                                                                    ; (Pair (Just ri) (Just rii)) ->  (Left (Pair (Just (or ri rii)) (bar resumei resumeii)))
                                                                                                                                                                                                  }
                                                                                                                                                                 })
                                                                                              
                                                                                            }))
                                      })
end

rseq :: Machine Char -> Machine Char -> Machine Char
is 
  \mi -> \mii -> Machine (\input -> case fst input of
                                      {
                                        Nothing -> returnRe (Pair Nothing (rseq mi mii))
                                        ; Just zd -> ReacT (apply (stepMachine input mi) (\p -> case p of
                                                                                                    {
                                                                                                      Pair outputi resumei -> case outputi of
                                                                                                                                  {
                                                                                                                                    Nothing ->  (Left (Pair Nothing (rseq resumei mii))) 
                                                                                                                                    ; Just zd -> (apply (stepMachine (Pair outputi (snd input)) mii) (\r -> case r of
                                                                                                                                                                                                              {
                                                                                                                                                                                                                Pair outputii resumeii -> case outputii of
                                                                                                                                                                                                                                                  {
                                                                                                                                                                                                                                                    Nothing   ->  (Left (Pair Nothing (rseq mi resumeii))) 
                                                                                                                                                                                                                                                    ; Just zd ->  (Left (Pair outputii (rseq resumei resumeii)))
                                                                                                                                                                                                                                                  }
                                                                                                                                                                                                              }

                                                                                                                                     )
                                                                                                                                    )
                                                                                                                                  }
                                                                                                    })
                                        )})
end

star :: Machine Char -> Machine Char
is
  \mi -> starP (Just False) mi
end

starP :: (Maybe Bool) -> Machine Char -> Machine Char
is
  \output -> \mi -> Machine (\input -> case fst input of
                                          {
                                            Nothing -> returnRe (Pair Nothing (starP output mi))
                                            ; Just inval  -> ReacT (apply (case output of
                                                                            {
                                                                               Nothing     -> stepMachine input mi
                                                                               ; Just outval -> stepMachine (Pair (Just (or inval outval)) (snd input)) mi
                                                                             }) 
                                                             (\p -> case p of
                                                                      {
                                                                        (Pair inneroutput innerresume) ->  (Left (Pair inneroutput (starP inneroutput innerresume)))
                                                                      }))
                                          })
end
-}

a_char :: Char
is
  Char False True True False False False False True
end

b_char :: Char
is
  Char False True True False False False True  False
end

main :: (Pair (Maybe Bool) Char) -> ReacT (Pair (Maybe Bool) Char) (Maybe Bool) (Pair (Maybe Bool) Char)
is 
  match a_char
end
