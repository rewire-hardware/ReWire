module TestMain where

main = putStrLn $ "This is Test Main. " 

fj :: Maybe a -> a
fj (Just thing) = thing
fj Nothing = error "oh no!"

value = 1
