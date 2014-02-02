module TestMain where

--main = putStrLn $ "This is Test Main. " 

fj :: (Show a) => Maybe a -> String 
fj (Just thing) = show thing 
fj Nothing = "oh no!"


--value = 1
