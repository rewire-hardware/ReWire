{-# LANGUAGE TemplateHaskell #-}
module TestMain where
import Language.Haskell.TH
import Template


main = putStrLn $ "This is Test Main. " ++ (show ($(sel 2 3) (1,2,3)))

fj :: Maybe a -> a
fj (Just thing) = thing


