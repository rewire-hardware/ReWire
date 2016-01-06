module Prelude where

data Bool = False | True

(&&) :: Bool -> Bool -> Bool
True && b = b
_ && _    = False
