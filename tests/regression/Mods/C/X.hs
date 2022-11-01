module Mods.C.X (x, y, X(..), Y, Y(YB)) where

data X = XA | XB
      deriving Show

data Y = YA | YB
      deriving Show

x :: X
x = XA

y :: Y
y = YA

