--module PreludeTest where

import Prelude

x :: Bool
x = True

y :: Bool
y = x && x

loop :: Bool -> ReT Bool Bool I ()
loop a = do
  b <- signal a
  loop (a && x && y)

main :: ReT Bool Bool I ()
main = loop False

start :: ReT Bool Bool I ()
start = main
