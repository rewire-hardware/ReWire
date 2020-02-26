module Main where

import Mods.Infix ((<|), L (..), R (..), R' (..), L' (..), lt)

(|>) :: L -> R -> L
L |> R = L
infixl 1 |>

a :: L
a = L |> R |> L Mods.Infix.<| L <| R

b :: L
b = L <| R <| R <| R
      where (<|) :: L -> R -> L
            L <| R = L
            infixl 1 <|

c :: L
c = L <| R <| R <| R
      where (<|) :: L -> R -> L
            L <| R = L

d :: R
d = L |> L |> L |> R
      where (|>) :: L -> R -> R
            L |> R = R
            infixr 1 |>

e :: L
e = L <| R <| L |> L Mods.Infix.<| R
      where (<|) :: L -> R -> L
            L <| R = L
            infixl 1 <|
            (|>) :: L -> R -> R
            L |> R = R
            infixr 2 |>

rt :: L -> R -> L
L `rt` R = L
infixl 1 `rt`

a' :: L
a' = L `rt` R `rt` L `Mods.Infix.lt` L `lt` R

b' :: L
b' = L `lt` R `lt` R `lt` R
      where lt :: L -> R -> L
            L `lt` R = L
            infixl 1 `lt`

c' :: L
c' = L `lt` R `lt` R `lt` R
      where lt :: L -> R -> L
            L `lt` R = L

d' :: R
d' = L `rt` L `rt` L `rt` R
      where rt :: L -> R -> R
            L `rt` R = R
            infixr 1 `rt`

e' :: L
e' = L `lt` R `lt` L `rt` L `Mods.Infix.lt` R
      where lt :: L -> R -> L
            L `lt` R = L
            infixl 1 `lt`
            rt :: L -> R -> R
            L `rt` R = R
            infixr 2 `rt`

a'' :: L'
a'' = L' :> R' :> L' Mods.Infix.:< L' :< R'

main :: ReT L R I ()
main = do
  signal d
  main

start :: ReT L R I ()
start = main
