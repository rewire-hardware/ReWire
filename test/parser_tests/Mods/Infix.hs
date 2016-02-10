module Mods.Infix where

data L = L
data R = R

(<|) :: L -> R -> R
L <| R = R
infixr 2 <|

(|>) :: L -> R -> L
L |> R = L
infixr 1 |>

lt :: L -> R -> R
L `lt` R = R
infixr 2 `lt`

rt :: L -> R -> L
L `rt` R = L
infixr 1 `rt`

data R' = R' | L' :< R'
infixr 2 :<

data L' = L' | L' :> R'
infixl 1 :>
