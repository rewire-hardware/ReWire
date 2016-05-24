data G a b c = G a | Z b | D c
data Pup a b c d = Q d | R d d | Zuh a b c d

guppy :: a -> G a z w
guppy c = let c = c in
          case c of
             c -> G c
             d -> G d
