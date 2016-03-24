module ReWire.Core.Inline (inline) where

import ReWire.Scoping
import ReWire.Core.Syntax
import ReWire.Core.Expand (expand)
import ReWire.Core.Reduce (redmod)
import ReWire.Core.Purge (purge)

toInline :: RWCProgram -> [Id RWCExp]
toInline m = concatMap f (defns m)
  where f (RWCDefn _ n _ True _) = [n]
        f _                      = []

inline :: RWCProgram -> Maybe RWCProgram
inline m = purge (mkId "Main.start") $ redmod $ expand (toInline m) m
