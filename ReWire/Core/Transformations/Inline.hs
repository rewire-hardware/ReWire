module ReWire.Core.Transformations.Inline (inline) where

import ReWire.Scoping
import ReWire.Core.Syntax
import ReWire.Core.Transformations.Expand (expand)
import ReWire.Core.Transformations.Reduce (redmod)
import ReWire.Core.Transformations.Purge (purge)

toInline :: RWCModule -> [Id RWCExp]
toInline m = concatMap f (defns m)
  where f (RWCDefn n _ True _) = [n]
        f _                    = []

inline :: RWCModule -> Maybe RWCModule
inline m = purge (mkId "start") $ redmod $ expand (toInline m) m
