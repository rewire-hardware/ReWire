module ReWire.Core.Transformations.Inline (inline) where

import ReWire.Scoping
import ReWire.Core.Syntax
import ReWire.Core.Transformations.Expand (expand)
import ReWire.Core.Transformations.Reduce (redprog)
import ReWire.Core.Transformations.Purge (purge)

toInline :: RWCProg -> [Id RWCExp]
toInline p = concatMap f (defns p)
  where f (RWCDefn n _ True _) = [n]
        f _                    = []

inline :: RWCProg -> Maybe RWCProg
inline p = purge (mkId "start") $ redprog $ expand (toInline p) p
