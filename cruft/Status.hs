module ReWire.Core.Transformations.Status (cmdStatus) where


import ReWire.Core.Transformations.Types
import ReWire.Core.Syntax

import Unbound.LocallyNameless (luntrec,runLFreshM)


cmdStatus :: TransCommand
cmdStatus _ prog = (Nothing,Just str)
  where
    str = "Top Level Defns: " ++ (show $ count_terms prog)

count_terms :: RWCProg -> Int
count_terms (RWCProg {defns=defs}) = length $ runLFreshM (luntrec defs)
