module ReWire.CoreKC where

import Control.Monad.State
import Control.Monad.Identity
import Unbound.LocallyNameless

-- Kind checking for Core.

data Kind = Kvar (Name Kind) | Kstar | Kfun Kind Kind deriving (Eq,Show)

type KSubst = [(Name Kind,Kind)]

-- the monad
type KIM = FreshMT (StateT KSubst Identity)

kcprog :: RWCProg -> KIM ()
kcprog p = do mapM_ kcdatadecl (dataDecls p)
              ds <- untrec (defns p)
              mapM_ kcdefn ds
              