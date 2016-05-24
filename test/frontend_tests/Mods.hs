module Mods (CX.Y(..), x, y) where

import qualified Mods.X as BX (x, X(..), Y(..))
import qualified Mods.Y as BY (y, X(..), Y(..))
import qualified Mods.C.X as CX (x, y, X(..), Y(..))

import Mods.X hiding (x, X(..), Y(..))
import Mods.Y hiding (y, X(..), Y(..))
import Mods.C.X hiding (x, y, X(..), Y(..))

x :: BY.X
x = BX.x

y :: BX.Y
y = BY.y



