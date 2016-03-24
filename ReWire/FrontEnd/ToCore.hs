module ReWire.FrontEnd.ToCore (toCore) where

import ReWire.Core.Syntax
import ReWire.FrontEnd.Rename

toCore :: Monad m => RWCProgram -> m RWCProgram
toCore = return

