module ReWire.Core.PrimTypes where

import ReWire.Core.Kinds
import ReWire.Core.Syntax

primTys = [(TyConId "(->)", Kstar `Kfun` Kstar `Kfun` Kstar),
           (TyConId "ReT",  Kstar `Kfun` Kstar `Kfun` Kmonad `Kfun` Kmonad),
           (TyConId "StT",  Kstar `Kfun` Kmonad `Kfun` Kmonad),
           (TyConId "I",    Kmonad)]
