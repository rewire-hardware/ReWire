module ReWire.FrontEnd.Purify (purify) where

import ReWire.FrontEnd.Syntax

purify :: Fresh m => RWMProgram -> m RWMProgram
purify = return


