module ReWire.FrontEnd.Purify (purify) where

import ReWire.FrontEnd.Syntax

purify :: Fresh m => Program -> m Program
purify = return


