{-# LANGUAGE Safe #-}
module ReWire.FrontEnd.Purify (purify) where

import ReWire.FrontEnd.Syntax

purify :: Monad m => FreeProgram -> m FreeProgram
purify = return


