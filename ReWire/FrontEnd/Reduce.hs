{-# LANGUAGE LambdaCase #-}
module ReWire.FrontEnd.Reduce (reduce) where

import ReWire.FrontEnd.Syntax

import Control.Monad (zipWithM)
