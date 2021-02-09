{-# LANGUAGE LambdaCase, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.MiniHDL.ToFIRRTL (toFirrtl) where

import ReWire.Error
import ReWire.MiniHDL.Syntax as M
import ReWire.FIRRTL.Syntax as F

toFirrtl :: MonadError AstError m => M.Program -> m F.Circuit
toFirrtl = undefined

