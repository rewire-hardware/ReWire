{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
module ReWire.FIRRTL.ToLoFIRRTL (toLoFirrtl) where

import ReWire.Error
import ReWire.FIRRTL.Syntax
import qualified ReWire.LoFIRRTL.Syntax as L

toLoFirrtl :: MonadError AstError m => Circuit -> m L.Circuit
toLoFirrtl (Circuit x _ ms) = L.Circuit (L.Id x) <$> mapM transModule ms

transModule :: MonadError AstError m => Module -> m L.Module
transModule = undefined
