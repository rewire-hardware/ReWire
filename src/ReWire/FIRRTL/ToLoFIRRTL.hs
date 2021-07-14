{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.FIRRTL.ToLoFIRRTL (toLoFirrtl) where

import ReWire.Error
import ReWire.FIRRTL.Syntax
import qualified ReWire.LoFIRRTL.Syntax as L
import Data.Bits (Bits (..))
import Control.Monad (foldM)
import ReWire.Annotation (noAnn)
import Numeric.Natural (Natural)

toLoFirrtl :: MonadError AstError m => Circuit -> m L.Circuit
toLoFirrtl (Circuit x _ ms) = L.Circuit (L.Id x) <$> mapM transModule ms

transModule :: MonadError AstError m => Module -> m L.Module
transModule = undefined
-- transModule (Module x _ ps ss) = L.ModuleDef (L.Id x) <$> 
