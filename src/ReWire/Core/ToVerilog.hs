{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.ToVerilog (compileProgram) where

import ReWire.Annotation
import ReWire.Error
import ReWire.Flags (Flag (..))
import ReWire.Core.Syntax as C
import ReWire.Verilog.Syntax as V

compileProgram :: MonadError AstError m => [Flag] -> C.Program -> m V.Program
compileProgram _ (C.Program st ds) = V.Program <$> ((:) <$> compileStartDefn st <*> mapM compileDefn ds)

compileStartDefn :: Monad m => C.StartDefn -> m V.Module
compileStartDefn (C.StartDefn a inps outps (loop, loopSig) (state0, state0Sig)) = pure $ V.Module "top_level" [] [] [] $ V.Block []

compileDefn :: Monad m => C.Defn -> m V.Module
compileDefn (C.Defn a n sig body) = pure $ V.Module n [] [] [] $ V.Block []
