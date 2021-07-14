{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.MiniHDL.ToLoFIRRTL (toLoFirrtl) where

import ReWire.Error
import qualified ReWire.MiniHDL.Syntax as V
import qualified ReWire.LoFIRRTL.Syntax as F
import Data.Bits (Bits (..))
import Control.Monad (foldM)
import ReWire.Annotation (noAnn)
import Numeric.Natural (Natural)

concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM f xs = concat <$> mapM f xs

toLoFirrtl :: MonadError AstError m => V.Program -> m F.Circuit
toLoFirrtl (V.Program units) = F.Circuit (F.Id "rewire_root") <$> mapM transUnit units

transUnit :: MonadError AstError m => V.Unit -> m F.Module
transUnit (V.Unit e a) = F.ModuleDef (F.Id $ V.entityName e) <$> mapM transPort (V.entityPorts e) <*> transArch a

transPort :: Applicative m => V.Port -> m F.Port
transPort = \ case
      V.Port n V.In t  -> F.Input  (F.Id n) <$> transType t
      V.Port n V.Out t -> F.Output (F.Id n) <$> transType t

transType :: Applicative m => V.Ty -> m F.Type
transType = \ case
      V.TyBool             -> pure $ F.UIntTy 1
      V.TyStdLogicVector n -> pure $ F.UIntTy n
      V.TyStdLogic         -> pure $ F.UIntTy 1 -- TODO(chathhorn)
      V.TyClock            -> pure $ F.ClockTy
      V.TyRegister _ n     -> pure $ F.UIntTy n

transArch :: MonadError AstError m => V.Architecture -> m [F.Stmt]
transArch (V.Architecture _ _ sigs _ stmts) = (++) <$> mapM transSig sigs <*> concatMapM transStmt stmts

transSig :: Applicative m => V.Signal -> m F.Stmt
transSig = \ case
      V.Signal n t@(V.TyRegister clk _) -> F.Reg (F.Id n) <$> transType t <*> pure (F.Ref $ F.Id clk)
      V.Signal n t                      -> F.Wire (F.Id n) <$> transType t

transStmt :: MonadError AstError m => V.Stmt -> m [F.Stmt]
transStmt = \ case
      V.Assign (V.LHSName n) e                 -> pure <$> (F.Connect (F.Ref $ F.Id n) <$> transExpr e)
      V.WithAssign _ _             [] Nothing  -> pure []
      V.WithAssign e (V.LHSName n) bs Nothing  -> pure <$> (F.Connect (F.Ref $ F.Id n) <$> foldM (transBranch e) (F.Ref $ F.Id n) bs)
      V.WithAssign e (V.LHSName n) bs (Just b) -> do
            b' <- transExpr b
            pure <$> (F.Connect (F.Ref $ F.Id n) <$> foldM (transBranch e) b' bs)
      V.Instantiate n1 n2 (V.PortMap ports)    -> (F.Inst (F.Id n1) (F.Id n2) :) <$> mapM (transPort n1) ports
      V.ClkProcess _ stmts                     -> concatMapM transStmt stmts
      where transBranch :: MonadError AstError m => V.Expr -> F.Exp   -> (V.Expr, V.Expr) -> m F.Exp
            transBranch e d (e1, e2) = F.Mux <$> (F.Eq <$> transExpr e <*> transExpr e1) <*> transExpr e2 <*> pure d

            transPort :: MonadError AstError m => V.Name -> (V.Name, V.Expr) -> m F.Stmt
            transPort m (n, e) = F.Connect (F.Field (F.Ref $ F.Id m) $ F.Id n) <$> transExpr e

transExpr :: MonadError AstError m => V.Expr -> m F.Exp
transExpr = \ case
      V.ExprName n -> pure $ F.Ref $ F.Id n
      V.ExprBoolConst False -> pure $ F.UInt 1 0
      V.ExprBoolConst True -> pure $ F.UInt 1 1
      V.ExprBit V.Zero -> pure $ F.UInt 1 0
      V.ExprBit V.One -> pure $ F.UInt 1 1
      V.ExprBitString []  -> pure $ F.UInt 0 0
      V.ExprBitString [b] -> transExpr $ V.ExprBit b
      V.ExprBitString (V.Zero:bs) -> transExpr (V.ExprBitString bs) >>= \ case
            F.UInt w v -> pure $ F.UInt (w + 1) v
            _          -> failAt noAnn "ToLoFIRRTL.transExpr: this shouldn't happen."
      V.ExprBitString (V.One:bs) -> transExpr (V.ExprBitString bs) >>= \ case
            F.UInt w v -> pure $ F.UInt (w + 1) (v .|. bit (fromIntegral w))
            _          -> failAt noAnn "ToLoFIRRTL.transExpr: this shouldn't happen."
      V.ExprConcat e1 e2 -> F.Cat <$> transExpr e1 <*> transExpr e2
      V.ExprSlice e l h -> F.Bits <$> transExpr e <*> pure (toNat h) <*> pure (toNat l)
      V.ExprIsEq e1 e2 -> F.Eq <$> transExpr e1 <*> transExpr e2
      V.ExprAnd e1 e2 -> F.And <$> transExpr e1 <*> transExpr e2
      where toNat :: Integral a => a -> Natural -- TODO(chathhorn)
            toNat a | a >= 0 = fromIntegral a
            toNat _          = 0
