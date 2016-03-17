module ReWire.PreHDL.ElimEmpty (elimEmpty) where

import ReWire.PreHDL.Syntax

elimFunDefn :: FunDefn -> FunDefn
elimFunDefn fd = fd { funDefnParams = filter (not . isEmptyParam) (funDefnParams fd),
                      funDefnBody   = elimCmd (funDefnBody fd) }
  where isEmptyParam (RegDecl _ (TyBits 0)) = True
        isEmptyParam _                      = False

elimHeader :: Header -> Header
elimHeader h = h { funDefns = map elimFunDefn (funDefns h) }

elimRHS :: RHS -> RHS
elimRHS (BoolRHS b)       = BoolRHS (elimBool b)
elimRHS (FunCallRHS f ls) = FunCallRHS f (filter (/="EMPTY") ls)
elimRHS (ConcatRHS ls)    = ConcatRHS (filter (/="EMPTY") ls)
elimRHS rhs               = rhs

elimBool :: BoolExp -> BoolExp
elimBool (And b1 b2)        = And (elimBool b1) (elimBool b2)
elimBool (Or b1 b2)         = Or (elimBool b1) (elimBool b2)
elimBool (Not b)            = Not (elimBool b)
elimBool (BoolEq rhs1 rhs2) = BoolEq (elimRHS rhs1) (elimRHS rhs2)
elimBool b                  = b

elimCmd :: Cmd -> Cmd
elimCmd (Assign "EMPTY" _) = Skip
elimCmd (Assign r rhs)     = Assign r (elimRHS rhs)
elimCmd (If b c)           = If (elimBool b) (elimCmd c)
elimCmd (Seq c1 c2)        = Seq (elimCmd c1) (elimCmd c2)
elimCmd (CaseIf bcmds)     = CaseIf ((map (\(b,cmd) -> (elimBool b, elimCmd cmd))) bcmds)
elimCmd c                  = c

elimEmpty :: Prog -> Prog
elimEmpty p = Prog { progHeader = elimHeader (progHeader p), progBody = elimCmd (progBody p) }
