{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.PreHDL.ToVHDL where

import ReWire.PreHDL.Syntax
import Data.List (intercalate)

vTy (TyBits n) = "std_logic_vector(0 to " ++ show (n-1) ++ ")"
vTy TyBoolean  = "boolean"

vInit (TyBits _) = "(others => '0')"
vInit TyBoolean  = "false"

vRegDecl rd = "variable " ++ regDeclName rd ++ " : " ++ vTy (regDefnTy rd) ++ " := " ++ vInit (regDefnTy rd) ++ ";"

vHeader :: Header -> String
vHeader h = concatMap ((++"\n") . vRegDecl) (regDecls h) -- FIXME: fundefns, state names, start state

vBool :: BoolExp -> String
vBool (And b1 b2)        = "(" ++ vBool b1 ++ " AND " ++ vBool b2 ++ ")"
vBool (Or b1 b2)         = "(" ++ vBool b1 ++ " OR " ++ vBool b2 ++ ")"
vBool (Not b)            = "(NOT " ++ vBool b ++ ")"
vBool (BoolVar l)        = l
vBool (BoolConst True)   = "true"
vBool (BoolConst False)  = "false"
vBool (InState n)        = "(state = STATE" ++ show n ++ ")"
vBool (BoolEq rhs1 rhs2) = "(" ++ vRHS rhs1 ++ " = " ++ vRHS rhs2 ++ ")"

vRHS :: RHS -> String
vRHS (BoolRHS b)        = vBool b
vRHS (LocRHS l)         = l
vRHS (FunCallRHS s [])  = s
vRHS (FunCallRHS s ls)  = s ++ "(" ++ intercalate "," ls ++ ")"
vRHS (ConstRHS bs)      = "\"" ++ concatMap show bs ++ "\""
vRHS (SliceRHS lo hi r) = r ++ "(" ++ show lo ++ " to " ++ show hi ++ ")"
vRHS (ConcatRHS ls)     = "(" ++ intercalate " & " ls ++ ")"

vCmd :: Cmd -> String
vCmd (Rem c)        = "-- " ++ c
vCmd (Assign "output" rhs) = "output <= " ++ vRHS rhs ++ ";" -- FIXME; kludge :/
vCmd (Assign l rhs) = l ++ " := " ++ vRHS rhs ++ ";"
vCmd (NextState n)  = "state := STATE" ++ show n ++ ";"
vCmd (If b c)       = "if " ++ vBool b ++ " then\n"
                   ++ indent (vCmd c) ++ "\n"
                   ++ "end if;"
vCmd (Seq c1 c2)    = vCmd c1 ++ "\n" ++ vCmd c2
vCmd Skip           = "null;"
vCmd (Goto _ _)     = error "vCmd: encountered a goto"
vCmd (Lbl l)        = "null; -- label " ++ l

vFunDefnProto :: FunDefn -> String
vFunDefnProto fd = "function " ++ funDefnName fd ++ (if null params
                                                        then ""
                                                        else "(" ++ intercalate " ; " (map ((++" : std_logic_vector") . regDeclName) params) ++ ")") 
                                                 ++ " return std_logic_vector;\n"
                   where params = funDefnParams fd

vFunDefn :: FunDefn -> String
vFunDefn fd = "function " ++ funDefnName fd ++ (if null params
                                                   then ""
                                                   else "(" ++ intercalate " ; " (map ((++" : std_logic_vector") . regDeclName) params) ++ ")") 
                                            ++ " return std_logic_vector\n"
           ++ "is\n"
           ++ indent (concatMap ((++"\n") . vRegDecl) (funDefnRegDecls fd))
           ++ "begin\n"
           ++ indent (vCmd (funDefnBody fd) ++ "\n")
           ++ indent ("return " ++ funDefnResultReg fd ++ ";\n")
           ++ "end " ++ funDefnName fd ++ ";\n"
      where params = funDefnParams fd

toVHDL :: Prog -> String
toVHDL p = "library ieee;\n"
        ++ "use ieee.std_logic_1164.all;\n"
        ++ "-- Uncomment the following line if VHDL primitives are in use.\n"
        ++ "-- use prims.all;\n"
        ++ "entity rewire is\n"
        ++ "  Port ( clk : in std_logic ;\n"
        ++ "         input : in std_logic_vector (0 to " ++ show (inputSize (progHeader p)-1) ++ ");\n"
        ++ "         output : out std_logic_vector (0 to " ++ show (outputSize (progHeader p)-1) ++ "));\n"
        ++ "end rewire;\n"
        ++ "\n"
        ++ "architecture behavioral of rewire is\n"
        ++ "  type control_state is (" ++ intercalate "," (stateNames (progHeader p)) ++ ");\n"
        ++ indent (concatMap vFunDefnProto (funDefns (progHeader p))) ++ "\n"
        ++ indent (concatMap vFunDefn (funDefns (progHeader p))) ++ "\n"
        ++ "begin\n"
        ++ indent (
           "process (clk)\n"
        ++ indent (concatMap ((++"\n") . vRegDecl) (regDecls (progHeader p)))
        ++ "  variable state : control_state := " ++ startState (progHeader p)  ++ ";\n"
        ++ "begin\n"
        ++ "  if clk'event and clk='1' then\n"
        ++ indent (indent (vCmd (progBody p))) ++ "\n"
        ++ "  end if;\n"
        ++ "end process;\n"
           )
        ++ "end behavioral;\n"
--        vHeader (progHeader p) ++ vCmd (progBody p)