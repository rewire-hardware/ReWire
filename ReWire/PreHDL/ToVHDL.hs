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
vBool (InState n)        = "(control = STATE" ++ show n ++ ")"
vBool (BoolEq rhs1 rhs2) = "(" ++ vRHS rhs1 ++ " = " ++ vRHS rhs2 ++ ")"

vRHS :: RHS -> String
vRHS (BoolRHS b)        = vBool b
vRHS (LocRHS "input")   = "input_tmp" -- FIXME: kludge
vRHS (LocRHS l)         = l
vRHS (FunCallRHS s [])  = s
vRHS (FunCallRHS s ls)  = s ++ "(" ++ intercalate "," ls ++ ")"
vRHS (ConstRHS bs)      = "\"" ++ concatMap show bs ++ "\""
vRHS (SliceRHS lo hi r) = r ++ "(" ++ show lo ++ " to " ++ show hi ++ ")"
vRHS (ConcatRHS ls)     = "(" ++ intercalate " & " ls ++ ")"

vCmd :: Cmd -> String
vCmd (Rem c)        = "-- " ++ c
vCmd (Assign "output" rhs) = "output_tmp := " ++ vRHS rhs ++ ";" -- FIXME; kludge :/
vCmd (Assign l rhs) = l ++ " := " ++ vRHS rhs ++ ";"
vCmd (NextState n)  = "control := STATE" ++ show n ++ ";"
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
                                                 ++ " return std_logic_vector;"
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
           ++ "end " ++ funDefnName fd ++ ";"
      where params = funDefnParams fd

flopName n = n ++ "_flop"
flopNextName n = n ++ "_flop_next"

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
        ++ indent ("type control_state is (" ++ intercalate "," (stateNames (progHeader p)) ++ ");\n")
        ++ indent (concatMap (++"\n") vFunProtos)
        ++ indent (concatMap (++"\n") vFunDefns)
        ++ indent (curControlFlopDecl ++ "\n")
        ++ indent (nextControlFlopDecl ++ "\n")
        ++ indent (inputFlopDecl ++ "\n")
        ++ indent (concatMap (++"\n") curFlopDecls)
        ++ indent (concatMap (++"\n") nextFlopDecls)
        ++ "begin\n"
        ++ indent loopProcess ++ "\n"
        ++ indent flopProcess ++ "\n"
        ++ "end behavioral;\n"
  where varNames = map regDeclName (regDecls (progHeader p))

        vFunProtos = map vFunDefnProto (funDefns (progHeader p))
        vFunDefns  = map vFunDefn (funDefns (progHeader p))

        curControlFlopDecl = "signal " ++ flopName "control" ++ " : control_state := " ++ startState (progHeader p) ++ ";"
        nextControlFlopDecl = "signal " ++ flopNextName "control" ++ " : control_state := " ++ startState (progHeader p) ++ ";"

        inputFlopDecl = "signal " ++ flopName "input" ++ " : " ++ vTy (TyBits (inputSize (progHeader p))) ++ " := (others => '0');"

        curFlopDecls  = map curFlopDecl (regDecls (progHeader p))
        curFlopDecl d = "signal " ++ flopName (regDeclName d) ++ " : " ++ vTy (regDefnTy d) ++ " := " ++ vInit (regDefnTy d) ++ ";"
        
        nextFlopDecls  = map nextFlopDecl (regDecls (progHeader p))
        nextFlopDecl d = "signal " ++ flopNextName (regDeclName d) ++ " : " ++ vTy (regDefnTy d) ++ " := " ++ vInit (regDefnTy d) ++ ";"

        loopProcess = "-- Logic loop process.\n"
                   ++ "process (" ++ intercalate "," loopSensitivityList ++ ")\n"
                   ++ indent (loopControlTmpDecl ++ "\n")
                   ++ indent (loopInputTmpDecl ++ "\n")
                   ++ indent (concatMap (++"\n") loopTmpDecls)
                   ++ indent (loopOutputTmpDecl ++ "\n")
                   ++ "begin\n"
                   ++ indent "-- Read reg temps.\n"
                   ++ indent (loopControlTmpInit ++ "\n")
                   ++ indent (loopInputTmpInit ++ "\n")
                   ++ indent (concatMap (++"\n") loopTmpInits)
                   ++ indent "-- Loop body.\n"
                   ++ indent (loopBody ++ "\n")
                   ++ indent "-- Write back reg temps.\n"
                   ++ indent (loopControlTmpWriteback ++ "\n")
                   ++ indent (concatMap (++"\n") loopTmpWritebacks)
                   ++ indent "-- Update output line.\n"
                   ++ indent "output <= output_tmp;\n"
                   ++ "end process;\n"
        loopSensitivityList     = [flopName "control",flopName "input"] ++ map flopName varNames
        loopControlTmpDecl      = "variable control : control_state;"
        loopInputTmpDecl        = "variable input_tmp : " ++ vTy (TyBits (inputSize (progHeader p))) ++ ";"
        loopTmpDecls            = map loopTmpDecl (regDecls (progHeader p))
        loopTmpDecl d           = "variable " ++ regDeclName d ++ " : " ++ vTy (regDefnTy d) ++ " := " ++ vInit (regDefnTy d) ++ ";"
        loopOutputTmpDecl       = "variable output_tmp : " ++ vTy (TyBits (outputSize (progHeader p))) ++ ";"
        loopControlTmpInit      = "control := " ++ flopName "control" ++ ";"
        loopInputTmpInit        = "input_tmp := " ++ flopName "input" ++ ";"
        loopTmpInits            = map loopTmpInit varNames
        loopTmpInit n           = n ++ " := " ++ flopName n ++ ";"
        loopBody                = vCmd (progBody p)
        loopControlTmpWriteback = flopNextName "control" ++ " <= control;"
        loopTmpWritebacks       = map loopTmpWriteback varNames
        loopTmpWriteback n      = flopNextName n ++ " <= " ++ n ++ ";"

        flopProcess = "-- Flip flop update process.\n"
                   ++ "process (" ++ intercalate "," flopSensitivityList ++ ")\n"
                   ++ "begin\n"
                   ++ indent ("if clk'event and clk='1' then\n"
                           ++ indent (inputFlopUpdate ++ "\n")
                           ++ indent (controlFlopUpdate ++ "\n")
                           ++ indent (concatMap (++"\n") varFlopUpdates)
                           ++ "end if;\n")
                   ++ "end process;\n"
        flopSensitivityList = ["clk","input"] ++ map flopNextName varNames
        inputFlopUpdate     = flopName "input" ++ " <= input;"
        flopUpdate n        = flopName n ++ " <= " ++ flopNextName n ++ ";"
        controlFlopUpdate   = flopUpdate "control"
        varFlopUpdates      = map flopUpdate varNames
--        vHeader (progHeader p) ++ vCmd (progBody p)
