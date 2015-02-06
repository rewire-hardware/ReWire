{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.PreHDL.Codgen where

import Prelude (undefined, show, (++), map, Maybe(..), (>>), error, return, ($), (.), mapM)
import ReWire.PreHDL.Syntax 
import ReWire.Ext.CodeGen.Prog 
import ReWire.Ext.CodeGen.Types hiding (And, Or, Not)

import qualified ReWire.PreHDL.Syntax as PHDL
import qualified ReWire.Ext.CodeGen.Types as RXT


import Data.Monoid
import Control.Monad.Trans.Writer
import Control.Monad.Trans


compProg :: Prog -> Module Name
compProg (Prog (Header funs rdecls states startState insize outsize) body) = undefined
  where
    --ea = entarch "main" $ do
                            

hBit :: PHDL.Bit -> RXT.Bit
hBit PHDL.One  = RXT.One
hBit PHDL.Zero = RXT.Zero


cb :: BoolExp -> BExp Name
cb x = case x of
             And b1 b2       -> and (cb b1) (cb b2)
             Or b1 b2        -> or  (cb b1) (cb b2)
             Not b           -> not (cb b)
             BoolVar n       -> BVar n
             BoolConst b     -> BConst b 
             BoolEq e1 e2    -> Equ (ce e1) (ce e2)
             InState i       -> (evar "control") === (evar ("STATE" ++ show i))

ce :: RHS -> Exp Name
ce x = case x of
          BoolRHS be        -> BExp (cb be)
          LocRHS n          -> evar n
          FunCallRHS n args -> call n (map evar args)
          ConstRHS bits     -> Const (map hBit bits)
          SliceRHS s e n    -> slice s e (evar n)
          ConcatRHS ns      -> concat (map evar ns)


cc :: Monoid w => Cmd -> Dec w Name ()
cc x = case x of
          Rem s -> comment s
          PHDL.Assign n e  -> n =:= (ce e)
          NextState i      -> "control" =:= (evar ("STATE" ++ show i))
          PHDL.If b c      -> do
                                r <- plugDec (cc c)
                                iif (cb b) r Nothing 
          Seq c1 c2        -> (cc c1) >> (cc c2)
          PHDL.Skip        -> skip
          Goto _ _         -> error "Encountered a goto at VHDL gen phase."
          Lbl l            -> label l

nvect = STD_LOGIC_VECTOR Nothing

convTy :: Ty -> Type
convTy x = case x of
            TyBoolean -> STD_LOGIC
            TyBits i  -> vector i

convParamTy :: Ty -> Type
convParamTy x = case x of
                  TyBoolean -> STD_LOGIC
                  TyBits i  -> nvect
  

convRD :: RegDecl -> VarDec Name
convRD x = case x of
              RegDecl name ty -> Variable name (convTy ty) Nothing

fun :: FunDefn -> Function Name
fun x = case x of
            FunDefn name params decls body retreg -> function' name nvect $ do
                                                                              _ <- mapM param' params
                                                                              _ <- mapM rdVar decls
                                                                              cc body
                                                                              ret (evar retreg)
                                                                              return ()
  where
    rdVar :: RegDecl -> Fun Name () 
    rdVar rd = lift $ tell [Var (convRD rd)]

    param' :: RegDecl -> Fun Name ()
    param' (RegDecl n t) = lift $ tell $ [Param (Typed n (convParamTy t))]
   
    
                                                                               
