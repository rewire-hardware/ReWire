module ReWire.Transformations.LambdaLifter where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Types
import Unbound.LocallyNameless
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity




--type TransCommand = String -> RWCProg -> (Maybe RWCProg,Maybe String)

type ReaderEnv = ([(Name RWCExp,RWCTy)])

type LLM = (WriterT [RWCDefn] (ReaderT ReaderEnv (FreshMT Identity)))



ll_def :: RWCDefn -> LLM RWCDefn
ll_def defn = undefined
  where
    ll_def_exp :: RWCExp -> LLM RWCExp 
    ll_def_exp (RWCLam ty bexp) = do
                                    return undefined

ll_exp :: RWCExp -> LLM RWCExp
ll_exp = undefined


{-| Peel off the first type (for a function)  and return the remaining type
 -  of a given type if it exists.  Nothing, otherwise -}
peel_first_type :: RWCTy -> (RWCTy, Maybe RWCTy)
peel_first_type (RWCTyApp (RWCTyApp (RWCTyCon "->") ty) rem) = (ty, Just rem)
peel_first_type x = (x, Nothing)


{-| Gets the final type of a function (ex c from a -> b -> c) or in the case
 -  of a zero-arity thing, just the type itself. -}
peel_final_type :: RWCTy -> RWCTy
peel_final_type ty = pft $ peel_first_type ty
  where
   pft (ty, Nothing) = ty
   pft (ty, Just x ) = pft $ peel_first_type x

{-| Given some expression we are lifting out, this closes the current environment
 -  over it with applies -}
close :: RWCExp -> LLM RWCExp
close expr = do
               env <- ask
               let closure = foldl (\acc (name,name_ty) -> 
                                        RWCApp (rest_ty acc) expr (RWCVar name_ty name)
                                         ) expr env
               return closure 
   where
     rest_ty expr = case peel_first_type $ rwc_expr_ty expr of --This will fail if we are ill-formed 
                            (_, Just rest) -> rest

--build_defn :: RWCExp -> LLM ()
--build_defn expr = do
--                    env <- ask
--                    let expr_ty = rwc_expr_ty expr
--                    let ek


rwc_expr_ty (RWCApp ty _ _) = ty
rwc_expr_ty (RWCLam ty _)   = ty
rwc_expr_ty (RWCVar ty _)   = ty
rwc_expr_ty (RWCCon ty _)   = ty
rwc_expr_ty (RWCLiteral ty _) = ty
rwc_expr_ty (RWCCase ty _ _ ) = ty

