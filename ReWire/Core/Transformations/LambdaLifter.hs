module ReWire.Transformations.LambdaLifter where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Types
import Unbound.LocallyNameless
import Unbound.LocallyNameless.Types
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Data.List (nub)




--type TransCommand = String -> RWCProg -> (Maybe RWCProg,Maybe String)

type ReaderEnv = ([(Name RWCExp,RWCTy)])

type LLM = (WriterT [RWCDefn] (ReaderT ReaderEnv (StateT Int (LFreshMT Identity))))



ll_def :: RWCDefn -> LLM RWCDefn
ll_def defn = undefined
  where
    ll_def_exp :: RWCExp -> LLM RWCExp 
    ll_def_exp (RWCLam ty bexp) = do
                                    return undefined

ll_exp :: RWCExp -> LLM RWCExp
ll_exp l@(RWCLam _ _)  = do
                          var <- build_defn_expr l
                          var' <- wrap var
                          error "Lambda Expressions need to added to environment to work" --return var'
ll_exp (RWCApp ty e1 e2) = do
                            e1' <- ll_exp e1
                            e2' <- ll_exp e2
                            return (RWCApp ty e1' e2')
ll_exp v@(RWCVar _ _)    = return v
ll_exp c@(RWCCon _ _)    = return c
ll_exp l@(RWCLiteral _ _) = return l
ll_exp (RWCCase ty e alts) = do
                               e' <- ll_exp e
                               alts' <- ll_alts alts
                               return (RWCCase ty e' alts')

ll_alts :: [RWCAlt] -> LLM [RWCAlt]
ll_alts = mapM ll_alt

ll_alt :: RWCAlt -> LLM RWCAlt
ll_alt bnd = undefined -- do
               --bnd' <- lunbind $ (\(pat,exp) -> do
                                                    --exp' <- ll_exp


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
wrap :: RWCExp -> LLM RWCExp
wrap expr = do
               env <- ask
               let closure = foldl (\acc (name,name_ty) -> 
                                        RWCApp (rest_ty acc) expr (RWCVar name_ty name)
                                         ) expr env
               return closure 
   where
     rest_ty expr = case peel_first_type $ rwc_expr_ty expr of --This will fail if we are ill-formed 
                            (_, Just rest) -> rest

build_defn_expr :: RWCExp -> LLM (RWCExp)
build_defn_expr expr = do
                    env <- ask
                    let expr_ty = rwc_expr_ty expr
                        closure = foldl (\acc (name,name_ty) ->
                                    (RWCLam 
                                      (RWCTyApp (RWCTyApp (RWCTyCon "->") name_ty) expr_ty)
                                        (bind name acc)
                                       ) 
                                        ) expr env
                    lbl <- getLabel 
                    let ctype = rwc_expr_ty closure
                    let tys = nub $ rwc_ty_vars ctype --FIXME: Should we be nubbing this?
                        defn = RWCDefn (s2n lbl) $ embed $ setbind tys (ctype,closure)
                    tell [defn]
                    return (RWCVar ctype $ s2n lbl)


rwc_expr_ty (RWCApp ty _ _) = ty
rwc_expr_ty (RWCLam ty _)   = ty
rwc_expr_ty (RWCVar ty _)   = ty
rwc_expr_ty (RWCCon ty _)   = ty
rwc_expr_ty (RWCLiteral ty _) = ty
rwc_expr_ty (RWCCase ty _ _ ) = ty

rwc_ty_vars :: RWCTy -> [Name RWCTy]
rwc_ty_vars (RWCTyVar nme) = [nme]
rwc_ty_vars (RWCTyCon _  ) = []
rwc_ty_vars (RWCTyApp a1 a2) = (rwc_ty_vars a1) ++ (rwc_ty_vars a2)


getLabel :: LLM String 
getLabel = do
              s <- get
              put (s+1)
              return ("#!ll" ++ (show s))
