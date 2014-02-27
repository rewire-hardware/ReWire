module ReWire.Core.Transformations.LambdaLifter (lambdaLift) where

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

import Debug.Trace


--type TransCommand = String -> RWCProg -> (Maybe RWCProg,Maybe String)

lambdaLift :: TransCommand
lambdaLift cmd prog = let prog' = runLL (ll_prog prog)
                        in (Just prog', Nothing)
                                        

type ReaderEnv = ([(Name RWCExp,RWCTy)])
type LLM = (WriterT [RWCDefn] (ReaderT ReaderEnv (StateT Int (LFreshM))))

runDefns :: LLM RWCProg -> (ReaderT ReaderEnv (StateT Int (LFreshM))) RWCProg
runDefns llm = runWriterT llm >>= (\(RWCProg {dataDecls=pdecls, defns=trec_defns},new_defns) -> do 
                                                                                                  defns' <- luntrec trec_defns
                                                                                                  let defns'' = trec $ new_defns ++ defns'
                                                                                                  return $ RWCProg {dataDecls=pdecls, defns=defns''}
                                  )

runLL llm = runLFreshM $ evalStateT (runReaderT (runDefns llm) []) 0

runLLM llm = runLFreshM $ evalStateT (runReaderT (runWriterT llm) []) 0

ll_prog :: RWCProg -> LLM RWCProg
ll_prog (RWCProg {dataDecls=pdecls, defns=trec_defns}) = do
                                    defns' <- luntrec trec_defns
                                    let avoids = map (AnyName . defn_name) defns' 
                                    defns'' <- avoid avoids $ mapM ll_def defns'
                                    return $ (RWCProg {dataDecls=pdecls, defns=(trec defns'')})
   where
    defn_name :: RWCDefn -> (Name RWCExp)
    defn_name (RWCDefn n _) = n


ll_def :: RWCDefn -> LLM RWCDefn
ll_def (RWCDefn name ebnd) = do
                               let bnd = unembed ebnd 
                               lunbind bnd (\(tys,(body_ty,expr_body)) -> do
                                                                   expr_body' <- ll_def_exp expr_body
                                                                   return $ RWCDefn name $ embed $ setbind tys (body_ty,expr_body')
                                           )
  where
    ll_def_exp :: RWCExp -> LLM RWCExp 
    ll_def_exp (RWCLam ty bexp) = lunbind bexp (\(name, exp) -> local (\env -> (name,fst $ peel_first_type ty):env) $ do
                                                                                                              exp' <- ll_def_exp exp
                                                                                                              return $ RWCLam ty $ bind name exp'
                                                                                                              
                                               )
                                    
    ll_def_exp x = ll_exp x

ll_exp :: RWCExp -> LLM RWCExp
ll_exp l@(RWCLam ty bnd)  = do
                              let arg_ty = fst $ peel_first_type ty
                              (expr,wrapenv) <- lunbind bnd (\(pat,term) -> local (\env -> (pat,arg_ty):env) $ do
                                                                                                         term' <- ll_exp term  --Lambda Lift the interior first
                                                                                                         build_defn_expr term' --Build the defn for this lambda
                                                  )
                              local (\_ -> wrapenv) (wrap expr) --Wrap the expr by applying in-scope variables to it. 
                               
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
ll_alt (RWCAlt bnd) = lunbind bnd (\(pat, exp) -> do
                                                    exp' <- env_with_pattern pat (ll_exp exp)
                                                    return (RWCAlt $ bind pat exp')
                                  )
                         
   where
      env_with_pattern :: RWCPat -> LLM a -> LLM a
      env_with_pattern pat m = local (\env -> (pat_vars pat) ++ env) m
                                

      pat_vars :: RWCPat -> ReaderEnv
      pat_vars (RWCPatLiteral _) = []
      pat_vars (RWCPatCon _ pats) = concatMap pat_vars pats
      pat_vars (RWCPatVar ty name) = [(name,unembed ty)]


{-| Peel off the first type (for a function)  and return the remaining type
 -  of a given type if it exists.  Nothing, otherwise -}
peel_first_type :: RWCTy -> (RWCTy, Maybe RWCTy)
peel_first_type (RWCTyApp (RWCTyApp (RWCTyCon "(->)") ty) rem) = (ty, Just rem)
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
               let closure = foldr (\(name,name_ty) acc -> 
                                        RWCApp (rest_ty acc) acc (RWCVar name_ty name)
                                         ) expr (env)
               return closure 
   where
     rest_ty expr = case peel_first_type $ rwc_expr_ty expr of --This will fail if we are ill-formed 
                            (_, Just rest) -> rest
                            x              -> error $ show x

build_defn_expr :: RWCExp -> LLM (RWCExp,ReaderEnv)
build_defn_expr expr = do
                    env <- ask
                    let frees = fv expr 
                        env'  = filter (\x -> elem (fst x) frees) env
                        expr_ty = rwc_expr_ty expr
                        closure = foldl (\acc (name,name_ty) ->
                                    (RWCLam 
                                      (RWCTyApp (RWCTyApp (RWCTyCon "(->)") name_ty) expr_ty)
                                        (bind name acc)
                                       ) 
                                        ) expr env'
                    lbl <- getLabel 
                    let ctype = rwc_expr_ty closure
                    let tys = nub $ rwc_ty_vars ctype --FIXME: Should we be nubbing this?
                        defn = RWCDefn (s2n lbl) $ embed $ setbind tys (ctype,closure)
                    tell [defn]
                    return (RWCVar ctype $ s2n lbl,env')


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
