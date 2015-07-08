module ReWire.Core.Transformations.LambdaLift (lambdaLift) where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Types
import Unbound.LocallyNameless
import Unbound.LocallyNameless.Types
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Data.List (nub,nubBy)

import Debug.Trace


--type TransCommand = String -> RWCProg -> (Maybe RWCProg,Maybe String)

lambdaLift :: TransCommand
lambdaLift cmd prog = let prog' = runLL (ll_prog prog)
                        in (Just prog', Nothing)
                                        

type ReaderEnv = ([(Name RWCExp,RWCTy)])
type LLM = WriterT [RWCDefn] (ReaderT ReaderEnv FreshM)

runDefns :: LLM RWCProg -> (ReaderT ReaderEnv FreshM) RWCProg
runDefns llm = runWriterT llm >>= return . fst 
                                  {-(\(RWCProg {dataDecls=pdecls, defns=trec_defns},new_defns) -> do 
                                                                                                  defns' <- luntrec trec_defns
                                                                                                  let defns'' = trec $ new_defns ++ defns'
                                                                                                  return $ RWCProg {dataDecls=pdecls, defns=defns''}
                                  ) -}

runLL llm = runFreshM $ runReaderT (runDefns llm) []

ll_prog :: RWCProg -> LLM RWCProg
ll_prog (RWCProg {dataDecls=pdecls, defns=trec_defns}) = do
                                    defns' <- untrec trec_defns
--                                    let avoids = map (AnyName . defn_name) defns' 
                                    (defns'',new_defns) <- listen {-$ avoid avoids-} $ mapM ll_def defns'
                                    return $ (RWCProg {dataDecls=pdecls, defns=(trec (defns''++new_defns))})
   where
    defn_name :: RWCDefn -> (Name RWCExp)
    defn_name (RWCDefn n _) = n


ll_def :: RWCDefn -> LLM RWCDefn
ll_def (RWCDefn name ebnd) = do
                               let bnd = unembed ebnd 
                               unbind bnd >>= \(tys,(body_ty,expr_body)) -> do
                                                                   expr_body' <- ll_def_exp expr_body
                                                                   return $ RWCDefn name $ embed $ setbind tys (body_ty,expr_body')
  where
    ll_def_exp :: RWCExp -> LLM RWCExp 
    ll_def_exp (RWCLam ty bexp) = unbind bexp >>= \(name, exp) -> local (\env -> (name,fst $ peel_first_type ty):env) $ do
                                                                                                              exp' <- ll_def_exp exp
                                                                                                              return $ RWCLam ty $ bind name exp'
    ll_def_exp x = ll_exp x

arrow_left :: RWCTy -> RWCTy
arrow_left (RWCTyApp (RWCTyApp (RWCTyCon "(->)") t1) _) = t1

arrow_right :: RWCTy -> RWCTy
arrow_right (RWCTyApp (RWCTyApp (RWCTyCon "(->)") _) t2) = t2

gather_lambda :: RWCExp -> LLM ([(Name RWCExp,RWCTy)],RWCExp)
gather_lambda (RWCLam t b) = unbind b >>= \(x,e) -> do (xts,e') <- gather_lambda e
                                                       return ((x,arrow_left t):xts,e')
gather_lambda e            = return ([],e)

mkLam :: (Name RWCExp,RWCTy) -> RWCExp -> RWCExp
mkLam (x,t) bod =  RWCLam (RWCTyApp (RWCTyApp (RWCTyCon "(->)") t) (rwc_expr_ty bod)) (bind x bod)

do_lift :: [(Name RWCExp,RWCTy)] -> RWCExp -> LLM RWCExp
do_lift xts e = do n                   <- fresh (s2n "ll")
                   let lam             =  foldr mkLam e xts
                       tlam            =  rwc_expr_ty lam
                       tvs             =  nub (fv tlam)
                       defn            =  RWCDefn n (embed $ setbind tvs (tlam,lam))
                   tell [defn]
                   return (RWCVar tlam n)

rewrap :: [(Name RWCExp,RWCTy)] -> RWCExp -> LLM RWCExp
rewrap xts e = return (foldl mkApp e xts)
    where mkApp e1 (x,t) = RWCApp (arrow_right (rwc_expr_ty e1)) e1 (RWCVar t x)

ll_exp :: RWCExp -> LLM RWCExp
ll_exp l@(RWCLam t _)  = do (closed_xts,body) <- gather_lambda l
                            env               <- ask
                            let open_xts      =  filter (\(x,_) -> x `elem` fv l) env
                            body'             <- ll_exp body
                            e                 <- do_lift open_xts (foldr mkLam body' closed_xts)
                            rewrap open_xts e
                               
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
ll_alt (RWCAlt bnd) = unbind bnd >>= \(pat, exp) -> do
                                                     exp' <- env_with_pattern pat (ll_exp exp)
                                                     return (RWCAlt $ bind pat exp')
                         
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

{-
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
                    let frees = nub (fv expr)
                        env'  = filter (\x -> elem (fst x) frees) env
                        expr_ty = rwc_expr_ty expr
                        closure = foldl (\acc (name,name_ty) ->
                                    (RWCLam 
                                      (RWCTyApp (RWCTyApp (RWCTyCon "(->)") name_ty) (rwc_expr_ty acc))
                                        (bind name acc)
                                       ) 
                                        ) expr env'
                    lbl <- getLabel 
                    let ctype = rwc_expr_ty closure
                        defn = RWCDefn (s2n lbl) $ embed $ setbind (nub (fv ctype)) (ctype,closure)
                    tell [defn]
                    let cfrees = nub (fv closure)
                        env_closure = filter (\x -> elem (fst x) cfrees) env'
                    return (RWCVar ctype $ s2n lbl,env_closure)
-}

rwc_expr_ty (RWCApp ty _ _) = ty
rwc_expr_ty (RWCLam ty _)   = ty
rwc_expr_ty (RWCVar ty _)   = ty
rwc_expr_ty (RWCCon ty _)   = ty
rwc_expr_ty (RWCLiteral ty _) = ty
rwc_expr_ty (RWCCase ty _ _ ) = ty

--getLabel :: LLM String 
--getLabel = do
--              s <- get
--              put (s+1)
--              return ("#!ll" ++ (show s))
