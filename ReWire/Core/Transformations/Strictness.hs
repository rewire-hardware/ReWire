-- Simple strictness analyzer for Core based on abstract interpretation.

module ReWire.Core.Transformations.Strictness where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Monad
import Unbound.LocallyNameless hiding (Val)

data Val = Bottom | Unknown | Constructed Identifier [Val] | Literal RWCLit | FunVal (Val -> Val)

type Env = [(Name RWCExp,Val)]

tweak :: Env -> Name RWCExp -> Val -> Env
tweak env n v = (n,v):env

patMatch :: MonadReWire m => RWCPat -> Val -> m MatchResult
patMatch (RWCPatCon i ps) v = case v of
                                Bottom                        -> return MatchBottom
                                Unknown                       -> return MatchUnknown
                                Constructed i' vs | i == i'   -> do ms <- zipWithM patMatch ps vs
                                                                    mergeMatches ms
                                                  | otherwise -> return MatchNoMatch
patMatch (RWCPatLiteral l) v = case v of
                                 Bottom                 -> return MatchBottom
                                 Unknown                -> return MatchUnknown
                                 Literal l' | l==l'     -> return (Match [])
                                            | otherwise -> return MatchNoMatch
patMatch (RWCPatVar _ x) v   = return (Match [(x,v)])

evalAlt :: MonadReWire m => Env -> Val -> RWCAlt -> m Val
evalAlt env v (RWCAlt b) = lunbind b (\(p,e) -> do
                             m <- patMatch p v
                             case m of
                               MatchUnknown -> return Unknown
                               MatchBottom  -> return Bottom
                               MatchNoMatch -> return Unknown
                               Match bs     -> evalExp (bs++env) e)

evalExp :: MonadRewire m => Env -> RWCExp -> m Val
evalExp env (RWCApp _ e1 e2)   = do v1 <- evalExp env e1
                                    case v1 of
                                      FunVal f -> do v2 <- evalExp env e2
                                                     return (f v2)
                                      Unknown  -> return Unknown
                                      Bottom   -> return Bottom
                                      -- other possibilities should not arise in a well-typed program
evalExp env (RWCLam _ b)       = lunbind b (\(x,e) ->
                                   return (FunVal (\ v -> evalExp (tweak env x v) e)))
evalExp env (RWCVar t x)       = case lookup x env of
                                   Just v  -> return v
                                   Nothing -> mkNonStrictWithArity (arity t) -- if we don't know anything about the variable we'll have to assume it's non-strict in all arguments
evalExp env (RWCCon t i)       = mkConstructorVal (arity t)
evalExp env (RWCLiteral _ l)   = return (RWCLiteral l)
evalExp env (RWCCase t e alts) = do v  <- evalExp env e
                                    vs <- mapM (evalAlt env v) alts
                                    return (mergeVs vs)
