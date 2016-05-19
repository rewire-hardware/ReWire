module ReWire.Core.ToMiniHDL where

import ReWire.MiniHDL.Syntax as M
import ReWire.Core.Syntax as C
import ReWire.Pretty
import ReWire.Annotation
import ReWire.Error
import Control.Monad.Reader
import Control.Monad.Identity
import Encoding (zEncodeString)   -- this is from the ghc package

mangle :: String -> String
mangle = zEncodeString

type CM = SyntaxErrorT (ReaderT [DataCon] Identity)

askCtors :: CM [DataCon]
askCtors = ask

type TySub = [(TyId,C.Ty)]

matchTy :: TySub -> C.Ty -> C.Ty -> CM TySub
matchTy s (TyApp _ t1 t2) (TyApp _ t1' t2')          = do s1 <- matchTy [] t1 t1'
                                                          s2 <- matchTy [] t2 t2'
                                                          s' <- merge s s1
                                                          merge s' s2
matchTy _ (TyCon _ tci) (TyCon _ tci') | tci == tci' = return []
matchTy s (TyComp _ t1 t2) (TyComp _ t1' t2')        = do s1 <- matchTy [] t1 t1'
                                                          s2 <- matchTy [] t2 t2'
                                                          s' <- merge s s1
                                                          merge s' s2
matchTy s (TyVar _ v) t                              = merge s [(v,t)]
matchTy _ t t'                                       = failNowhere $ "matchTy: can't match " ++ prettyPrint t
                                                                     ++ " with " ++ prettyPrint t'

merge :: TySub -> TySub -> CM TySub
merge [] s'        = return s'
merge ((v,t):s) s' = case lookup v s' of
                       Nothing             -> do s'' <- merge s s'
                                                 return ((v,t):s'')
                       Just t' | t == t'   -> merge s s'
                               | otherwise -> failNowhere $ "merge: inconsistent assignment of tyvar " ++ v
                                                          ++ ": " ++ prettyPrint t ++ " vs. "
                                                          ++ prettyPrint t'

apply :: TySub -> C.Ty -> C.Ty
apply s (TyApp an t1 t2)  = TyApp an (apply s t1) (apply s t2)
apply _ t@(TyCon {})      = t
apply s (TyComp an t1 t2) = TyComp an (apply s t1) (apply s t2)
apply s t@(TyVar _ i)     = case lookup i s of
                              Just t' -> t'
                              Nothing -> t

datanumctors :: TyConId -> CM Int
datanumctors tci = do ctors <- myctors tci
                      return (length ctors)

myctors :: TyConId -> CM [DataCon]
myctors tci = do ctors <- askCtors
                 return (filter isMine ctors)
   where isMine (DataCon _ _ _ t) = case flattenTyApp (last (flattenArrow t)) of
                                      (TyCon _ tci':_) -> tci == tci'
                                      _                -> False

ceilLog2 :: Int -> Int
ceilLog2 n = ceiling (logBase 2 (fromIntegral n :: Double))

tagwidthdata :: TyConId -> CM Int
tagwidthdata tci = do nctors <- datanumctors tci
                      return (max 0 (ceilLog2 nctors))

ctorwidth :: C.Ty -> DataCon -> CM Int
ctorwidth t (DataCon _ _ _ ct) = do let ts     =  flattenArrow ct
                                        tres   =  last ts
                                        targs  =  init ts
                                    s          <- matchTy [] tres t
                                    let targs' =  map (apply s) targs
                                    sizes      <- mapM sizeof targs'
                                    return (sum sizes)

sizeof :: C.Ty -> CM Int
sizeof t = case th of
             TyApp _ _ _  -> failAt (ann t) "sizeof: Got TyApp after flattening (can't happen)"
             TyCon _ tci  -> do ctors        <- myctors tci
                                let tagwidth =  max 0 (ceilLog2 (length ctors))
                                ctorwidths   <- mapM (ctorwidth t) ctors
                                return (tagwidth + maximum ctorwidths)
             TyComp _ _ _ -> failAt (ann t) "sizeof: Encountered computation type"
             TyVar _ _    -> failAt (ann t) "sizeof: Encountered type variable"
    where (th:_) = flattenTyApp t

getDefnPorts :: Defn -> CM [Port]
getDefnPorts (Defn _ _ t _) = do let ts       =  flattenArrow t
                                     targs    =  init ts
                                     tres     =  last ts
                                     argnames =  zipWith (\ _ x -> "arg" ++ show x) targs ([0..]::[Int])
                                 argsizes     <- mapM sizeof targs
                                 ressize      <- sizeof tres
                                 let argports =  zipWith (\ n x -> Port n In (TyStdLogicVector x)) argnames argsizes
                                     resport  =  Port "res" Out (TyStdLogicVector ressize)
                                 return (argports ++ [resport])

mkDefnEntity :: Defn -> CM Entity
mkDefnEntity d@(Defn _ n _ _) = do ps <- getDefnPorts d
                                   return (Entity (mangle n) ps)

compileExp :: C.Exp -> CM ()
compileExp e_ = case e of
                  App {}        -> failAt (ann e_) "compileExp: Got App after flattening (can't happen)"
                  Prim {}       -> failAt (ann e_) "compileExp: Encountered Prim"
                  GVar {}       -> undefined {- instantiate, add components to so-far-list, stripe -}
                  LVar {}       -> undefined {- is just argN; must be empty eargs -}
                  Con {}        -> undefined {- get tag vector, pad vector, stripe -}
                  Match {}      -> undefined {- ... -}
                  NativeVHDL {} -> undefined {- same as gvar, just don't mangle! -}
  where (e:_eargs) = flattenApp e_

mkDefnArch :: Defn -> CM Architecture
mkDefnArch (Defn _ n _ _) = return (Architecture (mangle (n ++ "_impl")) (mangle n) [] [] []) -- FIXME(adam)

compileDefn :: Defn -> CM Unit
compileDefn d = do ent  <- mkDefnEntity d
                   arch <- mkDefnArch d
                   return (Unit ent arch)

compileProgram :: C.Program -> Either AstError M.Program
compileProgram p = runIdentity $ flip runReaderT (ctors p) $ runSyntaxError $
                     do units <- mapM compileDefn (defns p)
                        return (M.Program units)
