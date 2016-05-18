module ReWire.Core.ToMiniHDL where

import ReWire.MiniHDL.Syntax as M
import ReWire.Core.Syntax as C
import ReWire.Annotation
import ReWire.Error
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Identity
import Encoding (zEncodeString)   -- this is from the ghc package

type CM = SyntaxErrorT (ReaderT [DataCon] Identity)

mangle :: String -> String
mangle = zEncodeString

askCtors :: CM [DataCon]
askCtors = ask

datanumctors :: TyConId -> CM Int
datanumctors tci = do ctors <- askCtors
                      return (length (filter isMine ctors))
   where isMine (DataCon _ _ _ t) = case flattenTyApp (last (flattenArrow t)) of
                                      (TyCon _ tci':_) -> tci == tci'
                                      _            -> False

tagwidthdata :: TyConId -> CM Int
tagwidthdata tci = do nctors <- datanumctors tci
                      return (max 0 (ceiling (logBase 2 (fromIntegral nctors :: Double))))

sizeofdata :: Annote -> TyConId -> [C.Ty] -> CM Int
sizeofdata _an tci _ts = tagwidthdata tci -- FIXME(adam)

sizeof :: C.Ty -> CM Int
sizeof t_ = case t of
              TyApp an _ _  -> failAt an "sizeof: Got TyApp after flattening (can't happen)"
              TyCon an i    -> sizeofdata an i ts
              TyComp an _ _ -> failAt an "sizeof: Encountered computation type"
              TyVar an _    -> failAt an "sizeof: Encountered type variable"
    where (t:ts) = flattenTyApp t_

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

mkDefnArch :: Defn -> CM Architecture
mkDefnArch (Defn _ n _ _) = return (Architecture (mangle (n ++ "_impl")) (mangle n) [] [] []) -- FIXME(adam)

compileDefn :: Defn -> CM Unit
compileDefn d = do ent  <- mkDefnEntity d
                   arch <- mkDefnArch d
                   return (Unit ent arch)

-- FIXME(adam): Ugly hack until I know for sure what is coming out of the front
--              end; like mapM but skips places where we get an error.  :/
whereWeCan :: (a -> CM b) -> [a] -> CM [b]
whereWeCan f (x:xs) = do my <- (liftM Just (f x)) `catchError` (\ _ -> return Nothing)
                         ys <- whereWeCan f xs
                         case my of
                           Just y  -> return (y:ys)
                           Nothing -> return ys
whereWeCan _ []     = return []

compileProgram :: C.Program -> Either AstError M.Program
compileProgram p = runIdentity $ flip runReaderT (ctors p) $ runSyntaxError $
                     do units <- whereWeCan compileDefn (defns p)
                        return (M.Program units)
