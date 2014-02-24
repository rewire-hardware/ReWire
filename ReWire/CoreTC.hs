{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.CoreTC where

import ReWire.Core
import ReWire.CorePP (ppp,ppProg)
import ReWire.CoreParser
import Text.Parsec (runParser,eof)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Unbound.LocallyNameless
import Unbound.LocallyNameless.Types (SetPlusBind)
import Data.List (nub)
import Debug.Trace (trace)

-- Type checker for core.

type TySub = [(Name RWCTy,RWCTy)]
data TCEnv = TCEnv { as  :: [Assump],
                     cas :: [CAssump] } deriving Show
data TCState = TCState { varCounter :: Int,
                         tySub      :: TySub } deriving Show
                         
type Assump  = (Name RWCExp,SetPlusBind [Name RWCTy] RWCTy)
type CAssump = (Identifier,SetPlusBind [Name RWCTy] RWCTy)

type TCM = FreshMT (ReaderT TCEnv (StateT TCState Identity))

localAssumps f = local (\ tce -> tce { as = f (as tce) })
askAssumps = ask >>= \ tce -> return (as tce)
localCAssumps f = local (\ tce -> tce { cas = f (cas tce) })
askCAssumps = ask >>= \ tce -> return (cas tce)
getVarCounter = get >>= return . varCounter
putVarCounter i = get >>= \ s -> put (s { varCounter = i })
getTySub = get >>= return . tySub
updTySub f = get >>= \ s -> put (s { tySub = f (tySub s) })
putTySub sub = get >>= \ s -> put (s { tySub = sub })

freshv :: TCM (Name RWCTy)
freshv = do i     <- getVarCounter
            putVarCounter (i+1)
            let n =  s2n ("?"++show i)
            sub   <- getTySub
            updTySub ((n,RWCTyVar n):)
            return n

initPat :: RWCPat -> TCM RWCPat
initPat (RWCPatCon i ps_) = do ps <- mapM initPat ps_
                               return (RWCPatCon i ps)
initPat (RWCPatLiteral l) = return (RWCPatLiteral l)
initPat (RWCPatVar _ n)   = do tv <- freshv
                               return (RWCPatVar (embed $ RWCTyVar tv) n)

initAlt :: RWCAlt -> TCM RWCAlt
initAlt (RWCAlt b) = do (p_,e_) <- unbind b
                        p       <- initPat p_
                        e       <- initExp e_
                        return (RWCAlt (bind p e))

initExp :: RWCExp -> TCM RWCExp
initExp (RWCApp _ e1_ e2_)   = do e1 <- initExp e1_
                                  e2 <- initExp e2_
                                  tv <- freshv
                                  return (RWCApp (RWCTyVar tv) e1 e2)
initExp (RWCLam _ b)         = do (x,e_) <- unbind b
                                  e      <- initExp e_
                                  tv     <- freshv
                                  return (RWCLam (RWCTyVar tv) (bind x e))
initExp (RWCVar _ n)         = do tv <- freshv
                                  return (RWCVar (RWCTyVar tv) n)
initExp (RWCCon _ n)         = do tv <- freshv
                                  return (RWCCon (RWCTyVar tv) n)
initExp (RWCLiteral _ l)     = do tv <- freshv
                                  return (RWCLiteral (RWCTyVar tv) l)
initExp (RWCCase _ e_ alts_) = do e    <- initExp e_
                                  alts <- mapM initAlt alts_
                                  tv   <- freshv
                                  return (RWCCase (RWCTyVar tv) e alts)

initDefn :: RWCDefn -> TCM (RWCDefn,Assump)
initDefn (RWCDefn n (Embed b)) = do (tvs,(t,e)) <- unbind b
                                    e'          <- initExp e
                                    let a       =  (n,setbind tvs t)
                                    return (RWCDefn n (Embed (setbind tvs (t,e'))),a)

initDataCon :: [Name RWCTy] -> RWCTy -> RWCDataCon -> CAssump
initDataCon tvs rt (RWCDataCon i ts) = (i,setbind tvs (foldr arr rt ts))

initDataDecl :: RWCData -> TCM [CAssump]
initDataDecl (RWCData i b) = do (tvs,dcs) <- unbind b
                                let rt    =  foldl RWCTyApp (RWCTyCon i) (map RWCTyVar tvs)
                                return $ map (initDataCon tvs rt) dcs

arr :: RWCTy -> RWCTy -> RWCTy
arr t1 t2 = RWCTyApp (RWCTyApp (RWCTyCon "(->)") t1) t2

typeOf (RWCApp t _ _)   = t
typeOf (RWCLam t _)     = t
typeOf (RWCVar t _)     = t
typeOf (RWCCon t _)     = t
typeOf (RWCLiteral t _) = t
typeOf (RWCCase t _ _)  = t

(@@) :: TySub -> TySub -> TySub
s1@@s2 = [(u,substs s1 t) | (u,t) <- s2] ++ s1

isFlex :: Name RWCTy -> Bool
isFlex = (=='?') . head . name2String

varBind :: Monad m => Name RWCTy -> RWCTy -> m TySub
varBind u t | t `aeq` RWCTyVar u = return []
            | u `elem` fv t      = fail $ "occurs check fails: " ++ show u ++ ", " ++ show t
            | otherwise          = return [(u,t)]

mgu :: Monad m => RWCTy -> RWCTy -> m TySub
mgu (RWCTyApp tl tr) (RWCTyApp tl' tr')                = do s1 <- mgu tl tl'
                                                            s2 <- mgu (substs s1 tr) (substs s1 tr')
                                                            return (s2@@s1)
mgu (RWCTyVar u) t | isFlex u                          = varBind u t
mgu t (RWCTyVar u) | isFlex u                          = varBind u t
mgu (RWCTyCon c1) (RWCTyCon c2) | c1 == c2             = return []
mgu (RWCTyVar v) (RWCTyVar u) | not (isFlex v) && v==u = return []
mgu t1 t2                                              = fail $ "types do not unify: " ++ show t1 ++ ", " ++ show t2

unify :: RWCTy -> RWCTy -> TCM ()
unify t1 t2 = do s <- getTySub
                 u <- mgu (substs s t1) (substs s t2)
                 updTySub (u@@)

inst :: [Name RWCTy] -> RWCTy -> TCM RWCTy
inst tvs t = do sub <- mapM (\ tv -> freshv >>= \ v -> return (tv,RWCTyVar v)) tvs
                return (substs sub t)

patassumps :: RWCPat -> [Assump]
patassumps (RWCPatCon i ps)        = concatMap patassumps ps
patassumps (RWCPatLiteral _)       = []
patassumps (RWCPatVar (Embed t) n) = [(n,setbind [] t)]

flattenArrow :: RWCTy -> [RWCTy]
flattenArrow (RWCTyApp (RWCTyApp (RWCTyCon "(->)") t1) t2) = t1 : flattenArrow t2
flattenArrow t                                             = [t]

tcPat :: RWCTy -> RWCPat -> TCM ()
tcPat t (RWCPatLiteral l)         = case l of
                                      RWCLitInteger _ -> unify t (RWCTyCon "Integer")
                                      RWCLitFloat _   -> unify t (RWCTyCon "Float")
                                      RWCLitChar _    -> unify t (RWCTyCon "Char")
tcPat t (RWCPatVar (Embed tv) _)  = unify t tv
tcPat t (RWCPatCon i ps)          = do cas     <- askCAssumps
                                       let mpt =  lookup i cas
                                       case mpt of
                                         Nothing -> fail $ "Unknown constructor: " ++ i
                                         Just b  -> do (tvs,ta_) <- unbind b
                                                       ta        <- inst tvs ta_
                                                       let ts    =  flattenArrow ta
                                                           targs =  init ts
                                                           tres  =  last ts
                                                       if length ps /= length targs
                                                          then fail "Pattern is not applied to enough arguments"
                                                          else do zipWithM_ tcPat targs ps
                                                                  unify t tres

tcAlt :: RWCTy -> RWCTy -> RWCAlt -> TCM ()
tcAlt tres tscrut (RWCAlt b) = do (p,e) <- unbind b
                                  tcPat tscrut p
                                  localAssumps (patassumps p++) (tcExp e)
                                  unify tres (typeOf e)

tcExp :: RWCExp -> TCM ()
tcExp (RWCApp t e1 e2)   = do tcExp e1
                              tcExp e2
                              unify (typeOf e1) (typeOf e2 `arr` t)
tcExp (RWCLam t b)       = do (x,e) <- unbind b
                              tv    <- freshv
                              localAssumps ((x,setbind [] (RWCTyVar tv)):) (tcExp e)
                              unify t (RWCTyVar tv `arr` typeOf e)
tcExp (RWCVar t v)       = do as      <- askAssumps
                              let mpt =  lookup v as
                              case mpt of
                                Nothing -> return ()
                                Just b  -> do (tvs,ta_) <- unbind b
                                              ta        <- inst tvs ta_
                                              unify t ta
tcExp (RWCCon t i)       = do cas     <- askCAssumps
                              let mpt =  lookup i cas
                              case mpt of
                                Nothing -> fail $ "Unknown constructor: " ++ i
                                Just b  -> do (tvs,ta_) <- unbind b
                                              ta        <- inst tvs ta_
                                              unify t ta
tcExp (RWCLiteral t l)   = case l of
                             RWCLitInteger _ -> unify t (RWCTyCon "Integer")
                             RWCLitFloat _   -> unify t (RWCTyCon "Float")
                             RWCLitChar _    -> unify t (RWCTyCon "Char")
tcExp (RWCCase t e alts) = do tcExp e
                              mapM_ (tcAlt t (typeOf e)) alts

tcDefn :: RWCDefn -> TCM RWCDefn
tcDefn (RWCDefn n (Embed b)) = do (tvs,(t,e)) <- unbind b
                                  tcExp e
                                  unify t (typeOf e)
                                  s           <- getTySub
                                  return (RWCDefn n (embed $ setbind tvs (t,substs s e)))

tc :: RWCProg -> TCM RWCProg
tc p = do ds       <- untrec (defns p)
          (ds',as) <- liftM unzip (mapM initDefn ds)
          cas      <- liftM concat (mapM initDataDecl (dataDecls p))
          ds''     <- localAssumps (as++) (localCAssumps (cas++) (mapM tcDefn ds'))
          s        <- getTySub
          return (p { defns = trec ds'' })

ptc :: FilePath -> IO ()
ptc n = do ppp n
           guts        <- readFile n
           let res     =  runParser (whiteSpace >> prog >>= \ p -> whiteSpace >> eof >> return p) 0 n guts
           case res of
             Left err  -> print err
             Right ast -> let (p,s) = runIdentity (runStateT (runReaderT (runFreshMT $ tc ast) (TCEnv [] [])) (TCState 0 []))
                          in  print (runFreshM (ppProg p))