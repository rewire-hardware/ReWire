module ReWire.Core.ToMiniHDL where

import ReWire.MiniHDL.Syntax as M
import ReWire.Core.Syntax as C
import ReWire.Pretty
import ReWire.Annotation
import ReWire.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Encoding (zEncodeString)   -- this is from the ghc package
import Data.List (find)
import Data.Bits (testBit)

mangle :: String -> String
mangle = zEncodeString

type CM = SyntaxErrorT (
            StateT ([Signal],[Component],Int) (
              ReaderT [DataCon] Identity
            )
          )

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

tcictors :: TyConId -> CM [DataCon]
tcictors tci = do ctors <- askCtors
                  return (filter isMine ctors)
   where isMine (DataCon _ _ _ t) = case flattenTyApp (last (flattenArrow t)) of
                                      (TyCon _ tci':_) -> tci == tci'
                                      _                -> False

askDci :: DataConId -> CM DataCon
askDci dci = do ctors <- askCtors
                case find (\ (DataCon _ dci' _ _) -> dci == dci') ctors of
                  Just ctor -> return ctor
                  Nothing   -> failNowhere $ "askDci: no info for data constructor " ++ show dci

dcitci :: DataConId -> CM TyConId
dcitci dci = do DataCon _ _ _ t <- askDci dci
                case flattenTyApp (last (flattenArrow t)) of
                  (TyCon _ tci:_) -> return tci
                  _               -> failNowhere $ "dcitci: malformed type for data constructor "
                                                ++ show dci ++ " (does not end in application of TyCon)"

nvec :: Int -> Int -> [Bit]
nvec width n = nvec' 0 []
  where nvec' pos bits | pos >= width = bits
                       | otherwise    = nvec' (pos+1) ((if testBit n pos then One else Zero):bits)

dciTagVector :: DataConId -> CM [Bit]
dciTagVector dci = do tci               <- dcitci dci
                      ctors             <- tcictors tci
                      DataCon _ _ pos _ <- askDci dci
                      return (nvec (ceilLog2 (length ctors)) pos)

dciPadVector :: DataConId -> C.Ty -> CM [Bit]
dciPadVector dci t = do ctor         <- askDci dci
                        fieldwidth   <- ctorwidth t ctor
                        tci          <- dcitci dci
                        ctors        <- tcictors tci
                        let tot      =  ceilLog2 (length ctors) + fieldwidth
                        tysize       <- sizeof t
                        let padwidth =  tysize-tot
                        return (nvec padwidth 0)

ceilLog2 :: Int -> Int
ceilLog2 n = ceiling (logBase 2 (fromIntegral n :: Double))

ctorwidth :: C.Ty -> DataCon -> CM Int
ctorwidth t (DataCon _ _ _ ct) = do let ts     =  flattenArrow ct
                                        tres   =  last ts
                                        targs  =  init ts
                                    s          <- matchTy [] tres t
                                    let targs' =  map (apply s) targs
                                    sizes      <- mapM sizeof targs'
                                    return (sum sizes)

--ctorFieldWidthsAt :: DataConId -> C.Ty -> CM [Int]
--ctorFieldWidthsAt dci tspec = do 

sizeof :: C.Ty -> CM Int
sizeof t = case th of
             TyApp _ _ _  -> failAt (ann t) "sizeof: Got TyApp after flattening (can't happen)"
             TyCon _ tci  -> do ctors        <- tcictors tci
                                let tagwidth =  ceilLog2 (length ctors)
                                ctorwidths   <- mapM (ctorwidth t) ctors
                                return (tagwidth + maximum ctorwidths)
             TyComp _ _ _ -> failAt (ann t) "sizeof: Encountered computation type"
             TyVar _ _    -> failAt (ann t) "sizeof: Encountered type variable"
    where (th:_) = flattenTyApp t

getTyPorts :: C.Ty -> CM [Port]
getTyPorts t = do let ts       =  flattenArrow t
                      targs    =  init ts
                      tres     =  last ts
                      argnames =  zipWith (\ _ x -> "arg" ++ show x) targs ([0..]::[Int])
                  argsizes     <- mapM sizeof targs
                  ressize      <- sizeof tres
                  let argports =  zipWith (\ n x -> Port n In (TyStdLogicVector x)) argnames argsizes
                      resport  =  Port "res" Out (TyStdLogicVector ressize)
                  return (argports ++ [resport])

mkDefnEntity :: Defn -> CM Entity
mkDefnEntity (Defn _ n t _) = do ps <- getTyPorts t
                                 return (Entity (mangle n) ps)

freshName :: String -> CM Name
freshName s = do (sigs,comps,ctr) <- get
                 put (sigs,comps,ctr+1)
                 return (s ++ "_" ++ show ctr)

addSignal :: String -> M.Ty -> CM ()
addSignal n t = do (sigs,comps,ctr) <- get
                   put (sigs++[Signal n t],comps,ctr)

addComponent :: GId -> C.Ty -> CM ()
addComponent i t = do (sigs,comps,ctr) <- get
                      case find ((==mangle i) . componentName) comps of
                        Just _ -> return ()
                        Nothing -> do ps <- getTyPorts t
                                      put (sigs,Component (mangle i) ps:comps,ctr)

typeOfPat :: Pat -> C.Ty
typeOfPat (PatCon _ t _ _) = t
typeOfPat (PatVar _ t)     = t

compilePat :: Name -> Int -> Pat -> CM (Expr,[Expr])  -- first Expr is for whether match (std_logic); remaining Exprs are for extracted fields
compilePat nscr offset (PatCon _ _ dci ps) = do dcitagvec        <- dciTagVector dci
                                                let tagw         =  length dcitagvec
                                                fieldwidths      <- mapM (sizeof . typeOfPat) ps
                                                let fieldoffsets =  init $ scanl (+) (offset+tagw) fieldwidths
                                                rs               <- zipWithM (compilePat nscr) fieldoffsets ps
                                                let ematchs      =  map fst rs
                                                    eslices      =  concatMap snd rs
                                                    ematch       =  foldl ExprAnd
                                                                      (ExprIsEq
                                                                        (ExprSlice (ExprName nscr) offset (offset+tagw-1))
                                                                        (ExprBitString dcitagvec))
                                                                      ematchs
                                                return (ematch,eslices)
compilePat nscr offset (PatVar _ t)        = do size <- sizeof t
                                                return (ExprBoolConst True,[ExprSlice (ExprName nscr) offset (offset+size-1)])

compileExp :: C.Exp -> CM ([Stmt],Name)
compileExp e_ = case e of
                  App {}        -> failAt (ann e_) "compileExp: Got App after flattening (can't happen)"
                  Prim {}       -> failAt (ann e_) "compileExp: Encountered Prim"
                  GVar _ t i    -> do n           <- liftM (++"_res") $ freshName (mangle i)
                                      let tres    =  last (flattenArrow t)
                                      size        <- sizeof tres
                                      addSignal n (TyStdLogicVector size)
                                      sssns       <- mapM compileExp eargs
                                      let stmts   =  concatMap fst sssns
                                          ns      =  map snd sssns
                                      addComponent i t
                                      let argns   =  map (\ n -> "arg" ++ show n) ([0..]::[Int])
                                          pm      =  PortMap (zip argns (map ExprName ns) ++ [("res",ExprName n)])
                                      return (stmts++[Instantiate n (mangle i) pm],n)
                  LVar _ _ i       -> case eargs of
                                        [] -> return ([],"arg"++show i)
                                        _  -> failAt (ann e_) $ "compileExp: Encountered local variable in function position in " ++ prettyPrint e_
                  Con _ t i        -> do n           <- liftM (++"_res") $ freshName (mangle (deDataConId i))
                                         let tres    =  last (flattenArrow t)
                                         size        <- sizeof tres
                                         addSignal n (TyStdLogicVector size)
                                         sssns       <- mapM compileExp eargs
                                         let stmts   =  concatMap fst sssns
                                             ns      =  map snd sssns
                                         tagvec      <- dciTagVector i
                                         padvec      <- dciPadVector i tres 
                                         return (stmts++[Assign (LHSName n) (ExprConcat
                                                                               (foldl ExprConcat (ExprBitString tagvec) (map ExprName ns))
                                                                                  (ExprBitString padvec)
                                                                               )],
                                                 n)
                  -- FIXME(adam): In the following, need to add component for
                  -- the call; would be easiest if we had type info for gid.
                  Match _ t escr p gid lids malt -> case eargs of
                                                      [] -> case malt of
                                                              Just ealt -> do n                   <- freshName "match_res"
                                                                              sizeres             <- sizeof t
                                                                              addSignal n (TyStdLogicVector sizeres)
                                                                              (stmts_escr,n_escr) <- compileExp escr
                                                                              (ematch,efields)    <- compilePat n_escr 0 p
                                                                              n_call              <- liftM (++"_res") $ freshName (mangle gid)
                                                                              addSignal n_call (TyStdLogicVector sizeres)
                                                                              (stmts_ealt,n_ealt) <- compileExp ealt
                                                                              let argns           =  map (\ n -> "arg" ++ show n) ([0..]::[Int])
                                                                                  pm              =  PortMap (zip argns
                                                                                                              (map (ExprName . ("arg"++) . show) lids
                                                                                                               ++ efields)
                                                                                                              ++ [("res",ExprName n_call)])
                                                                              return (stmts_escr++
                                                                                      [WithAssign ematch (LHSName n)
                                                                                                  [(ExprName n_call,ExprBoolConst True)]
                                                                                                   (Just (ExprName n_ealt)),
                                                                                       Instantiate n_call (mangle gid) pm]++
                                                                                      stmts_ealt,
                                                                                      n)
                                                              Nothing   -> do sizeres             <- sizeof t
                                                                              (stmts_escr,n_escr) <- compileExp escr
                                                                              (_,efields)         <- compilePat n_escr 0 p
                                                                              n_call              <- liftM (++"_res") $ freshName (mangle gid)
                                                                              addSignal n_call (TyStdLogicVector sizeres)
                                                                              let argns           =  map (\ n -> "arg" ++ show n) ([0..]::[Int])
                                                                                  pm              =  PortMap (zip argns
                                                                                                              (map (ExprName . ("arg"++) . show) lids
                                                                                                               ++ efields)
                                                                                                              ++ [("res",ExprName n_call)])
                                                                              return (stmts_escr++[Instantiate n_call (mangle gid) pm],n_call)
                                                      _  -> failAt (ann e_) $ "compileExp: Encountered match in function position in " ++ prettyPrint e_
                  NativeVHDL _ t i -> do n           <- liftM (++"_res") $ freshName i
                                         let tres    =  last (flattenArrow t)
                                         size        <- sizeof tres
                                         addSignal n (TyStdLogicVector size)
                                         sssns       <- mapM compileExp eargs
                                         let stmts   =  concatMap fst sssns
                                             ns      =  map snd sssns
                                         addComponent i t
                                         let argns   =  map (\ n -> "arg" ++ show n) ([0..]::[Int])
                                             pm      =  PortMap (zip argns (map ExprName ns) ++ [("res",ExprName n)])
                                         return (stmts++[Instantiate n i pm],n)
                                         
  where (e:eargs) = flattenApp e_

mkDefnArch :: Defn -> CM Architecture
mkDefnArch (Defn _ n _ e) = do put ([],[],0) -- empty out the signal and component store, reset name counter
                               (stmts,nres)   <- compileExp e
                               (sigs,comps,_) <- get
                               return (Architecture (mangle n ++ "_impl") (mangle n) sigs comps (stmts++[Assign (LHSName "res") (ExprName nres)]))

compileDefn :: Defn -> CM Unit
compileDefn d = do ent  <- mkDefnEntity d
                   arch <- mkDefnArch d
                   return (Unit ent arch)

compileProgram :: C.Program -> Either AstError M.Program
compileProgram p = fst $ runIdentity $ flip runReaderT (ctors p) $ flip runStateT ([],[],0) $ runSyntaxError $
                     do units <- mapM compileDefn (defns p)
                        return (M.Program units)
