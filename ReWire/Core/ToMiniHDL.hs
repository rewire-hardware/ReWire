{-# LANGUAGE LambdaCase, FlexibleContexts #-}
{-# LANGUAGE Safe #-}
module ReWire.Core.ToMiniHDL where

import ReWire.Annotation
import ReWire.Core.Syntax as C
import ReWire.Error
import ReWire.Core.Mangle
import ReWire.MiniHDL.Syntax as M
import ReWire.Pretty

import Control.Monad (zipWithM)
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.State (StateT (..), get, put, lift)
import Control.Monad.Identity (Identity (..))
import Data.List (find)
import Data.Bits (testBit)
import Data.Maybe (fromMaybe)

type CM = StateT ([Signal], [Component], Int) (
              ReaderT ([DataCon], [Defn]) (
                  SyntaxErrorT Identity
              )
          )

askCtors :: CM [DataCon]
askCtors = asks fst

askDefns :: CM [Defn]
askDefns = asks snd

type TySub = [(TyId, C.Ty)]

matchTy :: MonadError AstError m => Annote -> C.Ty -> C.Ty -> m TySub
matchTy an (TyApp _ t1 t2) (TyApp _ t1' t2') = do
      s1 <- matchTy an t1 t1'
      s2 <- matchTy an t2 t2'
      merge an s1 s2
matchTy _ (TyVar _ v) t                      = pure [(v, t)]
matchTy _ (TyCon _ tci) (TyCon _ tci')
      | tci == tci'                          = pure []
matchTy an t t'                              = failAt an
      $ "ToMiniHDL: sizeof: matchTy: can't match "
      ++ prettyPrint t ++ " with " ++ prettyPrint t'

merge :: MonadError AstError m => Annote -> TySub -> TySub -> m TySub
merge an s'  = \ case
      []          -> pure s'
      (v, t) : s -> case lookup v s' of
            Nothing           -> ((v, t) :) <$> merge an s' s
            Just t' | t == t' -> merge an s' s
            Just t'           -> failAt an
                  $ "sizeof: merge: inconsistent assignment of tyvar " ++ v
                  ++ ": " ++ prettyPrint t ++ " vs. " ++ prettyPrint t'

apply :: TySub -> C.Ty -> C.Ty
apply s (TyApp an t1 t2)  = TyApp an (apply s t1) (apply s t2)
apply _ t@TyCon {}        = t
apply s t@(TyVar _ i)     = fromMaybe t $ lookup i s

tcictors :: TyConId -> CM [DataCon]
tcictors tci = filter isMine <$> askCtors
      where isMine :: DataCon -> Bool
            isMine (DataCon _ _ _ t) = case flattenTyApp $ last $ flattenArrow t of
                  (TyCon _ tci' : _) -> tci == tci'
                  _                  -> False

askDci :: DataConId -> CM DataCon
askDci dci = do ctors <- askCtors
                case find (\ (DataCon _ dci' _ _) -> dci == dci') ctors of
                  Just ctor -> return ctor
                  Nothing   -> lift $ failNowhere $ "askDci: no info for data constructor " ++ show dci

dcitci :: DataConId -> CM TyConId
dcitci dci = do DataCon l _ _ t <- askDci dci
                case flattenTyApp (last (flattenArrow t)) of
                  (TyCon _ tci:_) -> return tci
                  _               -> lift $ failAt l $ "dcitci: malformed type for data constructor "
                                          ++ show dci ++ " (does not end in application of TyCon)"

nvec :: Int -> Int -> [Bit]
nvec width n = nvec' 0 []
  where nvec' pos bits | pos >= width = bits
                       | otherwise    = nvec' (pos + 1) ((if testBit n pos then One else Zero):bits)

dciTagVector :: DataConId -> CM [Bit]
dciTagVector dci = do
      tci               <- dcitci dci
      ctors             <- tcictors tci
      DataCon _ _ pos _ <- askDci dci
      return $ nvec (ceilLog2 (length ctors)) pos

dciPadVector :: Annote -> DataConId -> C.Ty -> CM [Bit]
dciPadVector an dci t = do
      ctor         <- askDci dci
      fieldwidth   <- ctorwidth t ctor
      tci          <- dcitci dci
      ctors        <- tcictors tci
      let tot      =  ceilLog2 (length ctors) + fieldwidth
      tysize       <- sizeof an t
      let padwidth =  tysize - tot
      return $ nvec padwidth 0

ceilLog2 :: Int -> Int
ceilLog2 n | n < 1 = 0
ceilLog2 n = ceiling (logBase 2 (fromIntegral n :: Double))

ctorwidth :: C.Ty -> DataCon -> CM Int
ctorwidth t (DataCon an _ _ ct) = do
      let ts     =  flattenArrow ct
          tres   =  last ts
          targs  =  init ts
      s          <- matchTy (ann t) tres t
      let targs' =  map (apply s) targs
      sizes      <- mapM (sizeof an) targs'
      return $ sum sizes

sizeof :: Annote -> C.Ty -> CM Int
sizeof an t = case th of
      TyApp {}     -> failAt an $ "ToMiniHDL: sizeof: Got TyApp after flattening (can't happen): " ++ prettyPrint t
      TyCon _ tci  -> do
            ctors      <- tcictors tci
            ctorwidths <- mapM (ctorwidth t) ctors
            return $ ceilLog2 (length ctors) + maximum (0 : ctorwidths)
      TyVar _ _    -> failAt an $ "ToMiniHDL: sizeof: Encountered type variable: " ++ prettyPrint t
      where (th : _) = flattenTyApp t

getTyPorts :: Annote -> C.Ty -> CM [Port]
getTyPorts an t = do let ts       =  flattenArrow t
                         targs    =  init ts
                         tres     =  last ts
                         argnames =  zipWith (\ _ x -> "arg" ++ show x) targs ([0..]::[Int])
                     argsizes     <- mapM (sizeof an) targs
                     ressize      <- sizeof an tres
                     let argports =  zipWith (\ n x -> Port n In (TyStdLogicVector x)) argnames argsizes
                         resport  =  Port "res" Out (TyStdLogicVector ressize)
                     return (argports ++ [resport])

mkDefnEntity :: Defn -> CM Entity
mkDefnEntity (Defn an n t _) = do
      ps <- getTyPorts an t
      return $ Entity (mangle n) ps

freshName :: String -> CM Name
freshName s = do (sigs, comps, ctr) <- get
                 put (sigs, comps, ctr + 1)
                 return (s ++ "_" ++ show ctr)

addSignal :: String -> M.Ty -> CM ()
addSignal n t = do (sigs, comps, ctr) <- get
                   put (sigs ++ [Signal n t], comps, ctr)

addComponent :: Annote -> GId -> C.Ty -> CM ()
addComponent an i t = do
      (sigs, comps, ctr) <- get
      case find ((== mangle i) . componentName) comps of
            Just _ -> return ()
            Nothing -> do
                  ps <- getTyPorts an t
                  put (sigs, Component (mangle i) ps : comps, ctr)

compilePat :: Name -> Int -> Pat -> CM (Expr, [Expr])  -- first Expr is for whether match (std_logic); remaining Exprs are for extracted fields
compilePat nscr offset (PatCon an _ dci ps) = do
      dcitagvec        <- dciTagVector dci
      let tagw         =  length dcitagvec
      fieldwidths      <- mapM (sizeof an . typeOf) ps
      let fieldoffsets =  init $ scanl (+) (offset + tagw) fieldwidths
      rs               <- zipWithM (compilePat nscr) fieldoffsets ps
      let ematchs      =  map fst rs
          eslices      =  concatMap snd rs
          ematch       =  foldl ExprAnd
                            (ExprIsEq
                              (ExprSlice (ExprName nscr) offset (offset + tagw - 1))
                              (ExprBitString dcitagvec))
                            ematchs
      return (ematch, eslices)
compilePat nscr offset (PatVar an t)       = do
      size <- sizeof an t
      return (ExprBoolConst True, [ExprSlice (ExprName nscr) offset (offset + size - 1)])

askGIdTy :: GId -> CM C.Ty
askGIdTy i = do defns <- askDefns
                case find (\ (Defn _ i' _ _) -> i == i') defns of
                  Just (Defn _ _ t _) -> return t
                  Nothing             -> lift $ failNowhere $ "askGIdTy: no info for identifier " ++ show i

compileExp :: C.Exp -> CM ([Stmt], Name)
compileExp e_ = case e of
      App {}        -> failAt (ann e) "compileExp: Got App after flattening (can't happen)"
      Prim {}       -> failAt (ann e) $ "compileExp: Encountered unknown Prim: " ++ prettyPrint e
      GVar an t i   -> do n           <- (++ "_res") <$> freshName (mangle i)
                          n_inst      <- (++ "_call") <$> freshName (mangle i)
                          let tres    =  last (flattenArrow t)
                          size        <- sizeof an tres
                          addSignal n (TyStdLogicVector size)
                          sssns       <- mapM compileExp eargs
                          let stmts   =  concatMap fst sssns
                              ns      =  map snd sssns
                          addComponent an i t
                          let argns   =  map (\ n -> "arg" ++ show n) ([0..] :: [Int])
                              pm      =  PortMap (zip argns (map ExprName ns) ++ [("res", ExprName n)])
                          return (stmts ++ [Instantiate n_inst (mangle i) pm], n)
      LVar _ _ i       -> case eargs of
                            [] -> return ([], "arg" ++ show i)
                            _  -> failAt (ann e_) $ "compileExp: Encountered local variable in function position in " ++ prettyPrint e_
      Con an t i       -> do n           <- (++"_res") <$> freshName (mangle (deDataConId i))
                             let tres    =  last (flattenArrow t)
                             size        <- sizeof an tres
                             addSignal n (TyStdLogicVector size)
                             sssns       <- mapM compileExp eargs
                             let stmts   =  concatMap fst sssns
                                 ns      =  map snd sssns
                             tagvec      <- dciTagVector i
                             padvec      <- dciPadVector an i tres
                             return (stmts ++ [Assign (LHSName n) (ExprConcat
                                                                   (foldl ExprConcat (ExprBitString tagvec) (map ExprName ns))
                                                                      (ExprBitString padvec)
                                                                   )],
                                     n)
      Match an t escr p gid lids malt -> case eargs of
            [] -> case malt of
                  Just ealt -> do
                        n                   <- (++ "_res") <$> freshName "match"
                        sizeres             <- sizeof an t
                        addSignal n (TyStdLogicVector sizeres)
                        (stmts_escr, n_escr) <- compileExp escr
                        (ematch, efields)    <- compilePat n_escr 0 p
                        n_gid               <- (++ "_res") <$> freshName (mangle gid)
                        addSignal n_gid (TyStdLogicVector sizeres)
                        n_call              <- (++ "_call") <$> freshName (mangle gid)
                        (stmts_ealt, n_ealt) <- compileExp ealt
                        t_gid               <- askGIdTy gid
                        addComponent an gid t_gid
                        let argns           =  map (\ n -> "arg" ++ show n) ([0..]::[Int])
                            pm              =  PortMap (zip argns
                                                        (map (ExprName . ("arg" ++) . show) lids
                                                         ++ efields)
                                                        ++ [("res", ExprName n_gid)])
                        return (stmts_escr ++
                                [WithAssign ematch (LHSName n)
                                            [(ExprName n_gid, ExprBoolConst True)]
                                             (Just (ExprName n_ealt)),
                                 Instantiate n_call (mangle gid) pm] ++
                                stmts_ealt,
                                n)
                  Nothing   -> do
                        sizeres             <- sizeof an t
                        (stmts_escr, n_escr) <- compileExp escr
                        (_, efields)         <- compilePat n_escr 0 p
                        n_gid               <- (++ "_res") <$> freshName (mangle gid)
                        addSignal n_gid (TyStdLogicVector sizeres)
                        n_call              <- (++ "_call") <$> freshName (mangle gid)
                        t_gid               <- askGIdTy gid
                        addComponent an gid t_gid
                        let argns           =  map (\ n -> "arg" ++ show n) ([0..]::[Int])
                            pm              =  PortMap (zip argns
                                                        (map (ExprName . ("arg" ++) . show) lids
                                                        ++ efields)
                                                        ++ [("res", ExprName n_gid)])
                        return (stmts_escr ++ [Instantiate n_call (mangle gid) pm], n_gid)
            _  -> failAt (ann e_) $ "compileExp: Encountered match in function position in " ++ prettyPrint e_
      NativeVHDL an t i -> do
            n           <- (++ "_res") <$> freshName i
            n_call      <- (++ "_call") <$> freshName i
            let tres    =  last (flattenArrow t)
            size        <- sizeof an tres
            addSignal n (TyStdLogicVector size)
            sssns       <- mapM compileExp eargs
            let stmts   =  concatMap fst sssns
                ns      =  map snd sssns
            addComponent an i t
            let argns   =  map (\ n -> "arg" ++ show n) ([0..]::[Int])
                pm      =  PortMap (zip argns (map ExprName ns) ++ [("res", ExprName n)])
            return (stmts ++ [Instantiate n_call i pm], n)
  where (e:eargs) = flattenApp e_

mkDefnArch :: Defn -> CM Architecture
mkDefnArch (Defn _ n _ e) = do
      put ([], [], 0) -- empty out the signal and component store, reset name counter
      (stmts, nres)   <- compileExp e
      (sigs, comps, _) <- get
      return (Architecture (mangle n ++ "_impl") (mangle n) sigs comps (stmts ++ [Assign (LHSName "res") (ExprName nres)]))

compileDefn :: Defn -> CM Unit
compileDefn d | defnName d == "Main.start" = do
                  let t = defnTy d
                      e = defnBody d
                  case t of
                    TyApp _ (TyApp _ (TyApp _ (TyApp _ (TyCon _ (TyConId "ReT")) t_in) t_out) (TyCon _ (TyConId "I"))) t_res ->
                      case e of
                        App an (App _ (Prim _ _ "unfold") (GVar _ t_loopfun n_loopfun)) (GVar _ t_startstate n_startstate) -> do
                          put ([], [], 0) -- empty out signal and component store, reset name counter
                          insize    <- sizeof an t_in
                          outsize   <- sizeof an t_out
                          statesize <- sizeof an t_startstate
                          ressize   <- sizeof an t_res
                          addComponent an n_startstate t_startstate
                          addComponent an n_loopfun t_loopfun
                          addSignal "start_state" (TyStdLogicVector statesize)
                          addSignal "loop_out" (TyStdLogicVector statesize)
                          addSignal "current_state" (TyStdLogicVector statesize)
                          addSignal "done_or_next_state" (TyStdLogicVector statesize)
                          addSignal "next_state" (TyStdLogicVector statesize)
                          let ports       = [Port "clk" In TyStdLogic,
                                             Port "rst" In TyStdLogic,
                                             Port "inp" In (TyStdLogicVector insize),
                                             Port "outp" Out (TyStdLogicVector (1 + max outsize ressize))]
                              pad_for_out = ExprBitString (replicate (max 0 (ressize - outsize)) Zero)
                              pad_for_res = ExprBitString (replicate (max 0 (outsize - ressize)) Zero)
                          (sigs, comps, _) <- get
                          return (Unit
                                   (Entity "top_level" ports)
                                   (Architecture
                                       "top_level_impl"
                                       "top_level"
                                       sigs
                                       comps
                                       [Instantiate "start_call" (mangle n_startstate)
                                          (PortMap [("res", ExprName "start_state")]),
                                        Instantiate "loop_call" (mangle n_loopfun)
                                          (PortMap [("arg0", ExprSlice (ExprName "current_state") (outsize + 1) (statesize - 1)),
                                                    ("arg1", ExprName "inp"),
                                                    ("res", ExprName "loop_out")]),
                                        WithAssign (ExprName "rst") (LHSName "next_state")
                                         [(ExprName "start_state", ExprBit One)]
                                         (Just (ExprName "done_or_next_state")),
                                        WithAssign (ExprSlice (ExprName "current_state") 0 0) (LHSName "done_or_next_state")
                                         [(ExprName "loop_out", ExprBitString [One])]
                                         (Just (ExprName "current_state")),
                                        ClkProcess "clk"
                                         [Assign (LHSName "current_state") (ExprName "next_state")],
                                        WithAssign (ExprSlice (ExprName "current_state") 0 0) (LHSName "outp")
                                         [(ExprConcat (ExprBitString [One]) (ExprConcat (ExprSlice (ExprName "current_state") 1 outsize) pad_for_out), ExprBitString [One])]
                                         (Just (ExprConcat (ExprBitString [Zero]) (ExprConcat (ExprSlice (ExprName "current_state") 1 ressize) pad_for_res)))
                                       ]
                                   )
                                 )
                        _ ->
                          failAt (ann d) $ "compileDefn: definition of Main.start must have form `Main.start = unfold n m' where n and m are global IDs; got " ++ prettyPrint e
                    _ ->
                      failAt (ann d) $ "compileDefn: Main.start has illegal type: " ++ prettyPrint t
              | otherwise                  = do
                  ent  <- mkDefnEntity d
                  arch <- mkDefnArch d
                  return (Unit ent arch)

compileProgram :: C.Program -> Either AstError M.Program
compileProgram p = fmap fst $ runIdentity $ runSyntaxError $ flip runReaderT (ctors p, defns p) $ flip runStateT ([], [], 0) $
                     do
                        units <- mapM compileDefn (defns p)
                        return (M.Program units)

proj :: Defn -> GId
proj (Defn _ gid _ _) = gid
