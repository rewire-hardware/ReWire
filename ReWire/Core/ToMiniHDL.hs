{-# LANGUAGE LambdaCase, FlexibleContexts #-}
{-# LANGUAGE Safe #-}
module ReWire.Core.ToMiniHDL (compileProgram) where

import ReWire.Annotation
import ReWire.Core.Syntax as C
import ReWire.Error
import ReWire.Core.Mangle
import ReWire.MiniHDL.Syntax as M
import ReWire.Pretty

import Control.Monad (zipWithM)
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.State (StateT (..), get, put, lift)
import Data.List (find, foldl')
import Data.Bits (testBit)
import Data.Maybe (fromMaybe)

type CM m = StateT ([Signal], [Component], Int) (
              ReaderT ([DataCon], [Defn]) (
                  SyntaxErrorT m
              )
          )

askCtors :: Monad m => CM m [DataCon]
askCtors = asks fst

askDefns :: Monad m => CM m [Defn]
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
apply s (TyApp an t1 t2)  = TyApp an (apply s t1) $ apply s t2
apply _ t@TyCon {}        = t
apply s t@(TyVar _ i)     = fromMaybe t $ lookup i s

tcictors :: Monad m => TyConId -> CM m [DataCon]
tcictors tci = filter isMine <$> askCtors
      where isMine :: DataCon -> Bool
            isMine (DataCon _ _ _ t) = case flattenTyApp $ last $ flattenArrow t of
                  (TyCon _ tci' : _) -> tci == tci'
                  _                  -> False

askDci :: Monad m => DataConId -> CM m DataCon
askDci dci = do
      ctors <- askCtors
      case find (\ (DataCon _ dci' _ _) -> dci == dci') ctors of
            Just ctor -> pure ctor
            Nothing   -> lift $ failNowhere $ "askDci: no info for data constructor " ++ show dci

dcitci :: Monad m => DataConId -> CM m TyConId
dcitci dci = do
      DataCon l _ _ t <- askDci dci
      case flattenTyApp (last (flattenArrow t)) of
            (TyCon _ tci:_) -> pure tci
            _               -> lift $ failAt l $ "dcitci: malformed type for data constructor "
                                    ++ show dci ++ " (does not end in application of TyCon)"

nvec :: Int -> Int -> [Bit]
nvec width n = nvec' 0 []
  where nvec' pos bits | pos >= width = bits
                       | otherwise    = nvec' (pos + 1) ((if testBit n pos then One else Zero):bits)

dciTagVector :: Monad m => DataConId -> CM m [Bit]
dciTagVector dci = do
      tci               <- dcitci dci
      ctors             <- tcictors tci
      DataCon _ _ pos _ <- askDci dci
      pure $ nvec (ceilLog2 (length ctors)) pos

dciPadVector :: Monad m => Annote -> DataConId -> C.Ty -> CM m [Bit]
dciPadVector an dci t = do
      ctor         <- askDci dci
      fieldwidth   <- ctorwidth t ctor
      tci          <- dcitci dci
      ctors        <- tcictors tci
      let tot      =  ceilLog2 (length ctors) + fieldwidth
      tysize       <- sizeof an t
      let padwidth =  tysize - tot
      pure $ nvec padwidth 0

ceilLog2 :: Int -> Int
ceilLog2 n | n < 1 = 0
ceilLog2 n = ceiling (logBase 2 (fromIntegral n :: Double))

ctorwidth :: Monad m => C.Ty -> DataCon -> CM m Int
ctorwidth t (DataCon an _ _ ct) = do
      let ts     =  flattenArrow ct
          tres   =  last ts
          targs  =  init ts
      s          <- matchTy (ann t) tres t
      let targs' =  map (apply s) targs
      sizes      <- mapM (sizeof an) targs'
      pure $ sum sizes

sizeof :: Monad m => Annote -> C.Ty -> CM m Int
sizeof an t = case th of
      TyApp {}     -> failAt an $ "ToMiniHDL: sizeof: Got TyApp after flattening (can't happen): " ++ prettyPrint t
      TyCon _ tci  -> do
            ctors      <- tcictors tci
            ctorwidths <- mapM (ctorwidth t) ctors
            pure $ ceilLog2 (length ctors) + maximum (0 : ctorwidths)
      TyVar _ _    -> failAt an $ "ToMiniHDL: sizeof: Encountered type variable: " ++ prettyPrint t
      where (th : _) = flattenTyApp t

getTyPorts :: Monad m => Annote -> C.Ty -> CM m [Port]
getTyPorts an t = do
      let ts       =  flattenArrow t
          targs    =  init ts
          tres     =  last ts
          argnames =  zipWith (\ _ x -> "arg" ++ show x) targs ([0..]::[Int])
      argsizes     <- mapM (sizeof an) targs
      ressize      <- sizeof an tres
      let argports =  zipWith (\ n x -> Port n In (TyStdLogicVector x)) argnames argsizes
          resport  =  Port "res" Out (TyStdLogicVector ressize)
      pure $ argports ++ [resport]

mkDefnEntity :: Monad m => Defn -> CM m Entity
mkDefnEntity (Defn an n t _) = Entity (mangle n) <$> getTyPorts an t

freshName :: Monad m => String -> CM m Name
freshName s = do
      (sigs, comps, ctr) <- get
      put (sigs, comps, ctr + 1)
      pure (s ++ "_" ++ show ctr)

addSignal :: Monad m => String -> M.Ty -> CM m ()
addSignal n t = do
      (sigs, comps, ctr) <- get
      put (sigs ++ [Signal n t], comps, ctr)

addComponent :: Monad m => Annote -> GId -> C.Ty -> CM m ()
addComponent an i t = do
      (sigs, comps, ctr) <- get
      case find ((== mangle i) . componentName) comps of
            Just _ -> pure ()
            Nothing -> do
                  ps <- getTyPorts an t
                  put (sigs, Component (mangle i) ps : comps, ctr)

compilePat :: Monad m => Name -> Int -> Pat -> CM m (Expr, [Expr])  -- first Expr is for whether match (std_logic); remaining Exprs are for extracted fields
compilePat nscr offset (PatCon an _ dci ps) = do
      dcitagvec        <- dciTagVector dci
      let tagw         =  length dcitagvec
      fieldwidths      <- mapM (sizeof an . typeOf) ps
      let fieldoffsets =  init $ scanl (+) (offset + tagw) fieldwidths
      rs               <- zipWithM (compilePat nscr) fieldoffsets ps
      let ematchs      =  map fst rs
          eslices      =  concatMap snd rs
          ematch       =  foldl' ExprAnd
                            (ExprIsEq
                              (ExprSlice (ExprName nscr) offset (offset + tagw - 1))
                              (ExprBitString dcitagvec))
                            ematchs
      pure (ematch, eslices)
compilePat nscr offset (PatVar an t)       = do
      size <- sizeof an t
      pure (ExprBoolConst True, [ExprSlice (ExprName nscr) offset (offset + size - 1)])

askGIdTy :: Monad m => GId -> CM m C.Ty
askGIdTy i = do
      defns <- askDefns
      case find (\ (Defn _ i' _ _) -> i == i') defns of
            Just (Defn _ _ t _) -> pure t
            Nothing             -> lift $ failNowhere $ "askGIdTy: no info for identifier " ++ show i

compileExp :: Monad m => C.Exp -> CM m ([Stmt], Name)
compileExp e_ = case e of
      App {}        -> failAt (ann e) "compileExp: Got App after flattening (can't happen)"
      Prim {}       -> failAt (ann e) $ "compileExp: Encountered unknown Prim: " ++ prettyPrint e
      GVar an t i   -> do
            n           <- (++ "_res") <$> freshName (mangle i)
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
            pure (stmts ++ [Instantiate n_inst (mangle i) pm], n)
      LVar _ _ i       -> case eargs of
            [] -> pure ([], "arg" ++ show i)
            _  -> failAt (ann e_) $ "compileExp: Encountered local variable in function position in " ++ prettyPrint e_
      Con an t i       -> do
            n           <- (++"_res") <$> freshName (mangle (deDataConId i))
            let tres    =  last (flattenArrow t)
            size        <- sizeof an tres
            addSignal n (TyStdLogicVector size)
            sssns       <- mapM compileExp eargs
            let stmts   =  concatMap fst sssns
                ns      =  map snd sssns
            tagvec      <- dciTagVector i
            padvec      <- dciPadVector an i tres
            pure (stmts ++ [Assign (LHSName n) (ExprConcat
                                                  (foldl' ExprConcat (ExprBitString tagvec) (map ExprName ns))
                                                     (ExprBitString padvec)
                                                  )],
                    n)
      Match an t escr p gid lids malt -> case eargs of
            [] -> case malt of
                  Just ealt -> do
                        n                   <- (++ "_res") <$> freshName "match"
                        sizeres             <- sizeof an (last $ flattenArrow t)
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
                        pure (stmts_escr ++
                                [WithAssign ematch (LHSName n)
                                            [(ExprName n_gid, ExprBoolConst True)]
                                             (Just (ExprName n_ealt)),
                                 Instantiate n_call (mangle gid) pm] ++
                                stmts_ealt,
                                n)
                  Nothing   -> do
                        sizeres              <- sizeof an (last $ flattenArrow t)
                        (stmts_escr, n_escr) <- compileExp escr
                        (_, efields)         <- compilePat n_escr 0 p
                        n_gid                <- (++ "_res") <$> freshName (mangle gid)
                        addSignal n_gid (TyStdLogicVector sizeres)
                        n_call               <- (++ "_call") <$> freshName (mangle gid)
                        t_gid                <- askGIdTy gid
                        addComponent an gid t_gid
                        let argns            =  map (\ n -> "arg" ++ show n) ([0..]::[Int])
                            pm               =  PortMap (zip argns
                                                        (map (ExprName . ("arg" ++) . show) lids
                                                        ++ efields)
                                                        ++ [("res", ExprName n_gid)])
                        pure (stmts_escr ++ [Instantiate n_call (mangle gid) pm], n_gid)
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
            pure (stmts ++ [Instantiate n_call i pm], n)
  where (e:eargs) = flattenApp e_

mkDefnArch :: Monad m => Defn -> CM m Architecture
mkDefnArch (Defn _ n _ e) = do
      put ([], [], 0) -- empty out the signal and component store, reset name counter
      (stmts, nres)   <- compileExp e
      (sigs, comps, _) <- get
      pure (Architecture (mangle n ++ "_impl") (mangle n) sigs comps (stmts ++ [Assign (LHSName "res") (ExprName nres)]))

compileDefn :: Monad m => Defn -> CM m Unit
compileDefn = \ case
      d | defnName d == "Main.start" -> do
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
                                    arg0size  <- sizeof an (head $ flattenArrow t_loopfun)
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
                                                       -- ^ TODO(chathhorn): suspect "max outsize resize" should be "max ressize (outsize + Rsize + statesize)"
                                        pad_for_out = ExprBitString (replicate (max 0 (ressize - outsize)) Zero)
                                        pad_for_res = ExprBitString (replicate (max 0 (outsize - ressize)) Zero)
                                    (sigs, comps, _) <- get
                                    pure (Unit
                                             (Entity "top_level" ports)
                                             (Architecture
                                                 "top_level_impl"
                                                 "top_level"
                                                 sigs
                                                 comps
                                                 [Instantiate "start_call" (mangle n_startstate)
                                                    (PortMap [("res", ExprName "start_state")]),
                                                  Instantiate "loop_call" (mangle n_loopfun)
                                                    (PortMap [("arg0", ExprSlice (ExprName "current_state") (1 + outsize) (1 + outsize + arg0size - 1)),
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
                              _ -> failAt (ann d) $ "compileDefn: definition of Main.start must have form `Main.start = unfold n m' where n and m are global IDs; got " ++ prettyPrint e
                  _ -> failAt (ann d) $ "compileDefn: Main.start has illegal type: " ++ prettyPrint t
      d -> Unit <$> mkDefnEntity d <*> mkDefnArch d

compileProgram :: Monad m => C.Program -> SyntaxErrorT m M.Program
compileProgram p = fmap fst $ flip runReaderT (ctors p, defns p) $ flip runStateT ([], [], 0) $
      M.Program <$> mapM compileDefn (defns p)

