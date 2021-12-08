{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.ToMiniHDL (compileProgram) where

import ReWire.Annotation
import ReWire.Core.Syntax as C
import ReWire.Error
import ReWire.Core.Mangle
import ReWire.MiniHDL.Syntax as M
import ReWire.Pretty
import ReWire.Flags (Flag (..))

import Control.Monad (zipWithM)
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.State (StateT (..), get, put, lift)
import Data.List (find, foldl')
import Data.List.Split (splitOn)
import Data.Bits (testBit)
import Data.Text (Text, pack)

import TextShow (showt)

type CM m = StateT ([Signal], [Component], Int) (
              ReaderT ([DataCon], [Defn]) (
                  SyntaxErrorT m
              )
          )

askCtors :: Monad m => CM m [DataCon]
askCtors = asks fst

askDefns :: Monad m => CM m [Defn]
askDefns = asks snd

askDci :: Monad m => DataConId -> CM m DataCon
askDci dci = do
      ctors <- askCtors
      case find (\ (DataCon _ dci' _ _ _) -> dci == dci') ctors of
            Just ctor -> pure ctor
            Nothing   -> pure $ DataCon noAnn dci 0 0 (Ty noAnn [] 0) -- TODO(chathhorn): handle this better.

nvec :: Int -> Int -> [Bit]
nvec width n = nvec' 0 []
  where nvec' pos bits | pos >= width = bits
                       | otherwise    = nvec' (pos + 1) ((if testBit n pos then One else Zero):bits)

dciTagVector :: Monad m => DataConId -> CM m [Bit]
dciTagVector dci = do
      DataCon _ _ pos nctors _ <- askDci dci
      pure $ nvec (ceilLog2 nctors) pos

dciPadVector :: Monad m => DataConId -> Int -> Int -> CM m [Bit]
dciPadVector dci fieldwidth tysize = do
      DataCon _ _ _ nctors _ <- askDci dci
      let tot      =  ceilLog2 nctors + fieldwidth
          padwidth =  tysize - tot
      pure $ nvec padwidth 0

ceilLog2 :: Int -> Int
ceilLog2 n | n < 1 = 0
ceilLog2 n = ceiling (logBase 2 (fromIntegral n :: Double))

getTyPorts :: Monad m => C.Ty -> CM m [Port]
getTyPorts (Ty _ argsizes ressize) = do
      let argnames = zipWith (\ _ x -> "arg" <> showt x) argsizes ([0..]::[Int])
          argports = zipWith (\ n x -> Port n In (TyStdLogicVector x)) argnames argsizes
          resport  = Port "res" Out (TyStdLogicVector ressize)
      pure $ argports ++ [resport]

mkDefnEntity :: Monad m => Defn -> CM m Entity
mkDefnEntity (Defn _ n t _) = Entity (mangle n) <$> getTyPorts t

freshName :: Monad m => Text -> CM m Name
freshName s = do
      (sigs, comps, ctr) <- get
      put (sigs, comps, ctr + 1)
      pure (s <> "_" <> showt ctr)

addSignal :: Monad m => Text -> M.Ty -> CM m ()
addSignal n t = do
      (sigs, comps, ctr) <- get
      put (sigs ++ [Signal n t], comps, ctr)

addComponent :: Monad m => Annote -> GId -> C.Ty -> CM m ()
addComponent _ i t = do
      (sigs, comps, ctr) <- get
      case find ((== mangle i) . componentName) comps of
            Just _ -> pure ()
            Nothing -> do
                  ps <- getTyPorts t
                  put (sigs, Component (mangle i) ps : comps, ctr)

compilePat :: Monad m => Name -> Int -> Pat -> CM m (Expr, [Expr])  -- first Expr is for whether match (std_logic); remaining Exprs are for extracted fields
compilePat nscr offset (PatCon _ _ dci ps) = do
      dcitagvec        <- dciTagVector dci
      let tagw         =  length dcitagvec
          fieldwidths  =  map (sizeOf . typeOf) ps
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
compilePat nscr offset (PatVar _ t)       = pure (ExprBoolConst True, [ExprSlice (ExprName nscr) offset (offset + sizeOf t - 1)])

askGIdTy :: Monad m => GId -> CM m C.Ty
askGIdTy i = do
      defns <- askDefns
      case find (\ (Defn _ i' _ _) -> i == i') defns of
            Just (Defn _ _ t _) -> pure t
            Nothing             -> lift $ failNowhere $ "askGIdTy: no info for identifier " <> showt i

compileExp :: Monad m => C.Exp -> CM m ([Stmt], Name)
compileExp = \ case
      Call an (Ty an' _ sz) i args  -> do
            n           <- (<> "_res") <$> freshName (mangle i)
            n_inst      <- (<> "_call") <$> freshName (mangle i)
            addSignal n $ TyStdLogicVector sz
            sssns       <- mapM compileExp args
            let stmts   =  concatMap fst sssns
                ns      =  map snd sssns
            addComponent an i $ Ty an' (map (sizeOf . typeOf) args) sz
            let argns   =  map (\ n -> "arg" <> showt n) ([0..] :: [Int])
                pm      =  PortMap (zip argns (map ExprName ns) ++ [("res", ExprName n)])
            pure (stmts ++ [Instantiate n_inst (mangle i) pm], n)
      LVar _ _ i       -> pure ([], "arg" <> showt i)
      Con _ (Ty an' _ sz) w i args  -> do
            n           <- (<> "_res") <$> freshName (mangle (deDataConId i))
            addSignal n $ TyStdLogicVector sz
            sssns       <- mapM compileExp args
            let stmts   =  concatMap fst sssns
                ns      =  map snd sssns
            tagvec      <- dciTagVector i
            padvec      <- dciPadVector i w sz
            pure (stmts ++ [Assign (LHSName n) $ simplifyConcat (ExprConcat
                                                  (foldl' ExprConcat (ExprBitString tagvec) (map ExprName ns))
                                                     (ExprBitString padvec)
                                                  )],
                    n)
      Match an (Ty _ _ sz) escr p gid lids malt -> case malt of
            Just ealt -> do
                  n                    <- (<> "_res") <$> freshName "match"
                  addSignal n $ TyStdLogicVector sz
                  (stmts_escr, n_escr) <- compileExp escr
                  (ematch, efields)    <- compilePat n_escr 0 p
                  n_gid                <- (<> "_res") <$> freshName (mangle gid)
                  addSignal n_gid $ TyStdLogicVector sz
                  n_call               <- (<> "_call") <$> freshName (mangle gid)
                  (stmts_ealt, n_ealt) <- compileExp ealt
                  t_gid                <- askGIdTy gid
                  addComponent an gid t_gid
                  let argns           =  map (\ n -> "arg" <> showt n) ([0..]::[Int])
                      pm              =  PortMap (zip argns
                                                  (map (ExprName . ("arg" <>) . showt) lids
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
                  (stmts_escr, n_escr) <- compileExp escr
                  (_, efields)         <- compilePat n_escr 0 p
                  n_gid                <- (<> "_res") <$> freshName (mangle gid)
                  addSignal n_gid $ TyStdLogicVector sz
                  n_call               <- (<> "_call") <$> freshName (mangle gid)
                  t_gid                <- askGIdTy gid
                  addComponent an gid t_gid
                  let argns            =  map (\ n -> "arg" <> showt n) ([0..]::[Int])
                      pm               =  PortMap (zip argns
                                                  (map (ExprName . ("arg" <>) . showt) lids
                                                  ++ efields)
                                                  ++ [("res", ExprName n_gid)])
                  pure (stmts_escr ++ [Instantiate n_call (mangle gid) pm], n_gid)
      NativeVHDL _ (Ty _ _ sz) i args -> do
            n           <- (<> "_res") <$> freshName i
            addSignal n $ TyStdLogicVector sz
            sssns       <- mapM compileExp args
            let stmts   =  concatMap fst sssns
                ns      =  map snd sssns
            pure (stmts ++ [Assign (LHSName n) (ExprFunCall i (map ExprName ns))], n)
      NativeVHDLComponent an (Ty an' _ sz) i args -> do
            n           <- (<> "_res") <$> freshName i
            n_call      <- (<> "_call") <$> freshName i
            addSignal n $ TyStdLogicVector sz
            sssns       <- mapM compileExp args
            let stmts   =  concatMap fst sssns
                ns      =  map snd sssns
            addComponent an i $ Ty an' (map (sizeOf . typeOf) args) sz
            let argns   =  map (\ n -> "arg" <> showt n) ([0..]::[Int])
                pm      =  PortMap (zip argns (map ExprName ns) ++ [("res", ExprName n)])
            pure (stmts ++ [Instantiate n_call i pm], n)

mkDefnArch :: Monad m => Defn -> CM m Architecture
mkDefnArch (Defn _ n _ e) = do
      put ([], [], 0) -- empty out the signal and component store, reset name counter
      (stmts, nres)    <- compileExp e
      (sigs, comps, _) <- get
      pure $ Architecture (mangle n <> "_impl") (mangle n) sigs comps (stmts ++ [Assign (LHSName "res") (ExprName nres)])

compileStartDefn :: Monad m => [Flag] -> StartDefn -> CM m Unit
compileStartDefn flags (StartDefn an t_in t_out t_res e) = case e of
      Call an _ "unfold" [Call _ t_loopfun@(Ty _ (arg0size:_) _) n_loopfun [], Call _ t_startstate n_startstate []] -> do
            put ([], [], 0) -- empty out signal and component store, reset name counter
            let insize    = sizeOf t_in
                outsize   = sizeOf t_out
                statesize = sizeOf t_startstate
                ressize   = sizeOf t_res
            addComponent an n_startstate t_startstate
            addComponent an n_loopfun t_loopfun
            addSignal "start_state" (TyStdLogicVector statesize)
            addSignal "loop_out" (TyStdLogicVector statesize)
            addSignal "current_state" (TyRegister "clk" statesize)
            addSignal "done_or_next_state" (TyStdLogicVector statesize)
            addSignal "next_state" (TyStdLogicVector statesize)
            let ports       = [Port "clk"  In TyClock,
                               Port rst    In TyStdLogic,
                               Port "inp"  In (TyStdLogicVector insize),
                               -- Port "outp" Out (TyStdLogicVector (1 + max outsize ressize))]
                               Port "outp" Out (TyStdLogicVector outsize)]
                -- pad_for_out = ExprBitString (replicate (max 0 (ressize - outsize)) Zero)
                -- pad_for_res = ExprBitString (replicate (max 0 (outsize - ressize)) Zero)
            (sigs, comps, _) <- get
            pure (Unit (uses flags)
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
                          WithAssign (ExprName rst) (LHSName "next_state")
                           [(ExprName "start_state", ExprBit rstSignal)]
                           (Just (ExprName "done_or_next_state")),
                          WithAssign (ExprSlice (ExprName "current_state") 0 0) (LHSName "done_or_next_state")
                           [(ExprName "loop_out", ExprBitString [One])]
                           (Just (ExprName "current_state")),
                          ClkProcess "clk"
                           [Assign (LHSName "current_state") (ExprName "next_state")],
                          -- WithAssign (ExprSlice (ExprName "current_state") 0 0) (LHSName "outp")
                          --  [(ExprConcat (ExprBitString [One]) (ExprConcat (ExprSlice (ExprName "current_state") 1 outsize) pad_for_out), ExprBitString [One])]
                          --  (Just (ExprConcat (ExprBitString [Zero]) (ExprConcat (ExprSlice (ExprName "current_state") 1 ressize) pad_for_res)))
                           Assign (LHSName "outp") (ExprSlice (ExprName "current_state") 1 outsize)
                          --  [(ExprConcat (ExprBitString [One]) (ExprConcat (ExprSlice (ExprName "current_state") 1 outsize) pad_for_out), ExprBitString [One])]
                          --  (Just (ExprConcat (ExprBitString [Zero]) (ExprConcat (ExprSlice (ExprName "current_state") 1 ressize) pad_for_res)))
                         ]
                     )
                   )
      _ -> failAt an $ "compileStartDefn: definition of Main.start must have form `Main.start = unfold n m' where n and m are global IDs; got " <> prettyPrint e
      where rstSignal :: Bit
            rstSignal = if FlagInvertReset `elem` flags then Zero else One

            rst :: Text
            rst = if FlagInvertReset `elem` flags then "rst_n" else "rst"

compileDefn :: Monad m => [Flag] -> Defn -> CM m Unit
compileDefn flags d = Unit (uses flags) <$> mkDefnEntity d <*> mkDefnArch d

compileProgram :: Monad m => [Flag] -> C.Program -> SyntaxErrorT m M.Program
compileProgram flags p = fmap fst $ flip runReaderT (ctors p, defns p) $ flip runStateT ([], [], 0) $ M.Program <$> ((:) <$> compileStartDefn flags (start p) <*> mapM (compileDefn flags) (defns p))

uses :: [Flag] -> [Text]
uses flags = "ieee.std_logic_1164.all" : concatMap getUses flags
      where getUses :: Flag -> [Text]
            getUses (FlagPkgs pkgs) = map pack $ splitOn "," pkgs
            getUses _               = []



