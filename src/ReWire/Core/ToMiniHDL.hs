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
import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.State (StateT (..), get, put, lift)
import Data.List (find, foldl')
import Data.List.Split (splitOn)
import Data.Bits (testBit)
import Data.Text (Text, pack)

import TextShow (showt)

type CM m = StateT ([Signal], [Component], Int) (
              ReaderT [Defn] (
                  SyntaxErrorT m
              )
          )

askDefns :: Monad m => CM m [Defn]
askDefns = ask

nvec :: Int -> Int -> [Bit]
nvec n width = nvec' 0 []
  where nvec' pos bits | pos >= width = bits
                       | otherwise    = nvec' (pos + 1) ((if testBit n pos then One else Zero):bits)

getTyPorts :: Monad m => C.Sig -> CM m [Port]
getTyPorts (Sig _ argsizes ressize) = do
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

addComponent :: Monad m => Annote -> GId -> C.Sig -> CM m ()
addComponent _ i t = do
      (sigs, comps, ctr) <- get
      case find ((== mangle i) . componentName) comps of
            Just _ -> pure ()
            Nothing -> do
                  ps <- getTyPorts t
                  put (sigs, Component (mangle i) ps : comps, ctr)

compilePat :: Monad m => Name -> Int -> Pat -> CM m (Expr, [Expr])  -- first Expr is for whether match (std_logic); remaining Exprs are for extracted fields
compilePat nscr offset (PatCon _ _ tagValue tagSize ps) = do
      let dcitagvec    = nvec tagValue tagSize
      let tagw         = length dcitagvec
          fieldwidths  = map sizeOf ps
      let fieldoffsets = init $ scanl (+) (offset + tagw) fieldwidths
      rs              <- zipWithM (compilePat nscr) fieldoffsets ps
      let ematchs      = map fst rs
          eslices      = concatMap snd rs
          ematch       = foldl' ExprAnd (ExprIsEq (ExprSlice (ExprName nscr) offset (offset + tagw - 1)) (ExprBitString dcitagvec)) ematchs
      pure (ematch, eslices)
compilePat nscr offset (PatVar _ s)       = pure (ExprBoolConst True, [ExprSlice (ExprName nscr) offset (offset + s - 1)])

askGIdTy :: Monad m => GId -> CM m C.Sig
askGIdTy i = do
      defns <- askDefns
      case find (\ (Defn _ i' _ _) -> i == i') defns of
            Just (Defn _ _ t _) -> pure t
            Nothing             -> lift $ failNowhere $ "askGIdTy: no info for identifier " <> showt i

compileExp :: Monad m => C.Exp -> CM m ([Stmt], Name)
compileExp = \ case
      Call an (Sig _ _ sz) i args  -> do
            n          <- (<> "_res") <$> freshName (mangle i)
            n_inst     <- (<> "_call") <$> freshName (mangle i)
            addSignal n $ TyStdLogicVector sz
            sssns      <- mapM compileExp args
            let stmts   = concatMap fst sssns
                ns      = map snd sssns
            addComponent an i $ Sig an (map sizeOf args) sz
            let argns   = map (\ n -> "arg" <> showt n) ([0..] :: [Int])
                pm      = PortMap (zip argns (map ExprName ns) ++ [("res", ExprName n)])
            pure (stmts ++ [Instantiate n_inst (mangle i) pm], n)
      LVar _ _ i       -> pure ([], "arg" <> showt i)
      Con _ sz tagValue tagSize args -> do
            n          <- (<> "_res") <$> freshName (mangle $ "ctor_" <> showt tagValue <> "_" <> showt tagSize)
            addSignal n $ TyStdLogicVector sz
            sssns      <- mapM compileExp args
            let stmts   = concatMap fst sssns
                ns      = map snd sssns
                tagvec  = nvec tagValue tagSize
                padvec  = nvec 0 $ sz - tagSize - sum (map sizeOf args)
            pure (stmts ++ [Assign (LHSName n) $ simplifyConcat (ExprConcat
                                                  (foldl' ExprConcat (ExprBitString tagvec) (map ExprName ns))
                                                     (ExprBitString padvec)
                                                  )],
                    n)

      Match an sz escr p gid malt -> case malt of
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
                      pm              =  PortMap (zip argns efields ++ [("res", ExprName n_gid)])
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
                      pm               =  PortMap (zip argns efields ++ [("res", ExprName n_gid)])
                  pure (stmts_escr ++ [Instantiate n_call (mangle gid) pm], n_gid)
      NativeVHDL _ sz i args -> do
            n           <- (<> "_res") <$> freshName i
            addSignal n $ TyStdLogicVector sz
            sssns       <- mapM compileExp args
            let stmts   =  concatMap fst sssns
                ns      =  map snd sssns
            pure (stmts ++ [Assign (LHSName n) (ExprFunCall i (map ExprName ns))], n)
      NativeVHDLComponent an sz i args -> do
            n           <- (<> "_res") <$> freshName i
            n_call      <- (<> "_call") <$> freshName i
            addSignal n $ TyStdLogicVector sz
            sssns       <- mapM compileExp args
            let stmts   =  concatMap fst sssns
                ns      =  map snd sssns
            addComponent an i $ Sig an (map sizeOf args) sz
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
compileStartDefn flags (StartDefn an insize outsize _ (n_loopfun, t_loopfun@(Sig _ (arg0size:_) _)) (n_startstate, t_startstate)) = do
      put ([], [], 0) -- empty out signal and component store, reset name counter
      let statesize = sizeOf t_startstate
      addComponent an n_startstate t_startstate
      addComponent an n_loopfun t_loopfun
      addSignal "start_state"        $ TyStdLogicVector statesize
      addSignal "loop_out"           $ TyStdLogicVector statesize
      addSignal "current_state"      $ TyRegister "clk" statesize
      addSignal "done_or_next_state" $ TyStdLogicVector statesize
      addSignal "next_state"         $ TyStdLogicVector statesize
      let ports       = [Port "clk"  In TyClock,
                         Port rst    In TyStdLogic,
                         Port "inp"  In (TyStdLogicVector insize),
                         Port "outp" Out (TyStdLogicVector outsize)]
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
                     Assign (LHSName "outp") (ExprSlice (ExprName "current_state") 1 outsize)
                   ]
               )
             )
      where rstSignal :: Bit
            rstSignal = if FlagInvertReset `elem` flags then Zero else One

            rst :: Text
            rst = if FlagInvertReset `elem` flags then "rst_n" else "rst"

compileDefn :: Monad m => [Flag] -> Defn -> CM m Unit
compileDefn flags d = Unit (uses flags) <$> mkDefnEntity d <*> mkDefnArch d

compileProgram :: Monad m => [Flag] -> C.Program -> SyntaxErrorT m M.Program
compileProgram flags p = fmap fst $ flip runReaderT (defns p) $ flip runStateT ([], [], 0) $ M.Program <$> ((:) <$> compileStartDefn flags (start p) <*> mapM (compileDefn flags) (defns p))

uses :: [Flag] -> [Text]
uses flags = "ieee.std_logic_1164.all" : concatMap getUses flags
      where getUses :: Flag -> [Text]
            getUses (FlagPkgs pkgs) = map pack $ splitOn "," pkgs
            getUses _               = []

