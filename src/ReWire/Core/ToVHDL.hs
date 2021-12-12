{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.ToVHDL (compileProgram) where

import ReWire.Annotation
import ReWire.Core.Syntax as C
import ReWire.Error
import ReWire.Core.Mangle
import ReWire.VHDL.Syntax as M
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
                       | otherwise    = nvec' (pos + 1) $ (if testBit n pos then One else Zero) : bits

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
compilePat nscr offset = \ case
      PatVar _ s       -> pure (ExprBoolConst True, [ExprSlice (ExprName nscr) offset (offset + s - 1)])
      PatWildCard _ _  -> pure (ExprBoolConst True, [])
      PatLit _ tagSize tagValue -> do
            let tag    = nvec tagValue tagSize
                ematch = ExprIsEq (ExprSlice (ExprName nscr) offset (offset + length tag - 1)) $ ExprBitString tag
            pure (ematch, [])

askGIdTy :: Monad m => GId -> CM m C.Sig
askGIdTy i = do
      defns <- askDefns
      case find (\ (Defn _ i' _ _) -> i == i') defns of
            Just (Defn _ _ t _) -> pure t
            Nothing             -> lift $ failNowhere $ "askGIdTy: no info for identifier " <> showt i

compileExps :: Monad m => [C.Exp] -> CM m ([Stmt], Name)
compileExps es = do
      n          <- (<> "_res") <$> freshName (mangle $ "slice")
      addSignal n $ TyStdLogicVector $ sum $ map sizeOf es
      sssns      <- mapM compileExp es
      let stmts   = concatMap fst sssns
          ns      = map snd sssns
      pure  ( stmts ++
                  [ Assign (LHSName n)
                        $ simplifyConcat
                        $ foldl' ExprConcat (ExprBitString [])
                        $ map ExprName ns
                  ]
            , n
            )


compileExp :: Monad m => C.Exp -> CM m ([Stmt], Name)
compileExp = \ case
      LVar _ _ i       -> pure ([], "arg" <> showt i)
      Lit _ litSize litValue -> do
            n          <- (<> "_res") <$> freshName (mangle $ "lit_" <> showt litValue <> "_" <> showt litSize)
            addSignal n $ TyStdLogicVector litSize
            let litVec  = nvec litValue litSize
            pure  ( [ Assign (LHSName n) $ ExprBitString litVec ]
                  , n
                  )
      Match an sz gid escr ps [] -> do
            (stmts_escr, n_escr) <- compileExps escr

            let fieldwidths       = map sizeOf ps
                fieldoffsets      = init $ scanl (+) 0 fieldwidths
            efields              <- concatMap snd <$> zipWithM (compilePat n_escr) fieldoffsets ps

            n_gid                <- (<> "_res") <$> freshName (mangle gid)
            addSignal n_gid $ TyStdLogicVector sz
            n_call               <- (<> "_call") <$> freshName (mangle gid)
            t_gid                <- askGIdTy gid
            addComponent an gid t_gid
            let argns            =  map (\ n -> "arg" <> showt n) ([0..]::[Int])
                pm               =  PortMap (zip argns efields ++ [("res", ExprName n_gid)])
            pure (stmts_escr ++ [Instantiate n_call (mangle gid) pm], n_gid)
      Match an sz gid escr ps ealt -> do
            n                    <- (<> "_res") <$> freshName "match"
            addSignal n $ TyStdLogicVector sz
            (stmts_escr, n_escr) <- compileExps escr

            let fieldwidths       = map sizeOf ps
                fieldoffsets      = init $ scanl (+) 0 fieldwidths
            rs                   <- zipWithM (compilePat n_escr) fieldoffsets ps
            let ematch            = foldl' ExprAnd (ExprBoolConst True) $ map fst rs
                efields           = concatMap snd rs

            n_gid                <- (<> "_res") <$> freshName (mangle gid)
            addSignal n_gid $ TyStdLogicVector sz
            n_call               <- (<> "_call") <$> freshName (mangle gid)
            (stmts_ealt, n_ealt) <- compileExps ealt
            t_gid                <- askGIdTy gid
            addComponent an gid t_gid
            let argns             = map (\ n -> "arg" <> showt n) ([0..]::[Int])
                pm                = PortMap (zip argns efields ++ [("res", ExprName n_gid)])
            pure (stmts_escr ++
                    [WithAssign ematch (LHSName n)
                                [(ExprName n_gid, ExprBoolConst True)]
                                 (Just (ExprName n_ealt)),
                     Instantiate n_call (mangle gid) pm] ++
                    stmts_ealt,
                    n)
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
mkDefnArch (Defn _ n _ es) = do
      put ([], [], 0) -- empty out the signal and component store, reset name counter
      (stmts, nres)    <- compileExps es
      (sigs, comps, _) <- get
      pure $ Architecture (mangle n <> "_impl") (mangle n) sigs comps (stmts ++ [Assign (LHSName "res") (ExprName nres)])

compileStartDefn :: Monad m => [Flag] -> StartDefn -> CM m Unit
compileStartDefn flags (StartDefn an inps outps _ (n_loopfun, t_loopfun@(Sig _ (arg0size:_) _)) (n_startstate, t_startstate)) = do
      put ([], [], 0) -- empty out signal and component store, reset name counter
      let stateSize = sizeOf t_startstate
          inpSize   = sum $ map snd inps
          outpSize  = sum $ map snd outps
      addComponent an n_startstate t_startstate
      addComponent an n_loopfun t_loopfun
      addSignal "start_state"        $ TyStdLogicVector stateSize
      addSignal "loop_out"           $ TyStdLogicVector stateSize
      addSignal "current_state"      $ TyRegister "clk" stateSize
      addSignal "done_or_next_state" $ TyStdLogicVector stateSize
      addSignal "next_state"         $ TyStdLogicVector stateSize
      addSignal "inp"                $ TyStdLogicVector inpSize
      let ports =
            [ Port "clk"  In TyClock
            , Port rst    In TyStdLogic
            ] <> zipWith (\ n s -> Port n In  (TyStdLogicVector s)) (map fst inps)  (map snd inps)
              <> zipWith (\ n s -> Port n Out (TyStdLogicVector s)) (map fst outps) (map snd outps)
      (sigs, comps, _) <- get
      pure $ Unit (uses flags) (Entity "top_level" ports) $
            Architecture "top_level_impl" "top_level" sigs comps $
                  [ Instantiate "start_call" (mangle n_startstate) (PortMap [("res", ExprName "start_state")])
                  , Instantiate "loop_call" (mangle n_loopfun) (PortMap [("arg0", ExprSlice (ExprName "current_state") (1 + outpSize) (1 + outpSize + arg0size - 1)), ("arg1", ExprName "inp"), ("res", ExprName "loop_out")])
                  , WithAssign (ExprName rst) (LHSName "next_state") [(ExprName "start_state", ExprBit rstSignal)] (Just (ExprName "done_or_next_state"))
                  , WithAssign (ExprSlice (ExprName "current_state") 0 0) (LHSName "done_or_next_state") [(ExprName "loop_out", ExprBitString [One])] (Just (ExprName "current_state"))
                  , ClkProcess "clk" [Assign (LHSName "current_state") (ExprName "next_state")]
                  , Assign (LHSName "inp") $ foldl' ExprConcat (ExprBitString []) $ map (ExprName . fst) inps
                  ] <> fst (foldl' (\ (as, off) (n, sz) -> (as <> [Assign (LHSName n) (ExprSlice (ExprName "current_state") off (off + sz - 1))], off + sz)) ([], 1) outps)
      where rstSignal :: Bit
            rstSignal = if FlagInvertReset `elem` flags then Zero else One

            rst :: Text
            rst = if FlagInvertReset `elem` flags then "rst_n" else "rst"
compileStartDefn _ (StartDefn an _ _ _ _ _) = failAt an "toVHDL: compileStartDefn: start definition with invalid signature."

compileDefn :: Monad m => [Flag] -> Defn -> CM m Unit
compileDefn flags d = Unit (uses flags) <$> mkDefnEntity d <*> mkDefnArch d

compileProgram :: Monad m => [Flag] -> C.Program -> SyntaxErrorT m M.Program
compileProgram flags p = fmap fst $ flip runReaderT (defns p) $ flip runStateT ([], [], 0) $ M.Program <$> ((:) <$> compileStartDefn flags (start p) <*> mapM (compileDefn flags) (defns p))

uses :: [Flag] -> [Text]
uses flags = "ieee.std_logic_1164.all" : concatMap getUses flags
      where getUses :: Flag -> [Text]
            getUses (FlagPkgs pkgs) = map pack $ splitOn "," pkgs
            getUses _               = []

