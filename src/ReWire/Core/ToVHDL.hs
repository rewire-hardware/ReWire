{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.ToVHDL (compileProgram) where

import ReWire.Annotation
import ReWire.Core.Syntax as C hiding (Name, Index, Size)
import ReWire.Error
import ReWire.Core.Mangle
import ReWire.VHDL.Syntax as V
import ReWire.Flags (Flag (..))

import Control.Monad (zipWithM)
import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.State (StateT (..), get, put, lift)
import Data.List (genericLength, find, foldl')
import Data.List.Split (splitOn)
import Data.Bits (testBit)
import Data.Text (Text, pack)
import Data.BitVector (width, nat)

import TextShow (showt)

type CM m = StateT ([Signal], [Component], Index) (ReaderT [Defn] m)

askDefns :: Monad m => CM m [Defn]
askDefns = ask

nvec :: Value -> Size -> [Bit]
nvec n width = nvec' 0 []
      where nvec' :: Index -> [Bit] -> [Bit]
            nvec' pos bits | pos >= fromIntegral width = bits
                           | otherwise                 = nvec' (pos + 1) $ (if testBit n $ fromEnum pos then One else Zero) : bits

getTyPorts :: Monad m => C.Sig -> CM m [Port]
getTyPorts (Sig _ argsizes ressize) = do
      let argnames = zipWith (\ _ x -> "arg" <> showt x) argsizes [0::Int ..]
          argports = zipWith (\ n x -> Port n In (TyStdLogicVector x)) argnames argsizes
          resport  = Port "res" Out (TyStdLogicVector ressize)
      pure $ argports <> [resport]

mkDefnEntity :: Monad m => Defn -> CM m Entity
mkDefnEntity (Defn _ n t _) = Entity (mangle n) <$> getTyPorts t

freshName :: Monad m => Text -> CM m Name
freshName s = do
      (sigs, comps, ctr) <- get
      put (sigs, comps, ctr + 1)
      pure (s <> showt ctr)

addSignal :: Monad m => Text -> V.Ty -> CM m ()
addSignal n t = do
      (sigs, comps, ctr) <- get
      put (sigs <> [Signal n t], comps, ctr)

addComponent :: Monad m => Annote -> Name -> C.Sig -> CM m ()
addComponent _ i t = do
      (sigs, comps, ctr) <- get
      case find ((== mangle i) . componentName) comps of
            Just _ -> pure ()
            Nothing -> do
                  ps <- getTyPorts t
                  put (sigs, Component (mangle i) ps : comps, ctr)

-- | First Expr is for whether match (std_logic); remaining Exprs are for extracted fields.
compilePat :: Monad m => Name -> Index -> Pat -> CM m (Expr, [Expr])
compilePat nscr offset = \ case
      PatVar _ s       -> pure (ExprBoolConst True, [ExprSlice (ExprName nscr) offset (offset + fromIntegral s - 1)])
      PatWildCard _ _  -> pure (ExprBoolConst True, [])
      PatLit _ bv      -> do
            let tagSize  = fromIntegral $ width bv
                tagValue = if tagSize > 0 then nat bv else 0
                tag      = nvec tagValue tagSize
                ematch   = ExprIsEq (ExprSlice (ExprName nscr) offset (offset + genericLength tag - 1)) $ ExprBitString tag
            pure (ematch, [])

askGIdTy :: MonadError AstError m => Name -> CM m C.Sig
askGIdTy i = do
      defns <- askDefns
      case find (\ (Defn _ i' _ _) -> i == i') defns of
            Just (Defn _ _ t _) -> pure t
            Nothing             -> lift $ failAt noAnn $ "askGIdTy: no info for identifier " <> showt i

compileExps :: MonadError AstError m => [C.Exp] -> CM m ([Stmt], Name)
compileExps es = do
      n          <- (<> "Res") <$> freshName (mangle "slice")
      addSignal n $ TyStdLogicVector $ sum $ map sizeOf es
      sssns      <- mapM compileExp es
      let stmts   = concatMap fst sssns
          ns      = map snd sssns
      pure  ( stmts <>
                  [ Assign (LHSName n)
                        $ simplifyConcat
                        $ foldl' ExprConcat (ExprBitString [])
                        $ map ExprName ns
                  ]
            , n
            )


compileExp :: MonadError AstError m => C.Exp -> CM m ([Stmt], Name)
compileExp = \ case
      LVar _ _ i       -> pure ([], "arg" <> showt i)
      Lit _ bv -> do
            let litSize  = fromIntegral $ width bv
                litValue = if litSize > 0 then nat bv else 0
            n          <- (<> "Res") <$> freshName (mangle $ "lit" <> showt litValue <> "x" <> showt litSize)
            addSignal n $ TyStdLogicVector litSize
            let litVec  = nvec litValue litSize
            pure  ([ Assign (LHSName n) $ ExprBitString litVec ], n)
      Call an sz (Global gid) escr ps [] -> do
            (stmts_escr, n_escr) <- compileExps escr

            let fieldWidths       = map sizeOf ps
                fieldOffsets      = init $ scanl (+) 0 fieldWidths
            efields              <- concatMap snd <$> zipWithM (compilePat n_escr . fromIntegral) fieldOffsets ps

            n_gid                <- (<> "Res") <$> freshName (mangle gid)
            addSignal n_gid $ TyStdLogicVector sz
            n_call               <- (<> "Call") <$> freshName (mangle gid)
            t_gid                <- askGIdTy gid
            addComponent an gid t_gid
            let argns            =  map (("arg" <>) . showt) [0::Index ..]
                pm               =  PortMap (zip argns efields <> [("res", ExprName n_gid)])
            pure (stmts_escr <> [Instantiate n_call (mangle gid) pm], n_gid)
      Call an sz (Global gid) escr ps ealt -> do
            n                    <- (<> "Res") <$> freshName "match"
            addSignal n $ TyStdLogicVector sz
            (stmts_escr, n_escr) <- compileExps escr

            let fieldWidths       = map sizeOf ps
                fieldOffsets      = init $ scanl (+) 0 fieldWidths
            rs                   <- zipWithM (compilePat n_escr . fromIntegral) fieldOffsets ps
            let ematch            = foldl' ExprAnd (ExprBoolConst True) $ map fst rs
                efields           = concatMap snd rs

            n_gid                <- (<> "Res") <$> freshName (mangle gid)
            addSignal n_gid $ TyStdLogicVector sz
            n_call               <- (<> "Call") <$> freshName (mangle gid)
            (stmts_ealt, n_ealt) <- compileExps ealt
            t_gid                <- askGIdTy gid
            addComponent an gid t_gid
            let argns             = map (("arg" <>) . showt) [0::Index ..]
                pm                = PortMap (zip argns efields <> [("res", ExprName n_gid)])
            pure (stmts_escr <>
                    [WithAssign ematch (LHSName n)
                                [(ExprName n_gid, ExprBoolConst True)]
                                 (Just (ExprName n_ealt)),
                     Instantiate n_call (mangle gid) pm] <>
                    stmts_ealt,
                    n)
      e -> failAt (ann e) "ToVHDL: compileExp: encountered currently unsupported expression."
      -- TODO(chathhorn): extern
      -- Extern _ sz i args -> do
      --       n           <- (<> "Res") <$> freshName i
      --       addSignal n $ TyStdLogicVector sz
      --       sssns       <- mapM compileExp args
      --       let stmts   =  concatMap fst sssns
      --           ns      =  map snd sssns
      --       pure (stmts <> [Assign (LHSName n) (ExprFunCall i (map ExprName ns))], n)

mkDefnArch :: MonadError AstError m => Defn -> CM m Architecture
mkDefnArch (Defn _ n _ es) = do
      put ([], [], 0) -- empty out the signal and component store, reset name counter
      (stmts, nres)    <- compileExps es
      (sigs, comps, _) <- get
      pure $ Architecture (mangle n <> "Impl") (mangle n) sigs comps (stmts <> [Assign (LHSName "res") (ExprName nres)])

compileStartDefn :: MonadError AstError m => [Flag] -> StartDefn -> CM m Unit
compileStartDefn flags (StartDefn an inps outps (n_loopfun, t_loopfun@(Sig _ (arg0size:_) _)) (n_startstate, t_startstate)) = do
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
            Architecture "top_level_impl" "top_level" sigs comps
                  [ Instantiate "start_call" (mangle n_startstate) (PortMap [("res", ExprName "start_state")])
                  , Instantiate "loop_call" (mangle n_loopfun) (PortMap
                        [ ("arg0", ExprSlice (ExprName "current_state") (1 + fromIntegral outpSize) (1 + fromIntegral (outpSize + arg0size) - 1))
                        , ("arg1", ExprName "inp")
                        , ("res", ExprName "loop_out")
                        ] )
                  , WithAssign (ExprName rst) (LHSName "next_state")
                        [ (ExprName "start_state", ExprBit rstSignal) ]
                        (Just (ExprName "done_or_next_state"))
                  , WithAssign (ExprSlice (ExprName "current_state") 0 0) (LHSName "done_or_next_state")
                        [ (ExprName "loop_out", ExprBitString [One]) ]
                        (Just (ExprName "current_state"))
                  , ClkProcess "clk" $ Assign (LHSName "current_state") (ExprName "next_state") : outputs
                  , Assign (LHSName "inp") $ foldl' ExprConcat (ExprBitString []) $ map (ExprName . fst) inps

             -- TODO(chathhorn): fix output.
             --      , WithAssign (ExprSlice (ExprName "current_state") 0 0) (LHSName "outp")
             --               [ ( ExprConcat (ExprBitString [One])
             --                        ( ExprConcat (ExprSlice (ExprName "current_state") 1 outsize)
             --                              pad_for_out ) -- "1" & (current_state[1 to outsize] & pad_for_out)
             --                                            -- MSB
             --                 , ExprBitString [One]
             --                 )
             --               ]
             --           (Just ( ExprConcat (ExprBitString [Zero])
             --                        ( ExprConcat (ExprSlice (ExprName "current_state") 1 ressize)
             --                              pad_for_res) ) --  "0" & (current_state[1 to ressize] & pad_for_res)
             --           )
                  ]
      where rstSignal :: Bit
            rstSignal | FlagInvertReset `elem` flags = Zero
                      | otherwise                    = One

            rst :: Text
            rst | FlagInvertReset `elem` flags = "rst_n"
                | otherwise                    = "rst"

            outputs :: [Stmt]
            outputs = fst $ foldl' (\ (as, off) (n, sz) ->
                  (as <> [Assign (LHSName n) (ExprSlice (ExprName "current_state") off (off + fromIntegral sz - 1))], off + fromIntegral sz))
                  ([], 1) outps

compileStartDefn _ (StartDefn an _ _ _ _) = failAt an "toVHDL: compileStartDefn: start definition with invalid signature."

compileDefn :: MonadError AstError m => [Flag] -> Defn -> CM m Unit
compileDefn flags d = Unit (uses flags) <$> mkDefnEntity d <*> mkDefnArch d

compileProgram :: MonadError AstError m => [Flag] -> C.Program -> m V.Program
compileProgram flags p = fmap fst $ flip runReaderT (defns p) $ flip runStateT ([], [], 0) $ V.Program <$> ((:) <$> compileStartDefn flags (start p) <*> mapM (compileDefn flags) (defns p))

uses :: [Flag] -> [Text]
uses flags = "ieee.std_logic_1164.all" : concatMap getUses flags
      where getUses :: Flag -> [Text]
            getUses (FlagPkgs pkgs) = map pack $ splitOn "," pkgs
            getUses _               = []

