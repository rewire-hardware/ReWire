{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.ToVerilog (compileProgram) where

import ReWire.Error
import ReWire.Flags (Flag (..))
import ReWire.Core.Syntax as C
import ReWire.Verilog.Syntax as V
import ReWire.Core.Mangle (mangle)

import Data.Text (Text)
import Control.Monad.State (MonadState, get, put, evalStateT)
import Control.Monad.Writer (MonadWriter, tell, runWriterT)
import Control.Arrow ((&&&))
import TextShow (showt)
import Data.List (foldl')

type Fresh = Int

fresh :: MonadState Fresh m => Text -> m Name
fresh s = do
      ctr <- get
      put $ ctr + 1
      pure $ mangle s <> "_" <> showt ctr

compileProgram :: (MonadFail m, MonadError AstError m) => [Flag] -> C.Program -> m V.Program
compileProgram flags (C.Program st ds) = V.Program <$> ((:) <$> compileStartDefn flags st <*> mapM (flip evalStateT 0 . compileDefn) ds)

compileStartDefn :: MonadError AstError m => [Flag] -> C.StartDefn -> m Module
compileStartDefn flags (C.StartDefn _ inps outps (loop, (Sig _ (arg0Size:_) _)) (state0, Sig _ _ stateSize)) = do
      pure $ Module "top_level" (inputs <> outputs) sigs $
            [ instStart
            , instDispatch
            , assignInp
            , ifDone
            ] <> assignOutputs
              <> [clkProcess]
      where inputs :: [Port]
            inputs = [Input $ Wire 1 "clk", Input $ Wire 1 rst] <> map toInput inps

            outputs :: [Port]
            outputs = map (Output . uncurry (flip Wire)) outps

            sigs :: [Signal]
            sigs =
                  [ Reg stateSize "start_state"
                  , Reg stateSize "loop_out"
                  , Reg stateSize "current_state"
                  , Wire stateSize "done_or_next_state"
                  , Wire inpSize   "inp"
                  ]

            instStart :: Stmt
            instStart = Instantiate (mangle state0) "start_call" [Name "start_state"]

            instDispatch :: Stmt
            instDispatch = Instantiate (mangle loop) "loop_call"
                  [ Range (Name "current_state") (1 + outpSize) (1 + outpSize + arg0Size - 1)
                  , Name "inp"
                  , Name "loop_out"
                  ]

            assignInp :: Stmt
            assignInp = Assign (Name "inp") $ Concat $ map (LVal . Name . fst) inps

            ifRst :: Stmt
            ifRst = If (Eq (LVal $ Name rst) $ LitBits 1 [rstSignal])
                  (Block [ SeqAssign (Name "current_state") (LVal $ Name "start_state") ])
                  (Block [ SeqAssign (Name "current_state") (LVal $ Name "done_or_next_state") ])

            ifDone :: Stmt
            ifDone = Assign (Name "done_or_next_state") $
                  Cond (Eq (LVal $ Element (Name "current_state")  0) $ LitBits 1 [One])
                        (LVal $ Name "loop_out")
                        (LVal $ Name "current_state")

            clkProcess :: Stmt
            clkProcess = Always [Pos "clk", rstEdge] $ Block $
                  [ ifRst ]

            assignOutputs :: [Stmt]
            assignOutputs = fst $ foldl'
                  (\ (as, off) (n, sz) -> (as <> [Assign (Name n) (LVal $ Range (Name "current_state") off (off + sz - 1))], off + sz)) ([], 1) outps

            ---

            inpSize :: V.Size
            inpSize = sum $ map snd inps

            outpSize :: V.Size
            outpSize = sum $ map snd outps

            rstEdge :: Sensitivity
            rstEdge | FlagInvertReset `elem` flags = Neg rst
                    | otherwise                    = Pos rst

            rstSignal :: Bit
            rstSignal | FlagInvertReset `elem` flags = Zero
                      | otherwise                    = One

            rst :: Text
            rst | FlagInvertReset `elem` flags = "rst_n"
                | otherwise                    = "rst"
compileStartDefn _ (StartDefn an _ _ _ _) = failAt an "toVerilog: compileStartDefn: start definition with invalid signature."

compileDefn :: (MonadState Fresh m, MonadFail m) => C.Defn -> m V.Module
compileDefn (C.Defn _ n (Sig _ inps outp) body) = do
      ((ns, stmts), sigs) <- runWriterT (compileExps body)
      pure $ V.Module (mangle n) (inputs <> outputs) sigs $ stmts <> [Assign (Name "res") $ Concat $ map LVal ns]
      where inputs :: [Port]
            inputs = map toInput $ zip (zipWith (\ _ x -> "arg" <> showt x) inps [0::Int ..]) inps

            outputs :: [Port]
            outputs = [Output $ Wire outp "res"]

compileExps :: (MonadState Fresh m, MonadWriter [Signal] m, MonadFail m) => [C.Exp] -> m ([V.LVal], [Stmt])
compileExps es = (map fst &&& concatMap snd) <$> mapM compileExp es

compileExp :: (MonadState Fresh m, MonadWriter [Signal] m, MonadFail m) => C.Exp -> m (V.LVal, [Stmt])
compileExp = \ case
      LVar _ _ x       -> pure (Name $ "arg" <> showt x, [])
      Lit _ sz v       -> do
            n <- newWire sz "lit"
            pure (n, [Assign n $ LitInt sz v])
      Extern _ sz (binOp -> Just op) [x, y] -> do
            res             <- newWire sz "binop"
            ([x', y'], ss') <- compileExps [x, y]
            pure (res, ss' <> [Assign res $ op (LVal x') $ LVal y'])
      Extern _ sz (unOp -> Just op) [x] -> do
            res       <- newWire sz "unop"
            (x', ss') <- compileExp x
            pure (res, ss' <> [Assign res $ op $ LVal x'])
      Extern _ sz n es -> do
            n'         <- newWire sz n
            inst       <- fresh $ n <> "_inst"
            (ns', ss') <- compileExps es
            pure (n', ss' <> [Instantiate (mangle n) inst ns'])
      Match _ sz g es ps els -> do
            e  <- newWire (sum $ map sizeOf es) "match_exp"
            mr <- newWire sz "match_res"
            m  <- newWire sz "match"
            inst <- fresh g
            (ens, stmts)   <- compileExps es
            (ens', stmts') <- compileExps els
            let cond   = patMatches e ps
            pure (m, stmts <> stmts' <>
                  [ Instantiate (mangle g) inst (patArgs e ps <> [mr])
                  , Assign e $ Concat $ map LVal ens -- TODO(chathhorn): should reflect this behavior in the Core syntax.
                  , if cond == bTrue || ens' == [] then Assign m (LVal mr)
                    else Assign m $ Cond cond (LVal mr) $ Concat $ map LVal ens'
                  ])

binOp :: Name -> Maybe (V.Exp -> V.Exp -> V.Exp)
binOp = flip lookup primBinOps

unOp :: Name -> Maybe (V.Exp -> V.Exp)
unOp = flip lookup primUnOps

primBinOps :: [(Name, V.Exp -> V.Exp -> V.Exp)]
primBinOps =
      [ ( "+"   , Add)
      , ( "-"   , Sub)
      , ( "*"   , Mul)
      , ( "/"   , Div)
      , ( "%"   , Mod)
      , ( "**"  , Pow)
      , ( "&&"  , LAnd)
      , ( "||"  , LOr)
      , ( "&"   , And)
      , ( "|"   , Or)
      , ( "^"   , XOr)
      , ( "~^"  , XNor)
      , ( "<<"  , LShift)
      , ( ">>"  , RShift)
      , ( "<<<" , LShiftArith)
      , ( ">>>" , RShiftArith)
      ]

primUnOps :: [(Name, V.Exp -> V.Exp)]
primUnOps =
      [ ( "!"  , LNot)
      , ( "~"  , Not)
      , ( "&"  , RAnd)
      , ( "~&" , RNAnd)
      , ( "|"  , ROr)
      , ( "~|" , RNor)
      , ( "^"  , RXor)
      , ( "~^" , RXNor)
      ]

newWire :: (MonadState Fresh m, MonadWriter [Signal] m) => V.Size -> Name -> m LVal
newWire sz n = do
      n' <- fresh n
      tell [Wire sz n']
      pure $ Name n'

-- | Returns a boolean expression that is true when the pattern matches.
patMatches :: LVal -> [Pat] -> V.Exp
patMatches x = snd . foldl' patMatch (0, bTrue)
      where patMatch :: (Int, V.Exp) -> Pat -> (Int, V.Exp)
            patMatch (off, e) = \ case
                  PatVar _ sz      -> (off + sz, e)
                  PatWildCard _ sz -> (off + sz, e)
                  PatLit _ sz v    -> (off + sz, LAnd e (Eq (LVal $ Range x off (off + sz - 1)) (LitInt sz v)))

-- | Returns a list of ranges bound by pattern variables.
patArgs :: LVal -> [Pat] -> [LVal]
patArgs x = snd . foldl' patArg (0, [])
      where patArg :: (Int, [LVal]) -> Pat -> (Int, [LVal])
            patArg (off, lvs) = \ case
                  PatVar _ sz      -> (off + sz, lvs <> [Range x off (off + sz - 1)])
                  PatWildCard _ sz -> (off + sz, lvs)
                  PatLit _ sz _    -> (off + sz, lvs)

toInput :: (Text, C.Size) -> Port
toInput = Input . uncurry (flip Wire)

