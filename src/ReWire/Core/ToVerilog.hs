{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.ToVerilog (compileProgram) where

import ReWire.Error
import ReWire.Flags (Flag (..))
import ReWire.Core.Syntax as C hiding (Name)
import ReWire.Verilog.Syntax as V
import ReWire.Core.Mangle (mangle)

import Data.Text (Text)
import Control.Monad.State (MonadState, get, put, evalStateT)
import Control.Monad.Writer (MonadWriter, tell, runWriterT)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Arrow ((&&&))
import TextShow (showt)
import Data.List (foldl')

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

type Fresh = Int
type DefnMap = HashMap GId [C.Exp]

fresh :: MonadState Fresh m => Text -> m Name
fresh s = do
      ctr <- get
      put $ ctr + 1
      pure $ mangle s <> "_" <> showt ctr

compileProgram :: (MonadFail m, MonadError AstError m) => [Flag] -> C.Program -> m V.Program
compileProgram flags (C.Program st ds)
      | FlagFlatten `elem` flags = V.Program <$> pure <$> evalStateT (runReaderT (compileStartDefn flags st) defnMap) 0
      | otherwise                  = V.Program <$> ((:) <$> evalStateT (runReaderT (compileStartDefn flags st) Map.empty) 0 <*> mapM (flip evalStateT 0 . compileDefn flags) ds)
      where defnMap :: DefnMap
            defnMap = Map.fromList $ map (defnName &&& defnBody) ds

compileStartDefn :: (MonadError AstError m, MonadState Fresh m, MonadFail m, MonadReader DefnMap m)
                 => [Flag] -> C.StartDefn -> m Module
compileStartDefn flags (C.StartDefn _ inps outps (loop, (Sig _ (arg0Size:_) _)) (state0, Sig _ _ stateSize)) = do
      ((rStart, ssStart), startSigs) <- runWriterT $ compileCall flags state0 stateSize []
      ((rLoop, ssLoop), loopSigs)    <- runWriterT $ compileCall flags loop stateSize
            [ Range "current_state" (1 + outpSize) (1 + outpSize + arg0Size - 1)
            , Name "inp"
            ]
      pure $ Module "top_level" (inputs <> outputs) (sigs <> startSigs <> loopSigs)
            $  ssStart
            <> ssLoop
            <> assignInp
            <> ifDone rLoop
            <> assignOutputs
            <> [Always [Pos "clk", rstEdge] $ Block $ [ ifRst rStart ]]

      where inputs :: [Port]
            inputs = [Input $ Wire 1 "clk", Input $ Wire 1 rst] <> map (uncurry toInput) inps

            outputs :: [Port]
            outputs = map (Output . uncurry (flip Wire)) outps

            sigs :: [Signal]
            sigs =
                  [ Reg stateSize "current_state"
                  , Wire stateSize "done_or_next_state"
                  , Wire inpSize   "inp"
                  ]

            assignInp :: [Stmt]
            assignInp = [Assign (Name "inp") $ mkConcat $ map (Name . fst) inps]

            ifRst :: V.Exp -> Stmt
            ifRst start = If (Eq (LVal $ Name rst) $ LitBits 1 [rstSignal])
                  (Block [ SeqAssign (Name "current_state") start ])
                  (Block [ SeqAssign (Name "current_state") (LVal $ Name "done_or_next_state") ])

            ifDone :: V.Exp -> [Stmt]
            ifDone loop = pure $ Assign (Name "done_or_next_state") $
                  Cond (Eq (LVal $ Element "current_state" 0) $ LitBits 1 [One])
                        loop
                        (LVal $ Name "current_state")

            assignOutputs :: [Stmt]
            assignOutputs = fst $ foldl'
                  (\ (as, off) (n, sz) -> (as <> [Assign (Name n) (LVal $ Range "current_state" off (off + sz - 1))], off + sz)) ([], 1) outps

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
compileStartDefn _ (StartDefn an _ _ _ _) = failAt an "ToVerilog: compileStartDefn: start definition with invalid signature."

compileDefn :: (MonadState Fresh m, MonadFail m, MonadError AstError m) => [Flag] -> C.Defn -> m V.Module
compileDefn flags (C.Defn _ n (Sig _ inps outp) body) = do
      ((ns, stmts), sigs) <- runWriterT (runReaderT (compileExps flags (map Name argNames) body) Map.empty)
      pure $ V.Module (mangle n) (inputs <> outputs) sigs $ stmts <> [Assign (Name "res") $ mkConcat ns]
      where argNames :: [Name]
            argNames = zipWith (\ _ x -> "arg" <> showt x) inps [0::Int ..]

            inputs :: [Port]
            inputs = zipWith toInput argNames inps

            outputs :: [Port]
            outputs = [Output $ Wire outp "res"]

-- | Inlines a defn.
compileCall :: (MonadState Fresh m, MonadFail m, MonadError AstError m, MonadReader DefnMap m, MonadWriter [Signal] m)
             => [Flag] -> GId -> V.Size -> [LVal] -> m (V.Exp, [Stmt])
compileCall flags g sz lvars
      | FlagFlatten `elem` flags = do
            Just body   <- asks (Map.lookup g)
            (ns, stmts) <- compileExps flags lvars body
            pure (mkConcat ns, stmts)
      | otherwise = do
            mr          <- newWire sz "callRes"
            inst        <- Instantiate (mangle g) <$> fresh g <*> pure (lvars <> [mr])
            pure (LVal mr, [inst])

compileExps :: (MonadState Fresh m, MonadWriter [Signal] m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
            => [Flag] -> [LVal] -> [C.Exp] -> m ([V.LVal], [Stmt])
compileExps flags lvars es = (map fst &&& concatMap snd) <$> mapM (compileExp flags lvars) es

compileExp :: (MonadState Fresh m, MonadWriter [Signal] m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
            => [Flag] -> [LVal] -> C.Exp -> m (V.LVal, [Stmt])
compileExp flags lvars = \ case
      LVar _  _ (lkupLVal -> Just x) -> pure (x, [])
      LVar an _ _                    -> failAt an $ "ToVerilog: compileExp: encountered unknown LVar."
      Lit _ sz v       -> do
            n <- newWire sz "lit"
            pure (n, [Assign n $ LitInt sz v])
      Call _ sz (Global g) es ps els -> do
            Name n               <- newWire (sum $ map sizeOf es) "callPat"
            (callRes, callStmts) <- compileCall flags g sz (patArgs n ps)
            (m, stmts)           <- mkCall sz n es ps els callRes
            pure (m, callStmts <> stmts)
      Call _ sz (Extern (binOp -> Just op)) es ps els -> do
            Name n     <- newWire (sum $ map sizeOf es) "binopPat"
            let [x, y]  = patArgs n ps
            mkCall sz n es ps els $ op (LVal x) $ LVal y
      Call _ sz (Extern (unOp -> Just op)) es ps els -> do
            Name n  <- newWire (sum $ map sizeOf es) "unopPat"
            let [x]  = patArgs n ps
            mkCall sz n es ps els $ op $ LVal x
      Call _ sz (Extern "msbit") es ps els -> do
            Name n     <- newWire (sum $ map sizeOf es) "msbitPat"
            Name arg   <- newWire (argsSize ps) "msbitArg"
            let [x]     = patArgs n ps
                assign  = Assign (Name arg) $ LVal x
            (m, stmts) <- mkCall sz n es ps els $ LVal $ Element arg (argsSize ps - 1)
            pure (m, stmts <> [assign])
      Call _ sz Id es ps els -> do
            Name n  <- newWire (sum $ map sizeOf es) "idPat"
            let args  = patArgs n ps
            mkCall sz n es ps els $ Concat $ map LVal args
      Call _ sz (Const v) es ps els -> do
            Name n  <- newWire (sum $ map sizeOf es) "litPat"
            mkCall sz n es ps els $ LitInt sz v
      Call an _ (Extern ex) _ _ _ -> failAt an $ "ToVerilog: compileExp: unknown extern: " <> ex
      where mkCall :: (MonadState Fresh m, MonadWriter [Signal] m, MonadFail m, MonadError AstError m, MonadReader DefnMap m) => C.Size -> Name -> [C.Exp] -> [Pat] -> [C.Exp] -> V.Exp -> m (V.LVal, [Stmt])
            mkCall sz n es ps els arg = do
                  m              <- newWire sz "call"
                  (ens, stmts)   <- compileExps flags lvars es
                  (ens', stmts') <- compileExps flags lvars els
                  let cond        = patMatches n ps
                  pure (m, stmts <> stmts' <>
                        [ Assign (Name n) $ mkConcat ens
                        , if cond == bTrue || ens' == [] then Assign m arg
                          else Assign m $ Cond cond arg $ mkConcat ens'
                        ])

            lkupLVal :: Int -> Maybe LVal
            lkupLVal = flip lookup (zip [0::Int ..] lvars)

mkConcat :: [LVal] -> V.Exp
mkConcat = \ case
      [e] -> LVal e
      es  -> Concat $ map LVal es

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
      [ ( "!"      , LNot)
      , ( "~"      , Not)
      , ( "&"      , RAnd)
      , ( "~&"     , RNAnd)
      , ( "|"      , ROr)
      , ( "~|"     , RNor)
      , ( "^"      , RXor)
      , ( "~^"     , RXNor)
      , ( "resize" , id)
      ]

newWire :: (MonadState Fresh m, MonadWriter [Signal] m) => V.Size -> Name -> m LVal
newWire sz n = do
      n' <- fresh n
      tell [Wire sz n']
      pure $ Name n'

-- | Returns a boolean expression that is true when the pattern matches.
patMatches :: Name -> [Pat] -> V.Exp
patMatches x = snd . foldl' patMatch (0, bTrue)
      where patMatch :: (Int, V.Exp) -> Pat -> (Int, V.Exp)
            patMatch (off, e) = \ case
                  PatVar _ sz      -> (off + sz, e)
                  PatWildCard _ sz -> (off + sz, e)
                  PatLit _ sz v    -> (off + sz, LAnd e $ Eq (LVal $ Range x off (off + sz - 1)) $ LitInt sz v)

-- | Returns a list of ranges bound by pattern variables.
patArgs :: Name -> [Pat] -> [LVal]
patArgs x = snd . foldl' patArg (0, [])
      where patArg :: (Int, [LVal]) -> Pat -> (Int, [LVal])
            patArg (off, lvs) = \ case
                  PatVar _ sz      -> (off + sz, lvs <> [Range x off (off + sz - 1)])
                  PatWildCard _ sz -> (off + sz, lvs)
                  PatLit _ sz _    -> (off + sz, lvs)

argsSize :: [Pat] -> Int
argsSize = sum . map patToSize
      where patToSize :: Pat -> Int
            patToSize = \ case
                  PatVar _ sz -> sz
                  _           -> 0

toInput :: Text -> C.Size -> Port
toInput n sz = Input $ Wire sz n

