{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.ToVerilog (compileProgram) where

import ReWire.Error
import ReWire.Flags (Flag (..))
import ReWire.Annotation (noAnn)
import ReWire.Core.Syntax as C hiding (Name, Size, Index)
import ReWire.Verilog.Syntax as V
import ReWire.Core.Mangle (mangle)

import Data.Text (Text)
import Control.Monad.State (MonadState, get, put, evalStateT)
import Control.Monad.Writer (MonadWriter, tell, runWriterT)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Arrow ((&&&))
import TextShow (showt)
import Data.List (foldl')
import Data.BitVector (width, bitVec)
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
      | otherwise                = V.Program <$> ((:) <$> evalStateT (runReaderT (compileStartDefn flags st) defnMap) 0 <*> mapM (flip evalStateT 0 . compileDefn flags) ds)
      where defnMap :: DefnMap
            defnMap = Map.fromList $ map (defnName &&& defnBody) ds

compileStartDefn :: (MonadError AstError m, MonadState Fresh m, MonadFail m, MonadReader DefnMap m)
                 => [Flag] -> C.StartDefn -> m Module
compileStartDefn flags st@(C.StartDefn _ inps outps (loop, _) (state0, Sig _ _ initSize)) = do
      ((rStart, ssStart), startSigs) <- runWriterT $ compileCall (FlagFlatten : flags) state0 initSize []
      ((rLoop, ssLoop), loopSigs)    <- runWriterT $ compileCall flags loop initSize
            [ Name sState
            , Name sInp
            ]
      pure $ Module "top_level" (inputs <> outputs) (sigs <> startSigs <> loopSigs)
            $  ssStart
            <> ssLoop
            <> assignSigs rLoop
            <> [ Always [Pos sClk, rstEdge] $ Block [ ifRst rStart ] ]
      where inputs :: [Port]
            inputs = [Input $ Wire 1 sClk, Input $ Wire 1 rst] <> map (uncurry toInput) inps

            outputs :: [Port]
            outputs = map (Output . uncurry (flip Wire)) outps

            sCurState = "current_state"
            sNxtState = "done_or_next_state"
            sInp      = "inp"
            sContinue = "continue"
            sClk      = "clk"
            sState    = "state"

            sigs :: [Signal]
            sigs = [ Reg initSize    sCurState
                  , Wire initSize    sNxtState
                  , Wire inpSize     sInp
                  , Wire 1           sContinue
                  , Wire stateSize   sState
                  ]

            ifRst :: V.Exp -> Stmt
            ifRst start = IfElse (Eq (LVal $ Name rst) $ LitBits $ bitVec 1 $ fromEnum rstSignal)
                  (Block
                        [ SeqAssign (Name sCurState) start
                        ])
                  (Block
                        [ SeqAssign (Name sCurState) (LVal $ Name sNxtState)
                        ])

            assignSigs :: V.Exp -> [Stmt]
            assignSigs loop = filterAssigns (zipWith Assign (map (Name . fst) p_outps_st) (map LVal $ toSubRanges sCurState $ map snd p_outps_st))
                  <> [Assign (Name sNxtState) loop]
                  <> [Assign (Name sInp) $ mkConcat $ map (Name . fst) inps]

            filterAssigns :: [Stmt] -> [Stmt] -- TODO(chathhorn): do this in a better way.
            filterAssigns = filter (not . isPadding)
                  where isPadding :: Stmt -> Bool
                        isPadding = \ case
                              Assign (Name "padding") _ -> True
                              _                        -> False

            p_outps_st :: [(Name, Size)]
            p_outps_st = (sContinue, 1) : padding <> outps <> [(sState, stateSize)]

            padding :: [(Name, Size)]
            padding | paddingSize > 0 = [("padding", fromIntegral paddingSize)]
                    | otherwise       = []

            paddingSize :: Int
            paddingSize = fromIntegral initSize - 1
                        - fromIntegral outpSize
                        - fromIntegral stateSize

            stateSize :: Size
            stateSize = case st of
                  StartDefn _ _ _ (_, (Sig _ (arg0Size : _) _)) _ -> arg0Size
                  _                                               -> 0

            inpSize :: V.Size
            inpSize = sum $ snd <$> inps

            outpSize :: V.Size
            outpSize = sum $ snd <$> outps

            rstEdge :: Sensitivity
            rstEdge | FlagInvertReset `elem` flags = Neg rst
                    | otherwise                    = Pos rst

            rstSignal :: Bool
            rstSignal = not $ FlagInvertReset `elem` flags

            rst :: Text
            rst | FlagInvertReset `elem` flags = "rst_n"
                | otherwise                    = "rst"

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
      Lit _ bv                       -> do
            n <- newWire (fromIntegral $ width bv) "lit"
            pure (n, [Assign n $ LitBits bv])
      Call _ sz (Global g) es ps els -> do
            Name n               <- newWire (sum $ map sizeOf es) "callPat"
            (callRes, callStmts) <- compileCall flags g sz (patApply n ps)
            (m, stmts)           <- mkCall sz n es ps els callRes
            pure (m, callStmts <> stmts)
      Call _ sz (Extern _ (binOp -> Just op)) es ps els -> do
            Name n     <- newWire (sum $ map sizeOf es) "binopPat"
            let [x, y]  = patApply n ps
            mkCall sz n es ps els $ op (LVal x) $ LVal y
      Call _ sz (Extern _ (unOp -> Just op)) es ps els -> do
            Name n  <- newWire (sum $ map sizeOf es) "unopPat"
            let [x]  = patApply n ps
            mkCall sz n es ps els $ op $ LVal x
      Call _ sz (Extern _ "msbit") es ps els -> do
            Name n     <- newWire (sum $ map sizeOf es) "msbitPat"
            Name arg   <- newWire (argsSize ps) "msbitArg"
            let [x]     = patApply n ps
                assign  = Assign (Name arg) $ LVal x
            (m, stmts) <- mkCall sz n es ps els $ LVal $ Element arg (fromIntegral (argsSize ps) - 1)
            pure (m, stmts <> [assign])
      Call _ sz Id es ps els -> do
            Name n  <- newWire (sum $ map sizeOf es) "idPat"
            let args  = patApply n ps
            mkCall sz n es ps els $ mkConcat args
      Call _ sz (Const bv) es ps els -> do
            Name n  <- newWire (sum $ map sizeOf es) "litPat"
            mkCall sz n es ps els $ LitBits bv
      Call an _ (Extern _ ex) _ _ _ -> failAt an $ "ToVerilog: compileExp: unknown extern: " <> ex
      where mkCall :: (MonadState Fresh m, MonadWriter [Signal] m, MonadFail m, MonadError AstError m, MonadReader DefnMap m) => Size -> Name -> [C.Exp] -> [Pat] -> [C.Exp] -> V.Exp -> m (V.LVal, [Stmt])
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

            lkupLVal :: LId -> Maybe LVal
            lkupLVal = flip lookup (zip [0::LId ..] lvars)

mkConcat :: [LVal] -> V.Exp
mkConcat = \ case
      [e] -> LVal e
      es  -> Concat $ map LVal $ reverse es

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
patMatches x ps = snd $ foldl' patMatch (fromIntegral $ sum $ map sizeOf ps, bTrue) ps
      where patMatch :: (Index, V.Exp) -> Pat -> (Index, V.Exp)
            patMatch (off, e) = \ case
                  PatVar _ (fromIntegral -> sz)      | sz > 0       -> (off - sz, e)
                  PatWildCard _ (fromIntegral -> sz) | sz > 0       -> (off - sz, e)
                  PatLit _ bv                        | width bv > 0 -> (off - width bv, LAnd e $ Eq (LVal $ Range x (off - width bv) $ off - 1) $ LitBits bv)
                  _                                                 -> (off, e)

-- | Returns a list of ranges bound by pattern variables.
patApply :: Name -> [Pat] -> [LVal]
patApply x ps = snd $ foldl' patArg (fromIntegral $ sum $ map sizeOf ps, []) ps
      where patArg :: (Index, [LVal]) -> Pat -> (Index, [LVal])
            patArg (off, lvs) = \ case
                  PatVar _ (fromIntegral -> sz)      | sz > 0 -> (off - sz, lvs <> [Range x (off - sz) $ off - 1])
                  PatWildCard _ (fromIntegral -> sz) | sz > 0 -> (off - sz, lvs)
                  PatLit _ (width -> sz)             | sz > 0 -> (off - sz, lvs)
                  _                                           -> (off, lvs)

toSubRanges :: Name -> [Size] -> [LVal]
toSubRanges n szs = patApply n (map (PatVar noAnn) szs)

argsSize :: [Pat] -> Size
argsSize = sum . map patToSize
      where patToSize :: Pat -> Size
            patToSize = \ case
                  PatVar _ sz -> sz
                  _           -> 0

toInput :: Text -> Size -> Port
toInput n sz = Input $ Wire sz n

