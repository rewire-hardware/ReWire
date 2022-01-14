{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.ToVerilog (compileProgram) where

import ReWire.Error
import ReWire.Flags (Flag (..))
import ReWire.Annotation (noAnn)
import ReWire.Core.Syntax as C hiding (Name, Size, Index)
import ReWire.Verilog.Syntax as V
import ReWire.Core.Mangle (mangle)
import ReWire.Core.Interp (patApply', patMatches', subRange)

import Data.Text (Text, pack)
import Data.Maybe (fromMaybe)
import Control.Monad (msum)
import Control.Monad.State (MonadState, get, put, evalStateT)
import Control.Monad.Writer (MonadWriter, tell, runWriterT)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Arrow ((&&&))
import TextShow (showt)
import Data.BitVector (width, bitVec, BV, zeros, ones, lsb1, (==.), msb)
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
      | FlagFlatten `elem` flags = V.Program . pure <$> evalStateT (runReaderT (compileStartDefn flags st) defnMap) 0
      | otherwise                = do
            st' <- evalStateT (runReaderT (compileStartDefn flags st) defnMap) 0
            ds' <- mapM (flip evalStateT 0 . compileDefn flags) $ filter ((/= getState0 st) . defnName) ds
            pure $ V.Program $ st' : ds'
      where defnMap :: DefnMap
            defnMap = Map.fromList $ map (defnName &&& defnBody) ds

            -- | Initial state should be inlined, so we can filter out its defn.
            getState0 :: StartDefn -> Name
            getState0 (C.StartDefn _ _ _ _ (state0, _)) = state0

compileStartDefn :: (MonadError AstError m, MonadState Fresh m, MonadFail m, MonadReader DefnMap m)
                 => [Flag] -> C.StartDefn -> m Module
compileStartDefn flags st@(C.StartDefn _ inps outps (loop, _) (state0, Sig _ _ initSize)) = do
      ((rStart, ssStart), startSigs) <- runWriterT $ compileCall (FlagFlatten : flags) state0 initSize []
      ((rLoop, ssLoop), loopSigs)    <- runWriterT $ compileCall flags loop initSize
            [ LVal $ Name sState
            , LVal $ Name sInp
            ]
      pure $ Module "top_level" (inputs <> outputs) (sigs <> startSigs <> loopSigs)
            $  ssStart
            <> ssLoop
            <> assignSigs rLoop
            <> [ Initial $ ParAssign (Name sCurState) rStart ]
            <> [ Always ([Pos sClk] <> rstEdge) $ Block [ ifRst rStart ] ]
      where inputs :: [Port]
            inputs = [Input $ Logic 1 sClk]
                  <> (if noRst then [] else [Input $ Logic 1 sRst])
                  <> map (uncurry toInput) inps

            outputs :: [Port]
            outputs = map (Output . uncurry (flip Logic)) outps

            sCurState = "current_state"
            sNxtState = "done_or_next_state"
            sInp      = "inp"
            sContinue = "cont"
            sState    = "state"

            sClk :: Text
            sClk = fromMaybe "clk" $ msum $ map (\ case
                  FlagClockName s -> Just $ pack s
                  _               -> Nothing) flags

            sRst :: Text
            sRst = fromMaybe (if invertRst then "rst_n" else "rst") $ msum $ map (\ case
                  FlagResetName s -> Just $ pack s
                  _               -> Nothing) flags

            sigs :: [Signal]
            sigs = [ Logic initSize    sCurState
                  , Logic initSize    sNxtState
                  , Logic inpSize     sInp
                  , Logic 1           sContinue
                  , Logic stateSize   sState
                  ]

            ifRst :: V.Exp -> Stmt
            ifRst start | noRst     = assignNextState
                        | otherwise = IfElse (Eq (LVal $ Name sRst) $ LitBits $ bitVec 1 $ fromEnum $ not invertRst)
                  (Block [ ParAssign (Name sCurState) start ])
                  (Block [ assignNextState ])

            assignNextState :: Stmt
            assignNextState = ParAssign (Name sCurState) $ LVal $ Name sNxtState

            assignSigs :: V.Exp -> [Stmt]
            assignSigs loop = filterAssigns (zipWith Assign (map (Name . fst) p_outps_st) (map LVal $ toSubRanges sCurState $ map snd p_outps_st))
                  <> [Assign (Name sNxtState) loop]
                  <> [Assign (Name sInp) $ mkConcat $ map (LVal . Name . fst) inps]

            filterAssigns :: [Stmt] -> [Stmt] -- TODO(chathhorn): do this in a better way.
            filterAssigns = filter (not . isPadding)
                  where isPadding :: Stmt -> Bool
                        isPadding = \ case
                              Assign (Name "padding") _ -> True
                              _                         -> False

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
                  StartDefn _ _ _ (_, Sig _ (arg0Size : _) _) _ -> arg0Size
                  _                                             -> 0

            inpSize :: V.Size
            inpSize = sum $ snd <$> inps

            outpSize :: V.Size
            outpSize = sum $ snd <$> outps

            rstEdge :: [Sensitivity]
            rstEdge | noRst     = []
                    | invertRst = [Neg sRst]
                    | otherwise = [Pos sRst]

            invertRst :: Bool
            invertRst = FlagInvertReset `elem` flags

            noRst :: Bool
            noRst = FlagNoReset `elem` flags

compileDefn :: (MonadState Fresh m, MonadFail m, MonadError AstError m) => [Flag] -> C.Defn -> m V.Module
compileDefn flags (C.Defn _ n (Sig _ inps outp) body) = do
      ((es, stmts), sigs) <- runWriterT (runReaderT (compileExps flags (map (LVal . Name) argNames) body) Map.empty)
      pure $ V.Module (mangle n) (inputs <> outputs) sigs $ stmts <> [Assign (Name "res") $ mkConcat es]
      where argNames :: [Name]
            argNames = zipWith (\ _ x -> "arg" <> showt x) inps [0::Int ..]

            inputs :: [Port]
            inputs = zipWith toInput argNames inps

            outputs :: [Port]
            outputs = [Output $ Logic outp "res"]

-- | Inlines a defn or instantiates an already-compiled defn.
compileCall :: (MonadState Fresh m, MonadFail m, MonadError AstError m, MonadReader DefnMap m, MonadWriter [Signal] m)
             => [Flag] -> GId -> V.Size -> [V.Exp] -> m (V.Exp, [Stmt])
compileCall flags g sz lvars
      | FlagFlatten `elem` flags = do
            Just body   <- asks (Map.lookup g)
            (es, stmts) <- compileExps flags lvars body
            pure (wcast sz $ mkConcat es, stmts)
      | otherwise = do
            mr          <- newWire sz "callRes"
            inst        <- Instantiate (mangle g) <$> fresh g <*> pure (lvars <> [LVal mr])
            pure (LVal mr, [inst])

compileExps :: (MonadState Fresh m, MonadWriter [Signal] m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
            => [Flag] -> [V.Exp] -> [C.Exp] -> m ([V.Exp], [Stmt])
compileExps flags lvars es = (map fst &&& concatMap snd) <$> mapM (compileExp flags lvars) es

compileExp :: (MonadState Fresh m, MonadWriter [Signal] m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
            => [Flag] -> [V.Exp] -> C.Exp -> m (V.Exp, [Stmt])
compileExp flags lvars = \ case
      LVar _  _ (lkupLVal -> Just x)                    -> pure (x, [])
      LVar an _ _                                       -> failAt an "ToVerilog: compileExp: encountered unknown LVar."
      Lit _ bv                                          -> pure (bvToExp bv, [])
      Call _ sz (Global g) es ps els                    -> mkCall "glob" es ps els $ compileCall flags g sz
      Call _ sz (Extern _ (binOp -> Just op)) es ps els -> mkCall "binOp" es ps els $ \ [x, y] -> pure (wcast sz $ op x y, [])
      Call _ sz (Extern _ (unOp -> Just op)) es ps els  -> mkCall "unOp" es ps els $ \ [x] -> pure (wcast sz $ op x, [])
      Call _ sz (Extern _ "msbit") es ps els            -> mkCall "msbit" es ps els $ \ [x] -> pure (wcast sz $ msbit' (argsSize ps) x, [])
      Call _ sz Id es ps els                            -> mkCall "id" es ps els $ \ xs -> pure (wcast sz $ mkConcat xs, [])
      Call _ sz (Const bv) es ps els                    -> mkCall "lit" es ps els $ \ _ -> pure (wcast sz $ bvToExp bv, [])
      Call an _ (Extern _ ex) _ _ _                     -> failAt an $ "ToVerilog: compileExp: unknown extern: " <> ex
      where mkCall :: (MonadState Fresh m, MonadWriter [Signal] m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
                    => Name -> [C.Exp] -> [Pat] -> [C.Exp] -> ([V.Exp] -> m (V.Exp, [Stmt])) -> m (V.Exp, [Stmt])
            mkCall s es ps els f = do
                  (es', stmts)   <- compileExps flags lvars es
                  (els', stmts') <- compileExps flags lvars els
                  let (es'', els'') = (mkConcat es', mkConcat els')
                  case litVal es'' of
                        Just bv -> do
                              (fes, fstmts) <- f $ patApplyLit bv ps
                              pure (if patMatchesLit bv ps then fes else els'', stmts <> stmts' <> fstmts)
                        _      -> do
                              Name n <- newWire (sum $ map sizeOf es) s
                              (fes, fstmts) <- f $ map LVal $ patApply n ps
                              pure  ( case patMatches n ps of
                                          cond | litTrue cond || null els' -> fes
                                               | litFalse cond             -> els''
                                               | otherwise                 -> Cond cond fes els''
                                    , stmts <> stmts' <> [ Assign (Name n) es'' ] <> fstmts
                                    )

            litTrue :: V.Exp -> Bool
            litTrue e = case litVal e of
                  Just v  -> v /= zeros 1
                  Nothing -> False

            litFalse :: V.Exp -> Bool
            litFalse e = case litVal e of
                  Just v  -> v == zeros 1
                  Nothing -> False

            litVal :: V.Exp -> Maybe BV
            litVal = \ case
                  LitBits b -> Just b
                  _         -> Nothing

            lkupLVal :: LId -> Maybe V.Exp
            lkupLVal = flip lookup (zip [0::LId ..] lvars)

            msbit' :: Size -> V.Exp -> V.Exp
            msbit' sz = \ case
                  LVal (Range n _ j)     -> LVal (Range n j j)
                  LVal (Name n)          -> LVal (Element n $ fromIntegral sz - 1)
                  LitBits bv | msb bv    -> LitBits $ ones 1
                             | otherwise -> LitBits $ zeros 1
                  e                      -> e -- TODO(chathhorn): kludgy.

-- | Attempt to break up giant literals.
bvToExp :: BV -> V.Exp
bvToExp bv | width bv < maxLit      = LitBits bv
           | bv == zeros 1          = Repl (fromIntegral $ width bv) $ LitBits (zeros 1)
           | bv == ones (width bv)  = Repl (fromIntegral $ width bv) $ LitBits (ones 1)
           | zs > 8                 = Concat [LitBits $ subRange (fromIntegral zs, width bv - 1) bv, Repl zs $ LitBits $ zeros 1]
           | otherwise              = LitBits bv -- TODO(chathhorn)
      where zs :: Size -- trailing zeros
            zs | bv == zeros 1 = fromIntegral $ width bv
               | otherwise     = fromIntegral $ lsb1 bv

            maxLit :: Int
            maxLit = 32

mkConcat :: [V.Exp] -> V.Exp
mkConcat = \ case
      [e] -> e
      es  -> Concat $ es

wcast :: Size -> V.Exp -> V.Exp
wcast sz e = case expWidth e of
      Just sz' | sz == sz' -> e
      _                    -> WCast sz e

binOp :: Name -> Maybe (V.Exp -> V.Exp -> V.Exp)
binOp = flip lookup primBinOps

unOp :: Name -> Maybe (V.Exp -> V.Exp)
unOp = flip lookup primUnOps

primBinOps :: [(Name, V.Exp -> V.Exp -> V.Exp)]
primBinOps =
      [ ( "+"      , Add)
      , ( "-"      , Sub)
      , ( "*"      , Mul)
      , ( "/"      , Div)
      , ( "%"      , Mod)
      , ( "**"     , Pow)
      , ( "&&"     , LAnd)
      , ( "||"     , LOr)
      , ( "&"      , And)
      , ( "|"      , Or)
      , ( "^"      , XOr)
      , ( "~^"     , XNor)
      , ( "<<"     , LShift)
      , ( ">>"     , RShift)
      , ( "<<<"    , LShiftArith)
      , ( ">>>"    , RShiftArith)
      , ( "concat" , (\ x y -> Concat [x, y]))
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
      tell [Logic sz n']
      pure $ Name n'

-- | Returns a boolean expression that is true when the pattern matches.
patMatches :: Name -> [Pat] -> V.Exp
patMatches x = foldr LAnd bTrue . patMatches' (\ i j bv -> [Eq (LVal $ Range x i j) $ LitBits bv])

patMatchesLit :: BV -> [Pat] -> Bool
patMatchesLit bv = foldr (&&) True . patMatches' (\ i j bv' -> [subRange (i, j) bv ==. bv'])

-- | Returns a list of ranges bound by pattern variables.
patApply :: Name -> [Pat] -> [LVal]
patApply x = patApply' (\ i j -> [Range x i j])

patApplyLit :: BV -> [Pat] -> [V.Exp]
patApplyLit bv = patApply' (\ i j -> [LitBits $ subRange (i, j) bv])

toSubRanges :: Name -> [Size] -> [LVal]
toSubRanges n szs = patApply n (map (PatVar noAnn) szs)

argsSize :: [Pat] -> Size
argsSize = sum . map patToSize
      where patToSize :: Pat -> Size
            patToSize = \ case
                  PatVar _ sz -> sz
                  _           -> 0

toInput :: Text -> Size -> Port
toInput n sz = Input $ Logic sz n
