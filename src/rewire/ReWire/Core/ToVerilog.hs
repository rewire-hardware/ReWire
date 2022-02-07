{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Core.ToVerilog (compileProgram) where

import ReWire.Error
import ReWire.Flags (Flag (..))
import ReWire.Core.Syntax as C hiding (Name, Size, Index)
import ReWire.Verilog.Syntax as V
import ReWire.Core.Mangle (mangle)
import ReWire.Core.Interp
      ( patApply', patMatches', subRange
      , dispatchWires, pausePrefix, extraWires
      , resumptionSize
      , clk, rst
      )

import Data.Text (Text)
import Control.Monad (liftM2)
import Control.Monad.State (MonadState, get, runStateT, modify, gets)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Arrow ((&&&), first, second)
import TextShow (showt)
import Data.BitVector (width, bitVec, BV, zeros, ones, lsb1, (==.), (@.))
import qualified Data.BitVector as BV
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

type Fresh = Int
type DefnMap = HashMap GId [C.Exp]
type SigInfo = (Fresh, [Signal])

sigInfo0 :: SigInfo
sigInfo0 = (0, [])

fresh :: MonadState SigInfo m => Text -> m Name
fresh s = do
      (ctr, _) <- get
      modify $ first (+ 1)
      pure $ mangle s <> "_" <> showt ctr

compileProgram :: (MonadFail m, MonadError AstError m) => [Flag] -> C.Program -> m V.Program
compileProgram flags (C.Program st ds)
      | FlagFlatten `elem` flags = V.Program . pure <$> runReaderT (compileStartDefn flags st) defnMap
      | otherwise                = do
            st' <- runReaderT (compileStartDefn flags st) defnMap
            ds' <- mapM (compileDefn flags) $ filter ((/= getState0 st) . defnName) ds
            pure $ V.Program $ st' : ds'
      where defnMap :: DefnMap
            defnMap = Map.fromList $ map (defnName &&& defnBody) ds

            -- | Initial state should be inlined, so we can filter out its defn.
            getState0 :: StartDefn -> Name
            getState0 (C.StartDefn _ _ _ state0) = state0

compileStartDefn :: (MonadError AstError m, MonadFail m, MonadReader DefnMap m)
                 => [Flag] -> C.StartDefn -> m Module
compileStartDefn flags (C.StartDefn an w loop state0) = do

      ((rStart, ssStart), (fr, startSigs)) <- flip runStateT sigInfo0 $ compileCall (FlagFlatten : flags) state0 (resumptionSize w) []

      let (rStart', ssStart', fr', startSigs') = if clocked then (initState rStart, ssStart, fr, startSigs) else (Nothing, [], 0, [])

      ((rLoop, ssLoop),   (_, loopSigs))   <- flip runStateT (fr', []) $ compileCall flags loop (resumptionSize w)
            [ mkConcat $ map (LVal . Name . fst) $ dispatchWires w
            , mkConcat $ map (LVal . Name . fst) $ inputWires w
            ]
      pure $ Module "top_level" (inputs <> outputs) (loopSigs <> startSigs' <> sigs)
            $  ssStart'
            <> ssLoop
            <> [ Assign lvPause rLoop ]
            <> case rStart' of
                  Just init ->
                        [ Initial $ ParAssign lvCurrState init
                        , Always (map (Pos . fst) (clk flags) <> rstEdge) $ Block [ ifRst init ]
                        ]
                  _              -> [ ]
      where inputs :: [Port]
            inputs = map (Input . toLogic)  $ clk flags <> rst flags <> inputWires w

            outputs :: [Port]
            outputs = map (Output . toLogic) $ outputWires w

            sigs :: [Signal]
            sigs = map toLogic $ allWires w

            ifRst :: V.Exp -> Stmt
            ifRst init = case rst flags of
                  [(sRst, sz)] -> IfElse (Eq (LVal $ Name sRst) $ LitBits $ bitVec (fromIntegral sz) $ fromEnum $ not invertRst)
                        (Block [ ParAssign lvCurrState init ] )
                        (Block [ ParAssign lvCurrState $ LVal lvNxtState ])
                  _            -> ParAssign lvCurrState $ LVal lvNxtState

            clocked :: Bool
            clocked = FlagNoClock `notElem` flags

            -- | Initial/reset state.
            initState :: V.Exp -> Maybe V.Exp
            initState e = case (expToBV e, fromIntegral (sum $ snd <$> stateWires w)) of
                  (Just bv, n) | n > 0 -> pure $ bvToExp $ subRange (0, n - 1) bv
                  _                    -> Nothing

            lvPause :: LVal
            lvPause = mkLVals $ map (Name . fst) $ pauseWires w

            lvCurrState :: LVal
            lvCurrState = mkLVals $ map (Name . fst) $ stateWires w

            lvNxtState :: LVal
            lvNxtState = mkLVals $ map (Name . fst) $ nextStateWires w

            rstEdge :: [Sensitivity]
            rstEdge = case rst flags of
                  [(sRst, _)] | invertRst -> [Neg sRst]
                              | otherwise -> [Pos sRst]
                  _                       -> []

            invertRst :: Bool
            invertRst = FlagInvertReset `elem` flags

            pauseWires :: Wiring -> [(Name, Size)]
            pauseWires w = pausePrefix w <> nextStateWires w

            nextStateWires :: Wiring -> [(Name, Size)]
            nextStateWires w = map (first (<> "_next")) $ stateWires w

            allWires :: Wiring -> [(Name, Size)]
            allWires w = extraWires w <> stateWires w <> nextStateWires w

compileDefn :: (MonadFail m, MonadError AstError m) => [Flag] -> C.Defn -> m V.Module
compileDefn flags (C.Defn _ n (Sig _ inps outp) body) = do
      ((es, stmts), (_, sigs)) <- runStateT (runReaderT (compileExps flags (map (LVal . Name) argNames) body) Map.empty) sigInfo0
      pure $ V.Module (mangle n) (inputs <> outputs) sigs $ stmts <> [Assign (Name "res") $ mkConcat es]
      where argNames :: [Name]
            argNames = zipWith (\ _ x -> "arg" <> showt x) inps [0::Int ..]

            inputs :: [Port]
            inputs = map (Input . toLogic) (zip argNames inps)

            outputs :: [Port]
            outputs = map (Output . toLogic) [("res", outp)]

-- | Inlines a defn or instantiates an already-compiled defn.
compileCall :: (MonadState SigInfo m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
             => [Flag] -> GId -> V.Size -> [V.Exp] -> m (V.Exp, [Stmt])
compileCall flags g sz lvars
      | FlagFlatten `elem` flags = do
            Just body   <- asks (Map.lookup g)
            (es, stmts) <- compileExps flags lvars body
            es'         <- wcast sz $ mkConcat es
            pure (es', stmts)
      | otherwise = do
            mr          <- newWire sz "callRes"
            inst        <- Instantiate (mangle g) <$> fresh g <*> pure (lvars <> [LVal mr])
            pure (LVal mr, [inst])

compileExps :: (MonadState SigInfo m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
            => [Flag] -> [V.Exp] -> [C.Exp] -> m ([V.Exp], [Stmt])
compileExps flags lvars es = (map fst &&& concatMap snd) <$> mapM (compileExp flags lvars) es

compileExp :: (MonadState SigInfo m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
            => [Flag] -> [V.Exp] -> C.Exp -> m (V.Exp, [Stmt])
compileExp flags lvars = \ case
      LVar _  _ (lkupLVal -> Just x)                    -> pure (x, [])
      LVar an _ _                                       -> failAt an "ToVerilog: compileExp: encountered unknown LVar."
      Lit _ bv                                          -> pure (bvToExp bv, [])
      Call _ sz (Global g) es ps els                    -> mkCall "glob" es ps els $ compileCall flags g sz
      Call _ sz (Extern _ (binOp -> Just op)) es ps els -> mkCall "binOp" es ps els $ \ [x, y] -> (,[]) <$> wcast sz (op x y)
      Call _ sz (Extern _ (unOp -> Just op)) es ps els  -> mkCall "unOp" es ps els $ \ [x] -> (,[]) <$> wcast sz (op x)
      Call _ sz (Extern _ "msbit") es ps els            -> mkCall "msbit" es ps els $ \ [x] -> (,[]) <$> wcast sz (projBit (fromIntegral (argsSize ps) - 1) x)
      Call _ sz Id es ps els                            -> mkCall "id" es ps els $ \ xs -> (,[]) <$> wcast sz (mkConcat xs)
      Call _ sz (Const bv) es ps els                    -> mkCall "lit" es ps els $ \ _ -> (,[]) <$> wcast sz (bvToExp bv)
      Call an _ (Extern _ ex) _ _ _                     -> failAt an $ "ToVerilog: compileExp: unknown extern: " <> ex
      where mkCall :: (MonadState SigInfo m, MonadFail m, MonadError AstError m, MonadReader DefnMap m)
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

            projBit :: Index -> V.Exp -> V.Exp
            projBit ix = \ case
                  LVal (Range n i _)     -> LVal $ Element n $ ix + i
                  LVal (Name n)          -> LVal $ Element n ix
                  LitBits bv | bv @. ix  -> LitBits $ ones 1
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

expToBV :: V.Exp -> Maybe BV
expToBV = \ case
      LitBits bv -> Just bv
      Repl n e   -> BV.replicate n <$> expToBV e
      Concat es  -> BV.concat <$> mapM expToBV es
      _          -> Nothing

-- | Size of the inclusive range [i, j].
nbits :: Index -> Index -> Size
nbits i j = fromIntegral (j - i + 1)

mkConcat :: [V.Exp] -> V.Exp
mkConcat = \ case
      [e] -> e
      es  -> Concat es

mkLVals :: [V.LVal] -> V.LVal
mkLVals = \ case
      [e] -> e
      es  -> LVals es

mkRange :: Name -> Index -> Index -> V.LVal
mkRange n i j | i == j    = Element n i
              | otherwise = Range n i j

mkWCast :: Size -> V.Exp -> V.Exp
mkWCast sz = \ case
      LVal (Range n i j) | sz < nbits i j -> LVal $ mkRange n i (j - fromIntegral (nbits i j - sz))
      WCast _ e                           -> WCast sz e
      e                                   -> WCast sz e

-- | Recreates the effect of assignment by adding bitwidth casts to an expression:
-- > wcast s e
-- should return an e' that behaves the same as though e appeared as
-- > x = e
-- where x has a width of s. I.e., "wcast s e" fixes the "bitwidth context" for
-- the expression e to s.
wcast :: MonadState SigInfo m => Size -> V.Exp -> m V.Exp
wcast sz e = expWidth e >>= \ case
      Just sz' | sz < sz' -> trunc e
               | sz > sz' -> case e of
                  Add  e1 e2      -> Add         <$> pad e1 <*> pad e2
                  Sub  e1 e2      -> Sub         <$> pad e1 <*> pad e2
                  Mul  e1 e2      -> Mul         <$> pad e1 <*> pad e2
                  Div  e1 e2      -> Div         <$> pad e1 <*> pad e2
                  Mod  e1 e2      -> Mod         <$> pad e1 <*> pad e2
                  And  e1 e2      -> And         <$> pad e1 <*> pad e2
                  Or   e1 e2      -> Or          <$> pad e1 <*> pad e2
                  XOr  e1 e2      -> XOr         <$> pad e1 <*> pad e2
                  XNor e1 e2      -> XNor        <$> pad e1 <*> pad e2
                  Cond c e1 e2    -> Cond c      <$> pad e1 <*> pad e2
                  Pow         e n -> Pow         <$> pad e  <*> pure n
                  LShift      e n -> LShift      <$> pad e  <*> pure n
                  RShift      e n -> RShift      <$> pad e  <*> pure n
                  LShiftArith e n -> LShiftArith <$> pad e  <*> pure n
                  RShiftArith e n -> RShiftArith <$> pad e  <*> pure n
                  Not e           -> Not         <$> pad e
                  e               -> pad e
      _                   -> pure e
      where pad :: MonadState SigInfo m => V.Exp -> m V.Exp
            pad e = expWidth e >>= \ case
                        Just sz' | sz > sz' -> pure $ mkWCast sz e
                        _                   -> pure e
            trunc :: MonadState SigInfo m => V.Exp -> m V.Exp
            trunc e = expWidth e >>= \ case
                        Just sz' | sz < sz' -> pure $ mkWCast sz e
                        _                   -> pure e

-- Expression                     Bit Length         Notes
-- -----------------------------------------------------------------------------
-- Unsized constants              "Same as integer"  (see ** below)
-- Sized constants                As given
-- i [+ - * / % & | ^ ^~ ~^] j    max{L(i),L(j)}
-- [+ - ~] i                      L(i)
-- i [=== !== == != > >= < <=] j  1 bit              i,j sized to max(L(i),L(j))
-- i [&& ||] j                    1 bit              i,j self-determined
-- [& ~& | ~| ^ ~^ ^~ !] i        1 bit              i self-determined
-- i [>> << ** >>> <<<] j         L(i)               j self-determined
-- i ? j : k                      max(L(j),L(k))     i self-determined
-- {i, ..., j}                    L(i)+...+L(j)      all self-determined
-- {i {j, ..., k}}                i*(L(j)+...+L(k))  all self-determined
-- -----------------------------------------------------------------------------
--
expWidth :: MonadState SigInfo m => V.Exp -> m (Maybe Size)
expWidth = \ case
      Add    e1 e2    -> largest e1 e2
      Sub    e1 e2    -> largest e1 e2
      Mul    e1 e2    -> largest e1 e2
      Div    e1 e2    -> largest e1 e2
      Mod    e1 e2    -> largest e1 e2
      And    e1 e2    -> largest e1 e2
      Or     e1 e2    -> largest e1 e2
      XOr    e1 e2    -> largest e1 e2
      XNor   e1 e2    -> largest e1 e2
      Cond _ e1 e2    -> largest e1 e2
      Pow         e _ -> expWidth e
      LShift      e _ -> expWidth e
      RShift      e _ -> expWidth e
      LShiftArith e _ -> expWidth e
      RShiftArith e _ -> expWidth e
      Not         e   -> expWidth e
      LAnd _ _        -> pure $ Just 1
      LOr _ _         -> pure $ Just 1
      LNot _          -> pure $ Just 1
      RAnd _          -> pure $ Just 1
      RNAnd _         -> pure $ Just 1
      ROr _           -> pure $ Just 1
      RNor _          -> pure $ Just 1
      RXor _          -> pure $ Just 1
      RXNor _         -> pure $ Just 1
      Eq  _ _         -> pure $ Just 1
      NEq _ _         -> pure $ Just 1
      CEq _ _         -> pure $ Just 1
      CNEq _ _        -> pure $ Just 1
      Lt _ _          -> pure $ Just 1
      Gt _ _          -> pure $ Just 1
      LtEq _ _        -> pure $ Just 1
      GtEq _ _        -> pure $ Just 1
      Concat es       -> sumMaybes <$> mapM expWidth es
      Repl sz e       -> fmap (*sz) <$> expWidth e
      WCast sz _      -> pure $ Just sz
      LitBits bv      -> pure $ Just $ fromIntegral $ width bv
      LVal lv         -> lvalWidth lv
      where largest :: MonadState SigInfo m => V.Exp -> V.Exp -> m (Maybe Size)
            largest e1 e2 = liftM2 max <$> expWidth e1 <*> expWidth e2

            lvalWidth :: MonadState SigInfo m => LVal -> m (Maybe Size)
            lvalWidth = \ case
                  Element _ _ -> pure $ Just 1
                  Range _ i j -> pure $ Just $ fromIntegral $ nbits i j
                  Name n      -> lookupWidth n
                  LVals lvs   -> sumMaybes <$> mapM lvalWidth lvs

sumMaybes :: Num a => [Maybe a] -> Maybe a
sumMaybes = foldMaybes (+)

foldMaybes :: (a -> a -> a) -> [Maybe a] -> Maybe a
foldMaybes f = \ case
      a : b : ms -> foldMaybes f $ (f <$> a <*> b) : ms
      [a]        -> a
      _          -> Nothing

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
      , ( "concat" , \ x y -> Concat [x, y])
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

newWire :: MonadState SigInfo m => V.Size -> Name -> m LVal
newWire sz n = do
      n' <- fresh n
      modify $ second (++ [Logic sz n'])
      pure $ Name n'

lookupWidth :: MonadState SigInfo m => Name -> m (Maybe Size)
lookupWidth n = gets (lookup n . map proj <$> snd)
      where proj :: Signal -> (Name, Size)
            proj = \ case
                  Wire  sz n -> (n, sz)
                  Logic sz n -> (n, sz)
                  Reg   sz n -> (n, sz)

-- | Returns a boolean expression that is true when the pattern matches.
patMatches :: Name -> [Pat] -> V.Exp
patMatches x ps =  case patMatches' (\ i j bv -> [Eq (LVal $ mkRange x i j) $ LitBits bv]) ps of
      ps'@(_ : _) -> foldr1 LAnd ps'
      []          -> bTrue

patMatchesLit :: BV -> [Pat] -> Bool
patMatchesLit bv = and . patMatches' (\ i j bv' -> [subRange (i, j) bv ==. bv'])

-- | Returns a list of ranges bound by pattern variables.
patApply :: Name -> [Pat] -> [LVal]
patApply x = patApply' (\ i j -> [mkRange x i j])

patApplyLit :: BV -> [Pat] -> [V.Exp]
patApplyLit bv = patApply' (\ i j -> [LitBits $ subRange (i, j) bv])

argsSize :: [Pat] -> Size
argsSize = sum . map patToSize
      where patToSize :: Pat -> Size
            patToSize = \ case
                  PatVar _ sz -> sz
                  _           -> 0

toLogic :: (Name, Size) -> Signal
toLogic = uncurry $ flip Logic
