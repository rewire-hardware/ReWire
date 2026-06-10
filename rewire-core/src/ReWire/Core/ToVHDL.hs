{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | The VHDL backend first compiles Core to the Verilog AST with
--   ReWire.Core.ToVerilog, then translates that AST to VHDL-2008, so the two
--   backends agree behaviorally by construction. Verilog expression semantics
--   (unsigned operations, self-determined width rules, assignment
--   truncation/extension) are implemented by the rw_helpers package emitted
--   with every design (see ReWire.VHDL.Syntax).
module ReWire.Core.ToVHDL (compileProgram) where

import ReWire.Annotation (noAnn)
import ReWire.BitVector (BV, width, nat)
import ReWire.Config (Config, vhdlPackages)
import ReWire.Core.Interp (subRange)
import ReWire.Error (failAt, AstError, MonadError)
import ReWire.Pretty (prettyPrint, showt)

import qualified ReWire.BitVector      as BV
import qualified ReWire.Core.Syntax    as C
import qualified ReWire.Core.ToVerilog as Verilog
import qualified ReWire.Verilog.Syntax as V
import qualified ReWire.VHDL.Syntax    as H

import Control.Lens ((^.))
import Control.Monad (unless, zipWithM)
import Control.Monad.State (StateT, runStateT, modify, gets)
import Data.HashMap.Strict (HashMap)
import Data.List (sortOn)
import Data.Text (Text)

import qualified Data.HashMap.Strict as Map

compileProgram :: (MonadFail m, MonadError AstError m) => Config -> C.Device -> m H.Device
compileProgram conf dev = do
      vdev <- Verilog.compileProgram conf dev
      transDevice conf (externSigs dev) vdev

-- | Extern signatures from the Core program, needed to generate component
--   declarations for instantiations of external modules.
externSigs :: C.Device -> HashMap Text C.ExternSig
externSigs (C.Device _ _ loop state0 ds) = foldr (exps . C.defnBody) mempty (loop : state0 : ds)
      where exps :: C.Exp -> HashMap Text C.ExternSig -> HashMap Text C.ExternSig
            exps = \ case
                  C.Call _ _ (C.Extern sig ex _) e _ els -> Map.insert ex sig . exps e . exps els
                  C.Call _ _ _ e _ els                   -> exps e . exps els
                  C.Concat _ e1 e2                       -> exps e1 . exps e2
                  _                                      -> id

type PortInfo = (Text, H.Direction, H.Size)

data TS = TS
      { tsCtr    :: !Int
      , tsExtras :: ![H.Signal]                 -- ^ Temporaries introduced by the translation.
      , tsStmts  :: ![H.Stmt]                   -- ^ Concurrent assignments to those temporaries.
      , tsComps  :: !(HashMap Text H.Component) -- ^ Component declarations needed for instantiations.
      , tsInits  :: !(HashMap Text BV)          -- ^ Signal initial values (from Verilog initial blocks).
      , tsWidths :: !(HashMap Text H.Size)
      }

type TM m = StateT TS m

transDevice :: (MonadFail m, MonadError AstError m) => Config -> HashMap Text C.ExternSig -> V.Device -> m H.Device
transDevice conf exts (V.Device mods) = H.Device (conf^.vhdlPackages) <$> mapM (transModule exts modPorts) mods
      where modPorts :: HashMap Text [PortInfo]
            modPorts = Map.fromList $ map (\ m -> (V.modName m, map portInfo $ V.modPorts m)) mods

portInfo :: V.Port -> PortInfo
portInfo = \ case
      V.Input s  -> (V.sigName s, H.In, sigSize s)
      V.InOut s  -> (V.sigName s, H.In, sigSize s) -- not generated
      V.Output s -> (V.sigName s, H.Out, sigSize s)

sigSize :: V.Signal -> H.Size
sigSize = \ case
      V.Wire  [sz] _ _ -> sz
      V.Logic [sz] _ _ -> sz
      V.Reg   [sz] _ _ -> sz
      _                -> 0

transModule :: (MonadFail m, MonadError AstError m) => HashMap Text C.ExternSig -> HashMap Text [PortInfo] -> V.Module -> m H.Unit
transModule exts modPorts (V.Module n ports sigs stmts) = do
      (stmts', ts) <- runStateT (concat <$> mapM transStmt stmts) ts0
      let sigs'     = map (toSignal $ tsInits ts) sigs <> tsExtras ts
      pure $ H.Unit n (map toPort ports') (sortOn compName $ Map.elems $ tsComps ts) sigs' (tsStmts ts <> stmts')
      where ports' :: [PortInfo]
            ports' = map portInfo ports

            ts0 :: TS
            ts0 = TS 0 [] [] mempty mempty widths

            widths :: HashMap Text H.Size
            widths = Map.fromList $ map (\ (n', _, sz) -> (n', sz)) ports' <> map (\ s -> (V.sigName s, sigSize s)) sigs

            toPort :: PortInfo -> H.Port
            toPort (n', d, sz) = H.Port n' d sz

            toSignal :: HashMap Text BV -> V.Signal -> H.Signal
            toSignal inits s = H.Signal (V.sigName s) (sigSize s) (Map.lookup (V.sigName s) inits)

            compName :: H.Component -> Text
            compName (H.Component cn _ _) = cn

            fresh :: Monad m => TM m Text
            fresh = do
                  ctr <- gets tsCtr
                  modify $ \ ts -> ts { tsCtr = ctr + 1 }
                  pure $ "rwtmp" <> showt ctr

            newTemp :: Monad m => H.Size -> TM m Text
            newTemp sz = do
                  t <- fresh
                  modify $ \ ts -> ts { tsExtras = tsExtras ts <> [H.Signal t sz Nothing]
                                      , tsWidths = Map.insert t sz $ tsWidths ts }
                  pure t

            lookupWidth :: MonadError AstError m => Text -> TM m H.Size
            lookupWidth n' = gets tsWidths >>= maybe err pure . Map.lookup n'
                  where err = failAt noAnn $ "ToVHDL: unknown signal (rwc bug): " <> n'

            -- | The translation of a Verilog statement; temporaries and their
            --   assignments go into the state.
            transStmt :: (MonadFail m, MonadError AstError m) => V.Stmt -> TM m [H.Stmt]
            transStmt = \ case
                  V.Assign lv e               -> transAssign lv e
                  V.Initial s                 -> transInitial s >> pure []
                  V.Always sens body          -> transAlways sens body
                  V.Instantiate g inst ps cs  -> transInstantiate g inst ps cs
                  s                           -> failAt noAnn $ "ToVHDL: unsupported top-level Verilog statement (rwc bug): " <> prettyPrint s

            -- | Continuous assignment; a concatenated target becomes
            --   assignments from slices of a temporary.
            transAssign :: (MonadFail m, MonadError AstError m) => V.LVal -> V.Exp -> TM m [H.Stmt]
            transAssign lv e = do
                  tgts <- flattenLV lv
                  e'   <- transExp e
                  case tgts of
                        [(t, w)] -> do
                              e'' <- resizeTo w e'
                              pure [H.Assign t e'']
                        _        -> do
                              let total = sum $ map snd tgts
                              tmp  <- newTemp total
                              e''  <- resizeTo total e'
                              pure $ H.Assign (H.LVName tmp) e'' : map (uncurry H.Assign) (sliceParts tmp total tgts)

            -- | Initial blocks become signal initialization values.
            transInitial :: (MonadFail m, MonadError AstError m) => V.Stmt -> TM m ()
            transInitial = \ case
                  V.SeqAssign lv e | Just bv <- expToBV e -> do
                        tgts <- flattenLV lv
                        unless (sum (map snd tgts) == fromIntegral (width bv)) $
                              failAt noAnn "ToVHDL: initial value width mismatch (rwc bug)."
                        mapM_ (uncurry initOf) $ splitBV bv tgts
                  s -> failAt noAnn $ "ToVHDL: unsupported initial statement (rwc bug): " <> prettyPrint s
                  where initOf :: MonadError AstError m => H.LVal -> BV -> TM m ()
                        initOf lv bv = case lv of
                              H.LVName n' -> modify $ \ ts -> ts { tsInits = Map.insert n' bv $ tsInits ts }
                              _           -> failAt noAnn "ToVHDL: unsupported initial target (rwc bug)."

            -- | Clocked processes. ToVerilog only emits a state-update process
            --   with an optional (a)synchronous reset; translate exactly those
            --   shapes.
            transAlways :: (MonadFail m, MonadError AstError m) => [V.Sensitivity] -> V.Stmt -> TM m [H.Stmt]
            transAlways sens body = case (sens, unBlock body) of
                  (V.Pos clk : _, [V.IfElse (V.Eq (V.LVal (V.Name rstn)) (V.LitBits bv)) thn els])
                        | rstn `elem` map sensName (drop 1 sens) -> do -- asynchronous reset
                              thn' <- seqAssigns $ unBlock thn
                              els' <- seqAssigns $ unBlock els
                              pure [H.Process (map sensName sens) [H.SIf [(H.CondEq rstn bv, thn'), (H.CondRising clk, els')] []]]
                        | otherwise -> do                              -- synchronous reset
                              thn' <- seqAssigns $ unBlock thn
                              els' <- seqAssigns $ unBlock els
                              pure [H.Process [clk] [H.SIf [(H.CondRising clk, [H.SIf [(H.CondEq rstn bv, thn')] els'])] []]]
                  (V.Pos clk : _, ss@(_ : _))                          -- no reset
                        -> do
                              ss' <- seqAssigns ss
                              pure [H.Process [clk] [H.SIf [(H.CondRising clk, ss')] []]]
                  _     -> failAt noAnn $ "ToVHDL: unsupported process shape (rwc bug): " <> prettyPrint body
                  where unBlock :: V.Stmt -> [V.Stmt]
                        unBlock = \ case
                              V.Block ss -> concatMap unBlock ss
                              s          -> [s]

                        sensName :: V.Sensitivity -> Text
                        sensName = \ case
                              V.Pos n' -> n'
                              V.Neg n' -> n'

            -- | Nonblocking assignments inside a process. No temporaries can
            --   be introduced here, so the source must be decomposable: a
            --   literal or a (concatenation of) signal references.
            seqAssigns :: (MonadFail m, MonadError AstError m) => [V.Stmt] -> TM m [H.SeqStmt]
            seqAssigns = fmap concat . mapM seqAssign
                  where seqAssign :: (MonadFail m, MonadError AstError m) => V.Stmt -> TM m [H.SeqStmt]
                        seqAssign = \ case
                              V.ParAssign lv e -> do
                                    tgts <- flattenLV lv
                                    case expToBV e of
                                          Just bv -> pure $ map (uncurry H.SAssign . fmap H.Lit) $ splitBV bv tgts
                                          Nothing -> case (tgts, e) of
                                                ([(t, w)], _) -> do
                                                      e'  <- transExp e
                                                      e'' <- resizeTo w e'
                                                      pure [H.SAssign t e'']
                                                (_, V.LVal src) -> do
                                                      srcs <- flattenLV src
                                                      unless (map snd srcs == map snd tgts) $
                                                            failAt noAnn "ToVHDL: mismatched register assignment shape (rwc bug)."
                                                      pure $ zipWith (\ (t, _) (s, _) -> H.SAssign t $ lvToExp s) tgts srcs
                                                _ -> failAt noAnn "ToVHDL: unsupported register assignment (rwc bug)."
                              s -> failAt noAnn $ "ToVHDL: unsupported statement in process (rwc bug): " <> prettyPrint s

            -- | Instantiation: hoist nontrivial input actuals into
            --   temporaries (signal references connect directly, in
            --   particular the clock and reset).
            transInstantiate :: (MonadFail m, MonadError AstError m) => Text -> Text -> [(Text, V.Exp)] -> [(Text, V.Exp)] -> TM m [H.Stmt]
            transInstantiate g inst params conns' = do
                  comp@(H.Component _ _ cports) <- componentFor g
                  modify $ \ ts -> ts { tsComps = Map.insert g comp $ tsComps ts }
                  params' <- mapM transParam params
                  -- VHDL forbids mixing positional and named associations, so
                  -- go all-positional if any connection is unnamed (the order
                  -- always matches the port order by construction).
                  let conns | any ((== "") . fst) conns' = map (("", ) . snd) conns'
                            | otherwise                  = conns'
                  conns'' <- zipWithM transConn (connPorts conns cports) conns
                  pure [H.Instantiate g inst params' conns'']
                  where transParam :: MonadError AstError m => (Text, V.Exp) -> TM m (Text, Integer)
                        transParam = \ case
                              (n', expToBV -> Just bv) -> pure (n', nat bv)
                              (n', _)                  -> failAt noAnn $ "ToVHDL: unsupported module parameter (rwc bug): " <> n'

                        -- | The port corresponding to each connection: by name
                        --   for named connections, by position otherwise.
                        connPorts :: [(Text, V.Exp)] -> [H.Port] -> [H.Port]
                        connPorts cs cports = zipWith conn' cs cports
                              where conn' :: (Text, V.Exp) -> H.Port -> H.Port
                                    conn' (n', _) p = case [p' | p'@(H.Port pn _ _) <- cports, pn == n'] of
                                          p' : _ -> p'
                                          _      -> p -- unnamed (positional) connection
                        transConn :: (MonadFail m, MonadError AstError m) => H.Port -> (Text, V.Exp) -> TM m (Text, H.Exp)
                        transConn (H.Port _ d psz) (cn, e) = do
                              e' <- transExp e
                              w  <- expWidth e'
                              case d of
                                    _ | trivial e', w == Just psz -> pure (cn, e')
                                    H.Out                         -> failAt noAnn "ToVHDL: unsupported output port connection (rwc bug)."
                                    _                             -> do
                                          tmp <- newTemp psz
                                          e'' <- resizeTo psz e'
                                          modify $ \ ts -> ts { tsStmts = tsStmts ts <> [H.Assign (H.LVName tmp) e''] }
                                          pure (cn, H.Var tmp)
                              where trivial :: H.Exp -> Bool
                                    trivial = \ case
                                          H.Var {}   -> True
                                          H.Slice {} -> True
                                          H.Elem {}  -> True
                                          _          -> False

            componentFor :: MonadError AstError m => Text -> TM m H.Component
            componentFor g
                  | Just ps <- Map.lookup g modPorts = pure $ H.Component g [] $ map (\ (n', d, sz) -> H.Port n' d sz) ps
                  | Just (C.ExternSig _ ps clk rst args res) <- Map.lookup g exts = pure
                        $ H.Component g (map fst ps)
                        $ nameAnon
                        $  [ H.Port clk H.In 1 | clk /= "" ]
                        <> [ H.Port rst H.In 1 | rst /= "" ]
                        <> map (\ (n', sz) -> H.Port n' H.In sz) args
                        <> map (\ (n', sz) -> H.Port n' H.Out sz) res
                  | otherwise = failAt noAnn $ "ToVHDL: could not find module or extern signature for: " <> g
                  where nameAnon :: [H.Port] -> [H.Port]
                        nameAnon = zipWith nameAnon' [0 :: Int ..]
                              where nameAnon' i = \ case
                                          H.Port "" d sz -> H.Port ("p" <> showt i) d sz
                                          p              -> p

            -- | MSB-first decomposition of a Verilog lvalue.
            flattenLV :: MonadError AstError m => V.LVal -> TM m [(H.LVal, H.Size)]
            flattenLV = \ case
                  V.Name n'      -> do
                        w <- lookupWidth n'
                        pure [(H.LVName n', w)]
                  V.Range n' i j -> pure [(H.LVRange n' i j, fromIntegral $ j - i + 1)]
                  V.Element n' i -> pure [(H.LVElem n' i, 1)]
                  V.LVals lvs    -> concat <$> mapM flattenLV lvs

            lvToExp :: H.LVal -> H.Exp
            lvToExp = \ case
                  H.LVName n'      -> H.Var n'
                  H.LVRange n' i j -> H.Slice n' i j
                  H.LVElem n' i    -> H.Elem n' i

            -- | Split a bitvector MSB-first across targets.
            splitBV :: BV -> [(a, H.Size)] -> [(a, BV)]
            splitBV bv tgts = snd $ foldl' split' (fromIntegral (width bv) - 1, []) tgts
                  where split' :: (Int, [(a, BV)]) -> (a, H.Size) -> (Int, [(a, BV)])
                        split' (hi, acc) (t, w) = (hi - fromIntegral w, acc <> [(t, subRange (hi - fromIntegral w + 1, hi) bv)])

            -- | MSB-first slices of a temporary covering the given widths.
            sliceParts :: Text -> H.Size -> [(H.LVal, H.Size)] -> [(H.LVal, H.Exp)]
            sliceParts tmp total tgts = snd $ foldl' part' (fromIntegral total - 1, []) tgts
                  where part' :: (Int, [(H.LVal, H.Exp)]) -> (H.LVal, H.Size) -> (Int, [(H.LVal, H.Exp)])
                        part' (hi, acc) (t, w) = (hi - fromIntegral w, acc <> [(t, H.Slice tmp (hi - fromIntegral w + 1) hi)])

            -- | Add an rw_resize wrapper unless the expression already
            --   statically has the desired width.
            resizeTo :: MonadError AstError m => H.Size -> H.Exp -> TM m H.Exp
            resizeTo w e = expWidth e >>= \ case
                  Just w' | w' == w -> pure e
                  _                 -> pure $ H.FunCall "rw_resize" [e, H.Num $ fromIntegral w]

            expWidth :: MonadError AstError m => H.Exp -> TM m (Maybe H.Size)
            expWidth = \ case
                  H.Lit bv        -> pure $ Just $ fromIntegral $ width bv
                  H.Var n'        -> Just <$> lookupWidth n'
                  H.Slice _ i j   -> pure $ Just $ fromIntegral $ j - i + 1
                  H.Elem _ _      -> pure $ Just 1
                  H.Cat es        -> fmap (fmap sum . sequence) $ mapM expWidth es
                  H.FunCall f [_, H.Num n'] | f == "rw_resize" -> pure $ Just $ fromIntegral n'
                  _               -> pure Nothing

            transExp :: (MonadFail m, MonadError AstError m) => V.Exp -> TM m H.Exp
            transExp = \ case
                  V.Add e1 e2         -> binop "rw_add"     e1 e2
                  V.Sub e1 e2         -> binop "rw_sub"     e1 e2
                  V.Mul e1 e2         -> binop "rw_mul"     e1 e2
                  V.Div e1 e2         -> binop "rw_div"     e1 e2
                  V.Mod e1 e2         -> binop "rw_mod"     e1 e2
                  V.Pow e1 e2         -> binop "rw_pow"     e1 e2
                  V.LAnd e1 e2        -> binop "rw_land"    e1 e2
                  V.LOr e1 e2         -> binop "rw_lor"     e1 e2
                  V.And e1 e2         -> binop "rw_and"     e1 e2
                  V.Or e1 e2          -> binop "rw_or"      e1 e2
                  V.XOr e1 e2         -> binop "rw_xor"     e1 e2
                  V.XNor e1 e2        -> binop "rw_xnor"    e1 e2
                  V.LShift e1 e2      -> binop "rw_shiftl"  e1 e2
                  V.RShift e1 e2      -> binop "rw_shiftr"  e1 e2
                  V.LShiftArith e1 e2 -> binop "rw_shiftl"  e1 e2
                  V.RShiftArith e1 e2 -> binop "rw_ashiftr" e1 e2
                  V.Not e             -> unop  "rw_not"     e
                  V.LNot e            -> unop  "rw_lnot"    e
                  V.RAnd e            -> unop  "rw_rand"    e
                  V.RNAnd e           -> unop  "rw_rnand"   e
                  V.ROr e             -> unop  "rw_ror"     e
                  V.RNor e            -> unop  "rw_rnor"    e
                  V.RXOr e            -> unop  "rw_rxor"    e
                  V.RXNor e           -> unop  "rw_rxnor"   e
                  V.Eq e1 e2          -> binop "rw_eq"      e1 e2
                  V.NEq e1 e2         -> binop "rw_neq"     e1 e2
                  V.Lt e1 e2          -> binop "rw_lt"      e1 e2
                  V.Gt e1 e2          -> binop "rw_gt"      e1 e2
                  V.LtEq e1 e2        -> binop "rw_lteq"    e1 e2
                  V.GtEq e1 e2        -> binop "rw_gteq"    e1 e2
                  V.Cond c e1 e2      -> do
                        c'  <- transExp c
                        e1' <- transExp e1
                        e2' <- transExp e2
                        pure $ H.FunCall "rw_cond" [c', e1', e2']
                  V.Concat es         -> H.Cat <$> mapM transExp es
                  V.Repl n' e
                        | Just bv <- expToBV n' -> do
                              e' <- transExp e
                              pure $ H.FunCall "rw_repl" [H.Num $ fromIntegral $ nat bv, e']
                        | otherwise -> failAt noAnn "ToVHDL: unsupported replication count (rwc bug)."
                  V.WCast sz e        -> transExp e >>= resizeTo sz
                  V.LitBits bv        -> pure $ H.Lit bv
                  V.LVal lv           -> flattenLV lv >>= \ case
                        [(lv', _)] -> pure $ lvToExp lv'
                        lvs        -> pure $ H.Cat $ map (lvToExp . fst) lvs
                  e                   -> failAt noAnn $ "ToVHDL: unsupported expression (rwc bug): " <> prettyPrint e
                  where binop :: (MonadFail m, MonadError AstError m) => Text -> V.Exp -> V.Exp -> TM m H.Exp
                        binop f e1 e2 = do
                              e1' <- transExp e1
                              e2' <- transExp e2
                              pure $ H.FunCall f [e1', e2']

                        unop :: (MonadFail m, MonadError AstError m) => Text -> V.Exp -> TM m H.Exp
                        unop f e = do
                              e' <- transExp e
                              pure $ H.FunCall f [e']

expToBV :: V.Exp -> Maybe BV
expToBV = \ case
      V.LitBits bv                 -> Just bv
      V.Repl (expToBV -> Just n) e -> BV.replicate (BV.nat n) <$> expToBV e
      V.Concat es                  -> BV.concat <$> mapM expToBV es
      _                            -> Nothing
