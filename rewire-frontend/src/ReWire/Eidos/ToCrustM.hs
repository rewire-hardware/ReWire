{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The machine-level adapter (throwaway; dies with the retained Hyle
--   translation): lowers a procified program onto the retained pipeline
--   in the retired purifier's output shape, so the machine path owns the
--   output while ToHyle and the back passes stay untouched.
--
--   The emitted shape (matching ReWire.Crust.Purify's contract):
--
--   * @start = unfold $Pure.dispatch $Pure.start@ (NOINLINE);
--   * @$Pure.dispatch :: (R_, s1, ..., sm) -> i -> PuRe (s̄) o@, one case
--     arm per pause-target block:
--     @dispatch (R_L e̅, s̅) i = L_pure e̅ i s̅@;
--   * @data R_@: one constructor per pause-target block, payload = the
--     block's parameters minus the last (the resumed input); @data A_@:
--     one constructor per halt-answer type;
--   * a pure function per block (stores threaded as trailing
--     parameters): commands become lets (a cell read is the current
--     store variable; a cell write rebinds it), @goto@ is a direct call
--     (acyclic by signal-guardedness), @pause o -> L (a̅)@ becomes
--     @Pause (o, R_L a̅, s̅)@, @halt a@ becomes @Done (A_ a, s̅)@ — flat
--     tuples, as the purifier built them;
--   * @$Pure.start :: (S1, (..., Sm)) -> PuRe (s̄) o@ destructures the
--     (dead) initial-store tuple and runs the entry block; ToHyle's
--     existing initials evaluation consumes it unchanged — the entry's
--     cell writes before its first pause are the register initials, so
--     no adapter-side evaluator exists.
module ReWire.Eidos.ToCrustM (eidosToCrustM) where

import ReWire.Annotation (noAnn, unAnn)
import ReWire.Builtins (Builtin (Unfold))
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Crust.Types (tupleTy, mkArrowTy, poly, nilTy)
import ReWire.Crust.Util (mkApp, mkError, mkTuple, mkTuplePat)
import ReWire.Eidos.Syntax
import ReWire.Eidos.ToCrust (eidosToCrust, lowerExp, lowerTy, Locals, bindLocals)
import ReWire.Eidos.Types (typeOf, reacOrStateT)
import ReWire.Pretty (showt)
import ReWire.Unbound (Fresh, fresh, s2n, bind, Embed (..), Name)

import qualified ReWire.Crust.Syntax as M

import Data.Text (Text)

import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap.Strict  as IM
import qualified Data.IntSet         as IS

-- | Lower a procified program: the pure fragment through the ordinary
--   shim, the process through the machine adapter.
eidosToCrustM :: forall m. (Fresh m, MonadError AstError m) => Text -> Program -> m M.FreeProgram
eidosToCrustM startName (Program datas defns procs top) = case procs of
      [pr] -> do
            let pureDefns = [ d | d <- defns
                                , not (reacOrStateT $ sigTy $ idSig $ defnId d)
                                  || not (null $ sigTVs $ idSig $ defnId d) ]
            (ts, syns, ds)     <- eidosToCrust $ Program datas pureDefns [] top
            (mdatas, mdefns)   <- machineOf startName tops pr
            let ts' = filter (\ d -> M.dataName d `notElem` map M.dataName mdatas) ts
            pure (ts' <> mdatas, syns, ds <> mdefns)
      _    -> failAt noAnn "machine adapter: expected exactly one process."
      where tops :: IS.IntSet
            tops = IS.fromList $ map (idUniq . defnId) defns

machineOf :: forall m. (Fresh m, MonadError AstError m) => Text -> IS.IntSet -> Proc -> m ([M.DataDefn], [M.Defn])
machineOf startName tops pr = do
      -- One Crust name per block, and the pause-target inventory.
      let blocks   = procBlocks pr
          pauseTgs = IS.fromList $ concatMap (ptOf . blkTerm) $ procEntry pr : map snd blocks
          isPause l = IS.member (idUniq l) pauseTgs
          i'       = lowerTy $ procInTy pr
          o'       = lowerTy $ procOutTy pr
          ms       = map (lowerTy . cellTy) $ procCells pr
          cellIx   = Map.fromList $ zip (map cellName $ procCells pr) [0 :: Int ..]
          rangeTy  = M.TyApp noAnn (M.TyApp noAnn (M.TyCon noAnn $ s2n "PuRe") (tupleTy noAnn ms)) o'

          -- The R_ constructor for a pause target: payload = params minus input.
          rCtorName l = s2n $ "R_" <> idOcc l
          payloadTys b = map (lowerTy . sigTy . idSig) $ init' $ blkParams b
          init' xs = if null xs then [] else init xs

          rTy = M.TyCon noAnn $ s2n "R_"
          aTy = M.TyCon noAnn $ s2n "A_"

          rData = M.DataDefn noAnn (s2n "R_") M.KStar
                [ M.DataCon noAnn (rCtorName l) $ Embed $ poly [] $ mkArrowTy (payloadTys b) rTy
                | (l, b) <- blocks, isPause l ]

          -- Halt answers: one A_ constructor per (lowered) answer type.
          haltTys = dedup [ lowerTy $ typeOf a | b <- procEntry pr : map snd blocks, a <- haltsOf (blkTerm b) ]
          aCtors  = Map.fromList $ zip (map (prettyKey) haltTys) [ (s2n $ "A_halt" <> showT n, t) | (n, t) <- zip [0 :: Int ..] haltTys ]
          aData   = M.DataDefn noAnn (s2n "A_") M.KStar
                [ M.DataCon noAnn c $ Embed $ poly [] $ mkArrowTy [t] aTy | (c, t) <- Map.elems aCtors ]

          blockFnName l = s2n $ idOcc l
          blockFnTy b   = mkArrowTy (map (lowerTy . sigTy . idSig) (blkParams b) <> ms) rangeTy

          env = Env { envTops = tops, envMs = ms, envI = i', envO = o', envRange = rangeTy
                    , envCellIx = cellIx, envACtors = Map.map fst aCtors, envATys = Map.map snd aCtors
                    , envBlockTy = Map.fromList [ (idUniq l, blockFnTy b) | (l, b) <- blocks ]
                    , envBlockNm = Map.fromList [ (idUniq l, blockFnName l) | (l, _) <- blocks ]
                    , envRCtor   = Map.fromList [ (idUniq l, (rCtorName l, payloadTys b)) | (l, b) <- blocks, isPause l ]
                    , envRTy = rTy, envATy = aTy
                    }

      blockDefns <- mapM (blockDefn env) blocks
      dispatch   <- dispatchDefn env [ (l, b) | (l, b) <- blocks, isPause l ]
      startPure  <- startPureDefn env $ procEntry pr
      let startD = mkStartDefn startName env
      pure ([rData, aData], startD : dispatch : startPure : blockDefns)
      where ptOf :: Term -> [Uniq]
            ptOf = \ case
                  Pause _ _ l _  -> [idUniq l]
                  TCase _ _ alts -> concat [ ptOf t | TAlt _ _ _ t <- alts ]
                  _              -> []

            haltsOf :: Term -> [Exp]
            haltsOf = \ case
                  Halt _ a       -> [a]
                  TCase _ _ alts -> concat [ haltsOf t | TAlt _ _ _ t <- alts ]
                  _              -> []

            dedup :: [M.Ty] -> [M.Ty]
            dedup = go []
                  where go acc []       = reverse acc
                        go acc (t : ts) | prettyKey t `elem` map prettyKey acc = go acc ts
                                        | otherwise                            = go (t : acc) ts

data Env = Env
      { envTops    :: IS.IntSet
      , envMs      :: [M.Ty]
      , envI       :: M.Ty
      , envO       :: M.Ty
      , envRange   :: M.Ty
      , envCellIx  :: Map.HashMap Text Int
      , envACtors  :: Map.HashMap Text (Name M.DataConId)
      , envATys    :: Map.HashMap Text M.Ty
      , envBlockTy :: Map.HashMap Uniq M.Ty
      , envBlockNm :: Map.HashMap Uniq (Name M.Exp)
      , envRCtor   :: Map.HashMap Uniq (Name M.DataConId, [M.Ty])
      , envRTy     :: M.Ty
      , envATy     :: M.Ty
      }

-- | A structural key for the small halt-answer type set (annotations
--   stripped so identical types at different sites agree).
prettyKey :: M.Ty -> Text
prettyKey = showt . unAnn

showT :: Int -> Text
showT = showt

-- | One pure function per block: parameters, then the stores.
blockDefn :: forall m. (Fresh m, MonadError AstError m) => Env -> (Id, Block) -> m M.Defn
blockDefn env (l, b) = do
      (pns, lcl) <- bindLocals noAnn mempty $ blkParams b
      (sns, stores) <- storeParams env
      body <- lowerBlockBody env lcl stores b
      let lam = foldr (\ (n, t) e -> M.Lam noAnn Nothing (Just t) $ bind n e) body
                  $ zip pns (map (lowerTy . sigTy . idSig) $ blkParams b) <> zip sns (envMs env)
      pure M.Defn
            { M.defnAnnote = blkAnnote b
            , M.defnName   = Map.lookupDefault (s2n "$machine.block") (idUniq l) $ envBlockNm env
            , M.defnPolyTy = Embed $ poly [] $ Map.lookupDefault (envRange env) (idUniq l) $ envBlockTy env
            , M.defnAttr   = Just M.NoInline
            , M.defnBody   = Embed $ bind [] lam
            }

storeParams :: Fresh m => Env -> m ([Name M.Exp], [M.Exp])
storeParams env = do
      sns <- mapM (\ (n :: Int) -> fresh $ s2n $ "s" <> showT n) [0 .. length (envMs env) - 1]
      pure (sns, [ M.Var noAnn Nothing (Just t) n | (n, t) <- zip sns (envMs env) ])

-- | Lower commands (stores threaded) and the terminator.
lowerBlockBody :: forall m. (Fresh m, MonadError AstError m) => Env -> Locals -> [M.Exp] -> Block -> m M.Exp
lowerBlockBody env lcl0 stores0 b = go lcl0 stores0 $ blkCmds b
      where go :: Locals -> [M.Exp] -> [Cmd] -> m M.Exp
            go lcl stores = \ case
                  [] -> lowerTerm env lcl stores $ blkTerm b
                  CmdBind _ x r : rest -> do
                        r'          <- lowerExp (envTops env) lcl r
                        (ns, lcl')  <- bindLocals noAnn lcl [x]
                        body        <- go lcl' stores rest
                        case ns of
                              [n] -> pure $ mkLet (lowerTy $ sigTy $ idSig x) (envRange env) n r' body
                              _   -> failAt noAnn "machine adapter: bindLocals arity (rwc bug)"
                  CmdGet _ x c : rest -> do
                        ix <- cellIdx env c
                        go (IM.insert (idUniq x) (stores !! ix) lcl) stores rest
                  CmdPut _ c v : rest -> do
                        ix <- cellIdx env c
                        v' <- lowerExp (envTops env) lcl v
                        n  <- fresh $ s2n "s'"
                        let t       = envMs env !! ix
                            stores' = take ix stores <> [M.Var noAnn Nothing (Just t) n] <> drop (ix + 1) stores
                        body <- go lcl stores' rest
                        pure $ mkLet t (envRange env) n v' body

            cellIdx :: Env -> Text -> m Int
            cellIdx e c = maybe (failAt noAnn $ "machine adapter: unknown cell " <> c) pure
                  $ Map.lookup c $ envCellIx e

mkLet :: M.Ty -> M.Ty -> Name M.Exp -> M.Exp -> M.Exp -> M.Exp
mkLet tx tbody n rhs body =
      M.App noAnn Nothing (Just tbody) (M.Lam noAnn Nothing (Just tx) $ bind n body) rhs

lowerTerm :: forall m. (Fresh m, MonadError AstError m) => Env -> Locals -> [M.Exp] -> Term -> m M.Exp
lowerTerm env lcl stores = \ case
      Goto _ l as -> do
            as' <- mapM (lowerExp (envTops env) lcl) as
            let nm = Map.lookupDefault (s2n "$machine.block") (idUniq l) $ envBlockNm env
                ty = Map.lookupDefault (envRange env) (idUniq l) $ envBlockTy env
            pure $ mkApp noAnn (M.Var noAnn Nothing (Just ty) nm) $ as' <> stores
      Pause _ o l as -> do
            o'  <- lowerExp (envTops env) lcl o
            as' <- mapM (lowerExp (envTops env) lcl) as
            (rc, ptys) <- maybe (failAt noAnn "machine adapter: pause to a non-state block (rwc bug)") pure
                  $ Map.lookup (idUniq l) $ envRCtor env
            let rApp = mkApp noAnn (M.Con noAnn Nothing (Just $ mkArrowTy ptys $ envRTy env) rc) as'
                tup  = mkTuple noAnn $ o' : rApp : stores
            pure $ conApp "Pause" (envO env : envRTy env : envMs env) tup
      Halt _ a -> do
            a' <- lowerExp (envTops env) lcl a
            let k = prettyKey $ lowerTy $ typeOf a
            ac <- maybe (failAt noAnn "machine adapter: unregistered halt answer type (rwc bug)") pure
                  $ Map.lookup k $ envACtors env
            at <- maybe (failAt noAnn "machine adapter: unregistered halt answer type (rwc bug)") pure
                  $ Map.lookup k $ envATys env
            let aApp = mkApp noAnn (M.Con noAnn Nothing (Just $ mkArrowTy [at] $ envATy env) ac) [a']
            pure $ conApp "Done" (envATy env : envMs env) $ mkTuple noAnn $ aApp : stores
      TCase _ a alts -> do
            a' <- lowerExp (envTops env) lcl a
            let scrutTy = lowerTy $ typeOf a
            cascade a' scrutTy alts
      where conApp :: Text -> [M.Ty] -> M.Exp -> M.Exp
            conApp c comps tup = mkApp noAnn (M.Con noAnn Nothing (Just $ mkArrowTy [tupleTy noAnn comps] $ envRange env) (s2n c)) [tup]

            cascade :: M.Exp -> M.Ty -> [TAlt] -> m M.Exp
            cascade scrut scrutTy alts0 = case alts0 of
                  TAlt _ DefaultAlt _ t : rest -> do
                        d <- lowerTerm env lcl stores t
                        arms rest $ Just d
                  rest -> arms rest Nothing
                  where arms :: [TAlt] -> Maybe M.Exp -> m M.Exp
                        arms [] (Just d) = pure d
                        arms [] Nothing  = pure $ mkError noAnn (Just $ envRange env) "Pattern match failure: non-exhaustive patterns in case"
                        arms (TAlt _ (DataAlt c) xs t : rest) melse = do
                              (pns, lcl') <- bindLocals noAnn lcl xs
                              let pat = M.PatCon noAnn (Embed Nothing) (Embed $ Just scrutTy) (Embed $ s2n c)
                                          [ M.PatVar noAnn (Embed Nothing) (Embed $ Just $ lowerTy $ sigTy $ idSig x) n
                                          | (x, n) <- zip xs pns ]
                              body  <- lowerTermWith lcl' t
                              rest' <- case rest of
                                    [] -> pure melse
                                    _  -> Just <$> arms rest melse
                              pure $ M.Case noAnn Nothing (Just $ envRange env) scrut (bind pat body) rest'
                        arms (TAlt an' (LitAlt _) _ _ : _) _ =
                              failAt an' "machine adapter: literal terminator alternative is not supported."
                        arms (TAlt an' DefaultAlt _ _ : _) _ =
                              failAt an' "the default terminator alternative must come first"

                        lowerTermWith :: Locals -> Term -> m M.Exp
                        lowerTermWith l' = lowerTerm env l' stores

-- | @dispatch (R_L e̅, s̅) i = L_pure e̅ i s̅@ for each pause target.
dispatchDefn :: forall m. (Fresh m, MonadError AstError m) => Env -> [(Id, Block)] -> m M.Defn
dispatchDefn env pblocks = do
      disc <- fresh $ s2n "disc"
      iv   <- fresh $ s2n "i"
      let domTy  = tupleTy noAnn $ envRTy env : envMs env
          dispTy = mkArrowTy [domTy, envI env] $ envRange env
          discV  = M.Var noAnn Nothing (Just domTy) disc
          ivV    = M.Var noAnn Nothing (Just $ envI env) iv
      body <- arms discV ivV pblocks
      pure M.Defn
            { M.defnAnnote = noAnn
            , M.defnName   = s2n "$Pure.dispatch"
            , M.defnPolyTy = Embed $ poly [] dispTy
            , M.defnAttr   = Just M.NoInline
            , M.defnBody   = Embed $ bind []
                  $ M.Lam noAnn Nothing (Just domTy)
                  $ bind disc $ M.Lam noAnn Nothing (Just $ envI env) $ bind iv body
            }
      where arms :: M.Exp -> M.Exp -> [(Id, Block)] -> m M.Exp
            arms _ _ [] = failAt noAnn "empty dispatch: invalid ReWire (is recursion guarded by signal?)"
            arms discV ivV ((l, _) : rest) = do
                  (rc, ptys) <- maybe (failAt noAnn "machine adapter: missing R_ constructor (rwc bug)") pure
                        $ Map.lookup (idUniq l) $ envRCtor env
                  pns <- mapM (const $ fresh $ s2n "e") ptys
                  sns <- mapM (const $ fresh $ s2n "s") $ envMs env
                  let rPat  = M.PatCon noAnn (Embed Nothing) (Embed $ Just $ envRTy env) (Embed rc)
                                    [ M.PatVar noAnn (Embed Nothing) (Embed $ Just t) n | (n, t) <- zip pns ptys ]
                      pat   = mkTuplePat noAnn $ rPat : [ M.PatVar noAnn (Embed Nothing) (Embed $ Just t) n | (n, t) <- zip sns (envMs env) ]
                      nm    = Map.lookupDefault (s2n "$machine.block") (idUniq l) $ envBlockNm env
                      ty    = Map.lookupDefault (envRange env) (idUniq l) $ envBlockTy env
                      call  = mkApp noAnn (M.Var noAnn Nothing (Just ty) nm)
                                    $ [ M.Var noAnn Nothing (Just t) n | (n, t) <- zip pns ptys ]
                                   <> [ivV]
                                   <> [ M.Var noAnn Nothing (Just t) n | (n, t) <- zip sns (envMs env) ]
                  rest' <- case rest of
                        [] -> pure Nothing
                        _  -> Just <$> arms discV ivV rest
                  pure $ M.Case noAnn Nothing (Just $ envRange env) discV (bind pat call) rest'

-- | @$Pure.start@: destructure the (dead) initial-store tuple, run entry.
startPureDefn :: forall m. (Fresh m, MonadError AstError m) => Env -> Block -> m M.Defn
startPureDefn env entry = do
      s0  <- fresh $ s2n "s0"
      sns <- mapM (const $ fresh $ s2n "s") $ envMs env
      let state0Ty = tupleTy noAnn $ envMs env
          ty       = mkArrowTy [state0Ty] $ envRange env
          stores   = [ M.Var noAnn Nothing (Just t) n | (n, t) <- zip sns (envMs env) ]
          pat      = mkTuplePat noAnn [ M.PatVar noAnn (Embed Nothing) (Embed $ Just t) n | (n, t) <- zip sns (envMs env) ]
      body <- lowerBlockBody env mempty stores entry
      pure M.Defn
            { M.defnAnnote = noAnn
            , M.defnName   = s2n "$Pure.start"
            , M.defnPolyTy = Embed $ poly [] ty
            , M.defnAttr   = Just M.NoInline
            , M.defnBody   = Embed $ bind []
                  $ M.Lam noAnn Nothing (Just state0Ty)
                  $ bind s0 $ M.Case noAnn Nothing (Just $ envRange env)
                        (M.Var noAnn Nothing (Just state0Ty) s0) (bind pat body) Nothing
            }

-- | @start = unfold $Pure.dispatch $Pure.start@.
mkStartDefn :: Text -> Env -> M.Defn
mkStartDefn startName env = M.Defn
      { M.defnAnnote = noAnn
      , M.defnName   = s2n startName
      , M.defnPolyTy = Embed $ poly [] startTy
      , M.defnAttr   = Just M.NoInline
      , M.defnBody   = Embed $ bind [] appl
      }
      where domTy    = tupleTy noAnn $ envRTy env : envMs env
            dispTy   = mkArrowTy [domTy, envI env] $ envRange env
            state0Ty = tupleTy noAnn $ envMs env
            s0Ty     = mkArrowTy [state0Ty] $ envRange env
            reacT a  = foldl (M.TyApp noAnn) (M.TyCon noAnn $ s2n "ReacT") [envI env, envO env, M.TyCon noAnn $ s2n "Identity", a]
            startTy  = reacT nilTy
            unfoldE  = M.Builtin noAnn Nothing (Just $ mkArrowTy [dispTy, s0Ty] startTy) Unfold
            appl     = mkApp noAnn unfoldE
                  [ M.Var noAnn Nothing (Just dispTy) $ s2n "$Pure.dispatch"
                  , M.Var noAnn Nothing (Just s0Ty) $ s2n "$Pure.start"
                  ]
