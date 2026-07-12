{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
-- | Well-formedness checking for Hyle programs (doc/hyle.md, section 4):
--   syntax-directed expression typing (every node's cached width is
--   verified), declaration well-formedness, device scoping and
--   exactly-once assignment coverage, and acyclicity of the call graph
--   (including extern-model edges).
module ReWire.Hyle.Check (check) where

import ReWire.Annotation (ann, Annote)
import ReWire.Error (failAt, failInternal, MonadError, AstError)
import ReWire.Hyle.Syntax
import ReWire.Pretty (showt)

import qualified ReWire.BitVector as BV

import Control.Monad (unless, when, foldM, foldM_, zipWithM_)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.List (sort, group)
import Data.Text (Text)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified Data.Text           as T

data Env = Env
      { envDefns   :: HashMap GId Defn
      , envExterns :: HashMap Name Extern
      }

type Ctx = HashMap Name Size

check :: MonadError AstError m => Program -> m Program
check p@(Program exts ds dev) = do
      checkDistinct (ann dev) "global name" $ map extName exts <> map defnName ds
      mapM_ (checkExtern env) exts
      mapM_ (checkDefn env) ds
      checkDevice env dev
      checkRecursion env ds
      pure p
      where env :: Env
            env = Env (Map.fromList $ map (\ d -> (defnName d, d)) ds)
                      (Map.fromList $ map (\ e -> (extName e, e)) exts)

checkDistinct :: MonadError AstError m => Annote -> Text -> [Name] -> m ()
checkDistinct an what xs = case filter ((> 1) . length) $ group $ sort xs of
      []           -> pure ()
      (x : _) : _  -> failInternal an $ "duplicate " <> what <> ": " <> x
      _            -> pure ()

---

checkExtern :: MonadError AstError m => Env -> Extern -> m ()
checkExtern env (Extern an n gs k ins outs m) = do
      checkDistinct an ("port or generic name of extern " <> n) $ gs <> map fst ins <> map fst outs
      mapM_ (checkPortName . fst) $ ins <> outs
      when (null outs) $ failInternal an $ "extern " <> n <> " has no outputs"
      case (k, m) of
            (Comb, Just g) -> case Map.lookup g $ envDefns env of
                  Nothing -> failInternal an $ "extern " <> n <> ": unknown model defn " <> g
                  Just d | Sig _ args res <- defnSig d -> do
                        unless (args == map snd ins) $
                              failInternal an $ "extern " <> n <> ": model " <> g <> ": argument widths do not match the extern's inputs"
                        unless (res == sum (map snd outs)) $
                              failInternal an $ "extern " <> n <> ": model " <> g <> ": result width does not match the extern's outputs"
            (Seq _ _, Just _) -> failInternal an $ "extern " <> n <> ": sequential externs cannot carry a model"
            _                 -> pure ()
      where checkPortName :: MonadError AstError m => Name -> m ()
            checkPortName x = when (T.null x) $
                  failInternal an $ "extern " <> n <> ": empty port name"

---

checkDefn :: MonadError AstError m => Env -> Defn -> m ()
checkDefn env (Defn an n (Sig _ args res) ps body _ _) = do
      unless (length ps == length args) $
            failInternal an $ n <> ": parameter count does not match signature"
      checkDistinct an ("parameter of " <> n) ps
      sz <- checkExp env (Map.fromList $ zip ps args) body
      unless (sz == res) $
            failInternal an $ n <> ": body width " <> showt sz <> " does not match declared result width " <> showt res

-- | Verifies every node bottom-up, including its cached width, and returns
--   the expression's width.
checkExp :: MonadError AstError m => Env -> Ctx -> Exp -> m Size
checkExp env = go
      where go :: MonadError AstError m => Ctx -> Exp -> m Size
            go ctx = \ case
                  Lit an bv -> do
                        when (BV.nat bv < 0 || BV.nat bv >= 2 ^ BV.width bv) $
                              failInternal an "literal value out of range for its width"
                        pure $ fromIntegral $ BV.width bv
                  Undef _ sz -> pure sz
                  Var an sz x -> case Map.lookup x ctx of
                        Just sz' | sz == sz' -> pure sz
                                 | otherwise -> failInternal an $ "variable " <> x <> ": cached width " <> showt sz <> " does not match its binding (" <> showt sz' <> ")"
                        Nothing              -> failInternal an $ "unbound variable: " <> x
                  Cat _ e1 e2 -> (+) <$> go ctx e1 <*> go ctx e2
                  Slice an i k e -> do
                        sz <- go ctx e
                        unless (fromIntegral i + k <= sz) $
                              failAt an $ "slice [" <> showt i <> " +: " <> showt k <> "] out of bounds for width " <> showt sz
                        pure k
                  Prim an sz op es -> do
                        szs <- mapM (go ctx) es
                        case opResultSize op szs of
                              Just sz' | sz == sz' -> pure sz
                                       | otherwise -> failInternal an $ opName op <> ": cached width mismatch"
                              Nothing              -> failInternal an $ "ill-typed application of " <> opName op <> " to operand widths " <> showt szs
                  Call an sz g es -> case Map.lookup g $ envDefns env of
                        Nothing -> failInternal an $ "call to unknown definition: " <> g
                        Just (defnSig -> Sig _ args res) -> do
                              szs <- mapM (go ctx) es
                              checkArgs an g args szs
                              unless (sz == res) $ failInternal an $ "call to " <> g <> ": cached width mismatch"
                              pure res
                  XCall an sz x cs es -> case Map.lookup x $ envExterns env of
                        Nothing -> failAt an $ "call to unknown extern: " <> x
                        Just ex -> do
                              unless (extKind ex == Comb) $
                                    failInternal an $ "extern " <> x <> " is sequential and cannot be called (instantiate it at device level)"
                              unless (length cs == length (extGenerics ex)) $
                                    failInternal an $ "extern " <> x <> ": expected " <> showt (length $ extGenerics ex) <> " generic arguments, got " <> showt (length cs)
                              szs <- mapM (go ctx) es
                              checkArgs an x (map snd $ extInputs ex) szs
                              unless (sz == externResultSize ex) $
                                    failInternal an $ "call to extern " <> x <> ": cached width mismatch"
                              pure sz
                  If an sz c t e -> do
                        szc <- go ctx c
                        unless (szc == 1) $ failInternal an $ "if condition has width " <> showt szc <> " (expected 1)"
                        szt <- go ctx t
                        sze <- go ctx e
                        unless (szt == sze) $ failInternal an $ "if branches have unequal widths (" <> showt szt <> " and " <> showt sze <> ")"
                        unless (sz == szt) $ failInternal an "if: cached width mismatch"
                        pure sz
                  Let an sz x e1 e2 -> do
                        sz1 <- go ctx e1
                        sz2 <- go (Map.insert x sz1 ctx) e2
                        unless (sz == sz2) $ failInternal an $ "let " <> x <> ": cached width mismatch"
                        pure sz

            checkArgs :: MonadError AstError m => Annote -> Name -> [Size] -> [Size] -> m ()
            checkArgs an who args szs = do
                  unless (length args == length szs) $
                        failInternal an $ "call to " <> who <> ": expected " <> showt (length args) <> " arguments, got " <> showt (length szs)
                  zipWithM_ (\ i (w, w') -> unless (w == w') $
                              failInternal an $ "call to " <> who <> ": argument " <> showt i <> " has width " <> showt w' <> " (expected " <> showt w <> ")")
                        [0 :: Int ..] (zip args szs)

---

checkDevice :: MonadError AstError m => Env -> Device -> m ()
checkDevice env (Device an n ins outs regs insts body _) = do
      checkDistinct an ("local name of device " <> n) locals
      mapM_ checkLocalName locals
      mapM_ checkRegister regs
      instCtx <- foldM checkInstance mempty insts
      let ambient = Map.fromList ins
                 <> Map.fromList (map (\ (Register _ x sz _) -> (x, sz)) regs)
                 <> instCtx
      (_, assigned) <- foldM checkStmt (ambient, mempty) body
      mapM_ (checkAssigned assigned "output" . fst) outs
      mapM_ (\ (Register _ x _ _) -> checkAssigned assigned "register" $ "next " <> x) regs
      mapM_ (checkInstAssigned assigned) insts
      where locals :: [Name]
            locals = map fst ins <> map fst outs <> map (\ (Register _ x _ _) -> x) regs
                  <> map (\ (Instance _ x _ _) -> x) insts <> [ x | SLet _ x _ <- body ]

            outsCtx :: Ctx
            outsCtx = Map.fromList outs

            checkLocalName :: MonadError AstError m => Name -> m ()
            checkLocalName x = do
                  when (T.null x) $ failInternal an "empty device-local name"
                  when ("." `T.isInfixOf` x) $
                        failInternal an $ "device-local name may not contain a dot: " <> x

            checkRegister :: MonadError AstError m => Register -> m ()
            checkRegister (Register an' x sz bv) =
                  unless (fromIntegral (BV.width bv) == sz) $
                        failInternal an' $ "register " <> x <> ": initial value width " <> showt (BV.width bv) <> " does not match declared width " <> showt sz

            -- | Returns the ambient context entries for the instance's
            --   output ports.
            checkInstance :: MonadError AstError m => Ctx -> Instance -> m Ctx
            checkInstance ctx (Instance an' x ex cs) = case Map.lookup ex $ envExterns env of
                  Nothing -> failAt an' $ "instance " <> x <> ": unknown extern: " <> ex
                  Just e  -> do
                        when (extKind e == Comb) $
                              failInternal an' $ "instance " <> x <> ": extern " <> ex <> " is combinational (call it instead)"
                        unless (length cs == length (extGenerics e)) $
                              failInternal an' $ "instance " <> x <> ": expected " <> showt (length $ extGenerics e) <> " generic arguments, got " <> showt (length cs)
                        pure $ foldr (\ (p, sz) -> Map.insert (x <> "." <> p) sz) ctx $ extOutputs e

            checkStmt :: MonadError AstError m => (Ctx, HashSet Name) -> Stmt -> m (Ctx, HashSet Name)
            checkStmt (ctx, assigned) = \ case
                  SLet _ x e -> do
                        sz <- checkExp env ctx e
                        pure (Map.insert x sz ctx, assigned)
                  SOutput an' x e -> case Map.lookup x outsCtx of
                        Nothing -> failInternal an' $ "assignment to unknown output: " <> x
                        Just sz -> (, ) ctx <$> assignOnce an' assigned x sz e ctx
                  SNext an' x e -> case [ sz | Register _ x' sz _ <- regs, x' == x ] of
                        [sz] -> (, ) ctx <$> assignOnce an' assigned ("next " <> x) sz e ctx
                        _    -> failInternal an' $ "next-assignment to unknown register: " <> x
                  SInstIn an' x p e -> case [ e' | Instance _ x' ex _ <- insts, x' == x, Just e' <- [Map.lookup ex $ envExterns env] ] of
                        [ex] -> case lookup p $ extInputs ex of
                              Nothing -> failInternal an' $ "instance " <> x <> " has no input port " <> p
                              Just sz -> (, ) ctx <$> assignOnce an' assigned (x <> "." <> p) sz e ctx
                        _    -> failInternal an' $ "assignment to unknown instance: " <> x

            assignOnce :: MonadError AstError m => Annote -> HashSet Name -> Name -> Size -> Exp -> Ctx -> m (HashSet Name)
            assignOnce an' assigned target sz e ctx = do
                  when (target `Set.member` assigned) $
                        failInternal an' $ target <> " is assigned more than once"
                  sz' <- checkExp env ctx e
                  unless (sz == sz') $
                        failInternal an' $ "assignment to " <> target <> ": width " <> showt sz' <> " (expected " <> showt sz <> ")"
                  pure $ Set.insert target assigned

            checkAssigned :: MonadError AstError m => HashSet Name -> Text -> Name -> m ()
            checkAssigned assigned what x = unless (x `Set.member` assigned) $
                  failInternal an $ what <> " " <> x <> " is never assigned"

            checkInstAssigned :: MonadError AstError m => HashSet Name -> Instance -> m ()
            checkInstAssigned assigned (Instance _ x ex _) = case Map.lookup ex $ envExterns env of
                  Nothing -> pure () -- already reported by checkInstance
                  Just e  -> mapM_ (\ (p, _) -> checkAssigned assigned "instance input" $ x <> "." <> p) $ extInputs e

---

-- | Check that the call graph is acyclic: a depth-first search over calls
--   (and extern-model references, which the interpreter follows like
--   calls), each defn visited at most once.
checkRecursion :: forall m. MonadError AstError m => Env -> [Defn] -> m ()
checkRecursion env = foldM_ visitDefn mempty
      where visitDefn :: HashSet GId -> Defn -> m (HashSet GId)
            visitDefn done (Defn _ n _ _ body _ _)
                  | n `Set.member` done = pure done
                  | otherwise           = Set.insert n <$> visitExp (Set.singleton n) done body

            visitExp :: HashSet GId -> HashSet GId -> Exp -> m (HashSet GId)
            visitExp stack done = \ case
                  Cat _ e1 e2        -> visitExp stack done e1 >>= flip (visitExp stack) e2
                  Slice _ _ _ e      -> visitExp stack done e
                  Prim _ _ _ es      -> foldM (visitExp stack) done es
                  If _ _ c t e       -> foldM (visitExp stack) done [c, t, e]
                  Let _ _ _ e1 e2    -> visitExp stack done e1 >>= flip (visitExp stack) e2
                  Call an _ g es     -> foldM (visitExp stack) done es >>= \ done' -> visitCallee an stack done' g
                  XCall an _ x _ es  -> do
                        done' <- foldM (visitExp stack) done es
                        case Map.lookup x (envExterns env) >>= extModel of
                              Just g  -> visitCallee an stack done' g
                              Nothing -> pure done'
                  _                  -> pure done

            visitCallee :: Annote -> HashSet GId -> HashSet GId -> GId -> m (HashSet GId)
            visitCallee an stack done g
                  | g `Set.member` stack                       = failAt an $ "unsupported use of recursion (hyle id: " <> g <> ")"
                  | g `Set.member` done                        = pure done
                  | Just d <- Map.lookup g $ envDefns env      = Set.insert g <$> visitExp (Set.insert g stack) done (defnBody d)
                  | otherwise                                  = pure done
