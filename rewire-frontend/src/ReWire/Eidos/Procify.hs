{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | procify (doc/eidos.md §7; the plan's one hard pass): convert the
--   reactive fragment of a monomorphic ANF program into one process. The
--   traversal is CPS-shaped — @compile e k@ compiles a reactive
--   computation under a continuation — which right-associates binds by
--   construction:
--
--   * @rwPrimSignal o >>= k@ becomes @pause o -> L_k@ (the continuation
--     block's last parameter is the resumed input);
--   * @rwPrimReturn v@ becomes @goto L_k (v)@ (or @halt v@ at the root);
--   * lifted @rwPrimGet@/@rwPrimPut@ become cell commands, the cell
--     resolved from the operation's own residual state stack;
--   * @rwPrimExtrude m s@ is a cell write (@put@) followed by @m@ — cells
--     written in entry before the first pause become register initials
--     (§7.1), so no separate initials extraction exists;
--   * a reactive case becomes a terminator case (arms with commands get
--     their own blocks — continuations are shared blocks, so the retired
--     lift-once rule is the default by construction);
--   * join points become blocks; jumps become gotos;
--   * a reactive call is compiled once per continuation — block-graph
--     splicing memoized on @(definition, continuation)@, the retired
--     unfoldHead's successor; recursion closes through the memo table
--     (a tail self-call recurs under the same continuation, so it hits
--     the memo and becomes a goto). Two shapes are rejected with located
--     errors: a NOINLINE reactive callee on the left-hand side of a bind
--     (the user's opt-out from per-continuation splicing), and recursion
--     THROUGH a bind's left-hand side (re-entering a definition whose
--     splice is still open mints a fresh continuation per iteration —
--     each pending continuation is a resumption-stack frame, and the
--     machine has no stack).
--
--   Blocks are closure-converted afterward: a fixpoint over the block
--   graph computes each block's live-in locals, which become leading
--   parameters, with every pause/goto site supplying them (this is the
--   machine record's @args@ field).
--
--   This stage constructs and lint-checks the process; the compiled
--   output still lowers through the P-level shim (dump-and-fall-through,
--   like the bridge before it) until the machine-step fold and its
--   adapter land.
module ReWire.Eidos.Procify (procify) where

import ReWire.Annotation (Annote, ann, noAnn)
import ReWire.Builtins (Builtin (..))
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Eidos.Naming (blockLabel)
import ReWire.Eidos.Subst (nextUniq, freeUniqs, occIds)
import ReWire.Eidos.Syntax
import ReWire.Eidos.Types (typeOf, flattenApp, flattenArrow, flattenTyApp, reacOrStateT)
import ReWire.Pretty (showt)

import Control.Monad (when)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap.Strict  as IM
import qualified Data.IntSet         as IS

-- | Add the process compiled from the program's reactive root; the
--   definitions are left in place (the P-level fall-through). A program
--   whose root is not reactive is returned unchanged.
procify :: forall m. MonadError AstError m => Program -> m Program
procify p@(Program datas defns procs top)
      | (TyCon _ "ReacT", [ti, to, _, _]) <- flattenTyApp $ sigTy $ idSig top
      , [] <- fst (flattenArrow $ sigTy $ idSig top) = do
            let cells = stackCells $ maxStack [ typeOf $ defnBody d | d <- reactives ]
            pr <- evalStateT (compileRoot ti to cells) $ PSt (nextUniq p) [] mempty 1 mempty
            pure $ Program datas defns (procs <> [pr]) top
      | otherwise = pure p
      where dmap :: IM.IntMap Defn
            dmap = IM.fromList [ (idUniq $ defnId d, d) | d <- defns ]

            -- The reactive code (monomorphic; the polymorphic rwPrim*
            -- signature carriers are type assumptions, not code).
            reactives :: [Defn]
            reactives = [ d | d <- defns, reacOrStateT $ sigTy $ idSig $ defnId d
                            , null $ sigTVs $ idSig $ defnId d ]

            -- The state cells: one per layer of the deepest reactive
            -- stack, outermost first, named s0, s1, ....
            maxStack :: [Ty] -> [Ty]
            maxStack ts = case [ st | t <- ts, Just st <- [stackOf t], not (null st) ] of
                  [] -> []
                  ss -> foldr1 (\ a b -> if length a >= length b then a else b) ss

            stackCells :: [Ty] -> [Cell]
            stackCells ts = [ Cell (ann t) ("s" <> showt i) t Nothing | (i, t) <- zip [0 :: Int ..] ts ]

            compileRoot :: Ty -> Ty -> [Cell] -> PM m Proc
            compileRoot ti to cells = do
                  let cx = Cx { cxIn = ti, cxOut = to, cxCells = map (\ c -> (cellName c, cellTy c)) cells
                              , cxJoins = mempty, cxRLets = mempty }
                  body <- case IM.lookup (idUniq top) dmap of
                        Just d  -> pure $ defnBody d
                        Nothing -> failAt (ann $ sigTy $ idSig top) "procify: device root has no definition (rwc bug)."
                  (cmds, term) <- compile cx body KHalt
                  blks <- gets stBlocks
                  pure $ closureConvert Proc
                        { procAnnote = ann body
                        , procName   = "main"
                        , procInTy   = ti
                        , procOutTy  = to
                        , procClock  = Nothing
                        , procCells  = cells
                        , procEntry  = Block (ann body) [] cmds term
                        , procBlocks = reverse blks
                        }

            -- The compile-time context: device i/o types, the cell table
            -- (name and type, outermost layer first), joins in scope
            -- (label and parameter count), and reactive let bindings
            -- (compiled at use).
            compile :: Cx -> Exp -> K -> PM m ([Cmd], Term)
            compile cx e k = case e of
                  Let an (NonRec x rhs) body
                        | reacOrStateT $ sigTy $ idSig x ->
                              compile (cx { cxRLets = IM.insert (idUniq x) rhs $ cxRLets cx }) body k
                        | otherwise -> do
                              (cmds, term) <- compile cx body k
                              pure (CmdBind an x rhs : cmds, term)
                  Let _ (Join j ps b) body -> do
                        l <- freshLabel (idOcc $ jpId j) (map (sigTy . idSig) ps) $ cxOut cx
                        -- The join body compiles under the SAME continuation
                        -- (it is a tail of the definition); its block binds
                        -- the join parameters.
                        (jcmds, jterm) <- compile cx b k
                        emitBlock l $ Block (ann b) ps jcmds jterm
                        compile (cx { cxJoins = IM.insert (idUniq $ jpId j) l $ cxJoins cx }) body k
                  Let an (Rec _) _ -> failAt an "procify: unsupported local recursive binding."
                  Jump an j args -> case IM.lookup (idUniq $ jpId j) $ cxJoins cx of
                        Just l  -> pure ([], Goto an l args)
                        Nothing -> failAt an "procify: jump to an uncompiled join point (rwc bug)."
                  Case an t s cb alts | reacOrStateT t -> do
                        alts' <- mapM (arm cx k) alts
                        -- The case binder is dead in ANF output (the
                        -- scrutinee is an atom); drop it.
                        when (IS.member (idUniq cb) $ IS.unions $ map (freeUniqs . altBody) alts) $ failAt an
                              "procify: a terminator case's binder is live (unsupported)."
                        pure ([], TCase an s alts')
                  Var an x
                        | Just rhs <- IM.lookup (idUniq x) $ cxRLets cx -> compile cx rhs k
                        | otherwise -> call cx an x [] k
                  App {}  -> spine cx e k
                  Prim {} -> spine cx e k -- bare nullary operation (get)
                  _ -> failAt (ann e) "procify: unsupported reactive tail."

            altBody :: Alt -> Exp
            altBody (Alt _ _ _ b) = b

            arm :: Cx -> K -> Alt -> PM m TAlt
            arm cx k (Alt an c xs b) = do
                  (cmds, term) <- compile cx b k
                  if null cmds then pure $ TAlt an c xs term else do
                        l <- freshLabel "arm" (map (sigTy . idSig) xs) $ cxOut cx
                        emitBlock l $ Block an xs cmds term
                        pure $ TAlt an c xs $ Goto an l $ map (Var an) xs

            spine :: Cx -> Exp -> K -> PM m ([Cmd], Term)
            spine cx e k = case flattenApp e of
                  (Prim an _ Bind, [EArg m, EArg kont]) -> do
                        k' <- contOf cx an kont k
                        compile cx m k'
                  (Prim an _ Signal, [EArg o]) -> do
                        l <- resumeLabel cx an k
                        pure ([], Pause an o l [])
                  (Prim an _ Return, [EArg v]) -> applyK an cx k v
                  (Prim an _ Extrude, [EArg m, EArg s]) -> do
                        c <- cellFor cx an $ typeOf m
                        (cmds, term) <- compile cx m k
                        pure (CmdPut an c s : cmds, term)
                  -- Lift is a type-adjusting identity: cells resolve from
                  -- each operation's own residual state stack, so the
                  -- wrapper carries no information.
                  (Prim _ _ Lift, [EArg inner]) -> compile cx inner k
                  (Prim an t Get, []) -> do
                        c <- cellFor cx an t
                        x <- freshId (resultTy t) "$s"
                        (cmds, term) <- applyK an cx k $ Var an x
                        pure (CmdGet an x c : cmds, term)
                  (Prim an t Put, [EArg v]) -> do
                        c <- cellFor cx an t
                        (cmds, term) <- applyK an cx k unitE
                        pure (CmdPut an c v : cmds, term)
                  (Prim an t Error, [EArg msg]) -> do
                        x <- freshId (resultTy t) "$err"
                        let t' = Arrow an (TyCon an "String") $ sigTy $ idSig x
                        pure ([CmdBind an x $ App an (Prim an t' Error) $ EArg msg], Halt an $ Var an x)
                  (Var an f, args) -> call cx an f [ a | EArg a <- args ] k
                  -- A residual reactive beta redex (the simplifier's single
                  -- round can leave one): lower to a let.
                  (Lam lan x b, EArg a : rest) ->
                        compile cx (Let lan (NonRec x a) $ foldl (App lan) b rest) k
                  -- A let-headed application: hoist the application inside
                  -- (the arguments predate the binder, so this is scope-safe).
                  (Let lan bnd body, args) ->
                        compile cx (Let lan bnd $ foldl (App lan) body args) k
                  (h, _) -> failAt (ann e) $ "procify: unsupported reactive computation (head: " <> headKind h <> ")."

            -- Apply the continuation to a value: goto (or halt at the root).
            applyK :: Annote -> Cx -> K -> Exp -> PM m ([Cmd], Term)
            applyK an _ KHalt v        = pure ([], Halt an v)
            applyK an _ (KLabel l _) v = pure ([], Goto an l [v])

            -- The label a pause resumes at: the continuation's own block
            -- (its last parameter is the resumed input), or a fresh
            -- halt-adapter block at the root.
            resumeLabel :: Cx -> Annote -> K -> PM m Id
            resumeLabel _ _ (KLabel l _) = pure l
            resumeLabel cx an KHalt      = do
                  x <- freshId (cxIn cx) "$i"
                  l <- freshLabel "halt" [sigTy $ idSig x] $ cxOut cx
                  emitBlock l $ Block an [x] [] $ Halt an $ Var an x
                  pure l

            -- The continuation of a bind: a lambda becomes a block (bind
            -- its parameter, last); anything else eta-expands first.
            contOf :: Cx -> Annote -> Exp -> K -> PM m K
            contOf cx an kont k = case kont of
                  Lam _ x b -> do
                        l <- freshLabel (idOcc x) [sigTy $ idSig x] $ cxOut cx
                        (cmds, term) <- compile cx b k
                        emitBlock l $ Block (ann b) [x] cmds term
                        pure $ KLabel l 1
                  _ -> do
                        x <- freshId (contDom $ typeOf kont) "$x"
                        contOf cx an (Lam an x $ App an kont $ EArg $ Var an x) k
                  where contDom :: Ty -> Ty
                        contDom t = case flattenArrow t of
                              (dom : _, _) -> dom
                              _            -> t

            -- A reactive call: compiled once per continuation (memoized
            -- block-graph splicing; plan §12 Q3 resolved as accept).
            -- Rejected: NOINLINE at bind-LHS (the splicing opt-out), and
            -- re-entry into a definition whose splice is still open under
            -- a fresh continuation (recursion through a bind's LHS needs
            -- an unbounded resumption stack; only tail recursion — which
            -- re-enters under the SAME continuation and hits the memo —
            -- compiles to a finite machine).
            call :: Cx -> Annote -> Id -> [Exp] -> K -> PM m ([Cmd], Term)
            call cx an f args k = do
                  d <- maybe (failAt an $ "procify: call to an unknown reactive definition: " <> idOcc f) pure
                        $ IM.lookup (idUniq f) dmap
                  let noinline = defnAttr d == Just NoInline
                      bindLhs  = case k of { KHalt -> False; KLabel {} -> True }
                  when (bindLhs && noinline) $ failAt an
                        $ "the reactive computation " <> idOcc f <> " on the left-hand side of a bind might pause"
                        <> " (a NOINLINE reactive definition may only be called in tail position)"
                  mm <- gets $ Map.lookup (idUniq f, kKey k) . stMemo
                  l  <- case mm of
                        Just l  -> pure l
                        Nothing -> do
                              active <- gets stActive
                              when (IS.member (idUniq f) active) $ failAt an
                                    $ "the reactive computation " <> idOcc f <> " recurses on the left-hand side of a bind,"
                                    <> " which would need an unbounded resumption stack"
                                    <> " (recursion must reach itself in tail position to compile to a finite machine)"
                              l <- freshLabel (idOcc f) (map (sigTy . idSig) $ defnParams d) $ cxOut cx
                              modify $ \ st -> st { stMemo   = Map.insert (idUniq f, kKey k) l $ stMemo st
                                                  , stActive = IS.insert (idUniq f) $ stActive st }
                              (cmds, term) <- compile cx (defnBody d) k
                              modify $ \ st -> st { stActive = IS.delete (idUniq f) $ stActive st }
                              emitBlock l $ Block an (defnParams d) cmds term
                              pure l
                  pure ([], Goto an l args)

            cellFor :: Cx -> Annote -> Ty -> PM m Text
            cellFor cx an t = case stackOf t of
                  Just st | not (null st)
                          , idx <- length (cxCells cx) - length st
                          , idx >= 0, idx < length (cxCells cx) -> pure $ fst $ cxCells cx !! idx
                  _ -> failAt an "procify: cannot resolve the state cell for this operation (rwc bug)."

headKind :: Exp -> Text
headKind = \ case
      Var {}     -> "variable"
      Con {}     -> "constructor"
      Prim _ _ b -> "primitive " <> showt b
      Lam {}     -> "lambda"
      Let {}     -> "let"
      Case {}    -> "case"
      Jump {}    -> "jump"
      LitInt {}  -> "integer literal"
      LitStr {}  -> "string literal"
      LitList {} -> "list literal"
      LitVec {}  -> "vector literal"
      App {}     -> "application"

data Cx = Cx
      { cxIn    :: !Ty
      , cxOut   :: !Ty
      , cxCells :: ![(Text, Ty)]
      , cxJoins :: !(IM.IntMap Id)
      , cxRLets :: !(IM.IntMap Exp)
      }

data K = KHalt | KLabel !Id !Int

kKey :: K -> Uniq
kKey = \ case
      KHalt      -> minBound
      KLabel l _ -> idUniq l

data PSt = PSt
      { stSupply :: !Uniq
      , stBlocks :: ![(Id, Block)]
      , stMemo   :: !(Map.HashMap (Uniq, Uniq) Id)
      , stOrd    :: !Int
      , stActive :: !IS.IntSet -- ^ Definitions whose splice is in progress.
      }

type PM m = StateT PSt m

freshU :: Monad m => PM m Uniq
freshU = do
      u <- gets stSupply
      modify $ \ st -> st { stSupply = u + 1 }
      pure u

freshId :: Monad m => Ty -> Text -> PM m Id
freshId t occ = do
      u <- freshU
      pure $ Id occ u $ monoSig t

freshLabel :: Monad m => Text -> [Ty] -> Ty -> PM m Id
freshLabel src ptys ot = do
      i <- gets stOrd
      modify $ \ st -> st { stOrd = i + 1 }
      u <- freshU
      pure $ Id (blockLabel src i) u $ monoSig $ foldr (Arrow (ann ot)) ot ptys

emitBlock :: Monad m => Id -> Block -> PM m ()
emitBlock l b = modify $ \ st -> st { stBlocks = (l, b) : stBlocks st }

unitE :: Exp
unitE = Con noAnn (TyCon noAnn "()") "()"

-- | The result type of a reactive/monadic type (last type application).
resultTy :: Ty -> Ty
resultTy t = case flattenTyApp $ snd $ flattenArrow t of
      (_, args) | not (null args) -> last args
      _                           -> t

-- | The state stack of a reactive or state-monadic type: the cell types,
--   outermost first.
stackOf :: Ty -> Maybe [Ty]
stackOf t = case flattenTyApp $ snd $ flattenArrow t of
      (TyCon _ "ReacT", [_, _, m, _])  -> Just $ layers m
      (TyCon _ "StateT", [s, m, _])    -> Just $ s : layers m
      _                                -> Nothing
      where layers :: Ty -> [Ty]
            layers ty = case flattenTyApp ty of
                  (TyCon _ "StateT", [s, m']) -> s : layers m'
                  (TyCon _ "StateT", [s, m', _]) -> s : layers m'
                  _                           -> []

-- | Closure conversion: a fixpoint over the block graph computes each
--   block's live-in locals; they become leading parameters, and every
--   transfer site supplies them.
closureConvert :: Proc -> Proc
closureConvert pr = pr { procEntry  = patchBlock $ procEntry pr
                       , procBlocks = [ (l, (patchBlock b) { blkParams = capIds l <> blkParams b }) | (l, b) <- procBlocks pr ]
                       }
      where blocks :: [(Id, Block)]
            blocks = procBlocks pr

            binfo :: IM.IntMap Block
            binfo = IM.fromList [ (idUniq l, b) | (l, b) <- blocks ]

            -- Locals bound within a block.
            bound :: Block -> IS.IntSet
            bound b = IS.fromList $ map idUniq (blkParams b) <> concatMap cmdB (blkCmds b) <> termB (blkTerm b)
                  where cmdB :: Cmd -> [Uniq]
                        cmdB = \ case
                              CmdBind _ x _ -> [idUniq x]
                              CmdGet _ x _  -> [idUniq x]
                              CmdPut {}     -> []

                        termB :: Term -> [Uniq]
                        termB = \ case
                              TCase _ _ alts -> concat [ map idUniq xs <> termB t | TAlt _ _ xs t <- alts ]
                              _              -> []

            -- Direct free locals of a block's expressions (excluding
            -- top-level definitions, which occIds distinguishes by never
            -- being block-bound; anything not bound in the process is a
            -- top-level reference and is dropped by intersecting with
            -- process-bound uniques).
            procBound :: IS.IntSet
            procBound = IS.unions $ map bound $ procEntry pr : map snd blocks

            ownFree :: Block -> IS.IntSet
            ownFree b = (IS.unions (map freeUniqs $ blockExps b) `IS.intersection` procBound) IS.\\ bound b

            blockExps :: Block -> [Exp]
            blockExps b = concatMap ce (blkCmds b) <> te (blkTerm b)
                  where ce :: Cmd -> [Exp]
                        ce = \ case
                              CmdBind _ _ e -> [e]
                              CmdGet {}     -> []
                              CmdPut _ _ e  -> [e]

                        te :: Term -> [Exp]
                        te = \ case
                              Pause _ a _ as -> a : as
                              Goto _ _ as    -> as
                              Halt _ a       -> [a]
                              TCase _ a alts -> a : concat [ te t | TAlt _ _ _ t <- alts ]

            targets :: Block -> [Uniq]
            targets b = go $ blkTerm b
                  where go :: Term -> [Uniq]
                        go = \ case
                              Pause _ _ l _  -> [idUniq l]
                              Goto _ l _     -> [idUniq l]
                              TCase _ _ alts -> concat [ go t | TAlt _ _ _ t <- alts ]
                              Halt {}        -> []

            -- live(L) = ownFree(L) ∪ ⋃_{L'∈targets} live(L') , minus bound(L).
            live :: IM.IntMap IS.IntSet
            live = go $ IM.map ownFree binfo
                  where go :: IM.IntMap IS.IntSet -> IM.IntMap IS.IntSet
                        go cur =
                              let nxt = IM.mapWithKey (\ u s ->
                                          let b = binfo IM.! u
                                          in (s <> IS.unions [ IM.findWithDefault mempty t cur | t <- targets b ]) IS.\\ bound b) cur
                              in if nxt == cur then cur else go nxt

            ids :: IM.IntMap Id
            ids = IM.unions $ map (\ b -> IM.unions $ map occIds $ blockExps b) (procEntry pr : map snd blocks)
                  <> [ IM.fromList [ (idUniq x, x) | x <- blkParams b <> cmdIds b ] | b <- procEntry pr : map snd blocks ]
                  where cmdIds :: Block -> [Id]
                        cmdIds b = [ x | CmdBind _ x _ <- blkCmds b ] <> [ x | CmdGet _ x _ <- blkCmds b ]

            capIds :: Id -> [Id]
            capIds l = [ fromMaybe (idPanic u) $ IM.lookup u ids | u <- IS.toList $ IM.findWithDefault mempty (idUniq l) live ]
                  where idPanic :: Uniq -> Id
                        idPanic u = Id "$cap" u $ monoSig $ TyCon (procAnnote pr) "()"

            patchBlock :: Block -> Block
            patchBlock b = b { blkTerm = patchTerm $ blkTerm b }

            patchTerm :: Term -> Term
            patchTerm = \ case
                  Pause an a l as -> Pause an a l $ caps an l <> as
                  Goto an l as    -> Goto an l $ caps an l <> as
                  TCase an a alts -> TCase an a [ TAlt aan c xs (patchTerm t) | TAlt aan c xs t <- alts ]
                  t               -> t
                  where caps :: Annote -> Id -> [Exp]
                        caps an l = map (Var an) $ capIds l


