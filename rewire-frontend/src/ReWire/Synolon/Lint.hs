{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
-- | Well-formedness checking for Synolon programs: the per-process machine
--   rules of doc/synolon.md §4 — signal-guardedness (the goto-only
--   subgraph of the block graph is acyclic), representability (every
--   binder, block parameter, cell, port, and halt answer has a fixed bit
--   width, by 'ReWire.Synolon.Repr'), block normal form (command
--   right-hand sides are simple computations; terminator operands and put
--   payloads are atoms or primitive expressions), cell-initial constness
--   and typing, full-arity pauses and gotos, the resumed-input parameter
--   rule, and "the process pauses" — over the expression-level checker
--   of 'ReWire.Eidos.Lint' at mono+ANF strength with the reactive types
--   out of the type grammar,
--   plus the program rules: name distinctness, global uniqueness over the
--   datatype-and-definition fragment, the definition rules on the
--   definitions the machine calls, and pure-acyclicity (the call graph of
--   the definitions reachable from a process is acyclic, and no recursive
--   let is reachable).
--
--   Process binding sites are scoped, not globally unique: purify splices
--   one definition per continuation and passes one binder along goto
--   chains, so a unique is legitimately bound by several blocks. Labels
--   and cells are distinct per process; within a block every binding site
--   is distinct and disjoint from the definition-level sites; and in-block
--   binding is validated by scoping and occurrence-signature agreement.
module ReWire.Synolon.Lint (lint, lintPre, lintProc, isOperand) where

import ReWire.Annotation (Annote, Annotated (ann))
import ReWire.Eidos.ANF (isAtom, isPrimExp)
import ReWire.Eidos.Lint (Env (..), LintMode (..), envFromDecls, bindVar, nonTail, checkExp, checkAgainst, checkTy, checkRepr, checkValueBinder, checkOccSig, checkDistinct, checkDataDefn, checkDefn, lookupCon, dconFieldTys, LitRep (..), litRep, fitsRep, declSites, expSites, idSite)
import ReWire.Eidos.Types (tyEq, typeOf, flattenApp, hasArrow)
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Eidos.Pretty ()
import ReWire.Pretty (prettyPrint, showt)
import ReWire.Synolon.Repr (dataEnv, sizeOf)
import ReWire.Synolon.Syntax

import Control.Monad (foldM, foldM_, unless, when, zipWithM_)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set

-- | The environment of the machine rules: the datatypes and every
--   definition in scope, at mono+ANF strength, with the reactive types
--   banned from the type grammar and every binder required to have a
--   fixed bit width.
machineEnv :: Program -> Env
machineEnv (Program datas defns _) = (envFromDecls LintMonoANF datas defns)
      { envBanReactive = True
      , envRepr        = Just $ sizeOf $ dataEnv datas
      }

-- | Check a whole program: name distinctness, global uniqueness over the
--   datatype-and-definition fragment, the datatypes, every definition
--   (all of them are the machine's: pure, monomorphic, first-order),
--   every process, and pure-acyclicity.
lint :: MonadError AstError m => Program -> m ()
lint = lintWith True

-- | 'lint' for the program before the block-graph cleanup: the same rules
--   except signal-guardedness, the one rule the cleanup may establish
--   (purify can leave an orphaned, unguarded block — the continuation of
--   a computation that never returns — which the cleanup removes).
lintPre :: MonadError AstError m => Program -> m ()
lintPre = lintWith False

lintWith :: forall m. MonadError AstError m => Bool -> Program -> m ()
lintWith guarded p@(Program datas defns procs) = do
      checkDistinct decls
      checkDistinct [ (dataName d, dataAnnote d, "datatype name " <> dataName d) | d <- datas ]
      checkDistinct [ (c, an, "data constructor name " <> c) | d <- datas, DataCon an c _ <- dataCons d ]
      checkDistinct [ (procName pr, procAnnote pr, "process name " <> procName pr) | pr <- procs ]
      mapM_ (checkDataDefn env) datas
      mapM_ (checkDefn env) defns
      mapM_ (checkProcWith decls guarded env) procs
      checkPureAcyclic defns procs
      where env :: Env
            env = machineEnv p

            decls :: [(Uniq, Annote, Text)]
            decls = declSites datas defns

-- | Check a single process (the machine rules, and pure-acyclicity from
--   it) against a program's global context.
lintProc :: MonadError AstError m => Program -> Proc -> m ()
lintProc p@(Program datas defns _) pr = do
      checkProcWith (declSites datas defns) True (machineEnv p) pr
      checkPureAcyclic defns [pr]

---
--- Processes (doc/synolon.md §4): the machine rules, per-proc.
---


-- | The machine rules of one process, given the program's definition-level
--   binding sites (every block's own sites must be disjoint from them) and
--   whether to check signal-guardedness.
checkProcWith :: forall m. MonadError AstError m => [(Uniq, Annote, Text)] -> Bool -> Env -> Proc -> m ()
checkProcWith decls guarded env pr@(Proc an n it ot _clk cells entry blocks) = do
      checkTy env an it
      checkRepr env an ("the input type of process " <> n) it
      checkTy env an ot
      checkRepr env an ("the output type of process " <> n) ot
      checkDistinct [ (cellName c, cellAnnote c, "state cell " <> cellName c <> " of process " <> n) | c <- cells ]
      checkDistinct [ (idUniq l, blkAnnote b, "block label " <> prettyPrint l <> " of process " <> n) | (l, b) <- blocks ]
      mapM_ checkCell cells
      checkBlock entry
      mapM_ (checkBlock . snd) blocks
      mapM_ checkInput blocks
      unless (anyPause $ map (blkTerm . snd) blocks <> [blkTerm entry]) $ failAt an
            $ "process " <> n <> " never pauses (no machine to generate)"
      when guarded checkGuarded
      where ltab :: HashMap Uniq (Id, Block)
            ltab = Map.fromList [ (idUniq l, (l, b)) | (l, b) <- blocks ]

            ctab :: HashMap Text Ty
            ctab = Map.fromList [ (cellName c, cellTy c) | c <- cells ]

            -- Cells are representable; their initials are closed (checked
            -- in the top-level-only environment: no locals are in scope),
            -- cell-typed, and simple computations.
            checkCell :: Cell -> m ()
            checkCell (Cell can s t e0) = do
                  checkTy env can t
                  checkRepr env can ("state cell " <> s <> " of process " <> n) t
                  case e0 of
                        Nothing -> pure ()
                        Just e  -> do
                              t' <- checkExp (nonTail env) e
                              unless (tyEq t t') $ failAt can
                                    $ "the initial value of state cell " <> s <> " has type "
                                    <> prettyPrint t' <> ", not the cell's type " <> prettyPrint t
                              rhsOk e

            checkBlock :: Block -> m ()
            checkBlock b@(Block ban ps cmds term) = do
                  checkDistinct $ decls <> blockSites b
                  mapM_ (checkValueBinder env ban "block parameter") ps
                  env' <- foldM checkCmd (foldr bindVar env ps) cmds
                  checkTerm env' term

            checkCmd :: Env -> Cmd -> m Env
            checkCmd env' = \ case
                  CmdBind can x rhs -> do
                        checkValueBinder env' can "command binder" x
                        checkAgainst (nonTail env') rhs $ sigTy $ idSig x
                        rhsOk rhs
                        pure $ bindVar x env'
                  CmdGet can x s    -> do
                        checkValueBinder env' can "command binder" x
                        t <- cell can s
                        unless (tyEq (sigTy $ idSig x) t) $ failAt can
                              $ "get: binder " <> prettyPrint x <> " has type " <> prettyPrint (sigTy $ idSig x)
                              <> ", not the type of state cell " <> s <> " (" <> prettyPrint t <> ")"
                        pure $ bindVar x env'
                  CmdPut can s a    -> do
                        t <- cell can s
                        checkAgainst (nonTail env') a t
                        operand can "put payload" a
                        pure env'

            -- Every binding site of a block: its parameters, its command
            -- binders, the binders inside its expressions, and its
            -- terminator alternatives' pattern binders.
            blockSites :: Block -> [(Uniq, Annote, Text)]
            blockSites (Block ban ps cmds term) =
                  map (idSite ban "block parameter") ps
                        <> concatMap cmdSites cmds
                        <> termSites term
                  where cmdSites :: Cmd -> [(Uniq, Annote, Text)]
                        cmdSites = \ case
                              CmdBind can x e -> idSite can "command binder" x : expSites e
                              CmdGet can x _  -> [idSite can "command binder" x]
                              CmdPut _ _ e    -> expSites e

                        termSites :: Term -> [(Uniq, Annote, Text)]
                        termSites = \ case
                              TCase _ a alts -> expSites a <> concat [ map (idSite tan "pattern binder") xs <> termSites t | TAlt tan _ xs t <- alts ]
                              t              -> concatMap expSites $ termExps t

            cell :: Annote -> Text -> m Ty
            cell can s = maybe (failAt can $ "unknown state cell: " <> s <> " (process " <> n <> ")") pure
                  $ Map.lookup s ctab

            checkTerm :: Env -> Term -> m ()
            checkTerm env' = \ case
                  Pause tan a l args -> do
                        checkAgainst (nonTail env') a ot
                        operand tan "pause output" a
                        (lB, b) <- target tan l
                        when (null $ blkParams b) $ failAt tan
                              $ "pause target " <> prettyPrint lB <> " has no parameters (the last is the resumed input)"
                        unless (length args == length (blkParams b) - 1) $ failAt tan
                              $ "pause to " <> prettyPrint lB <> " supplies " <> showt (length args)
                              <> " arguments (its target takes " <> showt (length (blkParams b) - 1)
                              <> " plus the resumed input)"
                        zipWithM_ (\ a' p -> checkAgainst (nonTail env') a' $ sigTy $ idSig p) args $ blkParams b
                        mapM_ (operand tan "pause argument") args
                  Goto tan l args    -> do
                        (lB, b) <- target tan l
                        unless (length args == length (blkParams b)) $ failAt tan
                              $ "goto " <> prettyPrint lB <> " supplies " <> showt (length args)
                              <> " arguments (its target takes " <> showt (length $ blkParams b) <> ")"
                        zipWithM_ (\ a' p -> checkAgainst (nonTail env') a' $ sigTy $ idSig p) args $ blkParams b
                        mapM_ (operand tan "goto argument") args
                  Halt tan a         -> do
                        t <- checkExp (nonTail env') a
                        checkRepr env tan ("a halt answer of process " <> n) t
                        operand tan "halt answer" a
                  TCase tan a alts   -> do
                        ts <- checkExp (nonTail env') a
                        operand tan "terminator case scrutinee" a
                        case [ tan' | TAlt tan' DefaultAlt _ _ <- drop 1 alts ] of
                              tan' : _ -> failAt tan' "the default terminator alternative must come first"
                              []       -> pure ()
                        checkDistinct [ (c, tan', "terminator alternative for constructor " <> c) | TAlt tan' (DataAlt c) _ _ <- alts ]
                        when (null alts) $ failAt tan "terminator case with no alternatives"
                        mapM_ (checkTAlt env' ts) alts

            checkTAlt :: Env -> Ty -> TAlt -> m ()
            checkTAlt env' ts (TAlt tan c xs t) = case c of
                  DefaultAlt -> do
                        unless (null xs) $ failAt tan "default terminator alternative binds fields"
                        checkTerm env' t
                  LitAlt ln  -> do
                        unless (null xs) $ failAt tan "literal terminator alternative binds fields"
                        case litRep ts of
                              RepBad -> failAt tan $ "literal terminator alternative on a scrutinee of type " <> prettyPrint ts
                              rep    -> unless (fitsRep rep ln) $ failAt tan
                                    $ "literal " <> showt ln <> " is not representable at the scrutinee type " <> prettyPrint ts
                        checkTerm env' t
                  DataAlt c' -> do
                        (tcon, sig) <- lookupCon env tan c'
                        fields      <- dconFieldTys tan c' tcon sig ts
                        unless (length xs == length fields) $ failAt tan
                              $ "terminator alternative for " <> c' <> " binds " <> showt (length xs)
                              <> " fields (the constructor has " <> showt (length fields) <> ")"
                        mapM_ (checkValueBinder env' tan "pattern binder") xs
                        checkTerm (foldr bindVar env' xs) t

            target :: Annote -> Id -> m (Id, Block)
            target tan l = case Map.lookup (idUniq l) ltab of
                  Just lb -> checkOccSig tan l (fst lb) >> pure lb
                  Nothing -> failAt tan $ "terminator targets an undeclared block label: " <> prettyPrint l

            anyPause :: [Term] -> Bool
            anyPause = any go
                  where go :: Term -> Bool
                        go = \ case
                              Pause {}       -> True
                              TCase _ _ alts -> any (\ (TAlt _ _ _ t) -> go t) alts
                              _              -> False

            -- Pause targets: their last parameter is the resumed input.
            checkInput :: (Id, Block) -> m ()
            checkInput (l, b)
                  | idUniq l `Set.member` pauseTargets
                  , p : _ <- reverse $ blkParams b
                  , not $ tyEq (sigTy $ idSig p) it = failAt (blkAnnote b)
                        $ "the last parameter of pause target " <> prettyPrint l
                        <> " (the resumed input) has type " <> prettyPrint (sigTy $ idSig p)
                        <> ", not the process input type " <> prettyPrint it
                  | otherwise = pure ()

            pauseTargets :: HashSet Uniq
            pauseTargets = Set.fromList $ concatMap (pt . blkTerm) $ entry : map snd blocks
                  where pt :: Term -> [Uniq]
                        pt = \ case
                              Pause _ _ l _  -> [idUniq l]
                              TCase _ _ alts -> concatMap (\ (TAlt _ _ _ t) -> pt t) alts
                              _              -> []

            -- Signal-guardedness (doc/synolon.md §4): the goto-only subgraph of the
            -- block graph is acyclic — every cycle crosses a pause.
            checkGuarded :: m ()
            checkGuarded = mapM_ (visit mempty) $ Map.keys gotoEdges
                  where gotoEdges :: HashMap Uniq [Id]
                        gotoEdges = Map.fromList $ (entryKey, gotos $ blkTerm entry)
                              : [ (idUniq l, gotos $ blkTerm b) | (l, b) <- blocks ]

                        entryKey :: Uniq
                        entryKey = minBound

                        gotos :: Term -> [Id]
                        gotos = \ case
                              Goto _ l _     -> [l]
                              TCase _ _ alts -> concatMap (\ (TAlt _ _ _ t) -> gotos t) alts
                              _              -> []

                        visit :: HashSet Uniq -> Uniq -> m ()
                        visit stack u
                              | Set.member u stack = failAt (procAnnote pr)
                                    $ "process " <> n <> ": a cycle of gotos crosses no pause (is recursion guarded by signal?)"
                              | otherwise = mapM_ (visit (Set.insert u stack) . idUniq)
                                    $ Map.lookupDefault [] u gotoEdges

---
--- Block normal form (doc/synolon.md §3.2).
---

-- | A right-hand side is a simple computation: an atom, a saturated call
--   of a definition or constructor, a primitive expression, or a case
--   over an atom whose alternatives are let chains of such right-hand
--   sides ending in an atom. A call's arguments are atoms or primitive
--   expressions (which nest freely: the pure data path is a tree of
--   primitives over atoms), or the naming-exempt lambda and
--   function-typed forms — the higher-order builtins' function
--   arguments, kept in place with A-normalized bodies, and partial
--   applications. A-normalization establishes this shape for the
--   reactive fragment (doc/eidos.md §6), purification carries it into
--   blocks, and the cleanup transforms preserve it (epsilon inlining
--   substitutes operands for block parameters only where the result is
--   still in the form, 'isOperand').
rhsOk :: forall m. MonadError AstError m => Exp -> m ()
rhsOk r = case r of
      _ | isAtom r         -> pure ()
      Case an _ s _ alts -> do
            unless (isAtom s) $ failAt an "block normal form: a case scrutinee is not an atom (it must be let-bound)"
            mapM_ (\ (Alt _ _ _ b) -> tailOk "an alternative" b) alts
      App {}             -> spineOk r
      _                  -> failAt (ann r)
            "block normal form: a right-hand side must be an atom, a saturated call, or a case over an atom"
      where spineOk :: Exp -> m ()
            spineOk e = do
                  let (h, args) = flattenApp e
                  case h of
                        Var {}  -> pure ()
                        Con {}  -> pure ()
                        Prim {} -> pure ()
                        Lam {}  -> failAt (ann e) "block normal form: a residual beta-redex (the application's lambda head must be let-bound)"
                        _       -> failAt (ann e) "block normal form: the application's head is not a definition, constructor, or primitive"
                  mapM_ argOk [ a | EArg a <- args ]

            argOk :: Exp -> m ()
            argOk a
                  | isAtom a           = pure ()
                  | Lam _ _ b <- a     = tailOk "a lambda body" b
                  | isPrimExp a        = spineOk a
                  | hasArrow $ typeOf a = case a of
                        App {}  -> spineOk a
                        Con {}  -> pure () -- a bare constructor reference
                        Prim {} -> pure () -- a bare operator primitive
                        _       -> failAt (ann a) "block normal form: a function-typed argument must be a lambda, a partial application, or a definition, constructor, or primitive reference"
                  | otherwise          = failAt (ann a) "block normal form: a computed argument (must be let-bound)"

            tailOk :: Text -> Exp -> m ()
            tailOk what e = case e of
                  _ | isAtom e               -> pure ()
                  Let _ (NonRec _ r') body -> rhsOk r' >> tailOk what body
                  Let an (Join {}) _       -> failAt an "block normal form: a join point in a block (join points and jumps occur only in definition bodies)"
                  Jump an _ _              -> failAt an "block normal form: a jump in a block (join points and jumps occur only in definition bodies)"
                  _                        -> failAt (ann e)
                        $ "block normal form: " <> what <> " must be a let chain of simple computations ending in an atom"

-- | Is the expression an operand (an atom, or a primitive expression over
--   such)? The pure form of 'operand', for the cleanup's epsilon inliner.
isOperand :: Exp -> Bool
isOperand e = case operand (ann e) "" e :: Either AstError () of
      Left _  -> False
      Right _ -> True

-- | A terminator operand or put payload is an atom or a primitive
--   expression (over such operands).
operand :: MonadError AstError m => Annote -> Text -> Exp -> m ()
operand an what e
      | isAtom e    = pure ()
      | isPrimExp e = rhsOk e
      | otherwise   = failAt an
            $ "block normal form: " <> what <> " is neither an atom nor a primitive expression (it must be let-bound)"

---
--- Pure-acyclicity (doc/synolon.md §4).
---

-- | The call graph of the definitions reachable from a process — through
--   its cell initials, command right-hand sides, put payloads, and
--   terminator operands, transitively — is acyclic, and no recursive let
--   is reachable. Recursion in a device compiles only when it is guarded
--   by signal, and that recursion is reactive: it became the block graph,
--   which signal-guardedness checks.
checkPureAcyclic :: forall m. MonadError AstError m => [Defn] -> [Proc] -> m ()
checkPureAcyclic defns procs = foldM_ (\ done pr -> procRefs pr >>= foldM (visit mempty) done) mempty procs
      where dtab :: HashMap Uniq Defn
            dtab = Map.fromList [ (idUniq $ defnId d, d) | d <- defns ]

            procRefs :: Proc -> m [Uniq]
            procRefs pr = concat <$> mapM refs (mapMaybe cellInit (procCells pr) <> concatMap blockExps (allBlocks pr))

            -- Depth-first over the definitions, with the current path as
            -- the stack and the fully explored definitions as done.
            visit :: HashSet Uniq -> HashSet Uniq -> Uniq -> m (HashSet Uniq)
            visit stack done u = case Map.lookup u dtab of
                  Nothing -> pure done
                  Just d
                        | Set.member u stack -> failAt (defnAnnote d)
                              $ "unsupported use of recursion: " <> prettyPrint (defnId d)
                              <> " is recursive and reachable from a process (the pure call graph of a machine must be acyclic; only recursion guarded by signal compiles)"
                        | Set.member u done  -> pure done
                        | otherwise          -> do
                              rs    <- refs $ defnBody d
                              done' <- foldM (visit $ Set.insert u stack) done rs
                              pure $ Set.insert u done'

            -- The variable occurrences of an expression (locals are
            -- filtered by the table lookup); a recursive let is rejected
            -- on sight.
            refs :: Exp -> m [Uniq]
            refs = \ case
                  Var _ x                   -> pure [idUniq x]
                  App _ f a                 -> (<>) <$> refs f <*> argRefs a
                  Lam _ _ b                 -> refs b
                  Let an (Rec _) _          -> failAt an
                        "unsupported use of recursion: a recursive let binding is reachable from a process (the pure call graph of a machine must be acyclic)"
                  Let _ (NonRec _ rhs) body -> (<>) <$> refs rhs <*> refs body
                  Let _ (Join _ _ b) body   -> (<>) <$> refs b <*> refs body
                  Jump _ _ es               -> concat <$> mapM refs es
                  Case _ _ s _ alts         -> (<>) <$> refs s <*> (concat <$> mapM (\ (Alt _ _ _ b) -> refs b) alts)
                  LitList _ _ es            -> concat <$> mapM refs es
                  LitVec _ _ es             -> concat <$> mapM refs es
                  _                         -> pure []

            argRefs :: Arg -> m [Uniq]
            argRefs = \ case
                  EArg e -> refs e
                  TArg _ -> pure []
