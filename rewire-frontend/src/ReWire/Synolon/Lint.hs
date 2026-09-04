{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
-- | Well-formedness checking for Synolon programs: the per-process machine
--   rules of doc/eidos.md §7.4 — signal-guardedness (the goto-only
--   subgraph of the block graph is acyclic), cell-initial constness and
--   typing, full-arity pauses and gotos, the resumed-input parameter rule,
--   and "the process pauses" — over the expression-level checker of
--   'ReWire.Eidos.Lint' at mono+ANF strength with the reactive types out of
--   the type grammar, plus the program rules: name distinctness, global
--   uniqueness over the datatype-and-definition fragment, and the
--   definition rules on the definitions the machine calls.
--
--   Process binding sites are scoped, not globally unique: procify splices
--   one definition per continuation and passes one binder along goto
--   chains, so a unique is legitimately bound by several blocks; labels
--   are distinct per process, and in-block binding is validated by scoping
--   and occurrence-signature agreement.
module ReWire.Synolon.Lint (lint, lintProc) where

import ReWire.Annotation (Annote, noAnn)
import ReWire.Eidos.Lint (Env (..), LintMode (..), envFromDecls, bindVar, nonTail, checkExp, checkAgainst, checkTy, checkValueBinder, checkOccSig, checkDistinct, checkDataDefn, checkDefn, lookupCon, dconFieldTys, LitRep (..), litRep, fitsRep, declSites)
import ReWire.Eidos.Types (tyEq, reacOrStateT)
import ReWire.Error (AstError, MonadError, failAt)
import ReWire.Eidos.Pretty ()
import ReWire.Pretty (prettyPrint, showt)
import ReWire.Synolon.Syntax

import Control.Monad (foldM, unless, void, when, zipWithM_)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Text (Text)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified Data.Text           as T

-- | The environment of the machine rules: the datatypes and every
--   definition in scope, at mono+ANF strength, with the reactive types
--   banned from the type grammar.
machineEnv :: Program -> Env
machineEnv (Program datas defns _ _) = (envFromDecls LintMonoANF datas defns) { envBanReactive = True }

-- | Check a whole program: name distinctness, global uniqueness over the
--   datatype-and-definition fragment, the datatypes, the definitions the
--   machine calls (the definitions the fold lowers: dotted-named,
--   monomorphic, and not reactive-typed — the builtin-named signature
--   carriers and the consumed reactive definitions a program may still
--   carry are skipped, as the fold skips them), @top@ (which must name a
--   definition), and every process.
lint :: forall m. MonadError AstError m => Program -> m ()
lint p@(Program datas defns procs top) = do
      checkDistinct $ declSites datas defns
      checkDistinct [ (dataName d, dataAnnote d, "datatype name " <> dataName d) | d <- datas ]
      checkDistinct [ (c, an, "data constructor name " <> c) | d <- datas, DataCon an c _ <- dataCons d ]
      checkDistinct [ (procName pr, procAnnote pr, "process name " <> procName pr) | pr <- procs ]
      mapM_ (checkDataDefn env) datas
      mapM_ (checkDefn env) $ filter lowered defns
      case [ d | d <- defns, idUniq (defnId d) == idUniq top ] of
            []    -> failAt noAnn $ "top: designated device root " <> prettyPrint top <> " does not name a definition"
            d : _ -> checkOccSig (defnAnnote d) top $ defnId d
      mapM_ (checkProc env) procs
      where env :: Env
            env = machineEnv p

            lowered :: Defn -> Bool
            lowered d = T.any (== '.') (idOcc $ defnId d)
                  && null (sigTVs $ idSig $ defnId d)
                  && not (reacOrStateT $ sigTy $ idSig $ defnId d)

-- | Check a single process (the machine rules) against a program's global
--   context.
lintProc :: MonadError AstError m => Program -> Proc -> m ()
lintProc p = checkProc (machineEnv p)

---
--- Processes (doc/eidos.md §7.4): the machine rules, per-proc.
---


checkProc :: forall m. MonadError AstError m => Env -> Proc -> m ()
checkProc env pr@(Proc an n it ot _clk cells entry blocks) = do
      checkTy env an it
      checkTy env an ot
      checkDistinct [ (cellName c, cellAnnote c, "state cell " <> cellName c <> " of process " <> n) | c <- cells ]
      mapM_ checkCell cells
      checkBlock entry
      mapM_ (checkBlock . snd) blocks
      mapM_ checkInput blocks
      unless (anyPause $ map (blkTerm . snd) blocks <> [blkTerm entry]) $ failAt an
            $ "process " <> n <> " never pauses (no machine to generate)"
      checkGuarded
      where ltab :: HashMap Uniq (Id, Block)
            ltab = Map.fromList [ (idUniq l, (l, b)) | (l, b) <- blocks ]

            ctab :: HashMap Text Ty
            ctab = Map.fromList [ (cellName c, cellTy c) | c <- cells ]

            -- Cell initials are closed (checked in the top-level-only
            -- environment: no locals are in scope) and cell-typed.
            checkCell :: Cell -> m ()
            checkCell (Cell can s t e0) = do
                  checkTy env can t
                  case e0 of
                        Nothing -> pure ()
                        Just e  -> do
                              t' <- checkExp (nonTail env) e
                              unless (tyEq t t') $ failAt can
                                    $ "the initial value of state cell " <> s <> " has type "
                                    <> prettyPrint t' <> ", not the cell's type " <> prettyPrint t

            checkBlock :: Block -> m ()
            checkBlock (Block ban ps cmds term) = do
                  mapM_ (checkValueBinder env ban "block parameter") ps
                  env' <- foldM checkCmd (foldr bindVar env ps) cmds
                  checkTerm env' term

            checkCmd :: Env -> Cmd -> m Env
            checkCmd env' = \ case
                  CmdBind can x rhs -> do
                        checkValueBinder env' can "command binder" x
                        checkAgainst (nonTail env') rhs $ sigTy $ idSig x
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
                        pure env'

            cell :: Annote -> Text -> m Ty
            cell can s = maybe (failAt can $ "unknown state cell: " <> s <> " (process " <> n <> ")") pure
                  $ Map.lookup s ctab

            checkTerm :: Env -> Term -> m ()
            checkTerm env' = \ case
                  Pause tan a l args -> do
                        checkAgainst (nonTail env') a ot
                        (lB, b) <- target tan l
                        when (null $ blkParams b) $ failAt tan
                              $ "pause target " <> prettyPrint lB <> " has no parameters (the last is the resumed input)"
                        unless (length args == length (blkParams b) - 1) $ failAt tan
                              $ "pause to " <> prettyPrint lB <> " supplies " <> showt (length args)
                              <> " arguments (its target takes " <> showt (length (blkParams b) - 1)
                              <> " plus the resumed input)"
                        zipWithM_ (\ a' p -> checkAgainst (nonTail env') a' $ sigTy $ idSig p) args $ blkParams b
                  Goto tan l args    -> do
                        (lB, b) <- target tan l
                        unless (length args == length (blkParams b)) $ failAt tan
                              $ "goto " <> prettyPrint lB <> " supplies " <> showt (length args)
                              <> " arguments (its target takes " <> showt (length $ blkParams b) <> ")"
                        zipWithM_ (\ a' p -> checkAgainst (nonTail env') a' $ sigTy $ idSig p) args $ blkParams b
                  Halt _ a           -> void $ checkExp (nonTail env') a
                  TCase tan a alts   -> do
                        ts <- checkExp (nonTail env') a
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

            -- Signal-guardedness (§7.4): the goto-only subgraph of the
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

