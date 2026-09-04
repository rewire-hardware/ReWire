{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.ModCache
      ( getDevice
      , LoadPath
      ) where

import ReWire.Config (Config)
import ReWire.Eidos.ToSynolon (procify)
import ReWire.GHC.Session (loadCore)
import ReWire.GHC.ToEidos (toEidos)
import ReWire.Error (AstError, MonadError, Warning (..), failAt, warnAt)
import ReWire.Pass (pass, verb')
import ReWire.Pretty (prettyPrint)
import ReWire.Synolon.ToHyle (synolonToHyle)

import Control.Lens ((^.))
import Control.Monad (when, (>=>))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State.Strict (MonadState)
import Data.Text (Text, pack)
import Numeric.Natural (Natural)
import System.Directory (renameFile)

import qualified Data.Text.IO                 as T
import qualified ReWire.Eidos.ANF             as Eidos
import qualified ReWire.Eidos.Externs         as Eidos
import qualified ReWire.Eidos.Inline          as Eidos
import qualified ReWire.Eidos.Lint            as Eidos
import qualified ReWire.Eidos.Pretty          as Eidos
import qualified ReWire.Eidos.Simplify        as Eidos
import qualified ReWire.Eidos.Spec            as Eidos
import qualified ReWire.Eidos.Syntax          as Eidos
import qualified ReWire.Synolon.Lint          as Synolon
import qualified ReWire.Synolon.Pretty        as Synolon
import qualified ReWire.Synolon.Syntax        as Synolon
import qualified ReWire.Synolon.Transform     as Synolon
import qualified ReWire.Hyle.Syntax           as Hyle
import qualified ReWire.Config                as C

type LoadPath = [FilePath]

-- The numbered pass pipeline: run rwc -v to see the bracketed pass numbers;
-- -d N (or --dump-all) dumps the IR after pass N to a file beside the
-- output (e.g., MiniISA.6.eir, MiniISA.8.syn). Pass 1 is the front end: GHC
-- (parse/typecheck/desugar over the whole home module graph) followed by
-- the Core-to-Eidos bridge. Passes 2-6 are the Eidos passes (doc/eidos.md),
-- pass 7 is procification (Eidos to Synolon), pass 8 the Synolon
-- block-graph cleanup, and pass 9 the Synolon-to-Hyle fold; the Hyle-level
-- passes (10-11) run in ReWire.FrontEnd, numbered after these so -d
-- numbering is uniform.
getDevice :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m) => Config -> FilePath -> m Hyle.Program
getDevice conf fp = do
      eir <- passEidos 1 "GHC front end and Core-to-Eidos bridge."
            (loadCore conf >=> toEidos conf) fp
      Eidos.lint Eidos.LintPoly eir
      -- The Eidos passes: specialize away
      -- polymorphism, inline INLINE-annotated definitions, neuter externs
      -- (before the partial evaluator, always), and partially evaluate to
      -- the synthable/dictionary-free fixpoint. The standing lints run
      -- between passes (poly mode after the bridge, then mono mode);
      -- --debug-lint adds a lint after the remaining Eidos passes.
      eirSpec <- passEidos 2 "Specializing polymorphic definitions (eidos)."
            (Eidos.specialize specDepth) eir
      lintDebug Eidos.LintMono eirSpec
      eirInl <- passEidos 3 "Inlining INLINE-annotated definitions (eidos)."
            Eidos.inlineAnnotated eirSpec
      Eidos.lint Eidos.LintMono eirInl
      eirExt <- passEidos 4 "Extracting extern models (eidos)."
            neuterExterns eirInl
      lintDebug Eidos.LintMono eirExt
      eirPE <- passEidos 5 "Partial evaluation (eidos)."
            (Eidos.simplify (conf^.C.depth)) eirExt
      Eidos.lint Eidos.LintMono eirPE
      -- Normalize the reactive fragment to ANF (the last Eidos pass; the
      -- --eidos dump is this program), then the machine level: procify to
      -- Synolon, clean the block graph, and check the machine rules.
      eirANF <- passEidos 6 "Normalizing to ANF (eidos)."
            Eidos.normalize eirPE
      Eidos.lint Eidos.LintMonoANF eirANF
      when (conf^.C.eidos) $ writeDump (C.eidosFile conf fp) "Eidos" $ Eidos.prettyProgram eirANF
      pr0 <- passSynolon 7 "Procifying (eidos to synolon)."
            procify eirANF
      -- The lint before the cleanup skips signal-guardedness, the one rule
      -- the cleanup may establish (it removes orphaned blocks).
      when (conf^.C.debugLint) $ Synolon.lintPre pr0
      pr <- passSynolon 8 "Cleaning the machine block graph (synolon)."
            (pure . optimizeProcs) pr0
      Synolon.lint pr
      mapM_ (flip verb () . Synolon.machineSummary) $ Synolon.progProcs pr
      -- The strict reachable-halt check (--no-halt): every block is
      -- reachable after the block-graph cleanup, so any halt terminator
      -- is a state the device can actually freeze in.
      when (conf^.C.noHalt) $ mapM_ noHaltCheck $ Synolon.progProcs pr
      -- --certify validates against exactly this dump (the Synolon program
      -- the fold consumes), so it implies --synolon.
      when (conf^.C.synolon || conf^.C.certify /= C.CertifyOff)
            $ writeDump (C.synolonFile conf fp) "Synolon" $ Synolon.prettyProgram pr
      -- The fold owns the lowering: Synolon straight to Hyle
      -- (ReWire.Synolon.ToHyle).
      pass conf fp 9 "Translating to Hyle." "rwc" prettyPrint (synolonToHyle conf) pr

      where passEidos :: MonadIO n => Natural -> Text -> (a -> n Eidos.Program) -> a -> n Eidos.Program
            passEidos n name = pass conf fp n name "eir" Eidos.prettyProgram

            passSynolon :: MonadIO n => Natural -> Text -> (a -> n Synolon.Program) -> a -> n Synolon.Program
            passSynolon n name = pass conf fp n name "syn" Synolon.prettyProgram

            -- An IR dump beside the output, published by a same-directory
            -- temporary plus rename, so a crash can't leave a torn artifact
            -- that later validates or replays.
            writeDump :: MonadIO n => FilePath -> Text -> Text -> n ()
            writeDump file what txt = do
                  verb ("Writing " <> what <> " IR to file: " <> pack file) ()
                  liftIO $ do
                        T.writeFile (file <> ".tmp") txt
                        renameFile (file <> ".tmp") file

            neuterExterns :: (MonadError AstError m', MonadIO m') => Eidos.Program -> m' Eidos.Program
            neuterExterns p = do
                  (p', ws) <- Eidos.neuterExterns p
                  mapM_ (\ (Warning a m') -> warnAt conf a m') ws
                  pure p'

            optimizeProcs :: Synolon.Program -> Synolon.Program
            optimizeProcs p = p { Synolon.progProcs = map Synolon.optimizeProc $ Synolon.progProcs p }

            -- The bound on the type-specialization fixpoint: at least the
            -- historical bound of 10; --depth raises it (e.g. for deep
            -- dictionary chains).
            specDepth :: Natural
            specDepth = max 10 $ conf^.C.depth

            noHaltCheck :: MonadError AstError m => Synolon.Proc -> m ()
            noHaltCheck p = case concatMap (Synolon.haltSites . Synolon.blkTerm) $ Synolon.allBlocks p of
                  an : _ -> failAt an $ "process " <> Synolon.procName p
                        <> " can halt, and post-halt outputs are unspecified (rejected by --no-halt)."
                  []     -> pure ()

            -- The standing lints (post-bridge, post-inline, post-PE,
            -- post-ANF) run always; --debug-lint re-lints after the
            -- remaining Eidos passes too.
            lintDebug :: MonadError AstError m => Eidos.LintMode -> Eidos.Program -> m ()
            lintDebug mode p | conf^.C.debugLint = Eidos.lint mode p
                             | otherwise         = pure ()

            verb :: MonadIO m => Text -> a -> m a
            verb = verb' conf
