{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.ModCache
      ( runCache
      , getDevice
      , LoadPath
      ) where

import ReWire.Annotation (noAnn, unAnn)
import ReWire.Config (Config)
import ReWire.Eidos.Pretty (prettyProgram)
import ReWire.Eidos.ToHyle (eidosToHyle)
import ReWire.GHC.Session (loadCore)
import ReWire.GHC.ToEidos (toEidos)
import ReWire.Error (AstError, MonadError, Warning (..), failAt, warnAt)
import ReWire.Pass (pass, verb')
import ReWire.Pretty (prettyPrint, showt)
import ReWire.Unbound (runFreshMT, FreshMT)

import Control.Lens ((^.))
import Control.Monad (when, (>=>))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State.Strict (MonadState)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Numeric.Natural (Natural)
import System.FilePath ((-<.>))

import qualified Data.Text.IO                 as T
import qualified ReWire.Eidos.ANF             as Eidos
import qualified ReWire.Eidos.Externs         as Eidos
import qualified ReWire.Eidos.Inline          as Eidos
import qualified ReWire.Eidos.Lint            as Eidos
import qualified ReWire.Eidos.ProcOpt         as Eidos
import qualified ReWire.Eidos.Procify         as Eidos
import qualified ReWire.Eidos.Simplify        as Eidos
import qualified ReWire.Eidos.Spec            as Eidos
import qualified ReWire.Eidos.Syntax          as Eidos
import qualified ReWire.Hyle.Syntax         as Hyle
import qualified ReWire.Config                as C

type LoadPath = [FilePath]

runCache :: (MonadIO m, MonadError AstError m) => FreshMT m a -> m a
runCache = runFreshMT

-- The numbered pass pipeline: run rwc -v to see the bracketed pass numbers;
-- -d N dumps the IR after pass N. Pass 1 is the front end: GHC
-- (parse/typecheck/desugar over the whole home module graph) followed by
-- the Core-to-Eidos bridge. Passes 2-8 are the Eidos passes (doc/eidos.md)
-- and pass 9 is the Eidos-to-Hyle fold; the Hyle-level passes (10-11) run
-- in ReWire.FrontEnd, numbered after these so -d numbering is uniform.
getDevice :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m) => Config -> FilePath -> FreshMT m Hyle.Program
getDevice conf fp = do
      eir <- passEidos 1 "GHC front end and Core-to-Eidos bridge."
            (loadCore conf >=> toEidos conf) fp
      Eidos.lint Eidos.LintPoly eir
      -- The front half of the Eidos pipeline: specialize away
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
      -- The machine half: normalize the reactive fragment to ANF,
      -- procify it, clean the block graph, and check the machine rules.
      eirANF <- passEidos 6 "Normalizing to ANF (eidos)."
            Eidos.normalize eirPE
      Eidos.lint Eidos.LintMonoANF eirANF
      pr0 <- passEidos 7 "Procifying (eidos)."
            Eidos.procify eirANF
      eirPE' <- passEidos 8 "Cleaning the machine block graph (eidos)."
            (pure . optimizeProcs) pr0
      mapM_ (Eidos.lintProc eirPE') $ Eidos.progProcs eirPE'
      mapM_ (flip verb () . Eidos.machineSummary) $ Eidos.progProcs eirPE'
      -- The strict reachable-halt check (--no-halt): every block is
      -- reachable after the block-graph cleanup, so any halt terminator
      -- is a state the device can actually freeze in.
      when (conf^.C.noHalt) $ mapM_ noHaltCheck $ Eidos.progProcs eirPE'
      when (conf^.C.eidos) $ do
            let eirFile = fromMaybe fp (conf^.C.outFile) -<.> "eir"
            verb ("Writing Eidos IR to file: " <> pack eirFile) ()
            liftIO $ T.writeFile eirFile $ prettyProgram eirPE'
      -- The machine adapter owns the lowering (the retired purifier's
      -- output shape). Every well-formed device has a reactive root
      -- (the mono lint's device rule), hence a process.
      when (null $ Eidos.progProcs eirPE') $ failAt noAnn
            "no process was constructed for the device root (rwc bug)."
      -- The fold owns the lowering: Eidos straight to Hyle
      -- (ReWire.Eidos.ToHyle).
      pass conf 9 "Translating to Hyle." renderHyle (eidosToHyle conf) eirPE'

      where passEidos :: MonadIO n => Natural -> Text -> (a -> n Eidos.Program) -> a -> n Eidos.Program
            passEidos n name = pass conf n name prettyProgram

            neuterExterns :: (MonadError AstError m', MonadIO m') => Eidos.Program -> m' Eidos.Program
            neuterExterns p = do
                  (p', ws) <- Eidos.neuterExterns p
                  mapM_ (\ (Warning a m') -> warnAt conf a m') ws
                  pure p'

            optimizeProcs :: Eidos.Program -> Eidos.Program
            optimizeProcs pr = pr { Eidos.progProcs = map Eidos.optimizeProc $ Eidos.progProcs pr }

            renderHyle :: Hyle.Program -> Text
            renderHyle p = prettyPrint p
                  <> (if conf^.C.verbose then "\n\n## Show hyle:\n\n" <> showt (unAnn p) else mempty)

            -- The bound on the type-specialization fixpoint: at least the
            -- historical bound of 10; --depth raises it (e.g. for deep
            -- dictionary chains).
            specDepth :: Natural
            specDepth = max 10 $ conf^.C.depth

            noHaltCheck :: MonadError AstError m => Eidos.Proc -> m ()
            noHaltCheck pr = case concatMap (halts . Eidos.blkTerm) $ Eidos.procEntry pr : map snd (Eidos.procBlocks pr) of
                  an : _ -> failAt an $ "process " <> Eidos.procName pr
                        <> " can halt, and post-halt outputs are unspecified (rejected by --no-halt)."
                  []     -> pure ()
                  where halts = \ case
                              Eidos.Halt an _      -> [an]
                              Eidos.TCase _ _ alts -> concat [ halts t | Eidos.TAlt _ _ _ t <- alts ]
                              _                    -> []

            -- The standing lints (post-bridge, post-inline, post-PE) run
            -- always; --debug-lint re-lints after the remaining Eidos
            -- passes too.
            lintDebug :: MonadError AstError m => Eidos.LintMode -> Eidos.Program -> m ()
            lintDebug mode p | conf^.C.debugLint = Eidos.lint mode p
                             | otherwise         = pure ()

            verb :: MonadIO m => Text -> a -> m a
            verb = verb' conf
