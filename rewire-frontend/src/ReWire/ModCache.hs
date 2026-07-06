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
import ReWire.Pass (printHeader, verb')
import ReWire.Pretty (prettyPrint, showt)
import ReWire.Unbound (runFreshMT, FreshMT)

import Control.Lens ((^.))
import Control.Monad (when)
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

-- Pass 1 is the front end: GHC (parse/typecheck/desugar over the whole
-- home module graph) followed by the Core-to-Eidos bridge; pass 2 is the
-- Eidos-to-Hyle fold (-d 2 dumps the Hyle IR; --eidos dumps the Eidos IR).
getDevice :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m) => Config -> FilePath -> FreshMT m Hyle.Program
getDevice conf fp = do
      gutss          <- loadCore conf fp
      -- The front half runs through the Eidos IR (doc/eidos.md): bridge
      -- Core to Eidos, lint it (poly mode), specialize away polymorphism,
      -- inline INLINE-annotated definitions, lint again (mono mode),
      -- neuter externs (before the partial evaluator, always), partially
      -- evaluate to the synthable/dictionary-free fixpoint, lint once
      -- more, and dump the .eir beside the output under --eidos.
      -- --debug-lint adds a lint after the remaining Eidos passes.
      eir <- toEidos conf gutss
      Eidos.lint Eidos.LintPoly eir
      eirSpec <- verb "Specializing polymorphic definitions (eidos)." eir
            >>= Eidos.specialize specDepth
      lintDebug Eidos.LintMono eirSpec
      eirInl <- verb "Inlining INLINE-annotated definitions (eidos)." eirSpec
            >>= Eidos.inlineAnnotated
      Eidos.lint Eidos.LintMono eirInl
      (eirExt, ws) <- verb "Extracting extern models (eidos)." eirInl
            >>= Eidos.neuterExterns
      mapM_ (\ (Warning a m') -> warnAt conf a m') ws
      lintDebug Eidos.LintMono eirExt
      eirPE <- verb "Partial evaluation (eidos)." eirExt
            >>= Eidos.simplify (conf^.C.depth)
      Eidos.lint Eidos.LintMono eirPE
      -- The machine half: normalize the reactive fragment to ANF,
      -- procify it, clean the block graph, and check the machine rules.
      eirANF <- verb "Normalizing to ANF (eidos)." eirPE >>= Eidos.normalize
      Eidos.lint Eidos.LintMonoANF eirANF
      pr0 <- verb "Procifying (eidos)." eirANF >>= Eidos.procify
      let eirPE' = pr0 { Eidos.progProcs = map Eidos.optimizeProc $ Eidos.progProcs pr0 }
      mapM_ (Eidos.lintProc eirPE') $ Eidos.progProcs eirPE'
      mapM_ (flip verb () . Eidos.machineSummary) $ Eidos.progProcs eirPE'
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
      p <- verb ("[" <> showt nFinal <> "] Translating to Hyle.") ()
            >> eidosToHyle conf eirPE'

      when ((conf^.C.dump) nFinal) $ liftIO $ do
            printHeader $ "[" <> showt nFinal <> "] Hyle"
            T.putStrLn $ prettyPrint p
            when (conf^.C.verbose) $ do
                  T.putStrLn "\n## Show hyle:\n"
                  T.putStrLn $ showt $ unAnn p

      pure p

      where nFinal :: Natural
            nFinal = 2 -- Pass 1 is the GHC front end + Core-to-Eidos bridge.

            -- The bound on the type-specialization fixpoint: at least the
            -- historical bound of 10; --depth raises it (e.g. for deep
            -- dictionary chains).
            specDepth :: Natural
            specDepth = max 10 $ conf^.C.depth

            -- The standing lints (post-bridge, post-inline, post-PE) run
            -- always; --debug-lint re-lints after the remaining Eidos
            -- passes too.
            lintDebug :: MonadError AstError m => Eidos.LintMode -> Eidos.Program -> m ()
            lintDebug mode p | conf^.C.debugLint = Eidos.lint mode p
                             | otherwise         = pure ()

            verb :: MonadIO m => Text -> a -> m a
            verb = verb' conf
