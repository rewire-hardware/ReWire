{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module ReWire.ModCache
      ( runCache
      , getDevice
      , LoadPath
      ) where

import ReWire.Annotation (unAnn)
import ReWire.Config (Config, typecheck)
import ReWire.Eidos.Pretty (prettyProgram)
import ReWire.Eidos.ToCrust (eidosToCrust)
import ReWire.GHC.Session (loadCore)
import ReWire.GHC.ToCrust (coreToCrust, purgeTyAnns)
import ReWire.GHC.ToEidos (toEidos)
import ReWire.Crust.PrimBasis (addPrims)
import ReWire.Crust.Purify (purify)
import ReWire.Crust.Syntax (FreeProgram, Defn (..), Exp, Ty, Kind, DataConId, TyConId, Program (Program), prettyFP)
import ReWire.Crust.ToHyle (toHyle)
import ReWire.Crust.Transform (removeMain, simplifyUntil, synthableDefn, dictFree, liftLambdas, etaAbsDefs, shiftLambdas, neuterExterns, expandTypeSynonyms, inlineAnnotated, normalizeBind, purge, purgeAll, mergeEquivDefns, inlineExtrudes, reduce)
import ReWire.Crust.TypeCheck (typeCheck, untype)
import ReWire.Error (AstError, MonadError, Warning (..), warnAt)
import ReWire.Pass (runPasses, printHeader, verb')
import ReWire.Pretty (prettyPrint, prettyPrint', showt)
import ReWire.Unbound (fv, trec, runFreshMT, FreshMT, Name, Fresh, s2n)

import Control.DeepSeq (deepseq)
import Control.Lens ((^.))
import Control.Monad ((>=>), when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State.Strict (MonadState)
import Data.Containers.ListUtils (nubOrd)
import Data.List (genericLength)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Numeric.Natural (Natural)
import System.FilePath ((-<.>))

import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified ReWire.Eidos.Inline          as Eidos
import qualified ReWire.Eidos.Lint            as Eidos
import qualified ReWire.Eidos.Spec            as Eidos
import qualified ReWire.Hyle.Syntax         as Hyle
import qualified ReWire.Config                as C

type LoadPath = [FilePath]

runCache :: (MonadIO m, MonadError AstError m) => FreshMT m a -> m a
runCache = runFreshMT

-- Pass 1 is the front end: GHC (parse/typecheck/desugar over the whole
-- home module graph) followed by the Core-to-Crust bridge. Whole-program
-- passes are numbered from 2 (see getDevice; run rwc -v for the full
-- list).

-- Pre-hyle transformations.
getDevice :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m) => Config -> FilePath -> FreshMT m Hyle.Program
getDevice conf fp = do
      gutss          <- loadCore conf fp
      -- Under --eidos, the front half runs through the Eidos IR: bridge
      -- Core to Eidos, lint it (poly mode), specialize away polymorphism,
      -- inline INLINE-annotated definitions, lint again (mono mode), dump
      -- the .eir beside the output, and lower onto the retained Crust
      -- pipeline through the shim (ReWire.Eidos.ToCrust), entering at the
      -- extern-neutering pass. The shim moves down the pipeline as passes
      -- are ported; see doc/eidos.md.
      prog0 <- if conf^.C.eidos
            then do
                  eir <- toEidos conf gutss
                  Eidos.lint Eidos.LintPoly eir
                  eir' <- verb "Specializing polymorphic definitions (eidos)." eir
                        >>= Eidos.specialize specDepth
                        >>= verb "Inlining INLINE-annotated definitions (eidos)."
                        >>= Eidos.inlineAnnotated
                  Eidos.lint Eidos.LintMono eir'
                  let eirFile = fromMaybe fp (conf^.C.outFile) -<.> "eir"
                  verb ("Writing Eidos IR to file: " <> pack eirFile) ()
                  liftIO $ T.writeFile eirFile $ prettyProgram eir'
                  eidosToCrust eir'
            else coreToCrust conf gutss
      (ts, syns, ds) <- passCrust conf 1 "Translating GHC Core to Crust IR." prog0

      p <- pure
       >=> runPasses pass forceProg            nFront frontPasses
       >=> runPasses pass (extraTC >=> forceProg) nMid   midPasses
       >=> runPasses pass forceProg            nBack  backPasses
       >=> pass nCore "Translating to hyle & HDL."
       >=> toHyle conf start
       >=> verb ("[" <> showt nFinal <> "] Hyle.")
       $ (ts, syns, ds)

      when ((conf^.C.dump) nFinal) $ liftIO $ do
            printHeader $ "[" <> showt nFinal <> "] Hyle"
            T.putStrLn $ prettyPrint p
            when (conf^.C.verbose) $ do
                  T.putStrLn "\n## Show hyle:\n"
                  T.putStrLn $ showt $ unAnn p

      pure p

      where -- Transformations applied up to and including typechecking.
            -- Under --eidos the front half has already run at the Eidos
            -- level: prims come from the shim, Main.main never survives
            -- the bridge's reachability pruning, INLINE inlining and
            -- specialization (in place of typechecking) ran on Eidos,
            -- synonyms were expanded by the bridge, and the shim's
            -- annotations are all concrete (exactly the ones the
            -- annotation purge keeps) — so only extern neutering remains.
            frontPasses
                  | conf^.C.eidos =
                  [ ("Extracting extern models; removing other Haskell definitions for externs.", neuterExterns')
                  ]
                  | otherwise =
                  [ ("Adding primitives.",                                                     pure . addPrims)
                  , ("Removing the Main.main definition (before attempting to typecheck it).", pure . removeMain)
                  , ("Inlining INLINE-annotated definitions.",                                 inlineAnnotated)
                  , ("Expanding type synonyms.",                                               expandTypeSynonyms)
                  , ("Typechecking; specializing polymorphic definitions.",                    typeCheck specDepth start)
                  , ("Extracting extern models; removing other Haskell definitions for externs.", neuterExterns')
                  -- The bridge annotates nodes liberally for the typecheck
                  -- pass, which has now consumed them; strip them, or the
                  -- annotation binders tax every later substitution
                  -- (ruinously, during partial evaluation).
                  , ("Purging type annotations.",                        pure . purgeTyAnns)
                  ]

            -- Transformations on the typechecked program: each one is
            -- followed by re-typechecking when --debug-typecheck is on.
            midPasses =
                  [ ("Removing unused definitions.",                     purge start)
                  , ("Lifting lambdas.",                                 liftLambdas)
                  -- Partial evaluation must also finish eliminating class
                  -- dictionaries (whose types look synthable; the function
                  -- types hide inside their constructor fields).
                  , ("Partial evaluation.",                              simplifyUntil (\ fp@(_, _, vs) -> all synthableDefn vs && dictFree fp) conf)
                  , ("Normalizing bind.",                                normalizeBind)
                  , ("Lifting lambdas.",                                 liftLambdas)
                  , ("Removing unused definitions.",                     purge start)
                  , ("Inlining extrudes.",                               inlineExtrudes)
                  , ("Reducing.",                                        reduce) -- TODO: reduce here really just for the start defn.
                  -- Inlining extrudes can orphan definitions and reintroduce
                  -- lambdas (e.g., as bind continuations); purify requires
                  -- lambda-lifted bodies and purifies every reachable
                  -- monadic defn, so clean up again before it runs.
                  , ("Removing unused definitions.",                     purge start)
                  , ("Lifting lambdas.",                                 liftLambdas)
                  -- Purify keys resumption states by continuation name, so
                  -- merge duplicate lifted continuations (normalizeBind and
                  -- inlineExtrudes copy them per case arm or call site)
                  -- before it runs.
                  , ("Merging duplicate definitions.",                   mergeEquivDefns)
                  -- TODO: typechecking before or after purify seems to
                  --       subtly effect ordering of things.
                  , ("Shifting lambdas.",                                shiftLambdas)
                  , ("Eta-abstracting definitions.",                     etaAbsDefs)
                  -- Purify emits irrefutable nested cases (a `let`-bound
                  -- `Done (...)` immediately destructured); the trailing
                  -- reduce collapses them into direct calls, which also pins
                  -- the PuRe output type in fragments that never signal (so
                  -- the --debug-typecheck re-typecheck stays unambiguous). It
                  -- is fused into the purify pass so that re-typecheck sees the
                  -- reduced form.
                  , ("Purifying.",                                       purify start >=> reduce)
                  ]

            -- Final cleanup before translation to hyle: no --debug-typecheck
            -- re-typechecking here (the IR is post-purify).
            backPasses =
                  [ ("Final lifting of lambdas.",                        liftLambdas)
                  , ("Final shifting of lambdas.",                       shiftLambdas)
                  , ("Final eta-abstraction of definitions.",            etaAbsDefs)
                  , ("Final purging of unused definitions.",             purgeAll start)
                  ]

            nFront, nMid, nBack, nCore, nFinal :: Natural
            nFront = 2 -- Pass 1 is the GHC front end + Core-to-Crust bridge.
            nMid   = nFront + genericLength frontPasses
            nBack  = nMid   + genericLength midPasses
            nCore  = nBack  + genericLength backPasses
            nFinal = nCore  + 1

            start :: Name Exp
            start = s2n $ conf^.C.start

            -- Force the IR to normal form after each pass. The IR is threaded
            -- lazily through the pipeline; without this, the lazy
            -- unbound-generics bind/unbind thunks produced by each pass chain
            -- through every later pass and are only forced (catastrophically,
            -- to many GB on large programs) when something finally demands
            -- them. Forcing here keeps the working set to one pass's output.
            forceProg :: Monad m => FreeProgram -> m FreeProgram
            forceProg p = p `deepseq` pure p

            extraTC :: (Fresh m, MonadIO m, MonadError AstError m) => FreeProgram -> m FreeProgram
            extraTC | conf^.typecheck = verb "Type-checking again (--debug-typecheck)." >=> typeCheck specDepth start
                    | otherwise       = pure

            -- The bound on the type-specialization fixpoint: at least the
            -- historical bound of 10; --depth raises it (e.g. for deep
            -- dictionary chains).
            specDepth :: Natural
            specDepth = max 10 $ conf^.C.depth

            neuterExterns' :: (Fresh m, MonadIO m, MonadError AstError m) => FreeProgram -> m FreeProgram
            neuterExterns' fp = do
                  (fp', ws) <- neuterExterns fp
                  mapM_ (\ (Warning a m') -> warnAt conf a m') ws
                  pure fp'

            pass :: MonadIO m => Natural -> Text -> FreeProgram -> m FreeProgram
            pass = passCrust conf

            verb :: MonadIO m => Text -> a -> m a
            verb = verb' conf

passCrust :: MonadIO m => Config -> Natural -> Text -> FreeProgram -> m FreeProgram
passCrust conf n m = verb' conf msg
            >=> if (conf^.C.dump) n then printInfo conf msg else pure
      where msg = "[" <> showt n <> "] " <> m

printInfo :: MonadIO m => Config -> Text -> FreeProgram -> m FreeProgram
printInfo conf hd fp = do
      let p = Program $ trec fp
      printHeader hd
      when verbose $ liftIO $ T.putStrLn "-- ## Free kind vars:\n"
      when verbose $ liftIO $ T.putStrLn $ T.concat $ map comVar (nubOrd $ map prettyPrint (fv p :: [Name Kind]))
      when verbose $ liftIO $ T.putStrLn "-- ## Free type vars:\n"
      when verbose $ liftIO $ T.putStrLn $ T.concat $ map comVar (nubOrd $ map prettyPrint (fv p :: [Name Ty]))
      when verbose $ liftIO $ T.putStrLn "-- ## Free tycon vars:\n"
      when verbose $ liftIO $ T.putStrLn $ T.concat $ map comVar (nubOrd $ map prettyPrint (fv p :: [Name TyConId]))
      liftIO $ T.putStrLn "-- ## Free con vars:\n"
      liftIO $ T.putStrLn $ T.concat $ map comVar (nubOrd $ map prettyPrint (fv p :: [Name DataConId]))
      liftIO $ T.putStrLn "-- ## Free exp vars:\n"
      liftIO $ T.putStrLn $ T.concat $ map comVar (nubOrd $ map prettyPrint (fv p :: [Name Exp]))
      liftIO $ T.putStrLn "-- ## Program:\n"
      fp' <- purgeAll start fp
      liftIO . T.putStrLn $ prettyPrint' $ prettyFP $ if verbose then fp' else untype' fp'
      when verbose $ liftIO $ T.putStrLn "\n-- ## Program (show):\n"
      when verbose $ liftIO $ T.putStrLn $ showt $ unAnn fp
      pure fp

      where untype' :: FreeProgram -> FreeProgram
            untype' (ts, syns, vs) = (ts, syns, map untype'' vs)

            untype'' :: Defn -> Defn
            untype'' d = d { defnBody = untype $ defnBody d }

            comVar :: Text -> Text
            comVar = (<> "\n") . ("-- " <>)

            verbose :: Bool
            verbose = conf^.C.verbose

            start :: Name Exp
            start = s2n $ conf^.C.start
