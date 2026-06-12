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
import ReWire.Crust.KindCheck (kindCheck)
import ReWire.Crust.PrimBasis (addPrims)
import ReWire.Crust.Purify (purify)
import ReWire.Crust.Syntax (FreeProgram, Defn (..), Module (Module), Exp, Ty, Kind, DataConId, TyConId, Program (Program), prettyFP)
import ReWire.Crust.ToHyle (toHyle)
import ReWire.Crust.Transform (removeMain, simplify, liftLambdas, etaAbsDefs, shiftLambdas, neuterExterns, expandTypeSynonyms, inlineAnnotated, normalizeBind, elimCase, purge, purgeAll, inlineExtrudes, reduce)
import ReWire.Crust.TypeCheck (typeCheck, untype)
import ReWire.Error (AstError, MonadError, Warning (..), warnAt)
import ReWire.HSE.Annotate (annotate)
import ReWire.HSE.Cache (LoadPath, getModuleWith)
import ReWire.HSE.Desugar (desugar)
import ReWire.HSE.Rename (Exports, Renamer, fixFixity)
import ReWire.HSE.ToCrust (toCrust)
import ReWire.Pass (runPasses, printHeader, printInfoHSE, verb')
import ReWire.Pretty (prettyPrint, prettyPrint', showt)
import ReWire.Unbound (fv, trec, runFreshMT, FreshMT, Name, Fresh, s2n)

import Control.Lens ((^.))
import Control.Monad ((>=>), when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State.Strict (MonadState, lift)
import Data.Containers.ListUtils (nubOrd)
import Data.List (genericLength)
import Data.Text (Text, pack)
import Numeric.Natural (Natural)

import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Language.Haskell.Exts.Syntax as S (Module (..))
import qualified ReWire.Hyle.Syntax         as Hyle
import qualified ReWire.HSE.Cache             as Cache
import qualified ReWire.Config                as C

type Cache m = Cache.Cache Module (FreshMT m)

runCache :: (MonadIO m, MonadError AstError m) => Cache m a -> m a
runCache = runFreshMT . Cache.runCache

-- Per-module passes (numbered in -v output):
-- Pass 1   Fixity fixing.
-- Pass 2   Annotation.
-- Pass 3   Desugaring.
-- Pass 4   Translation to the Crust IR.
-- Pass 5   Concatenation with imports.
-- Whole-program passes are numbered from 6 (see getDevice; run rwc -v for
-- the full list).

getModule :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m) => Config -> FilePath -> FilePath -> Cache m (Module, Exports)
getModule conf = getModuleWith translate conf
      where translate fp rn imps m = do
                  let pass :: MonadIO m => Natural -> Text -> S.Module a -> m (S.Module a)
                      pass = passHSE conf rn imps

                  -- Phase 1 (haskell-src-exts) transformations.
                  (m', exps) <- pure
                            >=> pass 1 "(Haskell) Fixing fixity."
                            >=> lift . lift . fixFixity rn
                            >=> pass 2 "(Haskell) Annotating."
                            >=> pure . annotate
                            >=> pass 3 "(Haskell) Desugaring."
                            >=> desugar rn
                            >=> pass 4 "(Haskell) Translating to Crust IR."
                            >=> toCrust rn
                            $ m

                  let Module ts syns ds = m' <> imps
                  _ <- passCrust conf 5 ("Concatenating Crust IR for module: " <> pack fp) (ts, syns, ds)

                  pure (m' <> imps, exps)

-- Phase 2 (pre-hyle) transformations.
getDevice :: (MonadIO m, MonadFail m, MonadError AstError m, MonadState AstError m) => Config -> FilePath -> Cache m Hyle.Program
getDevice conf fp = do
      (Module ts syns ds,  _)  <- getModule conf "." fp

      p <- pure
       >=> runPasses pass pure    nFront frontPasses
       >=> runPasses pass extraTC nMid   midPasses
       >=> runPasses pass pure    nBack  backPasses
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
            frontPasses =
                  [ ("Adding primitives.",                                                     pure . addPrims)
                  , ("Removing the Main.main definition (before attempting to typecheck it).", pure . removeMain)
                  , ("Inlining INLINE-annotated definitions.",                                 inlineAnnotated)
                  , ("Expanding type synonyms.",                                               expandTypeSynonyms)
                  , ("Typechecking, inference.",                                               kindCheck >=> typeCheck start)
                  , ("Extracting extern models; removing other Haskell definitions for externs.", neuterExterns')
                  ]

            -- Transformations on the typechecked program: each one is
            -- followed by re-typechecking when --debug-typecheck is on.
            midPasses =
                  [ ("Removing unused definitions.",                     purge start)
                  , ("Eliminating pattern bindings (case expressions).", elimCase)
                  , ("Lifting lambdas.",                                 liftLambdas)
                  , ("Partial evaluation.",                              simplify conf)
                  , ("Normalizing bind.",                                normalizeBind)
                  , ("Lifting lambdas.",                                 liftLambdas)
                  , ("Removing unused definitions.",                     purge start)
                  , ("Inlining extrudes.",                               inlineExtrudes)
                  , ("Reducing.",                                        reduce) -- TODO: reduce here really just for the start defn.
                  -- TODO: typechecking before or after purify seems to
                  --       subtly effect ordering of things.
                  , ("Shifting lambdas.",                                shiftLambdas)
                  , ("Eta-abstracting definitions.",                     etaAbsDefs)
                  , ("Purifying.",                                       purify start)
                  ]

            -- Final cleanup before translation to hyle: no --debug-typecheck
            -- re-typechecking here (e.g., kindCheck fails once purgeAll has
            -- purged type synonyms).
            backPasses =
                  [ ("Final lifting of lambdas.",                        liftLambdas)
                  , ("Final shifting of lambdas.",                       shiftLambdas)
                  , ("Final eta-abstraction of definitions.",            etaAbsDefs)
                  , ("Final purging of unused definitions.",             purgeAll start)
                  ]

            nFront, nMid, nBack, nCore, nFinal :: Natural
            nFront = 6 -- HSE passes are numbered 1-5 (see getModule).
            nMid   = nFront + genericLength frontPasses
            nBack  = nMid   + genericLength midPasses
            nCore  = nBack  + genericLength backPasses
            nFinal = nCore  + 1

            start :: Name Exp
            start = s2n $ conf^.C.start

            extraTC :: (Fresh m, MonadIO m, MonadError AstError m) => FreeProgram -> m FreeProgram
            extraTC | conf^.typecheck = verb "Type-checking again (--debug-typecheck)." >=> kindCheck >=> typeCheck start
                    | otherwise       = pure

            neuterExterns' :: (Fresh m, MonadIO m, MonadError AstError m) => FreeProgram -> m FreeProgram
            neuterExterns' fp = do
                  (fp', ws) <- neuterExterns fp
                  mapM_ (\ (Warning a m') -> warnAt conf a m') ws
                  pure fp'

            pass :: MonadIO m => Natural -> Text -> FreeProgram -> m FreeProgram
            pass = passCrust conf

            verb :: MonadIO m => Text -> a -> m a
            verb = verb' conf

passHSE :: MonadIO m => Config -> Renamer -> Module -> Natural -> Text -> S.Module a -> m (S.Module a)
passHSE conf rn imps n m = verb' conf msg
            >=> if (conf^.C.dump) n then printInfoHSE msg rn imps (conf^.C.verbose) else pure
      where msg = "[" <> showt n <> "] " <> m

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
