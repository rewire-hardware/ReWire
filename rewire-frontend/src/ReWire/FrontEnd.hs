{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.FrontEnd
      ( LoadPath
      , compileFile
      ) where

import ReWire.Annotation (Annote, noAnn, unAnn)
import ReWire.Config (Config, Language (..), Certify (..), getOutFile, target, cycles, inputsFile, defaultInputsFile, source, rtlOpt, testbench, pDebug, loadPath, locators, noLocators)
import ReWire.Error (MonadError, AstError, runSyntaxError, failAt, warnAt, printError, relocatingNoLocTo, filePath)
import ReWire.Hyle.Interp (Ins, run)
import ReWire.Hyle.Parse (parseHyle)
import ReWire.Hyle.Syntax (Program, progDevice)
import ReWire.ModCache (getDevice, LoadPath)
import ReWire.Pass (pass)
import ReWire.Pretty (Pretty, prettyPrint, fastPrint, showt)
import ReWire.Sha256 (hashHex)

import qualified ReWire.Config           as Config
import qualified ReWire.Hyle.Check     as Hyle
import qualified ReWire.Hyle.Interp    as Hyle
import qualified ReWire.Hyle.ToCryptol as HyleCry
import qualified ReWire.Hyle.ToVHDL    as HyleH
import qualified ReWire.Hyle.ToVerilog as HyleV
import qualified ReWire.Hyle.Transform as Hyle

import Control.Exception (IOException, try)
import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import Data.Aeson (FromJSON (..), eitherDecodeStrict, withObject, (.:))
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Clock (getMonotonicTimeNSec)
import Numeric.Natural (Natural)
import System.Directory (doesFileExist, executable, findExecutable, getPermissions, renameFile)
import System.Environment (lookupEnv, getExecutablePath)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (dropExtension, takeDirectory, takeExtension, (<.>), (-<.>), (</>))
import System.IO (stderr)
import System.Process (proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)

import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as Text
import qualified Data.Text.IO        as T
import qualified Data.Yaml           as YAML

-- | Loads and compiles a file (and, recursively, its imports) to a Hyle
--   program (the whole pass pipeline through the Eidos-to-Hyle fold).
loadProgram :: (MonadFail m, MonadError AstError m, MonadState AstError m, MonadIO m) => Config -> FilePath -> m Program
loadProgram = getDevice

compileFile :: MonadIO m => Config -> FilePath -> m ()
compileFile conf filename = do
      verb $ "Compiling: " <> pack filename

      runSyntaxError (relocatingNoLocTo (filePath filename) $ checkCertifyPaths >> load >>= Hyle.check >>= compile)
            >>= either (\ err -> printError (conf ^. loadPath) err >> liftIO exitFailure) pure

      where load :: (MonadError AstError m, MonadState AstError m, MonadFail m, MonadIO m) => m Program
            load = case conf^.source of
                  Haskell -> loadProgram conf filename
                  RWCore  -> parseHyle filename
                  s       -> failAt noAnn $ "Not a supported source language: " <> pack (show s)

            -- The Hyle-level passes are numbered after ReWire.ModCache's 1-9
            -- (which -d/-v also address, but which only run for Haskell
            -- source), so the -d numbering is uniform across --from-core.
            compile :: (MonadFail m, MonadError AstError m, MonadIO m) => Program -> m ()
            compile a = do
                  when (conf^.testbench && (conf^.target) `notElem` [VHDL, Verilog]) $
                        warnAt conf noAnn "--testbench: no testbench generated (only the Verilog and VHDL targets support testbench generation)."
                  when (certifyOn && conf^.target == Interpret) $
                        notCertified noAnn "nothing to certify (only device targets are certified)."
                  p10 <- pass conf filename 10 "Partially evaluating/reducing the Hyle IR (if this is slow, consider --rtl-opt=0)." "rwc" prettyPrint
                        (Hyle.check . Hyle.optimize (conf^.rtlOpt)) a
                  -- Pass 11 runs for every device target, so every consumer
                  -- -- the HDL and Cryptol backends, the interpreter, the
                  -- .rwc emitted by --core, and the certified artifact --
                  -- reads the same fully lowered program.
                  p <- pass conf filename 11 "Inlining Hyle definitions." "rwc" prettyPrint
                        (Hyle.check . Hyle.inline (conf^.Config.flatten)) p10
                  case conf^.target of
                        VHDL      -> do
                              HyleH.compileProgram conf p >>= writeOutput
                              writeTestbench $ HyleH.testbench conf $ progDevice p
                              certifyOutput p
                        Verilog   -> do
                              HyleV.compileProgram conf p >>= writeOutput
                              writeTestbench $ HyleV.testbench conf $ progDevice p
                              certifyOutput p
                        Cryptol   -> do
                              HyleCry.compileProgram conf p >>= writeOutput
                              certifyOutput p
                        RWCore    -> do
                              writeOutputText $ renderCore p
                              certifyOutput p
                        Interpret -> do
                              ips  <- loadInputs
                              verb $ "Interpreting hyle: running for " <> showt (length ips) <> " cycles."
                              outs <- run conf (Hyle.interp conf p) ips
                              let fout = getOutFile conf filename
                              verb $ "Interpreting hyle: done running; writing YAML output to file: " <> pack fout
                              liftIO $ YAML.encodeFile fout outs
                        Haskell   -> failAt noAnn "Haskell is not a supported target language."

            -- | Inputs for --interpret and --testbench, padded/truncated to
            --   the cycle count. An unreadable inputs file means all wires
            --   are driven to zero -- warn, unless the file is missing and
            --   the user never named one explicitly (driving a device with
            --   no inputs file is a legitimate workflow).
            loadInputs :: (MonadError AstError m, MonadIO m) => m [Ins]
            loadInputs = do
                  verb $ "Reading inputs: " <> pack (conf^.inputsFile)
                  r <- liftIO $ YAML.decodeFileEither $ conf^.inputsFile
                  case r of
                        Right ips -> pure $ boundInput (effectiveCycles conf ips) ips
                        Left err  -> do
                              exists <- liftIO $ doesFileExist $ conf^.inputsFile
                              when (exists || conf^.inputsFile /= defaultInputsFile) $ warnAt conf noAnn
                                    $ "could not read inputs from " <> pack (conf^.inputsFile)
                                    <> (if exists then " (" <> pack (YAML.prettyPrintParseException err) <> ")" else " (file does not exist)")
                                    <> "; driving all inputs with zeros."
                              pure $ boundInput (effectiveCycles conf mempty) mempty

            writeTestbench :: (MonadError AstError m, MonadIO m, Pretty tb) => ([Ins] -> tb) -> m ()
            writeTestbench gen = when (conf^.testbench) $ do
                  ips <- loadInputs
                  let fout = getOutFile conf filename
                      tbout = dropExtension fout <> "_tb" <.> takeExtension fout
                  verb $ "Writing testbench to file: " <> pack tbout
                  liftIO $ T.writeFile tbout $ (if conf^.Config.pretty then prettyPrint else fastPrint) $ gen ips

            writeOutput :: (MonadError AstError m, MonadIO m, Pretty a) => a -> m ()
            writeOutput = writeOutputText . (if conf^.Config.pretty then prettyPrint else fastPrint)

            writeOutputText :: (MonadError AstError m, MonadIO m) => Text -> m ()
            writeOutputText txt = do
                  let fout = getOutFile conf filename
                  verb $ "Writing to file: " <> pack fout
                  liftIO $ T.writeFile fout txt

            -- | The rendered .rwc text, shared by the RWCore target and the
            --   --certify artifact so the two outputs are byte-identical.
            --   It only carries source locators ('--@' lines) under
            --   --locators (and not under --no-locators, which wins): spans
            --   can embed absolute paths, which would destabilize golden
            --   files. Doc ('--|') and 'tag' lines are path-free and not
            --   gated.
            renderCore :: Program -> Text
            renderCore p
                  | conf^.locators && not (conf^.noLocators) = render p
                  | otherwise                                = render $ scrubSpans p
                  where render :: Program -> Text
                        render = if conf^.Config.pretty then prettyPrint else fastPrint

            -- | --certify: write the certified pair beside the output --
            --   the Synolon IR (<out>.syn, the --synolon dump, written by
            --   ReWire.ModCache) and the final backend-consumed
            --   Hyle program (<out>.rwc, byte-identical to the --core
            --   output) -- run the verified validator on it over the
            --   versioned response protocol, and surface the verdict: a
            --   one-line confirmation on VALIDATED, otherwise fatal
            --   (required mode, the default) or an unsuppressible status
            --   line (--certify=warn). See doc/certify.md.
            certifyOutput :: (MonadError AstError m, MonadIO m) => Program -> m ()
            certifyOutput p11 = when certifyOn $ case conf^.source of
                  -- The artifact shares the --core output naming, so an
                  -- HDL/Cryptol output explicitly named *.rwc (-o) would
                  -- collide with it; refuse rather than clobber the
                  -- requested output. (For the RWCore target the collision
                  -- is the point: the same bytes are written either way.)
                  Haskell | conf^.target /= RWCore && samePath rwcFile (getOutFile conf filename) ->
                        notCertified (filePath filename)
                              $ "the certify artifact (" <> pack rwcFile
                              <> ") would overwrite the requested output file; pass a different -o to certify this compilation."
                  Haskell -> do
                        verb $ "certify: writing the final (backend-consumed) Hyle IR to file: " <> pack rwcFile
                        liftIO $ do
                              T.writeFile (rwcFile <> ".tmp") $ renderCore p11
                              renameFile (rwcFile <> ".tmp") rwcFile
                        liftIO findRwv >>= \ case
                              Left why  -> notCertified (filePath filename) why
                              Right rwv -> do
                                    verb $ "certify: running the validator: " <> pack rwv <> " " <> pack synFile <> " " <> pack rwcFile
                                    (status, detail) <- liftIO $ runValidator rwv synFile rwcFile
                                    case status of
                                          "validated" -> liftIO $ T.putStrLn
                                                $ "certify: VALIDATED: the compiled device (" <> pack rwcFile
                                                <> ") implements the Synolon machine (" <> pack synFile <> ")."
                                          _ -> notCertified (filePath filename)
                                                $ Text.toUpper status <> ": " <> detail
                                                <> " (artifacts: " <> pack synFile <> ", " <> pack rwcFile <> ")."
                  -- Under --from-core the front-end passes never run, so
                  -- there is no Synolon IR to validate against.
                  _       -> notCertified noAnn "nothing to certify (certification requires compiling from Haskell source; no Synolon IR exists under --from-core)."

            -- | Refuse up front (in required mode) an output naming under
            --   which certification would corrupt its own artifacts: the
            --   pass-8 .syn dump and the final .rwc are written beside the
            --   output, so the requested output must not claim either name
            --   (modulo case, for case-insensitive filesystems). Under
            --   --certify=warn the compilation proceeds and the collision
            --   surfaces as a not-validated status.
            checkCertifyPaths :: (MonadError AstError m, MonadIO m) => m ()
            checkCertifyPaths = when (certifyOn && conf^.Config.certify == CertifyRequired && conf^.source == Haskell) $ do
                  let fout = getOutFile conf filename
                  when (samePath fout synFile) $ failAt (filePath filename)
                        $ "certify: the requested output file (" <> pack fout
                        <> ") collides with the certify artifact (" <> pack synFile <> "); pass a different -o."
                  when (conf^.target /= RWCore && samePath fout rwcFile) $ failAt (filePath filename)
                        $ "certify: the requested output file (" <> pack fout
                        <> ") collides with the certify artifact (" <> pack rwcFile <> "); pass a different -o."

            -- | A non-validated certification outcome: fatal when
            --   certification is required (the default); an unsuppressible
            --   status line under --certify=warn, printed directly to
            --   stderr -- an explicitly requested best-effort report must
            --   not vanish under -w, and -Werror does not govern it.
            notCertified :: (MonadError AstError m, MonadIO m) => Annote -> Text -> m ()
            notCertified an msg
                  | conf^.Config.certify == CertifyRequired = failAt an $ "certify: not validated: " <> msg
                  | otherwise = liftIO $ T.hPutStrLn stderr $ "certify: not validated: " <> msg

            certifyOn :: Bool
            certifyOn = conf^.Config.certify /= CertifyOff

            samePath :: FilePath -> FilePath -> Bool
            samePath a b = map toLower a == map toLower b

            fout'   = fromMaybe filename $ conf^.Config.outFile
            synFile = Config.synolonFile conf filename
            rwcFile = fout' -<.> "rwc"

            verb :: MonadIO m => Text -> m ()
            verb = pDebug conf

-- | Strip all provenance from the program's annotations (a generic sweep;
--   Hyle types derive Data) so the printed .rwc carries no '--@' locator
--   lines.
scrubSpans :: Program -> Program
scrubSpans = unAnn

-- | The verified Synolon-to-Hyle validator executable (built from verify/
--   with Lake; see doc/certify.md).
rwvExe :: String
rwvExe = "rwv-cstep-validate"

-- | Locate the validator: RWC_RWV (which must name an executable file --
--   a broken override fails closed rather than falling through), then
--   next to the rwc executable, then the PATH. There is deliberately no
--   cwd-relative fallback: the selected executable is part of the trust
--   base, and rwc must never execute a binary planted in whatever
--   directory it happens to be invoked from.
findRwv :: IO (Either Text FilePath)
findRwv = lookupEnv "RWC_RWV" >>= \ case
      Just r  -> executableAt r >>= \ case
            True  -> pure $ Right r
            False -> pure $ Left $ "RWC_RWV is set to " <> pack r <> ", which does not exist or is not executable."
      Nothing -> do
            cand <- (</> rwvExe) . takeDirectory <$> getExecutablePath
            executableAt cand >>= \ case
                  True  -> pure $ Right cand
                  False -> findExecutable rwvExe >>= \ case
                        Just r  -> pure $ Right r
                        Nothing -> pure $ Left $ "the validator (" <> pack rwvExe
                              <> ") was not found next to rwc or on the PATH; build it with"
                              <> " 'cd verify && lake build " <> pack rwvExe <> "' in a ReWire checkout"
                              <> " and install it next to rwc or on the PATH (or set RWC_RWV to its location)."
      where executableAt :: FilePath -> IO Bool
            executableAt f = doesFileExist f >>= \ case
                  False -> pure False
                  True  -> executable <$> getPermissions f

-- | The validator's protocol-2 response: exactly one JSON object on
--   stdout identifying the tool, verdict, echoed nonce, and the SHA-256
--   of the artifact bytes the validator actually read.
data RwvResponse = RwvResponse
      { rwvTool     :: !Text
      , rwvProtocol :: !Int
      , rwvStatus   :: !Text
      , rwvDetail   :: !Text
      , rwvNonce    :: !Text
      , rwvSource   :: !Text
      , rwvTarget   :: !Text
      }

instance FromJSON RwvResponse where
      parseJSON = withObject "rwv response" $ \ o -> RwvResponse
            <$> o .: "tool"
            <*> o .: "protocol"
            <*> o .: "status"
            <*> o .: "detail"
            <*> o .: "nonce"
            <*> (o .: "source" >>= withObject "artifact" (.: "sha256"))
            <*> (o .: "target" >>= withObject "artifact" (.: "sha256"))

-- | One validator invocation, fail-closed: the result is
--   (status, detail), where status is "validated" only if the process
--   exited successfully AND printed exactly one well-formed protocol-2
--   response whose nonce echoes this invocation and whose artifact
--   hashes match the bytes we hashed independently. Spawn failures,
--   timeouts, nonzero exits, malformed or ambiguous output, and
--   mismatched identities all classify as "error".
runValidator :: FilePath -> FilePath -> FilePath -> IO (Text, Text)
runValidator rwv synFile rwcFile = do
      synHash <- hashHex <$> BS.readFile synFile
      rwcHash <- hashHex <$> BS.readFile rwcFile
      nonce   <- (\ t -> hashHex $ encodeUtf8 $ synHash <> rwcHash <> pack (show t)) <$> getMonotonicTimeNSec
      r       <- try $ timeout (validatorTimeoutSecs * 1000000)
            $ readCreateProcessWithExitCode (proc rwv [synFile, rwcFile, "--protocol=2", "--nonce=" <> unpack nonce]) ""
      pure $ case r of
            Left (ex :: IOException)     -> ("error", "could not run the validator: " <> pack (show ex))
            Right Nothing                -> ("error", "the validator timed out after " <> showt validatorTimeoutSecs <> " seconds")
            Right (Just (code, out, _))  -> case filter (not . null) $ lines out of
                  [l] -> case eitherDecodeStrict $ encodeUtf8 $ pack l of
                        Left perr -> ("error", "malformed validator response: " <> pack perr)
                        Right resp
                              | rwvTool resp /= "rwv-cstep-validate" -> ("error", "the response names an unexpected tool: " <> rwvTool resp)
                              | rwvProtocol resp /= 2                -> ("error", "the response speaks an unexpected protocol version: " <> showt (rwvProtocol resp))
                              | rwvNonce resp /= nonce               -> ("error", "the response does not echo this invocation's nonce")
                              | rwvSource resp /= synHash            -> ("error", "the response's source hash does not match the artifact bytes")
                              | rwvTarget resp /= rwcHash            -> ("error", "the response's target hash does not match the artifact bytes")
                              | rwvStatus resp == "validated", code /= ExitSuccess -> ("error", "the validator reported validated but exited nonzero")
                              | rwvStatus resp /= "validated", code == ExitSuccess -> ("error", "the validator exited successfully without reporting validated")
                              | rwvStatus resp `elem` ["validated", "rejected", "unsupported", "error"] -> (rwvStatus resp, rwvDetail resp)
                              | otherwise -> ("error", "unknown validator status: " <> rwvStatus resp)
                  []  -> ("error", "the validator produced no response (exit: " <> pack (show code) <> ")")
                  _   -> ("error", "the validator produced multiple responses")

-- | How long one validator run may take. The corpus giants complete in
--   well under a minute; ten minutes is a generous ceiling.
validatorTimeoutSecs :: Int
validatorTimeoutSecs = 600

-- | The number of cycles to interpret/simulate: the explicit --cycles value if
--   the user gave one, otherwise the larger of 10 or the number of inputs
--   supplied in the inputs file.
effectiveCycles :: Config -> [Ins] -> Natural
effectiveCycles conf ips = fromMaybe (max 10 (fromIntegral (length ips))) (conf^.cycles)

-- | Replicates/truncates inputs to fill up exactly ncycles cycles.
boundInput :: Natural -> [Ins] -> [Ins]
boundInput ncycles ips = foldl' (\ ms m -> ms <> [Map.union m (last' ms)]) [] ips'
      where ips' :: [Ins]
            ips' = take (fromIntegral ncycles) $ ips <> repeat (last' ips)

lastMaybe :: [a] -> Maybe a
lastMaybe = \ case
      []       -> Nothing
      [a]      -> Just a
      (_ : as) -> lastMaybe as

last' :: Monoid a => [a] -> a
last' = fromMaybe mempty . lastMaybe
