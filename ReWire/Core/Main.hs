module ReWire.Core.Main where

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
import ReWire.Core.PrimBasis
import ReWire.Core.Syntax
import ReWire.Core.FrontEnd
--import ReWire.Core.PrettyPrint
import ReWire.Core.PrettyPrintHaskell
import ReWire.Core.KindChecker
import ReWire.Core.TypeChecker
import ReWire.Core.Transformations.Interactive
import ReWire.Core.Transformations.Inline
import ReWire.PreHDL.CFG (mkDot,gather,linearize,cfgToProg)
import ReWire.PreHDL.GotoElim (gotoElim)
import ReWire.PreHDL.ElimEmpty (elimEmpty)
import ReWire.PreHDL.ToVHDL (toVHDL)
import ReWire.Core.Transformations.ToPreHDL (cfgFromRW,eu)
import Control.Monad (when,unless)

data Flag = FlagCFG String
          | FlagLCFG String
          | FlagPre String
          | FlagGPre String
          | FlagO String
          | FlagI
          | FlagD
          deriving (Eq,Show)

options :: [OptDescr Flag]
options =
 [ Option ['d'] ["debug"] (NoArg FlagD)
                          "dump miscellaneous debugging information"
 , Option ['i'] []        (NoArg FlagI)
                          "run in interactive mode (overrides all other options)"
 , Option ['o'] []        (ReqArg FlagO "filename.vhd")
                          "generate VHDL"
 , Option []    ["cfg"]   (ReqArg FlagCFG "filename.dot")
                          "generate control flow graph before linearization"
 , Option []    ["lcfg"]  (ReqArg FlagLCFG "filename.dot")
                          "generate control flow graph after linearization"
 , Option []    ["pre"]   (ReqArg FlagPre "filename.phdl")
                          "generate PreHDL before goto elimination"
 , Option []    ["gpre"]  (ReqArg FlagGPre "filename.phdl")
                          "generate PreHDL after goto elimination"
 ]

exitUsage :: IO ()
exitUsage = hPutStr stderr (usageInfo "Usage: rwc [OPTION...] <filename.rw>" options) >> exitFailure

runFE :: Bool -> FilePath -> IO RWCModule
runFE fDebug filename = do
  res_m <- parseFile filename

  case res_m of
    ParseFailed loc m ->
       hPutStrLn stderr (prettyPrint loc ++ ":\n\t" ++ m) >> exitFailure
    ParseOk m         -> do

      when fDebug $ do
        putStrLn "parse finished"
        writeFile "show.out" (show m)
        putStrLn "show out finished"
        writeFile "Debug.hs" (show $ ppHaskell m)
        putStrLn "debug out finished"

      case kindcheck m of
        Left e  ->
          hPutStrLn stderr e >> exitFailure
        Right m' -> do
          when fDebug (putStrLn "kc finished")
          case typecheck m' of
            Left e   -> hPutStrLn stderr e >> exitFailure
            Right m'' -> do

              when fDebug $ do
                putStrLn "tc finished"
                writeFile "tc.out" (show m'')
                putStrLn "tc debug print finished"

              return m''

main :: IO ()
main = do args                       <- getArgs

          let (flags,filenames,errs) =  getOpt Permute options args

          unless (null errs) (mapM_ (hPutStrLn stderr) errs >> exitUsage)

          when (length filenames /= 1) (hPutStrLn stderr "exactly one source file must be specified" >> exitUsage)

          let isActFlag (FlagCFG _)  = True
              isActFlag (FlagLCFG _) = True
              isActFlag (FlagPre _)  = True
              isActFlag (FlagGPre _) = True
              isActFlag (FlagO _)    = True
              isActFlag FlagI        = True
              isActFlag _            = False

          unless (any isActFlag flags) (hPutStrLn stderr "must specify at least one of -i, -o, --cfg, --lcfg, --pre, or --gpre" >> exitUsage)

          let filename               =  head filenames

          m_ <- runFE (FlagD `elem` flags) filename

          if FlagI `elem` flags then trans m_
          else
            case inline m_ of
              Nothing -> hPutStrLn stderr "Inlining failed" >> exitFailure
              Just m  -> do
                let mergeModule m1 m2 = RWCModule (dataDecls m1 ++ dataDecls m2) (defns m1 ++ defns m2)
                    cfg     = cfgFromRW (m `mergeModule` primBasis)
                    cfgDot  = mkDot (gather (eu cfg))
                    lcfgDot = mkDot (gather (linearize cfg))
                    pre     = cfgToProg cfg
                    gpre    = gotoElim pre
                    vhdl    = toVHDL (elimEmpty gpre)
                    doDump (FlagCFG f)  = writeFile f $ mkDot $ gather $ eu cfg
                    doDump (FlagLCFG f) = writeFile f $ mkDot $ gather $ linearize cfg
                    doDump (FlagPre f)  = writeFile f $ show pre
                    doDump (FlagGPre f) = writeFile f $ show gpre
                    doDump (FlagO f)    = writeFile f vhdl
                    doDump _            = return ()
                mapM_ doDump flags
