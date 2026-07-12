{-# LANGUAGE Safe #-}
module RWC (main) where

import Driver (driverMain)
import ReWire.Flags (Flag (..))
import ReWire.FrontEnd (compileFile)

import System.Console.GetOpt (OptDescr (..), ArgDescr (..))

options :: [OptDescr Flag]
options =
       [ Option ['h'] ["help"]            (NoArg  FlagHelp)                          "This help message."
       , Option ['v'] ["verbose"]         (NoArg  FlagVerbose)                       "More verbose output."
       , Option ['w'] ["no-warn"]         (NoArg  FlagNoWarn)                        "Suppress warnings."
       , Option ['W'] []                  (ReqArg FlagW           "error")           "-Werror: treat warnings as errors."
       , Option []    ["verilog"]         (NoArg  FlagVerilog)                       "Produce Verilog output (default)."
       , Option []    ["vhdl"]            (NoArg  FlagVhdl)                          "Produce VHDL output."
       , Option []    ["cryptol"]         (NoArg  FlagCryptol)                       "Produce Cryptol output (a pure model of the device for verification or simulation)."
       , Option []    ["from-core"]       (NoArg  FlagFromCore)                      "Ingest ReWire core language files instead of Haskell."
       , Option []    ["core"]            (NoArg  FlagCore)                          "Produce ReWire core language (Hyle) output; see doc/hyle.md."
       , Option []    ["invert-reset"]    (NoArg  FlagInvertReset)                   "Invert the implicitly generated reset signal."
       , Option []    ["no-reset"]        (NoArg  FlagNoReset)                       "No implicitly generated reset signal."
       , Option []    ["no-clock"]        (NoArg  FlagNoClock)                       "No implicitly generated clock signal (implies no-reset: generate a purely combinatorial circuit)."
       , Option []    ["sync-reset"]      (NoArg  FlagSyncReset)                     "Only reset on positive clock edge."
       , Option ['d'] ["dump"]            (ReqArg FlagDump        "1,2,...")         "Dump the IR after the given passes (see -v for pass numbers) to files beside the output (e.g., foo.6.eir); -v adds the show output in a comment."
       , Option []    ["dump-all"]        (NoArg  FlagDumpAll)                       "Dump the IR after every pass (see -d)."
       , Option []    ["debug-lint"]      (NoArg  FlagDebugLint)                     "Re-lint the Eidos IR after every front-half pass to possibly catch compiler bugs."
       , Option []    ["eidos"]           (NoArg  FlagEidos)                         "Dump the Eidos IR (.eir) beside the output; see doc/eidos.md."
       , Option []    ["no-halt"]         (NoArg  FlagNoHalt)                        "Reject a device that can halt (post-halt outputs are unspecified; a rejected device must pause forever instead)."
       , Option []    ["flatten"]         (NoArg  FlagFlatten)                       "Flatten RTL output into a single module (currently slow, memory-intensive)."
       , Option ['o'] []                  (ReqArg FlagO           "filename")        "Name for output file."
       , Option ['p'] ["vhdl-packages"]   (ReqArg FlagVhdlPkgs    "pkg1,pkg2,...")   "Packages to use for external VHDL components (e.g., ieee.std_logic_1164.all)."
       , Option []    ["reset"]           (ReqArg FlagResetName   "name")            "Name to use for reset signal in generated RTL."
       , Option []    ["clock"]           (ReqArg FlagClockName   "name")            "Name to use for clock signal in generated RTL."
       , Option []    ["inputs"]          (ReqArg FlagInputNames  "name1,name2,...") "Names to use for input signals in generated RTL."
       , Option []    ["outputs"]         (ReqArg FlagOutputNames "name1,name2,...") "Names to use for output signals in generated RTL."
       , Option []    ["states"]          (ReqArg FlagStateNames  "name1,name2,...") "Names to use for internal state signals."
       , Option []    ["start"]           (ReqArg FlagStart       "name")            "Symbol to use for the definition of the top-level module (default: Main.start)."
       , Option []    ["top"]             (ReqArg FlagTop         "name")            "Name to use for the top-level module in generated RTL (default: top_level)."
       , Option []    ["loadpath"]        (ReqArg FlagLoadPath    "dir1,dir2,...")   "Additional directories for loadpath."
       , Option []    ["interpret"]       (OptArg FlagInterpret   "inputs.yaml")     "Interpret instead of compile, using inputs from the optional argument file (default: inputs.yaml)."
       , Option []    ["testbench"]       (OptArg FlagTestbench   "inputs.yaml")     "Also generate a testbench (foo_tb.sv/foo_tb.vhdl) driving inputs from the optional argument file (default: inputs.yaml) for --cycles cycles."
       , Option []    ["cycles"]          (ReqArg FlagCycles      "ncycles")         "Number of cycles to interpret or simulate (default: the larger of 10 and the number of inputs in the inputs file)."
       , Option []    ["depth"]           (ReqArg FlagEvalDepth   "depth")           "Partial evaluation depth. Higher values can cause non-termination. (default: 8)."
       , Option []    ["rtl-opt"]         (ReqArg FlagRtlOpt      "level")           "RTL optimization level. Disable with 0. (default: 8)."
       , Option []    ["pretty"]          (NoArg  FlagPretty)                        "Attempt to output prettier RTL at the expense of performance."
       , Option []    ["stable-names"]    (NoArg  FlagStableNames)                   "Suffix machine-block names with their unique keys (always collision-free, machine-greppable) instead of collision-only readable names."
       , Option []    ["locators"]        (NoArg  FlagLocators)                      "Emit source-locator comments (--@ file:line:col-line:col) in ReWire core (.rwc) output; --no-locators takes precedence."
       , Option []    ["no-locators"]     (NoArg  FlagNoLocators)                    "Suppress source-locator and provenance comments in generated output."
       ]

main :: IO ()
main = driverMain "rwc" options compileFile
