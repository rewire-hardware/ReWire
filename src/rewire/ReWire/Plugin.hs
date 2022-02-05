module ReWire.Plugin (plugin) where

import ReWire.Crust.GHCCoreToCrust (toCrust)

import GHC.Plugins
      ( ModGuts
      , CommandLineOption
      , defaultPlugin
      , Plugin (..)
      , DynFlags
      , Bind (..)
      , CoreBind
      , CoreToDo (..)
      , CoreM
      , ppr, putMsgS, showSDoc
      , bindsOnlyPass
      , getDynFlags
      )

plugin :: Plugin
plugin = defaultPlugin
      { installCoreToDos = install
      }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = pure $ CoreDoPluginPass "ReWire" pass : todo

pass :: ModGuts -> CoreM ModGuts
pass guts = do
      dflags <- getDynFlags
      putMsgS "RWDONE"
      bindsOnlyPass (mapM $ printBind dflags) guts
      where printBind :: DynFlags -> CoreBind -> CoreM CoreBind
            printBind dflags bndr@(NonRec b _) = do
                  putMsgS $ "Non-recursive(??) binding named " ++ showSDoc dflags (ppr b)
                  pure bndr
            printBind dflags bndr@(Rec bs) = do
                  putMsgS $ "Recursive binding, names: " ++ concatMap ((++ " ") . showSDoc dflags . ppr . fst) bs
                  pure bndr
            printBind _ bndr = pure bndr
