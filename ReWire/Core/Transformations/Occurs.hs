module ReWire.Core.Transformations.Occurs (cmdOccurs) where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Types
import Unbound.LocallyNameless
import Unbound.LocallyNameless.Name
import Data.List (nub)
import Control.Monad (filterM)



--type TransCommand = String -> RWCProg -> (Maybe RWCProg,Maybe String)

cmdOccurs :: TransCommand
cmdOccurs str prog = case split str ' ' of
                            [res] -> (Nothing, Just $ str ++ " occurs in : " ++ (showSList $ get_occurrences str prog))
                            _     -> (Nothing, Just "Usage: occurs <defn>")
  where
    split list del = split' list del [] []
    split' [] _ acc res = case acc of
                               [] -> res
                               x  -> (res ++ [x])
    split' (s:str) del acc res = if s == del
                                  then split' str del [] (res ++ [acc])
                                  else split' str del (acc ++ [s]) res


get_occurrences :: String -> RWCProg -> [String]
get_occurrences nme (RWCProg decls tdefns) = runLFreshM $ do
                                                            defns <- luntrec tdefns
                                                            let nme' = s2n nme
                                                            defns' <- filterM (occurs nme') defns 
                                                            return $ map (name2String . def_name) defns'

                                                      

                            


occurs :: Name RWCExp -> RWCDefn -> LFreshM Bool
occurs nme (RWCDefn dnme ebnd) = do
                                  let bnd = unembed ebnd
                                  lunbind bnd (\(_,(_,exp)) -> do
                                                                  let frees = nub (fv exp)
                                                                  return $ elem nme frees

                                              )

def_name :: RWCDefn -> Name RWCExp
def_name (RWCDefn name _) = name


showSList :: [String] -> String
showSList []   = "<None>"
showSList [s]  = s
showSList strs = "[" ++ (showSList' strs) ++ "]"

showSList' [] = ""
showSList' (s:[]) = s
showSList' (s:ss) = s ++ ", " ++ showSList' ss
