module ReWire.Core.Transformations.Uses (cmdUses) where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Types
import Unbound.LocallyNameless
import Unbound.LocallyNameless.Name
import Data.List (nub,find)
import Control.Monad (filterM)

cmdUses :: TransCommand
cmdUses str prog = case split str ' ' of
                             [res] -> (Nothing, case get_uses str prog of
                                                      Nothing   -> Just ("Invalid top-level defn: " ++ res)
                                                      Just strs -> Just $ showSList strs)
                             _     -> (Nothing, Just "Usage: uses <defn>")
  where
    split list del = split' list del [] []
    split' [] _ acc res = case acc of
                               [] -> res
                               x  -> (res ++ [x])
    split' (s:str) del acc res = if s == del
                                  then split' str del [] (res ++ [acc])
                                  else split' str del (acc ++ [s]) res



get_uses :: String -> RWCProg -> Maybe [String]
get_uses nme (RWCProg decls tdefns) = runLFreshM $ do
                                                      defns <- luntrec tdefns
                                                      let nme' = s2n nme
                                                      let defn = find (isDefn nme') defns
                                                      case defn of
                                                          Nothing  -> return Nothing
                                                          Just (RWCDefn _ ebexpr) -> do
                                                                                        let bexpr = unembed ebexpr
                                                                                        lunbind bexpr (\(_,(_,expr)) -> do
                                                                                                                           let frees = (fv expr) :: [Name RWCExp]
                                                                                                                           return $ Just $ map name2String $ nub frees
                                                                                                      )
    where
      isDefn name (RWCDefn n _) = n == name




showSList :: [String] -> String
showSList []   = "<None>"
showSList [s]  = s
showSList strs = "[" ++ (showSList' strs) ++ "]"

showSList' [] = ""
showSList' (s:[]) = s
showSList' (s:ss) = s ++ ", " ++ showSList' ss
