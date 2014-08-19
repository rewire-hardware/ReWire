-- FIXME: Rename this to RWCToVHDL or something  :P
module ReWire.AGToVHDL where

import ReWire.ActionGraph
import ReWire.Core.Syntax
import ReWire.Core.Transformations.ToAG
import ReWire.Core.Transformations.Types
import Data.Graph.Inductive
import Data.List (find,intercalate)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

nearestCommonReachable :: ActionGraph -> Node -> Node -> Maybe Node
nearestCommonReachable ag n1 n2 = find (`Set.member` ns2) ns1
  where ns1             = dfs [n1] ag'
        ns2             = Set.fromList (dfs [n2] ag')
        isSig (_,_,SIG) = True
        isSig _         = False
        ag'             = efilter (not . isSig) ag

areOpposites :: Branch -> Branch -> Bool
areOpposites (Is1 l1) (Is0 l2) = l1 == l2
areOpposites (Is0 l1) (Is1 l2) = l1 == l2
areOpposites _ _               = False

renderCond :: Branch -> String
renderCond (Is0 r) = r ++ " = \"0\""
renderCond (Is1 r) = r ++ " = \"1\""

renderState :: ActionGraph -> Node -> String
renderState ag n =
  "when STATE" ++ show n ++ " =>\n"
  ++ indent (renderNode ag Nothing n)

renderNode :: ActionGraph -> Maybe Node -> Node -> String
renderNode ag stop n =
  if Just n == stop
     then ""
     else show n ++ " " ++ show l ++ "\n" ++
           case sucs of
             [(n1,l1),(n2,l2)] | areOpposites l1 l2 ->
               let ncr = nearestCommonReachable ag n1 n2
                   cn1 = renderNode ag ncr n1
                   cn2 = renderNode ag ncr n2
               in "if " ++ renderCond l1 ++ " then\n"
                  ++ indent cn1
                  ++ "else\n"
                  ++ indent cn2
                  ++ "end if;\n"
                  ++ case ncr of
                       Just n' -> renderNode ag stop n'
                       Nothing -> ""
             [(n',Always)] -> renderNode ag stop n'
             [(n',SIG)]    -> "state <= STATE" ++ show n' ++ ";\n"
  where l    = fromJust $ lab ag n
        sucs = lsuc ag n
             
indent :: String -> String
indent s = "  " ++ idt s
  where idt "\n"     = "\n"
        idt ('\n':s) = "\n  " ++ idt s
        idt (c:s)    = c:idt s
        idt ""       = ""

stateNodes :: ActionGraph -> [Node]
stateNodes ag = 0:sigPosts
  where isSig (_,SIG) = True
        isSig _       = False
        
        isSigPost n = any isSig (lpre ag n)
        sigPosts = filter isSigPost (nodes ag)

renderSM :: ActionGraph -> String
renderSM ag = "process(clk)\n"
           ++ "begin\n"
           ++ indent ("if rising_edge(clk) then\n"
                   ++ indent ("case state is\n"
                           ++ indent (concat ss)
                           ++ "end case;\n")
                   ++ "end if;\n")
           ++ "end if;\n"
  where ss = map (renderState ag) ns
        ns = stateNodes ag

-- FIXME: can we be sure that the start state is first in the list?
renderStateSignal :: ActionGraph -> String
renderStateSignal ag = "type sm_state is (" ++ intercalate "," stateNames ++ ");\n"
                    ++ "signal state : sm_state := " ++ head stateNames ++ ";\n"
  where stateNames = map (("STATE"++) . show) (stateNodes ag)

renderTy :: RWCTy -> String
renderTy (RWCTyApp t1 t2)  = "L" ++ renderTy t1 ++ "R" ++ "L" ++ renderTy t2 ++ "R"
renderTy (RWCTyCon tci)    = deTyConId tci
renderTy (RWCTyVar _)      = error "renderTy: encountered variable"
--renderTy t@(RWCTyComp _ _) = error $ "renderTy: encountered computation" ++ show t
renderTy (RWCTyComp t1 t2) = "KL" ++ renderTy t1 ++ "RK" ++ "KL" ++ renderTy t2 ++ "RK"

renderRegMap :: RegMap -> String
renderRegMap rm = concatMap
                    (\ (r,n) -> "signal " ++ r ++ " : std_logic_vector(0 to " ++ show (n-1) ++ ");\n")
                    rm

renderArch :: RegMap -> ActionGraph -> String
renderArch rm ag = "architecture behavioral of rewire is\n"
                ++ indent (renderStateSignal ag)
                ++ indent (renderRegMap rm)
                ++ "begin\n"
                ++ indent (renderSM ag)
                ++ "end behavioral;\n"

rwToVHDL :: RWCProg -> String
rwToVHDL p = renderArch rm ag
  where (rm,ag) = agFromRW p

cmdToPseudo :: TransCommand
cmdToPseudo _ p = (Nothing,Just (rwToVHDL p))
