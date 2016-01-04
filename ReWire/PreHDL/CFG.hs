{-# OPTIONS -fwarn-incomplete-patterns #-}

module ReWire.PreHDL.CFG where

import ReWire.PreHDL.Syntax
import Data.Graph.Inductive
{-
import Data.GraphViz
import Data.GraphViz.Attributes
import qualified Data.GraphViz.Attributes.Complete as Attr
import Data.Text.Lazy (unpack,pack)
-}
import Data.Maybe (fromJust,catMaybes)

data Branch = Conditional BoolExp
            | Tick
            deriving (Eq,Show)

data CFG = CFG { cfgHeader :: Header, cfgGraph :: Gr Cmd Branch }

mkDot :: CFG -> String
mkDot cfg = "digraph {\n"
     ++ concat sNodes
     ++ concat sEdges
     ++ "}\n"
  where gr       = cfgGraph cfg

        sNodes   = map mkNode (labNodes gr)
        mkNode (n,l) = show n ++ " [label=\""
                    ++ dottifyCmds l
                    ++ "\",shape=box,fontname=Courier];\n"

        sEdges         = map mkEdge (labEdges gr)
        mkEdge (s,d,l) = show s ++ " -> " ++ show d ++ rest ++ ";\n"
            where rest = case l of
                           Tick                         -> "[style=dashed]"
                           Conditional (BoolConst True) -> ""
                           Conditional b                -> "[label=" ++ show (show b) ++ "]"

        dottifyCmds :: Cmd -> String
        dottifyCmds c = concatMap ((++"\\l") . escapeIt) (lines (show c))
           where escapeIt :: String -> String -- FIXME: terrible kludge
                 escapeIt s = init (tail (show s))

{-
mkDot :: CFG -> String
mkDot = unpack . printDotGraph . graphToDot params . cfgGraph
  where params = nonClusteredParams
                    { fmtNode = \ (n,l)    ->
                       [Attr.Label $ Attr.StrLabel (pack $ dottifyCmds l),
                         Attr.Shape Attr.BoxShape,
                         Attr.FontName (pack "Courier")],
                      fmtEdge = \ (_,_,lb) ->
                       case lb of
                         Tick                         -> [style dashed]
                         Conditional (BoolConst True) -> []
                         Conditional b                -> [toLabel (show b)]
                    }

        dottifyCmds :: Cmd -> String
        dottifyCmds c = concatMap (++"\\l") (lines (show c))
-}

elimUnreachable :: Node -> Gr Cmd Branch -> Gr Cmd Branch
elimUnreachable root gr = delNodes unreachable gr
  where reachable   = dfs [root] gr
        unreachable = filter (not . (`elem` reachable)) (nodes gr)

--
-- Linearize a cyclic signaling CFG.
--
-- assumed invariant: state enders have outdegree of 1 (should be the case
-- due to how signal is compiled)
--
linearize :: CFG -> CFG
linearize cfg = CFG { cfgHeader = header', cfgGraph = gr''''' }
  where gr = cfgGraph cfg
        [nEnter,nExit] = newNodes 2 gr

        isTick (_,Tick) = True
        isTick _        = False

        isTickPre n = any isTick (lsuc gr n)
        isTickPost n = any isTick (lpre gr n)

        stateStarters = 0:filter isTickPost (nodes gr)
        stateEnders   = filter isTickPre (nodes gr)

        edgeIsTick (_,_,Tick) = True
        edgeIsTick _         = False

        addEnder n g = insEdge (n,nNext,Conditional (BoolConst True)) $
                        insEdge (nNext,nExit,Conditional (BoolConst True)) $
                         insNode (nNext,NextState ns) g
          where [nNext] = newNodes 1 g
                ns = case lsuc gr n of
                       [(nn,Tick)] -> nn
                       zz          -> error $ "linearize: unexpected successors for state ender " ++ show zz

        gr'     = efilter (not . edgeIsTick) gr
        gr''    = insNodes [(nEnter,Rem "ENTER"),(nExit,Rem "EXIT")] gr'
        gr'''   = foldr (\ n -> insEdge (nEnter,n,Conditional (InState n))) gr'' stateStarters
        gr''''  = foldr addEnder gr''' stateEnders
        gr''''' = elimUnreachable nEnter gr''''

        header' = (cfgHeader cfg) { stateNames = map (("STATE"++) . show) stateStarters,
                                    startState = "STATE0" }

--
-- Gather basic blocks in a control flow graph.
--
-- Basic algorithm:
--    while (g has any subgraphs of the form "n1 -always-> n2" where the
--           indegree of n2 is 1) {
--      g.mergeNodes(n1,n2);
--    }
--
--    mergeNodes appends n2's command to n1's, causes n1 to subsume all
--    outbound edges of n2, and deletes n2 from the graph.
--
gather :: CFG -> CFG
gather cfg = cfg { cfgGraph = gath (cfgGraph cfg) }
  where gath gr | null seqs = gr
                | otherwise = gath $ mergeSeq (head seqs) gr
          where mergeSeq (n,n') gr = delNode n' $
                                      insEdges (map (\ (n'',lab) -> (n'',n,lab)) (lpre gr n)) $
                                       insEdges (map (\ (n'',lab) -> (n,n'',lab)) (lsuc gr n')) $
                                        insNode (n, fromJust (lab gr n) `mkSeq` fromJust (lab gr n')) $
                                         delNode n $
                                          gr
                allNodes           = nodes gr
                seqs               = catMaybes (map isSeq allNodes)
                isSeq n            = case lsuc gr n of
                                       [(n',Conditional (BoolConst True))] -> case lpre gr n' of
                                                                                [(n'',Conditional (BoolConst True))] | n == n'' -> Just (n,n')
                                                                                _                         -> Nothing
                                       _                                   -> Nothing

cfgToProg :: CFG -> Prog
cfgToProg cfg = Prog { progHeader = cfgHeader cfg',
                       progBody   = cmd }
  where cmd  = foldr1 Seq (concatMap renderOne bbs)
        cfg' = gather (linearize cfg)
        gr   = cfgGraph cfg'
        bbs  = topsort gr

        renderOne n = Lbl ("L" ++ show n) : cn : map renderSuc (lsuc gr n)
          where cn = fromJust (lab gr n)

        renderSuc (n,b) = Goto (branch2bool b) ("L" ++ show n)

        branch2bool (Conditional b) = b
        branch2bool Tick            = error "branch2bool: Tick"

