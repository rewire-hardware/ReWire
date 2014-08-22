module ReWire.ActionGraph where

import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes
import qualified Data.GraphViz.Attributes.Complete as Attr
import Data.GraphViz.Commands.IO
import Data.Text.Lazy (unpack,pack)
import Data.List (intercalate,find)
import Data.Maybe (fromJust,catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)

type Loc  = String
type Val  = Int
type Sto  = Loc -> Val
type Oper = Val -> Val -> Val

data Cmd = Rem String
         | FunCall Loc String [Loc]
         | Assign Loc Loc
         | NextState Int
         | Seq Cmd Cmd
         deriving Eq

instance Show Cmd where
  show (Rem s)          = "-- " ++ s
  show (FunCall l f ls) = l ++ " <= "
                            ++ f ++ "(" ++ intercalate "," ls ++ ");"
  show (Assign l1 l2)   = l1 ++ " <= " ++ l2 ++ ";"
  show (NextState n)    = "next state is " ++ show n ++ ";"
  show (Seq c1 c2)      = show c1 ++ "\n" ++ show c2
  
data Branch = StateIs Int
            | StateIsNot Int
            | Is0 Loc
            | Is1 Loc
            | Always
            | SIG
            deriving (Eq,Show)

type ActionGraph = Gr Cmd Branch

dottifyCmd :: Cmd -> String
dottifyCmd c = concatMap (++"\\l") (lines $ show c)

mkDot :: ActionGraph -> String
mkDot = unpack . printDotGraph . graphToDot params
  where params = nonClusteredParams
                    { fmtNode = \ (n,l)    ->
                       [Attr.Label $ Attr.StrLabel (pack $ dottifyCmd l),
                         Attr.Shape Attr.BoxShape,
                         Attr.FontName (pack "Courier")],
                      fmtEdge = \ (_,_,lb) ->
                       case lb of
                         StateIs n    -> [toLabel ("STATE = " ++ show n)]
                         StateIsNot n -> [toLabel ("STATE != " ++ show n)]
                         Is0 l        -> [toLabel (l ++ " = 0")] 
                         Is1 l        -> [toLabel (l ++ " = 1")]
                         Always       -> []
                         SIG          -> [style dashed]
                    }

-- assumed invariant: state enders have outdegree of 1 (should be the case
-- due to how signal is compiled)
linearize :: ActionGraph -> ActionGraph
linearize ag = ag''''
  where [nEnter,nExit] = newNodes 2 ag
        
        isSig (_,SIG) = True
        isSig _       = False
        
        isSigPre n = any isSig (lsuc ag n)
        isSigPost n = any isSig (lpre ag n)
        
        stateStarters = 0:filter isSigPost (nodes ag)
        stateEnders   = filter isSigPre (nodes ag)
        
        edgeIsSig (_,_,SIG) = True
        edgeIsSig _         = False
        
        addEnder n g = insEdge (n,nNext,Always) $
                        insEdge (nNext,nExit,Always) $
                         insNode (nNext,NextState ns) g
          where [nNext] = newNodes 1 g
                ns = case lsuc ag n of
                       [(nn,SIG)] -> nn
                       zz         -> error $ "linearize: unexpected successors for state ender " ++ show zz
        
        ag'    = efilter (not . edgeIsSig) ag
        ag''   = insNodes [(nEnter,Rem "ENTER"),(nExit,Rem "EXIT")] ag'
        ag'''  = foldr (\ n -> insEdge (nEnter,n,StateIs n)) ag'' stateStarters
        ag'''' = foldr addEnder ag''' stateEnders

gather :: ActionGraph -> ActionGraph
gather ag | null seqs = ag
          | otherwise = gather $ mergeSeq (head seqs) ag
  where mergeSeq (n,n') ag = delNode n' $
                              insEdges (map (\ (n'',lab) -> (n'',n,lab)) (lpre ag n)) $
                               insEdges (map (\ (n'',lab) -> (n,n'',lab)) (lsuc ag n')) $
                                insNode (n, fromJust (lab ag n) `Seq` fromJust (lab ag n')) $
                                 delNode n $
                                  ag
        allNodes           = nodes ag
        seqs               = catMaybes (map isSeq allNodes)
        isSeq n            = case lsuc ag n of
                               [(n',Always)] -> case lpre ag n' of
                                                  [(n'',Always)] | n == n'' -> Just (n,n')
                                                  _                         -> Nothing
                               _             -> Nothing

{-
notSig (_,SIG) = False
notSig _       = True

indent :: String -> String
indent s = "  " ++ idt s
  where idt "\n"     = "\n"
        idt ('\n':s) = "\n  " ++ idt s
        idt (c:s)    = c:idt s
        idt ""       = ""

edgeNotSig (_,_,SIG) = False
edgeNotSig _         = True

agToPseudo :: ActionGraph -> String
agToPseudo ag = intercalate "\n\n\n" $ map (\ n -> show n ++ ":\n" ++ mkp Nothing n) (0:sigPosts)
  where isSigPost n = any (not . notSig) (lpre ag n)
        sigPosts = filter isSigPost (nodes ag)
        
        renderCond (BZ r)  = r ++ " == \"0\""
        renderCond (BNZ r) = r ++ " == \"1\""
        
        nearestCommonReachable :: Node -> Node -> Maybe Node
        nearestCommonReachable n1 n2 = find (`Set.member` ns2) ns1
          where ns1 = dfs [n1] ag'
                ns2 = Set.fromList (dfs [n2] ag')
                ag' = efilter edgeNotSig ag

        areOpposites :: Branch -> Branch -> Bool
        areOpposites (BNZ l1) (BZ l2) = l1 == l2
        areOpposites (BZ l1) (BNZ l2) = l1 == l2
        areOpposites _ _              = False

        mkp stopNode n  | Just n == stopNode = ""
                        | otherwise     = show l ++ "\n" ++
                                           (case sucs of
                                            [(n1,l1),(n2,l2)] | areOpposites l1 l2 ->
                                              let ncr = nearestCommonReachable n1 n2
                                                  cn1 = mkp ncr n1
                                                  cn2 = mkp ncr n2
                                              in "if " ++ renderCond l1 ++ " then\n"
                                                 ++ indent cn1
                                                 ++ "else\n"
                                                 ++ indent cn2
                                                 ++ "end if;\n"
                                                 ++ (case ncr of
                                                       Just n' -> mkp stopNode n'
                                                       Nothing -> "")
                                            [(n',JMP)] -> mkp stopNode n'
                                            [(n',SIG)] -> "state <= STATE" ++ show n' ++ ";\n")
          where l    = fromJust $ lab ag n
                sucs = lsuc ag n
-}