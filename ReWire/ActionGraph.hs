module ReWire.ActionGraph where

import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes
import qualified Data.GraphViz.Attributes.Complete as Attr
import Data.GraphViz.Commands.IO
import Data.Text.Lazy (unpack)
import Data.List (intercalate,find)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

type Loc  = String
type Val  = Int
type Sto  = Loc -> Val
type Oper = Val -> Val -> Val

data Cmd = Rem String
         | FunCall Loc String [Loc]
         | Assign Loc Loc
         deriving Eq

instance Show Cmd where
  show (Rem s)          = "/* " ++ s ++ " */"
--  show (Seq c1 c2)      = show c1 ++ "\n" ++ show c2
  show (FunCall l f ls) = l ++ " := " ++ f ++ "(" ++ intercalate "," ls ++ ")"
  show (Assign l1 l2)   = l1 ++ " := " ++ l2
  
data Branch = BZ Loc
            | BNZ Loc
            | JMP
            | SIG
            deriving (Eq,Show)

type ActionGraph = Gr Cmd Branch

mkDot :: ActionGraph -> String
mkDot = unpack . printDotGraph . graphToDot params
          where params = nonClusteredParams { fmtNode = \ (n,l)    -> case l of
                                                                       Rem s -> [toLabel s,Attr.Shape Attr.Note]
                                                                       _     -> [toLabel (show n ++ ": " ++ show l)], 
                                              fmtEdge = \ (_,_,lb) -> case lb of
                                                                       BZ l  -> [toLabel (l ++ " == 0")] 
                                                                       BNZ l -> [toLabel (l ++ " != 0")]
                                                                       JMP   -> []
                                                                       SIG   -> [style dashed]}

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
        
        areOpposites :: Branch -> Branch -> Bool
        areOpposites (BNZ l1) (BZ l2) = l1 == l2
        areOpposites (BZ l1) (BNZ l2) = l1 == l2
        areOpposites _ _              = False
        
        nearestCommonReachable :: Node -> Node -> Maybe Node
        nearestCommonReachable n1 n2 = find (`Set.member` ns2) ns1
          where ns1 = dfs [n1] ag'
                ns2 = Set.fromList (dfs [n2] ag')
                ag' = efilter edgeNotSig ag
        
        mkp stopNode n  | Just n == stopNode = ""
                        | otherwise     = show l ++ "\n" ++
                                           (case sucs of
                                            [(n1,l1),(n2,l2)] | areOpposites l1 l2 ->
                                              let ncr = nearestCommonReachable n1 n2
                                                  cn1 = mkp ncr n1
                                                  cn2 = mkp ncr n2
                                              in "if " ++ show l1 ++ " {\n"
                                                 ++ indent cn1
                                                 ++ "}\nelse {\n"
                                                 ++ indent cn2
                                                 ++ "}\n"
                                                 ++ (case ncr of
                                                       Just n' -> mkp stopNode n'
                                                       Nothing -> "")
                                            [(n',JMP)] -> mkp stopNode n'
                                            [(n',SIG)] -> "yield(" ++ show n' ++ ")\n")
          where l    = fromJust $ lab ag n
                sucs = lsuc ag n
                                              
{-          
          let l               = fromJust $ lab ag n
                     sucs            = lsuc ag n
                 in  (show n ++ ": " ++ show l ++ "\n" ++ concatMap handle sucs)-}