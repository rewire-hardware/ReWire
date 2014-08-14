module ReWire.ActionGraph where

import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Commands.IO
import Data.Text.Lazy (unpack)

type Loc  = String
type Val  = Int
type Sto  = Loc -> Val
type Oper = Val -> Val -> Val

data Cmd = Rem String
         | Seq Cmd Cmd
         | FunCall Loc String [Loc]
         | Assign Loc Loc
         | Case Loc Cmd Cmd
         | Signal Loc Loc
         deriving (Eq,Show)

type ActionGraph = Gr Cmd (Maybe Loc)

mkDot :: ActionGraph -> String
mkDot = unpack . printDotGraph . graphToDot params
          where params = nonClusteredParams { fmtNode = \ (_,l) -> [toLabel (show l)], 
                                              fmtEdge = \ (_,_,l) -> [toLabel (show l)] }