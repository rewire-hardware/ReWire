{-# OPTIONS -fwarn-incomplete-patterns #-}

module ReWire.ActionGraph where

import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes
import qualified Data.GraphViz.Attributes.Complete as Attr
import Data.GraphViz.Commands.IO
import Data.Text.Lazy (unpack,pack)
import Data.List (intercalate,find,nub)
import Data.Maybe (fromJust,catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Control.Monad.State
import Control.Monad.Identity

type Label = String
type Loc  = String
type Val  = Int
type Sto  = Loc -> Val
type Oper = Val -> Val -> Val

indent :: String -> String
indent s = "  " ++ idt s
  where idt "\n"     = "\n"
        idt ('\n':s) = "\n  " ++ idt s
        idt (c:s)    = c:idt s
        idt ""       = ""

data BoolExp = And BoolExp BoolExp | Or BoolExp BoolExp | Not BoolExp | BoolVar Loc | BoolConst Bool | InState Int deriving Eq

type Block = [Cmd]

data Cmd = Rem String
         | FunCall Loc String [Loc]
         | Assign Loc Loc
         | NextState Int
         | If BoolExp Block
         | Seq Block
         | Skip
         | Goto BoolExp Label
         | Lbl Label
         deriving Eq

mkSeq :: Cmd -> Cmd -> Cmd
mkSeq (Seq cs1) (Seq cs2) = Seq (cs1++cs2)
mkSeq (Seq cs) c          = Seq (cs++[c])
mkSeq c (Seq cs)          = Seq (c:cs)
mkSeq c1 c2               = Seq [c1,c2]

instance Show BoolExp where
  show (And e1 e2)       = "(" ++ show e1 ++ " AND " ++ show e2 ++ ")"
  show (Or e1 e2)        = "(" ++ show e1 ++ " OR " ++ show e2 ++ ")"
  show (Not e)           = "(NOT " ++ show e ++ ")"
  show (BoolVar l)       = l
  show (BoolConst True)  = "true"
  show (BoolConst False) = "false"
  show (InState n)       = "(state = STATE" ++ show n ++ ")"

instance Show Cmd where
  show (Rem s)          = "-- " ++ s
  show (FunCall l f ls) = "" ++ l ++ " <= "
                             ++ f ++ "(" ++ intercalate "," ls ++ ");"
  show (Lbl l)          = l ++ ":"
  show (Goto b l)       = "if (" ++ show b ++ ") goto " ++ l ++ ";"
  show (If b [])        =    "if " ++ show b ++ " then\n"
                          ++ "  null;\n"
                          ++ "end if;"
  show (If b cs)        =    "if " ++ show b ++ " then\n"
                          ++ indent (concatMap ((++"\n") . show) cs)
                          ++ "end if;"
  show (Seq cs)         = intercalate "\n" (map show cs)
  show Skip             = "null;"
  show (Assign "EMPTY" _) = "null;"
  show (Assign l1 l2)   = l1 ++ " <= " ++ l2 ++ ";"
  show (NextState n)    = "next state is " ++ show n ++ ";"
  
data Branch = Conditional BoolExp
            | SIG
            deriving (Eq,Show)

type ActionGraph = Gr Cmd Branch

dottifyCmds :: Cmd -> String
dottifyCmds c = concatMap (++"\\l") (lines (show c))

mkDot :: ActionGraph -> String
mkDot = unpack . printDotGraph . graphToDot params
  where params = nonClusteredParams
                    { fmtNode = \ (n,l)    ->
                       [Attr.Label $ Attr.StrLabel (pack $ dottifyCmds l),
                         Attr.Shape Attr.BoxShape,
                         Attr.FontName (pack "Courier")],
                      fmtEdge = \ (_,_,lb) ->
                       case lb of
                         SIG                          -> [style dashed]
                         Conditional (BoolConst True) -> []
                         Conditional b                -> [toLabel (show b)]
                    }

--
-- Produce the linear CFG from the cyclic signaling CFG.
--
-- assumed invariant: state enders have outdegree of 1 (should be the case
-- due to how signal is compiled)
--
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
        
        addEnder n g = insEdge (n,nNext,Conditional (BoolConst True)) $
                        insEdge (nNext,nExit,Conditional (BoolConst True)) $
                         insNode (nNext,NextState ns) g
          where [nNext] = newNodes 1 g
                ns = case lsuc ag n of
                       [(nn,SIG)] -> nn
                       zz         -> error $ "linearize: unexpected successors for state ender " ++ show zz
        
        ag'    = efilter (not . edgeIsSig) ag
        ag''   = insNodes [(nEnter,Rem "ENTER"),(nExit,Rem "EXIT")] ag'
        ag'''  = foldr (\ n -> insEdge (nEnter,n,Conditional (InState n))) ag'' stateStarters
        ag'''' = foldr addEnder ag''' stateEnders

--
-- Given a single-command CFG, turn it into a basic block-structured CFG.
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
gather :: ActionGraph -> ActionGraph
gather ag | null seqs = ag
          | otherwise = gather $ mergeSeq (head seqs) ag
  where mergeSeq (n,n') ag = delNode n' $
                              insEdges (map (\ (n'',lab) -> (n'',n,lab)) (lpre ag n)) $
                               insEdges (map (\ (n'',lab) -> (n,n'',lab)) (lsuc ag n')) $
                                insNode (n, fromJust (lab ag n) `mkSeq` fromJust (lab ag n')) $
                                 delNode n $
                                  ag
        allNodes           = nodes ag
        seqs               = catMaybes (map isSeq allNodes)
        isSeq n            = case lsuc ag n of
                               [(n',Conditional (BoolConst True))] -> case lpre ag n' of
                                                                        [(n'',Conditional (BoolConst True))] | n == n'' -> Just (n,n')
                                                                        _                         -> Nothing
                               _                                   -> Nothing


branch2bool :: Branch -> BoolExp
branch2bool (Conditional b) = b
branch2bool SIG             = error "branch2bool: SIG"

toPseudo :: ActionGraph -> Cmd
toPseudo ag = gotoElim $ Seq $ concatMap renderOne bbs
  where bbs = topsort ag
        renderOne n = Lbl ("L" ++ show n) : cn : map renderSuc (lsuc ag n)
           where cn    = fromJust (lab ag n)
        renderSuc (n,b) = Goto (branch2bool b) ("L" ++ show n)

--
-- Zipper for Cmd.
--
data CmdNodeTag = TagSeq | TagIf BoolExp deriving (Eq,Show)
data CmdPath    = CmdTop | CmdNode [Cmd] CmdNodeTag CmdPath [Cmd] deriving (Eq,Show)
data CmdLoc     = CmdLoc Cmd CmdPath deriving (Eq,Show)

zipRoot :: Cmd -> CmdLoc
zipRoot c = CmdLoc c CmdTop

zipLeft :: CmdLoc -> CmdLoc
zipLeft (CmdLoc _ CmdTop)                          = error "zipLeft of top"
zipLeft (CmdLoc c (CmdNode (l:left) tag up right)) = CmdLoc l (CmdNode left tag up (c:right))
zipLeft (CmdLoc _ (CmdNode [] _ _ _))              = error "zipLeft of first"

zipRight :: CmdLoc -> CmdLoc
zipRight (CmdLoc _ CmdTop)                          = error "zipRight of top"
zipRight (CmdLoc c (CmdNode left tag up (r:right))) = CmdLoc r (CmdNode (c:left) tag up right)
zipRight (CmdLoc _ (CmdNode _ _ _ []))              = error "zipRight of last"

zipUp :: CmdLoc -> CmdLoc
zipUp (CmdLoc _ CmdTop)                      = error "zipUp of top"
zipUp (CmdLoc c (CmdNode left tag up right)) = CmdLoc (deTag tag ((reverse left) ++ (c:right))) up
  where deTag TagSeq    = Seq
        deTag (TagIf b) = If b

zipDown :: CmdLoc -> CmdLoc
zipDown (CmdLoc (Seq (c:cs)) p)  = CmdLoc c (CmdNode [] TagSeq p cs)
zipDown (CmdLoc (If b (c:cs)) p) = CmdLoc c (CmdNode [] (TagIf b) p cs)
zipDown _                        = error "zipDown of bottom"

-- goto elimination monad
type GEM = StateT CmdLoc Identity

here :: GEM Cmd
here = do (CmdLoc c _) <- get
          return c

path :: GEM CmdPath
path = do (CmdLoc _ p) <- get
          return p

putHere :: Cmd -> GEM ()
putHere c = do (CmdLoc _ p) <- get
               put (CmdLoc c p)

putPath :: CmdPath -> GEM ()
putPath p = do (CmdLoc c _) <- get
               put (CmdLoc c p)
               
atTop :: CmdPath -> Bool
atTop CmdTop = True
atTop _      = False

atRight :: CmdPath -> Bool
atRight (CmdNode _ _ _ []) = True
atRight _                  = False

goDown :: GEM ()
goDown = do l <- get
            put (zipDown l)

goRight :: GEM ()
goRight = do l <- get
             put (zipRight l)

goUp :: GEM ()
goUp = do l <- get
          put (zipUp l)

insertOnRight :: [Cmd] -> GEM ()
insertOnRight cs = do p <- path
                      case p of
                        CmdNode left tag up right -> putPath (CmdNode left tag up (cs++right))
                        CmdTop                    -> putPath (CmdNode [] TagSeq CmdTop cs)

advance :: GEM Bool
advance = do c <- here
             case c of
               If _ (_:_) -> goDown >> return True
               Seq (_:_)  -> goDown >> return True
               _          -> do p <- path
                                if       atTop p   then return False
                                 else if atRight p then upAndOver
                                  else                  goRight >> return True

upAndOver :: GEM Bool
upAndOver = do goUp
               p <- path
               if       atTop p   then return False
                else if atRight p then upAndOver
                 else                  goRight >> return True

addGotoResetStmts :: GEM ()
addGotoResetStmts = do c <- here
                       case c of
                         Lbl l -> do insertOnRight [Assign ("goto_" ++ l) "FIXMEFALSE"]
                                     advance
                                     addGotoResetStmts
                         _     -> do k <- advance
                                     when k addGotoResetStmts

deleteHere :: GEM ()
deleteHere = putHere Skip

elimGoto :: GEM Bool
elimGoto = do c <- here
              case c of
                Goto b l -> do (cs,mtarg) <- jumpOver l
                               case mtarg of
                                 Just (Lbl _)     -> do insertOnRight ([Assign ("goto_" ++ l) "FIXMECHECKL"]
                                                                    ++ if null cs then [] else [If (Not (BoolVar ("goto_" ++ l))) cs]
                                                                    ++ [Lbl l])
                                                        deleteHere
                                                        return True
                                 Just (If b' cs') -> do insertOnRight ([Assign ("goto_" ++ l) "FIXMECHECKI"]
                                                                    ++ if null cs then [] else [If (Not (BoolVar ("goto_" ++ l))) cs]
                                                                    ++ [If (Or b' (BoolVar ("goto_" ++ l)))
                                                                           (Goto (BoolVar ("goto_" ++ l)) l : cs')])
                                                        deleteHere
                                                        return True
                                 Just (Seq cs')   -> do insertOnRight ([Assign ("goto_" ++ l) "FIXMECHECKS"]
                                                                    ++ if null cs then [] else [If (Not (BoolVar ("goto_" ++ l))) cs]
                                                                    ++ [Seq (Goto (BoolVar ("goto_" ++ l)) l : cs')])
                                                        deleteHere
                                                        return True
                                 Just _           -> fail "can't happen: elimGoto: target is not label, if, or seq"
--                                 Nothing -> do { k <- advance ; if k then elimGoto else return False }
                                 Nothing          -> do insertOnRight ([Assign ("goto_" ++ l) "FIXMECHECKO"]
                                                                    ++ if null cs then [] else [If (Not (BoolVar ("goto_" ++ l))) cs])
                                                        deleteHere
                                                        goUp
                                                        insertOnRight [Goto (BoolVar ("goto_" ++ l)) l]
                                                        return True
                _        -> do k <- advance
                               if k then elimGoto else return False

countGotos :: Cmd -> Int
countGotos (Goto _ _) = 1
countGotos (If _ cs)  = sum (map countGotos cs)
countGotos (Seq cs)   = sum (map countGotos cs)
countGotos _          = 0

optimize (If b cs) = If b (squishGotos (map optimize cs))
optimize (Seq cs)  = Seq (squishGotos (map optimize cs))
optimize c         = c

squishGotos [] = []
squishGotos cs | null gs   = head cs : squishGotos (tail cs)
               | otherwise = nub gs ++ squishGotos cs'
  where notGoto (Goto _ _) = False
        notGoto _          = True
        (gs,cs') = break notGoto cs

loop :: GEM ()
loop = do rewind
          c <- here
          putHere (optimize c)
          c <- here
          trace (show $ countGotos c) $ do
             b <- elimGoto
             if b then loop else return ()

takeRight :: GEM Cmd
takeRight = do p <- path
               case p of
                 CmdTop                        -> fail "takeRight at top"
                 CmdNode left tag up (r:right) -> putPath (CmdNode left tag up right) >> return r
                 CmdNode _ _ _ []              -> fail "takeRight at last"

containsLabel :: Cmd -> Label -> Bool
containsLabel (Lbl l) l'  = l == l'
containsLabel (If _ cs) l = any (`containsLabel` l) cs
containsLabel (Seq cs) l  = any (`containsLabel` l) cs
containsLabel _ _         = False

jumpOver :: Label -> GEM ([Cmd],Maybe Cmd)
jumpOver l = do p <- path
                if atRight p
                   then return ([],Nothing)
                   else do c <- takeRight
                           if c `containsLabel` l
                              then return ([],Just c)
                              else do (cs,mc) <- jumpOver l
                                      return (c:cs,mc)

rewind :: GEM ()
rewind = do p <- path
            case p of
              CmdTop -> return ()
              _      -> goUp >> rewind
            
gotoElim :: Cmd -> Cmd
gotoElim c = runGEM (addGotoResetStmts >> loop >> rewind >> here) (zipRoot c)

runGEM :: GEM a -> CmdLoc -> a
runGEM m cl = fst $ runIdentity (runStateT m cl)