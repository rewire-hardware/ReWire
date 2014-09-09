{-# OPTIONS -fwarn-incomplete-patterns #-}

module ReWire.PreHDL.GotoElim where

import ReWire.PreHDL.Syntax
import Control.Monad.State
import Control.Monad.Identity
import Debug.Trace (trace)
import Data.List (nub)

--
-- A zipper for Cmd.
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
  where deTag TagSeq []    = Skip
        deTag TagSeq cs    = foldr1 Seq cs
        deTag (TagIf b) [] = If b Skip
        deTag (TagIf b) cs = If b (foldr1 Seq cs)

zipDown :: CmdLoc -> CmdLoc
zipDown (CmdLoc c_@(Seq _ _) p) = CmdLoc c (CmdNode [] TagSeq p cs)
    where (c:cs) = flattenSeq c_
zipDown (CmdLoc (If b c_) p)    = CmdLoc c (CmdNode [] (TagIf b) p cs)
    where (c:cs) = flattenSeq c_
zipDown _                        = error "zipDown of bottom"

-- goto elimination monad
type GEM = StateT (CmdLoc,[String]) Identity

getLoc :: GEM CmdLoc
getLoc = liftM fst get

putLoc :: CmdLoc -> GEM ()
putLoc l = do (_,ns) <- get
              put (l,ns)
              
getNames :: GEM [String]
getNames = liftM snd get

putNames :: [String] -> GEM ()
putNames ns = do (l,_) <- get
                 put (l,ns)

here :: GEM Cmd
here = do (CmdLoc c _) <- getLoc
          return c

path :: GEM CmdPath
path = do (CmdLoc _ p) <- getLoc
          return p

putHere :: Cmd -> GEM ()
putHere c = do (CmdLoc _ p) <- getLoc
               putLoc (CmdLoc c p)

putPath :: CmdPath -> GEM ()
putPath p = do (CmdLoc c _) <- getLoc
               putLoc (CmdLoc c p)
               
atTop :: CmdPath -> Bool
atTop CmdTop = True
atTop _      = False

atRight :: CmdPath -> Bool
atRight (CmdNode _ _ _ []) = True
atRight _                  = False

goDown :: GEM ()
goDown = do l <- getLoc
            putLoc (zipDown l)

goRight :: GEM ()
goRight = do l <- getLoc
             putLoc (zipRight l)

goUp :: GEM ()
goUp = do l <- getLoc
          putLoc (zipUp l)

insertOnRight :: [Cmd] -> GEM ()
insertOnRight cs = do p <- path
                      case p of
                        CmdNode left tag up right -> putPath (CmdNode left tag up (cs++right))
                        CmdTop                    -> putPath (CmdNode [] TagSeq CmdTop cs)

advance :: GEM Bool
advance = do c <- here
             case c of
               If _ _  -> goDown >> return True
               Seq _ _ -> goDown >> return True
               _       -> do p <- path
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
                         Lbl l -> do insertOnRight [Assign ("goto_" ++ l) (BoolRHS (BoolConst False))]
                                     ns <- getNames
                                     putNames (("goto_" ++ l) : ns)
                                     advance
                                     addGotoResetStmts
                         _     -> do k <- advance
                                     when k addGotoResetStmts

deleteHere :: GEM ()
deleteHere = do p <- path
                case p of
                  CmdTop                        -> putHere Skip
                  CmdNode [] tag up []          -> putHere Skip
                  CmdNode left tag up (r:right) -> do putHere r
                                                      putPath (CmdNode left tag up right)
                  CmdNode (l:left) tag up right -> do putHere l
                                                      putPath (CmdNode left tag up right)

elimGoto :: GEM Bool
elimGoto = do c <- here
              case c of
                Goto b l ->trace (show l) $
                            do (cs,mtarg) <- jumpOver l
                               case mtarg of
                                 Just (Lbl _)       ->trace "Lbl" $
                                                       do insertOnRight ([Assign ("goto_" ++ l) (BoolRHS b)]
                                                                      ++ (if null cs then [] else [If (Not (BoolVar ("goto_" ++ l))) (foldr1 Seq cs)])
                                                                      ++ [Lbl l])
                                                          deleteHere
                                                          return True
                                 Just (If b' c)     ->trace "If" $ 
                                                       do insertOnRight ([Assign ("goto_" ++ l) (BoolRHS b)]
                                                                      ++ (if null cs then [] else [If (Not (BoolVar ("goto_" ++ l))) (foldr1 Seq cs)])
                                                                      ++ [If (Or b' (BoolVar ("goto_" ++ l)))
                                                                             (Goto (BoolVar ("goto_" ++ l)) l `Seq` c)])
                                                          deleteHere
                                                          return True
                                 Just (Seq c1 c2)   ->trace "Seq" $
                                                       do insertOnRight ([Assign ("goto_" ++ l) (BoolRHS b)]
                                                                     ++ (if null cs then [] else [If (Not (BoolVar ("goto_" ++ l))) (foldr1 Seq cs)])
                                                                     ++ [Goto (BoolVar ("goto_" ++ l)) l `Seq` (c1 `Seq` c2)])
                                                          deleteHere
                                                          return True
                                 Just _           -> fail "can't happen: elimGoto: target is not label, if, or seq"
--                                 Nothing          -> do { k <- advance ; if k then elimGoto else return False }
                                 Nothing          ->trace "Out" $ 
                                                     do insertOnRight ([Assign ("goto_" ++ l) (BoolRHS b)]
                                                                    ++ (if null cs then [] else [If (Not (BoolVar ("goto_" ++ l))) (foldr1 Seq cs)]))
                                                        deleteHere
                                                        goUp
                                                        insertOnRight [Goto (BoolVar ("goto_" ++ l)) l]
                                                        return True
                _        -> do k <- advance
                               if k then elimGoto else return False

jumpOver :: Label -> GEM ([Cmd],Maybe Cmd)
jumpOver l = do p <- path
                if atRight p
                   then return ([],Nothing)
                   else do c <- takeRight
                           if c `containsLabel` l
                              then return ([],Just c)
                              else do (cs,mc) <- jumpOver l
                                      return (c:cs,mc)

takeRight :: GEM Cmd
takeRight = do p <- path
               case p of
                 CmdTop                        -> fail "takeRight at top"
                 CmdNode left tag up (r:right) -> putPath (CmdNode left tag up right) >> return r
                 CmdNode _ _ _ []              -> fail "takeRight at last"

containsLabel :: Cmd -> Label -> Bool
containsLabel (Lbl l) l'    = l == l'
containsLabel (If _ c) l    = c `containsLabel` l
containsLabel (Seq c1 c2) l = c1 `containsLabel` l || c2 `containsLabel` l
containsLabel _ _           = False

countGotos :: Cmd -> Int
countGotos (Goto _ _)  = 1
countGotos (If _ c)    = countGotos c
countGotos (Seq c1 c2) = countGotos c1 + countGotos c2
countGotos _           = 0

optimize (If b c)      = If b (optimize c)
optimize c@(Seq c1 c2) = squishGotos c
optimize c             = c

squishGotos c | null csOut = Skip
              | otherwise  = foldr1 Seq csOut
    where csOut = map optimize $ squish (flattenSeq c)
          squish []             = []
          squish cs | null gs   = head cs : squish (tail cs)
                    | otherwise = nub gs ++ squish cs'
           where notGoto (Goto _ _) = False
                 notGoto _          = True
                 (gs,cs')           = break notGoto cs

loop :: GEM ()
loop = do rewind
          c <- here
          putHere (optimize c)
          c <- here
          trace (show $ countGotos c) $ do
             b <- elimGoto
             if b then loop else return ()

rewind :: GEM ()
rewind = do p <- path
            case p of
              CmdTop -> return ()
              _      -> goUp >> rewind
            
gotoElimC :: Cmd -> (Cmd,[String])
gotoElimC c = let (c',(_,ns)) = runIdentity (runStateT (addGotoResetStmts >> loop >> rewind >> here) (zipRoot c,[]))
              in  (c',ns)

gotoElim :: Prog -> Prog
gotoElim p = Prog { progBody   = body',
                    progHeader = addGotoRegs (progHeader p) }
             where (body',ns)    = gotoElimC (progBody p)
                   addGotoRegs h = h { regDecls = gotoRegs ++ regDecls h }
                   gotoRegs      = map (\ x -> RegDecl x TyBoolean) ns

--runGEM :: GEM a -> CmdLoc -> a
--runGEM m cl = fst $ runIdentity (runStateT m cl)
