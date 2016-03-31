module ReWire.FrontEnd.Reduce (redmod) where

import ReWire.FrontEnd.Monad
import ReWire.Scoping

import Prelude hiding (sequence,mapM)
import ReWire.FrontEnd.Syntax
import Control.Monad hiding (sequence,mapM)
import Data.Traversable (mapM)

reduce :: RWMExp -> RW RWMExp
reduce (RWMApp an e1 e2)     = do e1' <- reduce e1
                                  e2' <- reduce e2
                                  case e1' of
                                    RWMLam _ n _ b -> do b' <- fsubstE n e2' b
                                                         reduce b'
                                    _              -> return (RWMApp an e1' e2')
reduce (RWMLam an n t e)      = do e' <- reduce e
                                   return (RWMLam an n t e')
reduce e@RWMVar {}            = return e
reduce e@RWMCon {}            = return e
reduce e@RWMLiteral {}        = return e
reduce (RWMCase an e p e1 e2) = do e' <- reduce e
                                   mr <- matchpat e' p
                                   case mr of
                                     MatchYes sub -> do
                                       e1' <- fsubstsE sub e1
                                       reduce e1'
                                     MatchMaybe ->
                                       liftM2 (RWMCase an e' p) (reduce e1) (reduce e2)
                                     MatchNo -> reduce e2
reduce e@RWMNativeVHDL {}     = return e
reduce e@RWMError {}          = return e


data MatchResult = MatchYes [(Id RWMExp,RWMExp)]
                 | MatchMaybe
                 | MatchNo
                 deriving Show

mergematches :: [MatchResult] -> MatchResult
mergematches []     = MatchYes []
mergematches (m:ms) = case mergematches ms of
                        MatchYes bs -> case m of
                                         MatchYes bs' -> MatchYes (bs'++bs)
                                         MatchNo      -> MatchNo
                                         MatchMaybe   -> MatchMaybe
                        MatchNo     -> MatchNo
                        MatchMaybe  -> case m of
                                         MatchYes _ -> MatchMaybe
                                         MatchNo    -> MatchNo
                                         MatchMaybe -> MatchMaybe

matchpat :: RWMExp -> RWMPat -> RW MatchResult
matchpat e (RWMPatCon _ i pats) = case flattenApp e of
                                    (RWMCon _ c _:es) | c == i && length es == length pats -> do ms <- zipWithM matchpat es pats
                                                                                                 return (mergematches ms)
                                                      | otherwise                          -> return MatchNo
                                    _                                                      -> return MatchMaybe
matchpat e (RWMPatVar _ n _)    = return (MatchYes [(n,e)])
matchpat e (RWMPatLiteral _ l)  = case e of
                                    RWMLiteral _ l' | l == l'   -> return (MatchYes [])
                                                    | otherwise -> return MatchNo
                                    _                           -> return MatchMaybe

reddefn :: RWMDefn -> RW RWMDefn
reddefn (RWMDefn an n pt b e) = do e' <- reduce e
                                   return $ RWMDefn an n pt b e'

redmod :: RWMProgram -> RW RWMProgram
redmod m = do ds' <- mapM reddefn $ defns m
              return m { defns = ds' }
