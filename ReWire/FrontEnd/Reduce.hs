{-# LANGUAGE LambdaCase #-}
module ReWire.FrontEnd.Reduce (reduce) where

import ReWire.FrontEnd.Monad
import ReWire.Scoping
import ReWire.FrontEnd.Syntax

import Control.Monad (zipWithM)

reduceExp :: RWMExp -> RW RWMExp
reduceExp = \ case
      RWMApp an e1 e2      -> do
            e1' <- reduceExp e1
            e2' <- reduceExp e2
            case e1' of
                  RWMLam _ n _ b -> fsubstE n e2' b >>= reduceExp
                  _              -> return $ RWMApp an e1' e2'
      RWMLam an n t e      -> RWMLam an n t <$> reduceExp e
      e@RWMVar {}          -> return e
      e@RWMCon {}          -> return e
      e@RWMLiteral {}      -> return e
      RWMCase an e p e1 e2 -> do
            e' <- reduceExp e
            mr <- matchpat e' p
            case mr of
                  MatchYes sub -> fsubstsE sub e1 >>= reduceExp
                  MatchMaybe   -> RWMCase an e' p <$> reduceExp e1 <*> reduceExp e2
                  MatchNo      -> reduceExp e2
      e@RWMNativeVHDL {}   -> return e
      e@RWMError {}        -> return e

data MatchResult = MatchYes [(Id RWMExp, RWMExp)]
                 | MatchMaybe
                 | MatchNo
                 deriving Show

mergematches :: [MatchResult] -> MatchResult
mergematches []     = MatchYes []
mergematches (m:ms) = case mergematches ms of
      MatchYes bs -> case m of
            MatchYes bs' -> MatchYes $ bs' ++ bs
            MatchNo      -> MatchNo
            MatchMaybe   -> MatchMaybe
      MatchNo     -> MatchNo
      MatchMaybe  -> case m of
            MatchYes _ -> MatchMaybe
            MatchNo    -> MatchNo
            MatchMaybe -> MatchMaybe

matchpat :: RWMExp -> RWMPat -> RW MatchResult
matchpat e = \ case
      RWMPatCon _ i pats -> case flattenApp e of
            RWMCon _ c _:es
                  | c == i && length es == length pats -> do
                        ms <- zipWithM matchpat es pats
                        return $ mergematches ms
                  | otherwise                          -> return MatchNo
            _                                          -> return MatchMaybe
      RWMPatVar _ n _    -> return $ MatchYes [(n, e)]
      RWMPatLiteral _ l  -> return $ case e of
            RWMLiteral _ l'
                  | l == l'   -> MatchYes []
                  | otherwise -> MatchNo
            _                 -> MatchMaybe

reddefn :: RWMDefn -> RW RWMDefn
reddefn (RWMDefn an n pt b e) = RWMDefn an n pt b <$> reduceExp e

reduce :: RWMProgram -> RW RWMProgram
reduce m = do
      ds' <- mapM reddefn $ defns m
      return m { defns = ds' }
