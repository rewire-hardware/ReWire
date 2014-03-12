{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.Expand (cmdExpand) where

import Prelude hiding (sequence,mapM)
import ReWire.Core.Syntax
import Unbound.LocallyNameless
import Control.Monad hiding (sequence,mapM)
import Data.List (isInfixOf,find)
import Data.Traversable (sequence,mapM)
import Data.Maybe (catMaybes,isNothing,fromJust)
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.Monad
import Control.Monad.State hiding (mapM)

import Debug.Trace (trace)

type EM = StateT Int RW

expandalt :: String -> Maybe Int -> RWCAlt -> EM RWCAlt
expandalt nexp mpos (RWCAlt b) = lunbind b (\(p,eb) ->
                                        do eb' <- expandexpr nexp mpos eb
                                           return (RWCAlt (bind p eb')))

expandexpr :: String -> Maybe Int -> RWCExp -> EM RWCExp
expandexpr nexp mpos (RWCApp t e1 e2)         = liftM2 (RWCApp t) (expandexpr nexp mpos e1) (expandexpr nexp mpos e2)
expandexpr nexp mpos (RWCLam t b)             = lunbind b (\(n,e) ->
                                                       do e' <- expandexpr nexp mpos e
                                                          return (RWCLam t (bind n e')))
expandexpr nexp mpos (RWCVar t n) | nexp == show n = do pos <- get
                                                        put (pos+1)
                                                        case mpos of
                                                          Just pos' | pos /= pos' -> return (RWCVar t n)
                                                          _                       -> askvar t n
                                  | otherwise = return (RWCVar t n)
expandexpr nexp mpos e@(RWCCon {})            = return e
expandexpr nexp mpos e@(RWCLiteral {})        = return e
expandexpr nexp mpos (RWCCase t e alts)       = do e'    <- expandexpr nexp mpos e
                                                   alts' <- mapM (expandalt nexp mpos) alts
                                                   return (RWCCase t e' alts')

expanddefn :: String -> Maybe String -> Maybe Int -> RWCDefn -> EM RWCDefn
expanddefn nexp mnin mpos (RWCDefn n (Embed b)) = case mnin of
                                                       Just n' | n' /= show n -> return (RWCDefn n (Embed b))
                                                       _                      -> lunbind b (\(tvs,(t,e)) ->
                                                                                             do e' <- expandexpr nexp mpos e
                                                                                                return (RWCDefn n (Embed (setbind tvs (t,e')))))

go :: String -> Maybe String -> Maybe Int -> EM [RWCDefn]
go n mnin mpos = do ds <- askDefns
                    mapM (expanddefn n mnin mpos) ds

cmdExpand :: TransCommand
cmdExpand s p = let ws = words s
                    mparse = case ws of
                      [n]                    -> Just (n,Nothing,Nothing)
                      [n,"in",nin]           -> Just (n,Just nin,Nothing)
                      [n,"at",spos]          -> Just (n,Nothing,Just (read spos::Int))
                      [n,"in",nin,"at",spos] -> Just (n,Just nin,Just (read spos::Int))
                      _                      -> Nothing
                 in case mparse of
                      Just (nexp,mnin,mpos) -> let ds = fst $ runRW p (runStateT (go nexp mnin mpos) 0)
                                                   p' = p { defns = trec ds }
                                               in  (Just p',Nothing)
                      Nothing               -> syntaxerr
   where syntaxerr = (Nothing,Just "Syntax: expand <symbol> [in <symbol>] [at <n>]")
