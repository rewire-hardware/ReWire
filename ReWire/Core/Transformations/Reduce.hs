{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.Reduce where

import Prelude hiding (sequence,mapM)
import ReWire.Core.Syntax
import Unbound.LocallyNameless
import Control.Monad hiding (sequence,mapM)
import Data.List (isInfixOf,find)
import Control.Monad.Reader hiding (sequence,mapM)
import Control.Monad.Identity hiding (sequence,mapM)
import Data.Traversable (sequence,mapM)
import Data.Maybe (catMaybes,isNothing,fromJust)
import ReWire.Core.Transformations.Types

import Debug.Trace (trace)

reduce :: LFresh m => RWCExp -> m RWCExp
reduce (RWCApp t e_ e'_)     = do e  <- reduce e_
                                  e' <- reduce e'_
                                  case e of
                                    RWCLam _ b -> lunbind b (\(x,eb) ->
                                                      reduce (subst x e' eb))
                                    _        -> return (RWCApp t e e')
reduce e@(RWCLam t b)        = lunbind b (\(x,eb) ->
                                  do eb' <- reduce eb
                                     return (RWCLam t (bind x eb')))
reduce e@(RWCVar _ _)      = return e
reduce e@(RWCCon _ _)      = return e
reduce e@(RWCLiteral _ _)  = return e
reduce (RWCCase t e_ alts_)  = do e    <- reduce e_
                                  alts <- mapM redalt alts_
                                  sr   <- redcase e alts
                                  case sr of
                                    Just e' -> return e'
                                    Nothing -> return (RWCCase t e alts)

redalt :: LFresh m => RWCAlt -> m RWCAlt
redalt (RWCAlt b) = lunbind b (\(p,ebody_) ->
                       do ebody <- reduce ebody_
                          return (RWCAlt (bind p ebody)))

flattenApp :: RWCExp -> [RWCExp]
flattenApp (RWCApp _ e e') = flattenApp e++[e']
flattenApp e               = [e]

redcase :: LFresh m => RWCExp -> [RWCAlt] -> m (Maybe RWCExp)
redcase escrut (RWCAlt b:alts) = lunbind b (\(p,ebody) ->
                                    do mr <- matchpat escrut p
                                       case mr of
                                         MatchYes sub -> liftM Just $ reduce (substs sub ebody)
                                         MatchMaybe   -> return Nothing
                                         MatchNo      -> redcase escrut alts)
redcase escrut []              = return Nothing -- FIXME: should return undefined

data MatchResult = MatchYes [(Name RWCExp,RWCExp)]
                 | MatchMaybe
                 | MatchNo
                 deriving Show

mergematches :: [MatchResult] -> MatchResult
mergematches []     = MatchYes []
mergematches (m:ms) = case mr of
                        MatchYes bs -> case m of
                                         MatchYes bs' -> MatchYes (bs'++bs)
                                         MatchNo      -> MatchNo
                                         MatchMaybe   -> MatchMaybe
                        MatchNo     -> MatchNo
                        MatchMaybe  -> case m of
                                         MatchYes _ -> MatchMaybe
                                         MatchNo    -> MatchNo
                                         MatchMaybe -> MatchMaybe
  where mr = mergematches ms

matchpat :: LFresh m => RWCExp -> RWCPat -> m MatchResult
matchpat e (RWCPatCon i pats) = case flattenApp e of
                                  (RWCCon _ c:es) | c == i && length es == length pats -> do ms <- zipWithM matchpat es pats
                                                                                             return (mergematches ms)
                                                  | otherwise                          -> return MatchNo
                                  _                                                    -> return MatchMaybe
matchpat e (RWCPatVar _ n)    = return (MatchYes [(n,e)])
matchpat e (RWCPatLiteral l)  = case e of
                                  RWCLiteral _ l' | l == l'   -> return (MatchYes [])
                                                  | otherwise -> return MatchNo
                                  _                           -> return MatchMaybe

reddefn :: LFresh m => RWCDefn -> m RWCDefn
reddefn (RWCDefn n (Embed b)) = lunbind b (\(tvs,(t,e_)) ->
                                   do e <- reduce e_
                                      return (RWCDefn n (embed $ setbind tvs (t,e))))

defnName :: RWCDefn -> AnyName
defnName (RWCDefn n _) = AnyName n

redprog :: LFresh m => RWCProg -> m RWCProg
redprog p = do ds  <- luntrec (defns p)
               ds' <- avoid (map defnName ds) (mapM reddefn ds)
               return (p { defns = trec ds' })
                        
cmdReduce :: TransCommand
cmdReduce n p = let p' = runLFreshM (redprog p)
                in  (Just p',Nothing)
