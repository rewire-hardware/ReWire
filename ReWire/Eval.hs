{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Eval where

import Prelude hiding (sequence,mapM)
import ReWire.Core
import ReWire.CorePP (pp)
import Unbound.LocallyNameless
import Text.Parsec (runParser,eof)
import Control.Monad hiding (sequence,mapM)
import Data.List (isInfixOf,find)
import Control.Monad.Reader hiding (sequence,mapM)
import Control.Monad.Identity hiding (sequence,mapM)
import Data.Traversable (sequence,mapM)
import Data.Maybe (catMaybes,isNothing,fromJust)

--import Debug.Trace (trace)

simplify :: LFresh m => RWCExp -> m RWCExp
simplify (RWCApp t e_ e'_)     = do e  <- simplify e_
                                    e' <- simplify e'_
                                    case e of
                                      RWCLam _ b -> lunbind b (\(x,eb) ->
                                                      simplify (subst x e' eb))
                                      _        -> return (RWCApp t e e')
simplify e@(RWCLam t b)        = lunbind b (\(x,eb) ->
                                  do eb' <- simplify eb
                                     return (RWCLam t (bind x eb')))
simplify e@(RWCVar _ _)      = return e
simplify e@(RWCCon _ _)      = return e
simplify e@(RWCLiteral _ _)  = return e
simplify (RWCCase t e_ alts_)  = do e    <- simplify e_
                                    alts <- mapM simplalt alts_
                                    sr   <- simplcase e alts
                                    case sr of
                                      Just e' -> return e'
                                      Nothing -> return (RWCCase t e alts)

simplalt :: LFresh m => RWCAlt -> m RWCAlt
simplalt (RWCAlt b) = lunbind b (\(p,ebody_) ->
                       do ebody <- simplify ebody_
                          return (RWCAlt (bind p ebody)))

flattenApp :: RWCExp -> [RWCExp]
flattenApp (RWCApp _ e e') = e:flattenApp e'
flattenApp e               = [e]

simplcase :: LFresh m => RWCExp -> [RWCAlt] -> m (Maybe RWCExp)
simplcase escrut (RWCAlt b:alts) = lunbind b (\(p,ebody) ->
                                    do mr <- matchpat escrut p
                                       case mr of
                                         MatchYes sub -> liftM Just $ simplify (substs sub ebody)
                                         MatchMaybe   -> return Nothing
                                         MatchNo      -> simplcase escrut alts)
simplcase escrut []              = return Nothing -- FIXME: should return undefined

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

simpldefn :: LFresh m => RWCDefn -> m RWCDefn
simpldefn (RWCDefn n (Embed b)) = lunbind b (\(tvs,(t,e_)) ->
                                   do e <- simplify e_
                                      return (RWCDefn n (embed $ setbind tvs (t,e))))

defnName :: RWCDefn -> AnyName
defnName (RWCDefn n _) = AnyName n

simplprog :: LFresh m => RWCProg -> m RWCProg
simplprog p = do ds  <- luntrec (defns p)
                 ds' <- avoid (map defnName ds) (mapM simpldefn ds)
                 return (p { defns = trec ds' })

type PEM = ReaderT [RWCDefn] (LFreshMT Identity)

runPEM :: PEM a -> a
runPEM m = runIdentity (runLFreshMT (runReaderT m []))

askDefns :: PEM [RWCDefn]
askDefns = ask

expandalt :: RWCAlt -> PEM RWCAlt
expandalt (RWCAlt b) = lunbind b (\(p,eb) ->
                        do eb' <- expandexpr eb
                           return (RWCAlt (bind p eb')))

mergesubs :: Monad m => [(Name RWCTy,RWCTy)] -> [(Name RWCTy,RWCTy)] -> m [(Name RWCTy,RWCTy)]
mergesubs ((n,t):sub) sub' = case lookup n sub' of
                               Just t' -> if t `aeq` t' then mergesubs sub sub'
                                                        else fail "mergesubs failed"
                               Nothing -> do sub'' <- mergesubs sub sub'
                                             return ((n,t):sub'')
mergesubs [] sub'          = return sub'

matchty :: Monad m => [(Name RWCTy,RWCTy)] -> RWCTy -> RWCTy -> m [(Name RWCTy,RWCTy)]
matchty sub (RWCTyVar n) t                         = case lookup n sub of
                                                       Nothing -> return ((n,t):sub)
                                                       Just t' -> if t `aeq` t' then return sub
                                                                                else fail "matchty failed (variable inconsistency)"
matchty sub (RWCTyCon i1) (RWCTyCon i2) | i1 == i2 = return sub
matchty sub (RWCTyApp t1 t2) (RWCTyApp t1' t2')    = do sub1 <- matchty [] t1 t1'
                                                        sub2 <- matchty [] t2 t2'
                                                        mergesubs sub1 sub2
matchty _ t1 t2                                    = fail $ "matchty failed (constructor head): " ++ show t1 ++ ", " ++ show t2

askvar :: RWCTy -> Name RWCExp -> PEM RWCExp
askvar t n = do ds <- askDefns
                case find (\ (RWCDefn n' _) -> n == n') ds of
                  Just (RWCDefn _ (Embed b)) -> lunbind b (\(tvs,(t',e)) ->
                                                 do sub <- matchty [] t' t
                                                    return (substs sub e))
                  _                          -> return (RWCVar t n)

expandexpr :: RWCExp -> PEM RWCExp
expandexpr (RWCApp t e1 e2)   = liftM2 (RWCApp t) (expandexpr e1) (expandexpr e2)
expandexpr (RWCLam t b)       = lunbind b (\(n,e) ->
                                 do e' <- expandexpr e
                                    return (RWCLam t (bind n e')))
expandexpr (RWCVar t n)       = askvar t n
expandexpr e@(RWCCon {})      = return e
expandexpr e@(RWCLiteral {})  = return e
expandexpr (RWCCase t e alts) = do e'    <- expandexpr e
                                   alts' <- mapM expandalt alts
                                   return (RWCCase t e' alts')

expanddefn :: RWCDefn -> PEM RWCDefn
expanddefn (RWCDefn n (Embed b)) = lunbind b (\(tvs,(t,e)) ->
                                    do e' <- expandexpr e
                                       return (RWCDefn n (Embed (setbind tvs (t,e')))))

pe :: RWCProg -> PEM RWCProg
pe p = do ds   <- luntrec (defns p)
          ds'  <- avoid (map defnName ds) (local (const ds) (mapM expanddefn ds))
          ds'' <- avoid (map defnName ds) (mapM simpldefn ds')
          return (p { defns = trec ds'' })

doPE :: RWCProg -> IO ()
doPE p = do print (pp p)
            loop p
  where loop p = do putStrLn "Press Ctrl-C to exit, or enter to do another round of partial evaluation."
                    getLine
                    let p' = runPEM (pe p)
                    putStrLn "================"
                    putStrLn "================"
                    putStrLn "================"
                    print (pp p')
                    loop p'
