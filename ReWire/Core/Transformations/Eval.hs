{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.Eval where

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
flattenApp (RWCApp _ e e') = flattenApp e++[e']
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

expandalt :: Name RWCExp -> RWCAlt -> PEM RWCAlt
expandalt nexp (RWCAlt b) = lunbind b (\(p,eb) ->
                             do eb' <- expandexpr nexp eb
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

expandexpr :: Name RWCExp -> RWCExp -> PEM RWCExp
expandexpr nexp (RWCApp t e1 e2)   = liftM2 (RWCApp t) (expandexpr nexp e1) (expandexpr nexp e2)
expandexpr nexp (RWCLam t b)       = lunbind b (\(n,e) ->
                                      do e' <- expandexpr nexp e
                                         return (RWCLam t (bind n e')))
expandexpr nexp (RWCVar t n) | nexp == n = askvar t n
                             | otherwise = return (RWCVar t n)
--expandexpr nexp (RWCVar t n)       = askvar t n
expandexpr nexp e@(RWCCon {})      = return e
expandexpr nexp e@(RWCLiteral {})  = return e
expandexpr nexp (RWCCase t e alts) = do e'    <- expandexpr nexp e
                                        alts' <- mapM (expandalt nexp) alts
                                        return (RWCCase t e' alts')

expanddefn :: Name RWCExp -> RWCDefn -> PEM RWCDefn
expanddefn nexp (RWCDefn n (Embed b)) = lunbind b (\(tvs,(t,e)) ->
                                         do e' <- expandexpr nexp e
                                            return (RWCDefn n (Embed (setbind tvs (t,e')))))

pe :: Name RWCExp -> RWCProg -> PEM RWCProg
pe n p = do ds   <- luntrec (defns p)
            ds'  <- avoid (map defnName ds) (local (const ds) (mapM (expanddefn n) ds))
            ds'' <- avoid (map defnName ds) (mapM simpldefn ds')
            return (p { defns = trec ds'' })

cmdExpand :: TransCommand
cmdExpand n p = let p' = runPEM (pe (s2n n) p)
                in  (Just p',Nothing)
