{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Eval where

import Prelude hiding (sequence,mapM)
import ReWire.Core
import ReWire.CorePP
import Unbound.LocallyNameless
import ReWire.CoreParser (rwcProg,whiteSpace)
import Text.Parsec (runParser,eof)
import Control.Monad hiding (sequence,mapM)
import Data.List (isInfixOf,find)
import Control.Monad.Reader hiding (sequence,mapM)
import Control.Monad.Identity hiding (sequence,mapM)
import Data.Traversable (sequence,mapM)
import Data.Maybe (catMaybes,isNothing,fromJust)

import Debug.Trace (trace)

simplify :: Fresh m => RWCExp -> m RWCExp
simplify (RWCApp e_ e'_)     = do e  <- simplify e_
                                  e' <- simplify e'_
                                  case e of
                                    RWCLam b -> do (x,eb) <- unbind b
                                                   simplify (subst x e' eb)
                                    _        -> return (RWCApp e e')
simplify e@(RWCLam b)        = do (x,eb) <- unbind b
                                  eb'    <- simplify eb
                                  return (RWCLam (bind x eb'))
simplify e@(RWCVar _ _)      = return e
simplify e@(RWCCon _ _)      = return e
simplify e@(RWCLiteral _ _)  = return e
simplify (RWCCase e_ alts_)  = do e    <- simplify e_
                                  alts <- mapM simplalt alts_
                                  sr   <- simplcase e alts
                                  case sr of
                                    Just e' -> return e'
                                    Nothing -> return (RWCCase e alts)

simplalt :: Fresh m => RWCAlt -> m RWCAlt
simplalt (RWCAlt b) = do (p,ebody_) <- unbind b
                         ebody      <- simplify ebody_
                         return (RWCAlt (bind p ebody))

flattenApp :: RWCExp -> [RWCExp]
flattenApp (RWCApp e e') = e:flattenApp e'
flattenApp e             = [e]

isTrueCon :: Identifier -> Bool
isTrueCon = ("True" `isInfixOf`) -- well, it's good enough if the program is well typed :)

simplcase :: Fresh m => RWCExp -> [RWCAlt] -> m (Maybe RWCExp)
simplcase escrut (RWCAlt b:alts) = do (p,ebody) <- unbind b
                                      mr        <- matchpat escrut p
                                      case mr of
                                        MatchYes sub -> liftM Just $ simplify (substs sub ebody)
                                        MatchMaybe   -> return Nothing
                                        MatchNo      -> simplcase escrut alts
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

matchpat :: Fresh m => RWCExp -> RWCPat -> m MatchResult
matchpat e (RWCPatCon i pats) = case flattenApp e of
                                  (RWCCon _ c:es) | c == i && length es == length pats -> do ms <- zipWithM matchpat es pats
                                                                                             return (mergematches ms)
                                                  | otherwise                          -> return MatchNo
                                  _                                                    -> return MatchMaybe
matchpat e (RWCPatVar n)      = return (MatchYes [(n,e)])
matchpat e (RWCPatLiteral l)  = case e of
                                  RWCLiteral _ l' | l == l'   -> return (MatchYes [])
                                                  | otherwise -> return MatchNo
                                  _                           -> return MatchMaybe

simpldefn :: Fresh m => RWCDefn -> m RWCDefn
simpldefn (RWCDefn n (Embed b))                      = do (tvs,(t,e_)) <- unbind b
                                                          e            <- simplify e_ 
                                                          return (RWCDefn n (embed $ setbind tvs (t,e)))

simplprog :: Fresh m => RWCProg -> m RWCProg
simplprog p = do ds  <- untrec (defns p)
                 ds' <- mapM simpldefn ds
                 return (p { defns = trec ds' })

psimp :: FilePath -> IO ()
psimp n = do guts    <- readFile n
             let res =  runParser (whiteSpace >> rwcProg >>= \ p -> whiteSpace >> eof >> return p) () n guts
             case res of
               Left err  -> print err
               Right ast -> print (runFreshM (ppProg ast)) >> putStrLn "===" >> print (runFreshM (simplprog ast >>= ppProg))

type PEM = ReaderT [RWCDefn] (FreshMT Identity)

runPEM :: PEM a -> a
runPEM m = runIdentity (runFreshMT (runReaderT m []))

askDefns :: PEM [RWCDefn]
askDefns = ask

expandalt :: RWCAlt -> PEM RWCAlt
expandalt (RWCAlt b) = do (p,eb) <- unbind b
                          eb'    <- expandexpr eb
                          return (RWCAlt (bind p eb'))

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
matchty _ _ _                                      = fail "matchty failed (constructor head)"

askvar :: RWCTy -> Name RWCExp -> PEM RWCExp
askvar t n = do ds <- askDefns
                case find (\ (RWCDefn n' _) -> n == n') ds of
                  Just (RWCDefn _ (Embed b)) -> do (tvs,(t',e)) <- unbind b
                                                   sub          <- matchty [] t' t
                                                   return (substs sub e)
                  _                          -> return (RWCVar t n)

expandexpr :: RWCExp -> PEM RWCExp
expandexpr (RWCApp e1 e2)    = liftM2 RWCApp (expandexpr e1) (expandexpr e2)
expandexpr (RWCLam b)        = do (n,e) <- unbind b
                                  e'    <- expandexpr e
                                  return (RWCLam (bind n e'))
expandexpr (RWCVar t n)      = askvar t n
expandexpr e@(RWCCon {})     = return e
expandexpr e@(RWCLiteral {}) = return e
expandexpr (RWCCase e alts)  = do e'    <- expandexpr e
                                  alts' <- mapM expandalt alts
                                  return (RWCCase e' alts')

expanddefn :: RWCDefn -> PEM RWCDefn
expanddefn (RWCDefn n (Embed b))                      = do (tvs,(t,e)) <- unbind b
                                                           e'          <- expandexpr e
                                                           return (RWCDefn n (Embed (setbind tvs (t,e'))))

pe :: RWCProg -> PEM RWCProg
pe p = do ds   <- untrec (defns p)
          ds'  <- local (const ds) (mapM expanddefn ds)
          ds'' <- mapM simpldefn ds'
          return (p { defns = trec ds'' })

doPE :: FilePath -> IO ()
doPE n = do guts    <- readFile n
            let res =  runParser (whiteSpace >> rwcProg >>= \ p -> whiteSpace >> eof >> return p) () n guts
            case res of
              Left err  -> print err
              Right ast -> do print (runFreshM (ppProg ast))
                              loop ast
                 where loop p = do getLine
                                   let p' = runPEM (pe p)
                                   putStrLn "================"
                                   putStrLn "================"
                                   putStrLn "================"
                                   print (runFreshM (ppProg p'))
                                   loop p'