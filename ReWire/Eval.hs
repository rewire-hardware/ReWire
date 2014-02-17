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
simplalt (RWCAlt b) = do (p,(eguard_,ebody_)) <- unbind b
                         eguard               <- simplify eguard_
                         ebody                <- simplify ebody_
                         return (RWCAlt (bind p (eguard,ebody)))

flattenApp :: RWCExp -> [RWCExp]
flattenApp (RWCApp e e') = e:flattenApp e'
flattenApp e             = [e]

isTrueCon :: Identifier -> Bool
isTrueCon = ("True" `isInfixOf`) -- well, it's good enough if the program is well typed :)

simplcase :: Fresh m => RWCExp -> [RWCAlt] -> m (Maybe RWCExp)
simplcase escrut (RWCAlt b:alts) = do (p,(eguard_,ebody)) <- unbind b
                                      mr                  <- matchpat escrut p
                                      case mr of
                                        MatchYes sub -> do let eguard =  substs sub eguard_
                                                           eguard'    <- simplify eguard
                                                           case eguard' of
                                                             (RWCCon _ c) | isTrueCon c -> liftM Just $ simplify (substs sub ebody)
                                                                          | otherwise   -> simplcase escrut alts
                                                             _                          -> return Nothing
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

simplclassmethod :: Fresh m => RWCClassMethod -> m RWCClassMethod
simplclassmethod (RWCClassMethod n (Embed b)) = do (tvs,(cs,ty,me_)) <- unbind b
                                                   me                <- mapM simplify me_
                                                   return (RWCClassMethod n (embed (setbind tvs (cs,ty,me))))

simplinstancemethod :: Fresh m => RWCInstanceMethod -> m RWCInstanceMethod
simplinstancemethod (RWCInstanceMethod n b) = do (tvs,(cs,t,e_)) <- unbind b
                                                 e               <- simplify e_
                                                 return (RWCInstanceMethod n (setbind tvs (cs,t,e)))

simplinstance :: Fresh m => RWCInstance -> m RWCInstance
simplinstance (RWCInstance b meths_) = do meths <- mapM simplinstancemethod meths_
                                          return (RWCInstance b meths)

simpldefn :: Fresh m => RWCDefn -> m RWCDefn
simpldefn (RWCDefn n (Embed b))                      = do (tvs,(cs,t,e_)) <- unbind b
                                                          e               <- simplify e_ 
                                                          return (RWCDefn n (embed $ setbind tvs (cs,t,e)))
simpldefn (RWCClass i (Embed b) cms_ (Embed insts_)) = do cms   <- mapM simplclassmethod cms_
                                                          insts <- mapM simplinstance insts_
                                                          return (RWCClass i (Embed b) cms (Embed insts))

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

type PEM = ReaderT ([RWCDefn],[RWCConstraint]) (FreshMT Identity)

runPEM :: PEM a -> a
runPEM m = runIdentity (runFreshMT (runReaderT m ([],[])))

askDefns :: PEM [RWCDefn]
askDefns = ask >>= \ (ds,_) -> return ds

askConstraints :: PEM [RWCConstraint]
askConstraints = ask >>= \ (_,cs) -> return cs

withConstraints :: [RWCConstraint] -> PEM a -> PEM a
withConstraints cs = local (\(ds,cs') -> (ds,cs++cs'))

expandalt :: RWCAlt -> PEM RWCAlt
expandalt (RWCAlt b) = do (p,(eg,eb)) <- unbind b
                          eg'         <- expandexpr eg
                          eb'         <- expandexpr eb
                          return (RWCAlt (bind p (eg',eb')))

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

checkbyinst :: RWCConstraint -> RWCInstance -> PEM Bool
checkbyinst (RWCConstraint i ts) (RWCInstance b meths) = do (tvs,(cs,ts')) <- unbind b
                                                            let subs       =  zipWith (matchty []) ts' ts
                                                            if any isNothing subs
                                                              then return False
                                                              else case foldM mergesubs [] (catMaybes subs) of
                                                                     Just sub -> liftM and (mapM checkconstraint (substs sub cs))
                                                                     Nothing  -> return False

checkbydefn :: RWCConstraint -> RWCDefn -> PEM Bool
checkbydefn _ (RWCDefn {})                                                   = return False
checkbydefn (RWCConstraint i ts) (RWCClass i' _ _ (Embed insts)) | i /= i'   = return False
                                                                 | otherwise = liftM or (mapM (checkbyinst (RWCConstraint i ts)) insts)

checkconstraint :: RWCConstraint -> PEM Bool
checkconstraint c = do cs <- askConstraints
                       if any (`aeq` c) cs
                          then return True
                          else do ds <- askDefns
                                  liftM or (mapM (checkbydefn c) ds)
                                  -- FIXME: if this returns False, should proceed to check if c is true by superclassing

tryinstancemethod :: RWCTy -> Name RWCExp -> RWCInstanceMethod -> PEM (Maybe RWCExp)
tryinstancemethod t n (RWCInstanceMethod n' b) | n /= n'   = return Nothing
                                               | otherwise = do (tvs,(cs,t',e)) <- unbind b
                                                                case matchty [] t' t of
                                                                  Just sub -> do issat <- liftM and (mapM checkconstraint (substs sub cs))
                                                                                 if issat then trace "satisfied" (return (Just $ substs sub e)) else return Nothing
                                                                  Nothing  -> trace ("no match: " ++ show t) $ return Nothing

tryinstance :: RWCTy -> Name RWCExp -> RWCInstance -> PEM [Maybe RWCExp]
tryinstance t n (RWCInstance b meths) = mapM (tryinstancemethod t n) meths

askvar :: RWCTy -> Name RWCExp -> PEM RWCExp
askvar t n = do ds <- askDefns
                -- First let's see if this is a non-overloaded function.
                case find (\ d -> case d of
                                    RWCDefn n' _ -> n == n'
                                    _            -> False) ds of
                  Just (RWCDefn _ (Embed b)) -> do (tvs,(cs,t',e)) <- unbind b
                                                   sub             <- matchty [] t' t
                                                   return (substs sub e)
                  _                          -> -- Nope. Now let's see if this is an overloaded function.
                                                case find (\ d -> case d of
                                                              RWCClass _  _ meths _ -> n `elem` map (\ (RWCClassMethod n _) -> n) meths
                                                              _                     -> False) ds of
                                                     Just (RWCClass _ _ _ (Embed insts))  -> -- Yes, it's overloaded. Let's see if we can nail down the instance.
                                                                                            trace ("overloaded: " ++ show n ++ " at " ++ show t) $ 
                                                                                             do tries <- liftM (catMaybes . concat) $ mapM (tryinstance t n) insts
                                                                                                case tries of
                                                                                                  [e] -> return e             -- one match: expand
                                                                                                  []  -> trace "no match" $ return (RWCVar t n)  -- no match: can't expand
                                                                                                  _   -> fail "askvar: overlapping instances"
                                                     _                                    -> -- Nope. We're stuck!
                                                                                             return (RWCVar t n)

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

expandinstancemethod :: RWCInstanceMethod -> PEM RWCInstanceMethod
expandinstancemethod (RWCInstanceMethod n b) = do (tvs,(cs,t,e_)) <- unbind b
                                                  e               <- withConstraints cs $ expandexpr e_
                                                  return (RWCInstanceMethod n (setbind tvs (cs,t,e)))

expandclassmethod :: RWCClassMethod -> PEM RWCClassMethod
expandclassmethod (RWCClassMethod n (Embed b)) = do (tvs,(cs,ty,me_)) <- unbind b
                                                    me                <- withConstraints cs $ mapM expandexpr me_
                                                    return (RWCClassMethod n (embed (setbind tvs (cs,ty,me))))

expandinstance :: RWCInstance -> PEM RWCInstance
expandinstance (RWCInstance b meths_) = do meths <- mapM expandinstancemethod meths_
                                           return (RWCInstance b meths)

expanddefn :: RWCDefn -> PEM RWCDefn
expanddefn (RWCClass i (Embed b) cms_ (Embed insts_)) = do cms   <- mapM expandclassmethod cms_
                                                           insts <- mapM expandinstance insts_
                                                           return (RWCClass i (embed b) cms (embed insts))
expanddefn (RWCDefn n (Embed b))                      = do (tvs,(cs,t,e)) <- unbind b
                                                           e'             <- expandexpr e
                                                           return (RWCDefn n (Embed (setbind tvs (cs,t,e'))))

pe :: RWCProg -> PEM RWCProg
pe p = do ds   <- untrec (defns p)
          ds'  <- local (const (ds,[])) (mapM expanddefn ds)
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