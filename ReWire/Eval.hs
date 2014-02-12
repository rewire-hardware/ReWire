module ReWire.Eval where

import ReWire.Core
import ReWire.CorePP
import Unbound.LocallyNameless
import ReWire.CoreParser (rwcProg,whiteSpace)
import Text.Parsec (runParser,eof)
import Control.Monad
import Data.List (isInfixOf)

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

simpldefn :: Fresh m => RWCDefn -> m RWCDefn
simpldefn (RWCDefn n (Embed b)) = do (tvs,(cs,t,e_)) <- unbind b
                                     e               <- simplify e_ 
                                     return (RWCDefn n (embed $ setbind tvs (cs,t,e)))
simpldefn d                     = return d

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

