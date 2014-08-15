{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.ToAG where

import ReWire.ActionGraph
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.Monad
import ReWire.Core.Transformations.Uniquify (uniquify)
import ReWire.Core.Syntax
import ReWire.Scoping
import Control.Monad.State
import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Graph.Inductive

type VarMap = Map (Id RWCExp) Loc
type ActionMap = Map (Id RWCExp) ([Loc],Node,Node,Loc) -- name -> arg regs, entry node, exit node, result reg
type AGM = ReaderT VarMap (StateT (Int,ActionGraph,ActionMap) RW)

getC :: AGM Int
getC = do (c,_,_) <- get
          return c

putC :: Int -> AGM ()
putC c = do (_,g,am) <- get
            put (c,g,am)
  
getGraph :: AGM ActionGraph
getGraph = do (_,g,_) <- get
              return g

putGraph :: ActionGraph -> AGM ()
putGraph g = do (c,_,am) <- get
                put (c,g,am)

getActionMap :: AGM ActionMap
getActionMap = do (_,_,am) <- get
                  return am

putActionMap :: ActionMap -> AGM ()
putActionMap am = do (c,g,_) <- get
                     put (c,g,am)

binding :: Id RWCExp -> Loc -> AGM a -> AGM a
binding n l = local (Map.insert n l)

askBinding :: Id RWCExp -> AGM (Maybe Loc)
askBinding n = do varm <- ask
                  return (Map.lookup n varm)

addEdge :: Node -> Node -> (Maybe Loc) -> AGM ()
addEdge ns nd mr = do g <- getGraph
                      putGraph (insEdge (ns,nd,mr) g)

freshLoc :: AGM Loc
freshLoc = do c <- getC
              putC (c+1)
              return ("r" ++ show c)

addFreshNode :: Cmd -> AGM Node
addFreshNode e = do c <- getC
                    putC (c+1)
                    g <- getGraph
                    putGraph (insNode (c,e) g)
                    return c

stringNodes :: [(Node,Node,Loc)] -> AGM ()
stringNodes ((_,no,_):x@(ni,_,_):xs) = do addEdge no ni Nothing
                                          stringNodes (x:xs)
stringNodes _                        = return ()

--agAcAlt :: Loc -> RWCAlt -> AGM (Node,Node,Node,Loc)
-- entry node, true exit node, false exit node, result reg if true
stringAlts :: Node -> [(Node,Node,Node,Loc)] -> AGM ()
stringAlts nl ((_,no1_t,no1_f,_):x@(no2_e,_,_,_):xs) = do addEdge no1_t nl Nothing
                                                          addEdge no1_f no2_e Nothing
                                                          stringAlts nl (x:xs)
stringAlts nl [(_,no_t,no_f,r)]                      = do addEdge no_t nl Nothing
                                                          addEdge no_f nl Nothing
stringAlts _  []                                     = return ()

agExpr :: RWCExp -> AGM (Node,Node,Loc)
agExpr e = case ef of
             RWCApp _ _     -> fail "agExpr: app in function position (can't happen)"
             RWCLiteral _   -> fail "agExpr: encountered literal"
             RWCLet x el eb -> do
               (niel,noel,lel) <- agExpr el
               (nieb,noeb,leb) <- binding x lel $ agExpr eb
               addEdge noel nieb Nothing
               return (niel,noeb,leb)
             RWCVar x _     -> do
               mr <- askBinding x
               case mr of
                 Just r  -> do n <- addFreshNode (Rem $ "got " ++ show x ++ " in " ++ r)
                               return (n,n,r)
                 Nothing -> do
                   ninors <- mapM agExpr eargs
                   stringNodes ninors
                   let rs =  map ( \ (_,_,r) -> r) ninors -- FIXME: must fill the regs in!
                   r      <- freshLoc
                   n      <- addFreshNode (FunCall r (show x) rs)
                   case ninors of
                     [] -> return (n,n,r)
                     _  -> do let (ni,_,_) = head ninors
                                  (_,no,_) = last ninors
                              addEdge no n Nothing
                              return (ni,n,r)
             RWCLam _ _ _   -> fail $ "agExpr: encountered lambda"
             RWCCon _ _     -> do r <- freshLoc
                                  n <- addFreshNode (FunCall r "FIXMECON" [])
                                  return (n,n,r)
--             RWCLet n el eb ->
--               case eargs of
--                 [] -> do r <- freshLoc
--                          
--                 _  -> fail $ "agExpr: encountered let in function position"
  where (ef:eargs) = flattenApp e

agAcAlt :: Loc -> Loc -> RWCAlt -> AGM (Node,Node,Node,Loc) -- entry node, true exit node, false exit node, result reg if true
agAcAlt lscr lres (RWCAlt p e) = do (nie,noe,le) <- agAcExpr e
                                    ni           <- addFreshNode (Rem "alt entry")
                                    no_t         <- addFreshNode (Assign lres le)
                                    no_f         <- addFreshNode (Rem "alt exit (no match)")
                                    addEdge noe no_t Nothing
                                    addEdge ni nie Nothing
                                    addEdge ni no_f Nothing
                                    return (ni,no_t,no_f,le)

agAcExpr :: RWCExp -> AGM (Node,Node,Loc) -- entry node, exit node, result reg
agAcExpr e = case ef of
               RWCApp _ _                    -> fail "agAcExpr: app in function position (can't happen)"
               RWCLam _ _ _                  -> fail "agAcExpr: encountered lambda"
               RWCLiteral _                  -> fail "agAcExpr: encountered literal"
               RWCCon _ _                    -> fail "agAcExpr: encountered con"
               RWCLet x el eb -> do
                 (niel,noel,lel) <- agAcExpr el
                 (nieb,noeb,leb) <- binding x lel $ agAcExpr eb
                 addEdge noel nieb Nothing
                 return (niel,noeb,leb)
               RWCVar x _ | x == mkId "bind" -> do
                 case eargs of
                   [el,RWCLam x _ er] -> do
                     (entl,exl,regl)  <- agAcExpr el
                     (entr,exr,regr)  <- binding x regl $ agAcExpr er
                     addEdge exl entr Nothing
                     return (entl,exr,regr)
                   _ -> fail "wrong rhs for bind"
               RWCVar x _ | x == mkId "return" -> do
                 case eargs of
                   [e] -> agExpr e
                   _   -> fail "agAcExpr: wrong number of arguments for return"
               RWCVar x _ | x == mkId "signal" -> do
                 case eargs of
                   [e] -> do
                     (ni,no,re) <- agExpr e
                     r          <- freshLoc
                     n          <- addFreshNode (Signal re r)
                     addEdge no n Nothing
                     return (ni,n,r)
                   _  -> fail "agAcExpr: wrong number of arguments for signal"
               RWCVar x _                      -> do
                 ninors           <- mapM agExpr eargs
                 stringNodes ninors
                 let rs           =  map (\ (_,_,r) -> r) ninors -- FIXME: must fill the regs in!
                 (rs,nif,nof,rrf) <- agAcDefn x
                 return (nif,nof,rrf)
               RWCCase escr alts               -> do
                 case eargs of
                   [] -> do
                     (ni,no,r_scr)   <- agExpr escr
                     r_res           <- freshLoc
                     ninotnoers      <- mapM (agAcAlt r_scr r_res) alts
                     n               <- addFreshNode (Rem "FIXMECASESTART")
                     let (ni0,_,_,_) =  head ninotnoers
                     addEdge n ni Nothing
                     addEdge no ni0 Nothing
                     nl              <- addFreshNode (Rem "FIXMECASEEND")
                     stringAlts nl ninotnoers
                     return (n,nl,r_res)
                   _  -> fail "agAcExpr: encountered case expression in function position"
                     
   where (ef:eargs) = flattenApp e  

peelLambdas (RWCLam n t e) = ((n,t):nts,e')
                             where (nts,e') = peelLambdas e
peelLambdas e              = ([],e)

agAcDefn :: Id RWCExp -> AGM ([Loc],Node,Node,Loc)
agAcDefn n = do am <- getActionMap
                case Map.lookup n am of
                  Just x  -> return x
                  Nothing -> do 
                    md <- lift $ lift $ queryG n
                    case md of
                      Just (RWCDefn _ _ e_) -> do
                        let (xts,e)   =  peelLambdas e_
                            xs        =  map fst xts
                        rs            <-  mapM (const freshLoc) xs
                        let xrs       =  zip xs rs
                        rr            <- freshLoc
                        ni            <- addFreshNode (Rem $ show n ++ " in")
                        no            <- addFreshNode (Rem $ show n ++ " out")
                        putActionMap (Map.insert n (rs,ni,no,rr) am)
                        (nie,noe,rre) <- foldr (uncurry binding) (agAcExpr e) xrs
                        addEdge ni nie Nothing
                        addEdge noe no Nothing
                        return (rs,ni,no,rr)
                      Nothing              -> fail $ "agAcDefn: " ++ show n ++ " not defined"

agStart :: RWCExp -> AGM ()
agStart (RWCApp (RWCApp (RWCVar x _) e) _) | x == mkId "extrude" = agStart e -- FIXME
agStart (RWCVar x _)                                             = agAcDefn x  >> return () -- FIXME
agStart _ = fail "agStart: malformed start expression"

agProg :: AGM ()
agProg = do md <- lift $ lift $ queryG (mkId "start")
            case md of
              Nothing              -> fail "agProg: `start' not defined"
              Just (RWCDefn _ _ e) -> agStart e

ag :: RWCProg -> ActionGraph
ag p_ = fst $ runRW ctr p (runStateT (runReaderT (agProg >> getGraph) Map.empty) s0)
  where s0      = (0,empty,Map.empty)
        (p,ctr) = uniquify 0 p_

cmdToAG :: TransCommand
cmdToAG _ p = (Nothing,Just (mkDot $ ag p))