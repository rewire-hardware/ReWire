{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Will need VHDL primitives:
--
-- andBits
-- ctor_*
-- checkTag*
-- getField*    (this is a mite tricky because we'll need one for each instance of polymorphic types)

module ReWire.Core.Transformations.ToPreHDL where

import ReWire.PreHDL.Syntax
import ReWire.PreHDL.CFG
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
import Data.List (foldl',find)
import Data.Maybe (fromJust)

type VarMap = Map (Id RWCExp) Loc
type RegMap = [(Loc,Int)]
type ActionMap = Map (Id RWCExp) ([Loc],Node,Node,Loc) -- name -> arg regs, entry node, exit node, result reg
type CGM = ReaderT Env (StateT (Int,RegMap,CFG,ActionMap) RW)

data Env = Env { stateLayer :: Int,
                 stateTys   :: [RWCTy],
                 inputTy    :: RWCTy,
                 outputTy   :: RWCTy,
                 varMap     :: VarMap }

localStateLayer :: (Int -> Int) -> CGM a -> CGM a
localStateLayer f = local (\ e -> e { stateLayer = f (stateLayer e) })

askStateLayer :: CGM Int
askStateLayer = ask >>= return . stateLayer

localVarMap :: (VarMap -> VarMap) -> CGM a -> CGM a
localVarMap f = local (\ e -> e { varMap = f (varMap e) })

askVarMap :: CGM VarMap
askVarMap = ask >>= return . varMap

askStateTys :: CGM [RWCTy]
askStateTys = ask >>= return . stateTys

askInputTy :: CGM RWCTy
askInputTy = ask >>= return . inputTy

askOutputTy :: CGM RWCTy
askOutputTy = ask >>= return . outputTy

getC :: CGM Int
getC = do (c,_,_,_) <- get
          return c

putC :: Int -> CGM ()
putC c = do (_,rm,g,am) <- get
            put (c,rm,g,am)
  
getGraph :: CGM (Gr Cmd Branch)
getGraph = do (_,_,cfg,_) <- get
              return (cfgGraph cfg)

putGraph :: Gr Cmd Branch -> CGM ()
putGraph g = do (c,rm,cfg,am) <- get
                put (c,rm,cfg { cfgGraph = g },am)

getActionMap :: CGM ActionMap
getActionMap = do (_,_,_,am) <- get
                  return am

putActionMap :: ActionMap -> CGM ()
putActionMap am = do (c,rm,g,_) <- get
                     put (c,rm,g,am)

getRegMap :: CGM RegMap
getRegMap = do (_,rm,_,_) <- get
               return rm

putRegMap :: RegMap -> CGM ()
putRegMap rm = do (c,_,g,am) <- get
                  put (c,rm,g,am)

binding :: Id RWCExp -> Loc -> CGM a -> CGM a
binding n l = localVarMap (Map.insert n l)

askBinding :: Id RWCExp -> CGM (Maybe Loc)
askBinding n = do varm <- askVarMap
                  return (Map.lookup n varm)

addEdge :: Node -> Node -> Branch -> CGM ()
addEdge ns nd br = do g <- getGraph
                      putGraph (insEdge (ns,nd,br) g)

freshLocSize :: Int -> CGM Loc
freshLocSize 0 = do rm <- getRegMap
                    putRegMap (("EMPTY",0):rm)
                    return "EMPTY"
freshLocSize n = do c  <- getC
                    putC (c+1)
                    let r = "r" ++ show c
                    rm <- getRegMap
                    putRegMap ((r,n):rm)
                    return r

freshLocTy :: RWCTy -> CGM Loc
freshLocTy t = do n <- tyWidth t
                  freshLocSize n

nBits 0 = 0
nBits n = nBits (n `quot` 2) + 1

getTagWidth :: TyConId -> CGM Int
getTagWidth i = do Just (TyConInfo (RWCData _ _ cs)) <- lift $ lift $ queryT i
                   return (nBits (length cs-1))

tyWidth :: RWCTy -> CGM Int
tyWidth (RWCTyVar _) = fail $ "tyWidth: type variable encountered"
tyWidth t            = {-do twc <- getTyWidthCache
                          case Map.lookup t twc of
                            Just size -> return size
                            Nothing   ->-} do
                              let (th:_) = flattenTyApp t
                              case th of
                                RWCTyApp _ _ -> fail "tyWidth: encountered TyApp in func position (can't happen)"
                                RWCTyVar _ -> fail $ "tyWidth: type variable encountered"
                                RWCTyComp _ _ -> fail $ "tyWidth: computation type encountered"
                                RWCTyCon i -> do
                                  Just (TyConInfo (RWCData _ _ dcs)) <- lift $ lift $ queryT i
                                  tagWidth <- getTagWidth i
                                  cws      <- mapM (dataConWidth i) dcs
                                  let size =  tagWidth + maximum cws
--                                  modifyTyWidthCache (Map.insert t size)
                                  return size
                where dataConWidth di (RWCDataCon i _) = do
                        fts <- getFieldTys i t
                        liftM sum (mapM tyWidth fts)

addFreshNode :: Cmd -> CGM Node
addFreshNode e = do c <- getC
                    putC (c+1)
                    g <- getGraph
                    putGraph (insNode (c,e) g)
                    return c

compBase :: RWCTy -> RWCTy
compBase (RWCTyComp _ t) = compBase t
compBase t               = t

stringAlts :: Node -> [(Node,Node,Node,Loc)] -> CGM ()
stringAlts nl ((_,no1_t,no1_f,_):x@(no2_e,_,_,_):xs) = do addEdge no1_t nl (Conditional (BoolConst True))
                                                          addEdge no1_f no2_e (Conditional (BoolConst True))
                                                          stringAlts nl (x:xs)
-- The last alt is a special case; its condition is ignored, so it does not
-- really have an no_f.
stringAlts nl [(_,no,_,r)]                           = do addEdge no nl (Conditional (BoolConst True))
stringAlts _  []                                     = return ()

agExpr :: RWCExp -> CGM (Node,Node,Loc)
agExpr e = case ef of
             RWCApp _ _     -> fail "agExpr: app in function position (can't happen)"
             RWCLiteral _   -> fail "agExpr: encountered literal"
             RWCLam _ _ _   -> fail "agExpr: encountered lambda"
             RWCLet x el eb -> do
               (niel,noel,lel) <- agExpr el
               (nieb,noeb,leb) <- binding x lel $ agExpr eb
               addEdge noel nieb (Conditional (BoolConst True))
               return (niel,noeb,leb)
             RWCVar x _     -> do
               mr <- askBinding x
               case mr of
                 Just r  -> do -- If it's a locally bound variable we just
                               -- return the reg it's bound to. Need an entry
                               -- and exit node, though, so we throw a nop
                               -- in.
                               n <- addFreshNode (Rem $ "got " ++ show x ++ " in " ++ r)
                               return (n,n,r)
                 Nothing -> do
                   -- If this is not a locally bound variable then it must
                   -- be a non-monadic global, which tranlates to a FunCall.
                   ninors <- mapM agExpr eargs
                   stringNodes (map (\(ni,no,_) -> (ni,no)) ninors)
                   let rs =  map ( \ (_,_,r) -> r) ninors
                   r      <- freshLocTy (typeOf e)
                   n      <- addFreshNode (Assign r (FunCallRHS (show x) rs))
                   case ninors of
                     [] -> return (n,n,r)
                     _  -> do let (ni,_,_) = head ninors
                                  (_,no,_) = last ninors
                              addEdge no n (Conditional (BoolConst True))
                              return (ni,n,r)
             RWCCon (DataConId c) _ -> do
               -- Compile argument expressions.
               ninors_args <- mapM agExpr eargs
               -- Sequence argument expressions.
               stringNodes (map (\(ni,no,_) -> (ni,no)) ninors_args)
               -- Allocate result reg.
               r           <- freshLocTy (typeOf e)
               -- Apply constructor function to the args, result in r.
               let rs_args =  map ( \ (_,_,r) -> r) ninors_args
               n           <- addFreshNode (Assign r (FunCallRHS ("ctor_" ++ c) rs_args))
               case ninors_args of
                 [] -> -- No arguments, so n is all we need.
                       return (n,n,r)
                 _  -> do -- Sequence argument expressions with ctor call.
                          let (ni,_,_) = head ninors_args
                              (_,no,_) = last ninors_args
                          addEdge no n (Conditional (BoolConst True))
                          -- Entry is the beginning of the argument
                          -- expressions, exit is the constructor call.
                          return (ni,n,r)
             RWCCase escr alts               -> do
               case eargs of
                 [] -> do
                   (ni,no,r_scr)   <- agExpr escr
                   r_res           <- freshLocTy (typeOf e)
                   ninotnoers_init <- mapM (agAlt r_scr (typeOf escr) r_res) (init alts)
                   ninotnoers_last <- agLastAlt r_scr (typeOf escr) r_res (last alts)
                   let ninotnoers           =  ninotnoers_init ++ [ninotnoers_last]
                       (ni0,_,_,_)          =  head ninotnoers
                   addEdge no ni0 (Conditional (BoolConst True))
                   nl              <- addFreshNode (Rem "end case")
                   stringAlts nl ninotnoers
                   return (ni,nl,r_res)
                 _  -> fail "agExpr: encountered case expression in function position"
  where (ef:eargs) = flattenApp e

stringNodes :: [(Node,Node)] -> CGM ()
stringNodes ((_,no):x@(ni,_):xs) = do addEdge no ni (Conditional (BoolConst True))
                                      stringNodes (x:xs)
stringNodes _                    = return ()

getFieldTys :: DataConId -> RWCTy -> CGM [RWCTy]
getFieldTys i t = do Just (DataConInfo tci _)             <- lift $ lift $ queryD i
                     Just (TyConInfo (RWCData _ tvs dcs)) <- lift $ lift $ queryT tci
                     let pt   = foldl' RWCTyApp (RWCTyCon tci) (map RWCTyVar tvs)
                         msub = matchty Map.empty pt t
                     case msub of
                       Nothing  -> fail $ "getFieldTys: type matching failed (type was " ++ show t ++ " and datacon was " ++ deDataConId i ++ ")"
                       Just sub -> do let (RWCDataCon _ targs) = fromJust $ find (\(RWCDataCon i' _) -> i==i') dcs
                                      return (subst sub targs)

-- must be nonempty
andRegs :: [Loc] -> CGM (Node,Node,Loc)
andRegs []     = fail "andRegs: empty list"
andRegs [r]    = do n <- addFreshNode (Rem "andRegsNop")
                    return (n,n,r)
andRegs (r:rs) = do (ni,no,ro) <- andRegs rs
                    ro'        <- freshLocSize 1
                    no'        <- addFreshNode (Assign ro' (FunCallRHS "andBits" [r,ro]))
                    addEdge no no' (Conditional (BoolConst True))
                    return (ni,no',ro')

zipWithM3 :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWithM3 f (x:xs) (y:ys) (z:zs) = do v    <- f x y z
                                      rest <- zipWithM3 f xs ys zs
                                      return (v:rest)
zipWithM3 _ _ _ _                = return []

agLastPat :: Loc -> RWCTy -> RWCPat -> CGM ([(Id RWCExp,Loc)],Node,Node,Loc) -- bindings, entry node, exit node, match bit loc
agLastPat lscr tscr (RWCPatCon dci ps) = do 
                                        ntm <- addFreshNode (Rem "final pat")
                                        let rtm =  "bogus"
                                        
                                        case ps of
                                          [] -> return ([],ntm,ntm,rtm)
                                          _  -> do
                                            tfs             <- getFieldTys dci tscr
                                            -- nfi: rfi <- field i
                                            rfs             <- mapM freshLocTy tfs
                                            nfs             <- zipWithM (\ r n -> addFreshNode (Assign r (FunCallRHS ("getField" ++ show n) [lscr]))) rfs [0..]
                                            stringNodes (map (\ n -> (n,n)) nfs)
                                            -- bsi: bindings for subpat i
                                            -- npi_i~>npo_i: entry, exit nodes for subpat i
                                            -- rmi: whether match for subpat i
                                            bsnpinporms       <- zipWithM3 agLastPat rfs tfs ps
                                            let bss           =  map (\ (bs,_,_,_) -> bs) bsnpinporms
                                                rms           =  map (\ (_,_,_,rm) -> rm) bsnpinporms
                                            -- nai,nao: entry/exit for final match-and
                                            -- rm: value for final match-and
                                            --(nai,nao,rm)      <- andRegs (rtm:rms)
                                            
                                            -- after tag match, fill fields
                                            addEdge ntm (head nfs) (Conditional (BoolConst True))
                                            -- after fill fields, do subpats
                                            let npinpos       =  map (\ (_,npi,npo,_) -> (npi,npo)) bsnpinporms
                                                (npi_f,_)     =  head npinpos
                                                (_,npo_l)     =  last npinpos
                                            addEdge (last nfs) npi_f (Conditional (BoolConst True))
                                            stringNodes npinpos
                                            -- after check pats, and results
--                                            addEdge npo_l nai (Conditional (BoolConst True))
                                            return (concat bss,ntm,npo_l,rtm)
agLastPat lscr tscr RWCPatWild         = do
                                        rtm <- freshLocSize 1
                                        ntm <- addFreshNode (Assign rtm (FunCallRHS "constOne" []))
                                        return ([],ntm,ntm,rtm)
agLastPat lscr tscr (RWCPatVar x _)    = do
                                        rtm <- freshLocSize 1
                                        ntm <- addFreshNode (Assign rtm (FunCallRHS "constOne" []))
                                        return ([(x,lscr)],ntm,ntm,rtm)
agLastPat _ _ (RWCPatLiteral _)        = fail "agPat: encountered literal"

agPat :: Loc -> RWCTy -> RWCPat -> CGM ([(Id RWCExp,Loc)],Node,Node,Loc) -- bindings, entry node, exit node, match bit loc
agPat lscr tscr (RWCPatCon dci ps) = do -- ntm: rtm <- tag match?
                                        rtm       <- freshLocSize 1
                                        ntm       <- addFreshNode (Assign rtm (FunCallRHS ("checkTag" ++ (deDataConId dci)) [lscr]))
                                        
                                        case ps of
                                          [] -> return ([],ntm,ntm,rtm)
                                          _  -> do
                                            tfs             <- getFieldTys dci tscr
                                            -- nfi: rfi <- field i
                                            rfs             <- mapM freshLocTy tfs
                                            nfs             <- zipWithM (\ r n -> addFreshNode (Assign r (FunCallRHS ("getField" ++ show n) [lscr]))) rfs [0..]
                                            stringNodes (map (\ n -> (n,n)) nfs)
                                            -- bsi: bindings for subpat i
                                            -- npi_i~>npo_i: entry, exit nodes for subpat i
                                            -- rmi: whether match for subpat i
                                            bsnpinporms       <- zipWithM3 agPat rfs tfs ps
                                            let bss           =  map (\ (bs,_,_,_) -> bs) bsnpinporms
                                                rms           =  map (\ (_,_,_,rm) -> rm) bsnpinporms
                                            -- nai,nao: entry/exit for final match-and
                                            -- rm: value for final match-and
                                            (nai,nao,rm)      <- andRegs (rtm:rms)
                                            
                                            -- after tag match, fill fields
                                            addEdge ntm (head nfs) (Conditional (BoolConst True))
                                            -- after fill fields, check pats
                                            let npinpos       =  map (\ (_,npi,npo,_) -> (npi,npo)) bsnpinporms
                                                (npi_f,_)     =  head npinpos
                                                (_,npo_l)     =  last npinpos
                                            addEdge (last nfs) npi_f (Conditional (BoolConst True))
                                            stringNodes npinpos
                                            -- after check pats, and results
                                            addEdge npo_l nai (Conditional (BoolConst True))
                                            return (concat bss,ntm,nao,rm)
agPat lscr tscr RWCPatWild         = do rtm <- freshLocSize 1
                                        ntm <- addFreshNode (Assign rtm (FunCallRHS "constOne" []))
                                        return ([],ntm,ntm,rtm)
agPat lscr tscr (RWCPatVar x _)    = do rtm <- freshLocSize 1
                                        ntm <- addFreshNode (Assign rtm (FunCallRHS "constOne" []))
                                        return ([(x,lscr)],ntm,ntm,rtm)
agPat _ _ (RWCPatLiteral _)        = fail "agPat: encountered literal"

agLastAlt :: Loc -> RWCTy -> Loc -> RWCAlt -> CGM (Node,Node,Node,Loc)
agLastAlt lscr tscr lres (RWCAlt p e) = do (bds,nip,nop,_) <- agLastPat lscr tscr p
                                           foldr (uncurry binding) (do
                                             (nie,noe,le) <- agExpr e
                                             no           <- addFreshNode (Assign lres (LocRHS le))
                                             addEdge noe no (Conditional (BoolConst True))
                                             addEdge nop nie (Conditional (BoolConst True))
                                             return (nip,no,no,le))
                                            bds

agAlt :: Loc -> RWCTy -> Loc -> RWCAlt -> CGM (Node,Node,Node,Loc) -- entry node, true exit node, false exit node, result reg if true
agAlt lscr tscr lres (RWCAlt p e) = do (bds,nip,nop,rp) <- agPat lscr tscr p
                                       foldr (uncurry binding) (do
                                         (nie,noe,le) <- agExpr e
                                         no_t         <- addFreshNode (Assign lres (LocRHS le))
                                         no_f         <- addFreshNode (Rem "alt exit (no match)")
                                         addEdge noe no_t (Conditional (BoolConst True))
                                         addEdge nop nie (Conditional (BoolVar rp))
                                         addEdge nop no_f (Conditional (Not (BoolVar rp)))
                                         return (nip,no_t,no_f,le))
                                        bds

agLastAcAlt :: Loc -> RWCTy -> Loc -> RWCAlt -> CGM (Node,Node,Node,Loc)
agLastAcAlt lscr tscr lres (RWCAlt p e) = do (bds,nip,nop,rp) <- agLastPat lscr tscr p
                                             foldr (uncurry binding) (do
                                               (nie,noe,le) <- agAcExpr e
                                               no           <- addFreshNode (Assign lres (LocRHS le))
                                               addEdge noe no (Conditional (BoolConst True))
                                               addEdge nop nie (Conditional (BoolConst True))
                                               return (nip,no,no,le))
                                              bds

agAcAlt :: Loc -> RWCTy -> Loc -> RWCAlt -> CGM (Node,Node,Node,Loc) -- entry node, true exit node, false exit node, result reg if true
agAcAlt lscr tscr lres (RWCAlt p e) = do (bds,nip,nop,rp) <- agPat lscr tscr p
                                         foldr (uncurry binding) (do
                                           (nie,noe,le) <- agAcExpr e
                                           no_t         <- addFreshNode (Assign lres (LocRHS le))
                                           no_f         <- addFreshNode (Rem "alt exit (no match)")
                                           addEdge noe no_t (Conditional (BoolConst True))
                                           addEdge nop nie (Conditional (BoolVar rp))
                                           addEdge nop no_f (Conditional (Not (BoolVar rp)))
                                           return (nip,no_t,no_f,le))
                                          bds

agAcExpr :: RWCExp -> CGM (Node,Node,Loc) -- entry node, exit node, result reg
agAcExpr e = case ef of
               RWCApp _ _                    -> fail "agAcExpr: app in function position (can't happen)"
               -- I suppose strictly speaking we *could* handle lambdas,
               -- but not for now.
               RWCLam _ _ _                  -> fail "agAcExpr: encountered lambda"
               RWCLiteral _                  -> fail "agAcExpr: encountered literal"
               -- Can't use data constructors to construct a resumption.
               RWCCon _ _                    -> fail "agAcExpr: encountered con"
               
               RWCLet x el eb -> do
                 -- Expression being bound.
                 (niel,noel,lel) <- agExpr el
                 -- Body expression.
                 (nieb,noeb,leb) <- binding x lel $ agAcExpr eb
                 -- Connect the two in sequence.
                 addEdge noel nieb (Conditional (BoolConst True))
                 
                 return (niel,noeb,leb)
                 
               RWCVar x _ | x == mkId "bind" -> do
                 -- Bind is only allowed with a lambda on RHS.
                 case eargs of
                   [el,RWCLam x _ er] -> do
                     -- Process is essentially identical to let.
                     (entl,exl,regl)  <- agAcExpr el
                     (entr,exr,regr)  <- binding x regl $ agAcExpr er
                     addEdge exl entr (Conditional (BoolConst True))
                     return (entl,exr,regr)
                   _ -> fail "wrong rhs for bind"
                   
               RWCVar x _ | x == mkId "return" -> do
                 case eargs of
                   [e] -> agExpr e
                   _   -> fail "agAcExpr: wrong number of arguments for return"
                   
               RWCVar x _ | x == mkId "signal" -> do
                 case eargs of
                   [e] -> do
                     -- First we compute the signal value.
                     (ni,no,re) <- agExpr e
                     -- Throw that in the output register.
                     npre       <- addFreshNode (Assign "output" (LocRHS re))
                     -- After the signal return, put the input into a fresh
                     -- register. (Is that necessary? Well, it shouldn't
                     -- hurt anything.)
                     ti         <- askInputTy
                     r          <- freshLocTy ti
                     npost      <- addFreshNode (Assign r (LocRHS "input"))
                     -- Chain everything together.
                     addEdge no npre (Conditional (BoolConst True))
                     addEdge npre npost Tick
                     return (ni,npost,r)
                   _  -> fail "agAcExpr: wrong number of arguments for signal"
               
               RWCVar x _ | x == mkId "lift" -> do
                 case eargs of
                   [e] -> localStateLayer (+1) (agAcExpr e)
                   _   -> fail "agAcExpr: wrong number of arguments for lift"

               RWCVar x _ | x == mkId "get" -> do
                 case eargs of
                   [] -> do l   <- askStateLayer
                            tss <- askStateTys
                            if l < 0
                              then fail "agAcExpr: not in a state monad (can't happen)"
                              else if l >= length tss
                                then fail $ "agAcExpr: current state layer " ++ show l ++ " exceeds number of state transformers (can't happen)"
                                else do
                                  tss <- askStateTys
                                  r   <- freshLocTy (tss!!l)
                                  n   <- addFreshNode (Assign r (LocRHS $ "statevar" ++ show l))
                                  return (n,n,r)
                   _  -> fail "agAcExpr: wrong number of arguments for get"

               RWCVar x _ | x == mkId "put" -> do
                 case eargs of
                   [e] -> do l <- askStateLayer
                             if l < 0
                               then fail "agAcExpr: not in a state monad (can't happen)"
                               else do
                                 (nie,noe,re) <- agExpr e
                                 no           <- addFreshNode (Assign ("statevar" ++ show l) (LocRHS re))
                                 r            <- freshLocSize 0
                                 addEdge noe no (Conditional (BoolConst True))
                                 return (nie,no,r)
                   _   -> fail "agAcExpr: wrong number of arguments for get"

               RWCVar x _                      -> do
                 -- This is required to be a tail call! Look up info for the
                 -- callee.
                 (rs_f,ni_f,no_f,rr_f) <- agAcDefn x
                 case eargs of
                   [] -> return (ni_f,no_f,rr_f)
                   _ -> do
                     -- Generate code for argument expressions.
                     ninor_args            <- mapM agExpr eargs
                     let no_args           =  map (\(_,no,_) -> no) ninor_args
                         ni_args           =  map (\(ni,_,_) -> ni) ninor_args
                     -- Sequence the code graphs for the argument expressions.
                     stringNodes (map (\(ni,no,_) -> (ni,no)) ninor_args)
                     -- Copy the values from the argument expressions' output
                     -- regs into the callee's input regs.
                     let r_args            =  map (\(_,_,r) -> r) ninor_args
                     n_copies              <- zipWithM (\ r_arg r_f -> addFreshNode (Assign r_f (LocRHS r_arg))) r_args rs_f
                     stringNodes (map (\ n -> (n,n)) n_copies)
                     -- Link everything up.
                     addEdge (last no_args) (head n_copies) (Conditional (BoolConst True))
                     addEdge (last n_copies) ni_f (Conditional (BoolConst True))
                     return (head ni_args,no_f,rr_f)
                 
               RWCCase escr alts               -> do
                 case eargs of
                   [] -> do
                     -- Compile scrutinee expression.
                     (ni_scr,no_scr,r_scr) <- agExpr escr
                     -- Pre-allocate result register (it's up to agAcAlt to
                     -- copy the result to this).
                     r_res                 <- freshLocTy (compBase $ typeOf e)
                     -- Landing node.
                     nl                    <- addFreshNode (Rem "end case")
                     -- Compile each alt *except* the last. (Note that we're
                     -- assuming there is at least one; a case with zero alts
                     -- is always undefined anyway.)
                     ninotnoers_init       <- mapM (agAcAlt r_scr (typeOf escr) r_res) (init alts)
                     -- Special treatment for the last alt.
                     ninotnoers_last       <- agLastAcAlt r_scr (typeOf escr) r_res (last alts)
                     let ninotnoers        =  ninotnoers_init ++ [ninotnoers_last]
                     -- Find entry point node for first alt, and jump to it
                     -- after the scrutinee is evaluated.
                     let (ni0,_,_,_)       =  head ninotnoers
                     addEdge no_scr ni0 (Conditional (BoolConst True))
                     -- This will link the alts together in sequence, and
                     -- connect them all the the landing node.
                     stringAlts nl ninotnoers
                     -- Start point is scr, end point is landing, result is
                     -- in r_res.
                     return (ni_scr,nl,r_res)
                   _  -> fail "agAcExpr: encountered case expression in function position"
                     
   where (ef:eargs) = flattenApp e  

peelLambdas (RWCLam n t e) = ((n,t):nts,e')
                             where (nts,e') = peelLambdas e
peelLambdas e              = ([],e)

-- Generate code for an action function.
agAcDefn :: Id RWCExp -> CGM ([Loc],Node,Node,Loc)
agAcDefn n = do am <- getActionMap
                case Map.lookup n am of
                  -- If the name is already in the map, codegen for the
                  -- function is already completed or in progress.
                  Just x  -> return x
                  Nothing -> do 
                    md <- lift $ lift $ queryG n
                    case md of
                      Nothing               -> fail $ "agAcDefn: " ++ show n ++ " not defined"
                      Just (RWCDefn _ _ e_) -> do
                        -- Allocate registers for arguments.
                        let (xts,e)   =  peelLambdas e_
                            xs        =  map fst xts
                            ts        =  map snd xts
                        rs            <- mapM freshLocTy ts
                        -- Allocate result register.
                        rr            <- freshLocTy (compBase $ typeOf e)
                        -- Allocate in and out nodes (no-ops). We cannot
                        -- just use the in and out nodes for the body
                        -- expression, because we don't know yet what those
                        -- are. So we will hook this up later.
                        ni            <- addFreshNode (Rem $ show n ++ " in")
                        no            <- addFreshNode (Rem $ show n ++ " out")
                        -- Insert the function info into the action map.
                        putActionMap (Map.insert n (rs,ni,no,rr) am)
                        -- Compile the body expression, with local bindings
                        -- in place.
                        let xrs       =  zip xs rs
                        (nie,noe,rre) <- foldr (uncurry binding) (agAcExpr e) xrs
                        -- Connect in and out nodes.
                        addEdge ni nie (Conditional (BoolConst True))
                        addEdge noe no (Conditional (BoolConst True))
                        -- Return function info (identical to what we
                        -- inserted into the map before).
                        return (rs,ni,no,rr)

agStart :: RWCExp -> CGM ()
agStart (RWCApp (RWCApp (RWCVar x _) e) _) | x == mkId "extrude" = agStart e
agStart (RWCVar x t) = local buildEnv $ do
                         n_start    <- addFreshNode (Rem "START")
                         (_,ni,_,_) <- agAcDefn x
                         addEdge n_start ni (Conditional (BoolConst True))
 where buildEnv env =
         case t of
           RWCTyComp (RWCTyApp (RWCTyApp (RWCTyApp (RWCTyCon (TyConId "ReT")) ti) to) tsm) _ ->
             let
               getStateTys (RWCTyApp (RWCTyApp (RWCTyCon (TyConId "StT")) tst) ts) = tst : getStateTys ts
               getStateTys (RWCTyCon (TyConId "I"))                                = []
               getStateTys _                                                       = error "agStart: start has malformed type (inner monad stack is not of form (StT (StT ... (StT I))))"
               tss                                                                 = getStateTys tsm
             in env { inputTy  = ti,
                      outputTy = to,
                      stateTys = tss }
           _ -> error "agStart: start has malformed type (not a computation with outer monad ReT)"
agStart _ = fail "agStart: malformed start expression"

agProg :: CGM ()
agProg = do md <- lift $ lift $ queryG (mkId "start")
            case md of
              Nothing              -> fail "agProg: `start' not defined"
              Just (RWCDefn _ _ e) -> agStart e

agFromRW :: RWCProg -> (RegMap,CFG)
agFromRW p_ = fst $ runRW ctr p (runStateT (runReaderT doit env0) s0)
  where doit    = do agProg
                     rm <- getRegMap
                     g  <- getGraph
                     return (rm,CFG { cfgHeader = (), cfgGraph = g })
        env0    = Env { stateLayer = -1, inputTy = error "input type not set", outputTy = error "output type not set", stateTys = [], varMap = Map.empty }
        s0      = (0,[],CFG { cfgHeader = (), cfgGraph = empty },Map.empty)
        (p,ctr) = uniquify 0 p_

cmdToAG :: TransCommand
cmdToAG _ p = (Nothing,Just (mkDot $ gather $ linearize $ snd $ agFromRW p))
