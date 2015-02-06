{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Transformations.ToPreHDL where

import ReWire.PreHDL.Syntax
import ReWire.PreHDL.CFG
import ReWire.PreHDL.GotoElim
import ReWire.PreHDL.ElimEmpty
import ReWire.PreHDL.ToVHDL
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
import Data.List (foldl',find,findIndex)
import Data.Maybe (fromJust)

type VarMap = Map (Id RWCExp) Loc
type ActionMap = Map (Id RWCExp) ([Loc],Node,Node,Loc) -- name -> arg regs, entry node, exit node, result reg
type FunMap = Map (Id RWCExp) String                   -- name -> generated VHDL name
type CGM = ReaderT Env (StateT (Int,CFG,ActionMap,FunMap) RW)

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
putC c = do (_,g,am,fm) <- get
            put (c,g,am,fm)
  
getHeader :: CGM Header
getHeader = do (_,cfg,_,_) <- get
               return (cfgHeader cfg)

putHeader :: Header -> CGM ()
putHeader h = do (c,cfg,am,fm) <- get
                 put (c,cfg { cfgHeader = h },am,fm)
  
getGraph :: CGM (Gr Cmd Branch)
getGraph = do (_,cfg,_,_) <- get
              return (cfgGraph cfg)

putGraph :: Gr Cmd Branch -> CGM ()
putGraph g = do (c,cfg,am,fm) <- get
                put (c,cfg { cfgGraph = g },am,fm)

getActionMap :: CGM ActionMap
getActionMap = do (_,_,am,_) <- get
                  return am

putActionMap :: ActionMap -> CGM ()
putActionMap am = do (c,g,_,fm) <- get
                     put (c,g,am,fm)

getFunMap :: CGM FunMap
getFunMap = do (_,_,_,fm) <- get
               return fm

putFunMap :: FunMap -> CGM ()
putFunMap fm = do (c,g,am,_) <- get
                  put (c,g,am,fm)

binding :: Id RWCExp -> Loc -> CGM a -> CGM a
binding n l = localVarMap (Map.insert n l)

askBinding :: Id RWCExp -> CGM (Maybe Loc)
askBinding n = do varm <- askVarMap
                  return (Map.lookup n varm)

askFun :: Id RWCExp -> CGM (Maybe String)
askFun n = do funm <- getFunMap
              return (Map.lookup n funm)
              
addEdge :: Node -> Node -> Branch -> CGM ()
addEdge ns nd br = do g <- getGraph
                      putGraph (insEdge (ns,nd,br) g)

freshFunName :: Id RWCExp -> CGM String
freshFunName n = do c <- getC
                    putC (c+1)
                    return ("rewire_" ++ show n ++ "_" ++ show c)
  
freshLocSize :: Int -> CGM Loc
freshLocSize 0 = return "EMPTY"
freshLocSize n = do c  <- getC
                    putC (c+1)
                    let r = "r" ++ show c
                    h  <- getHeader
                    putHeader (h { regDecls = RegDecl r (TyBits n) : regDecls h })
                    return r

freshLocTy :: RWCTy -> CGM Loc
freshLocTy t = do n <- tyWidth t
                  freshLocSize n

freshLocBool :: CGM Loc
freshLocBool = do c <- getC
                  putC (c+1)
                  let r = "b" ++ show c
                  h <- getHeader
                  putHeader (h { regDecls = RegDecl r TyBoolean : regDecls h })
                  return r
  
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
                                RWCTyApp _ _  -> fail "tyWidth: encountered TyApp in func position (can't happen)"
                                RWCTyVar _    -> fail $ "tyWidth: type variable encountered"
                                RWCTyComp _ _ -> fail $ "tyWidth: computation type encountered"
                                RWCTyCon i    -> do
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

cfgExpr :: RWCExp -> CGM (Node,Node,Loc)
cfgExpr e = case ef of
             RWCApp _ _     -> fail "cfgExpr: app in function position (can't happen)"
             RWCLiteral _   -> fail "cfgExpr: encountered literal"
             RWCLam _ _ _   -> fail "cfgExpr: encountered lambda"
             RWCLet x el eb -> do
               (niel,noel,lel) <- cfgExpr el
               (nieb,noeb,leb) <- binding x lel $ cfgExpr eb
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
                   nf     <- funDefn x
                   ninors <- mapM cfgExpr eargs
                   stringNodes (map (\(ni,no,_) -> (ni,no)) ninors)
                   let rs =  map ( \ (_,_,r) -> r) ninors
                   r      <- freshLocTy (typeOf e)
                   n      <- addFreshNode (Assign r (FunCallRHS nf rs))
                   case ninors of
                     [] -> return (n,n,r)
                     _  -> do let (ni,_,_) = head ninors
                                  (_,no,_) = last ninors
                              addEdge no n (Conditional (BoolConst True))
                              return (ni,n,r)
             RWCCon dci _ -> do
               -- Tag.
               t            <- getTag dci
               rt           <- freshLocSize (length t)
               nt           <- addFreshNode (Assign rt (ConstRHS t))
               -- Allocate result reg.
               r            <- freshLocTy (typeOf e)
               res_size     <- tyWidth (typeOf e)
               -- Padding.
               let targs    =  map typeOf eargs
               args_size    <- liftM sum (mapM tyWidth targs)
               let pad_size =  res_size - args_size - length t
               r_pad        <- freshLocSize pad_size
               n_pad        <- addFreshNode (Assign r_pad (ConstRHS (replicate pad_size Zero)))
               addEdge nt n_pad (Conditional (BoolConst True))
               case eargs of
                 [] -> -- No arguments, so tag and padding is all we need.
                       do n_fill <- addFreshNode (Assign r (ConcatRHS [rt,r_pad]))
                          addEdge n_pad n_fill (Conditional (BoolConst True))
                          return (nt,n_fill,r)
                 _  -> do -- Compile argument expressions.
                          ninors_args <- mapM cfgExpr eargs
                          -- Sequence tag and argument expressions.
                          stringNodes ((n_pad,n_pad) : map (\(ni,no,_) -> (ni,no)) ninors_args)
                          -- Concatenate tag, args, and padding, result in r.
                          let rs_args  =  map ( \ (_,_,r) -> r) ninors_args
                          n            <- addFreshNode (Assign r (ConcatRHS ([rt]++rs_args++[r_pad])))
                          -- Sequence argument expressions with ctor call.
                          let (_,no,_) = last ninors_args
                          addEdge no n (Conditional (BoolConst True))
                          -- Entry is the beginning of the argument
                          -- expressions, exit is the constructor call.
                          return (nt,n,r)
             RWCCase escr alts               -> do
               case eargs of
                 [] -> do
                   (ni,no,r_scr)   <- cfgExpr escr
                   r_res           <- freshLocTy (typeOf e)
                   ninotnoers_init <- mapM (cfgAlt r_scr (typeOf escr) r_res) (init alts)
                   ninotnoers_last <- cfgLastAlt r_scr (typeOf escr) r_res (last alts)
                   let ninotnoers           =  ninotnoers_init ++ [ninotnoers_last]
                       (ni0,_,_,_)          =  head ninotnoers
                   addEdge no ni0 (Conditional (BoolConst True))
                   nl              <- addFreshNode (Rem "end case")
                   stringAlts nl ninotnoers
                   return (ni,nl,r_res)
                 _  -> fail "cfgExpr: encountered case expression in function position"
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

{-
-- must be nonempty
andRegs :: [Loc] -> CGM (Node,Node,Loc)
andRegs []     = fail "andRegs: empty list"
andRegs [r]    = do n <- addFreshNode (Rem "andRegsNop")
                    return (n,n,r)
andRegs (r:rs) = do (ni,no,ro) <- andRegs rs
                    ro'        <- freshLocSize 1
                    no'        <- addFreshNode (Assign ro' (BoolRHS (And (BoolVar r) (BoolVar ro))))
                    addEdge no no' (Conditional (BoolConst True))
                    return (ni,no',ro')
-}

andRegs :: [Loc] -> CGM (Node,Node,Loc)
andRegs [] = do ro <- freshLocBool
                n  <- addFreshNode (Assign ro (BoolRHS (BoolConst True)))
                return (n,n,ro)
andRegs rs = do ro <- freshLocBool
                n  <- addFreshNode (Assign ro (BoolRHS (foldr1 And (map BoolVar rs))))
                return (n,n,ro)

zipWithM3 :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWithM3 f (x:xs) (y:ys) (z:zs) = do v    <- f x y z
                                      rest <- zipWithM3 f xs ys zs
                                      return (v:rest)
zipWithM3 _ _ _ _                = return []

cfgLastPat :: Loc -> RWCTy -> RWCPat -> CGM ([(Id RWCExp,Loc)],Node,Node,Loc) -- bindings, entry node, exit node, match bit loc
cfgLastPat lscr tscr (RWCPatCon dci ps) = do 
                                        ntm <- addFreshNode (Rem "final pat")
                                        let rtm =  "bogus"
                                        
                                        case ps of
                                          [] -> return ([],ntm,ntm,rtm)
                                          _  -> do
                                            tfs             <- getFieldTys dci tscr
                                            -- nfi: rfi <- field i
                                            (rfs,nfs)       <- liftM unzip $ mapM (mkGetField dci lscr tscr) [0..(length tfs - 1)]
                                            stringNodes (map (\ n -> (n,n)) nfs)
                                            -- bsi: bindings for subpat i
                                            -- npi_i~>npo_i: entry, exit nodes for subpat i
                                            -- rmi: whether match for subpat i
                                            bsnpinporms       <- zipWithM3 cfgLastPat rfs tfs ps
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
cfgLastPat lscr tscr RWCPatWild         = do
                                        rtm <- freshLocBool
                                        ntm <- addFreshNode (Assign rtm (BoolRHS (BoolConst True)))
                                        return ([],ntm,ntm,rtm)
cfgLastPat lscr tscr (RWCPatVar x _)    = do
                                        rtm <- freshLocBool
                                        ntm <- addFreshNode (Assign rtm (BoolRHS (BoolConst True)))
                                        return ([(x,lscr)],ntm,ntm,rtm)
cfgLastPat _ _ (RWCPatLiteral _)        = fail "cfgPat: encountered literal"

getDataConTyCon :: DataConId -> CGM TyConId
getDataConTyCon dci = do Just (DataConInfo n _) <- lift $ lift $ queryD dci
                         return n

bitConstFromIntegral :: Integral a => Int -> a -> [Bit]
bitConstFromIntegral 0 _     = []
bitConstFromIntegral width n = bitConstFromIntegral (width-1) (n`div`2) ++ thisBit where thisBit = if odd n then [One] else [Zero]

getTag :: DataConId -> CGM [Bit]
getTag i = do tci                 <- getDataConTyCon i
              Just (TyConInfo dd) <- lift $ lift $ queryT tci 
              tagWidth            <- getTagWidth tci
              case findIndex (\ (RWCDataCon i' _) -> i==i') (dataCons dd) of
                Just pos -> return (bitConstFromIntegral tagWidth pos)
                Nothing  -> fail $ "getTag: unknown constructor " ++ deDataConId i

mkTagCheck :: DataConId -> Loc -> CGM (Node,Loc)
mkTagCheck dci lscr = do rtm  <- freshLocBool
                         tci  <- getDataConTyCon dci
                         tagw <- getTagWidth tci
                         case tagw of
                           0 -> do ntm <- addFreshNode (Assign rtm (BoolRHS (BoolConst True)))
                                   return (ntm,rtm)
                           _ -> do tagv <- liftM ConstRHS (getTag dci)
                                   ntm  <- addFreshNode (Assign rtm (BoolRHS (BoolEq tagv (SliceRHS 0 (tagw-1) lscr))))
                                   return (ntm,rtm)

mkGetField :: DataConId -> Loc -> RWCTy -> Int -> CGM (Loc,Node)
mkGetField dci lscr tscr n = do tci         <- getDataConTyCon dci
                                tagWidth    <- getTagWidth tci
                                fieldTys    <- getFieldTys dci tscr
                                fieldWidths <- mapM tyWidth fieldTys
                                rf          <- freshLocSize (fieldWidths !! n)
                                let fieldOffsets    = scanl (+) tagWidth fieldWidths
                                    ranges []       = []
                                    ranges [n]      = []
                                    ranges (n:m:ns) = (n,m-1) : ranges (m:ns)
                                    fieldRanges     = ranges fieldOffsets
                                    (lo,hi)         = fieldRanges !! n
                                nf          <- addFreshNode (Assign rf (SliceRHS lo hi lscr))
                                return (rf,nf)
  
cfgPat :: Loc -> RWCTy -> RWCPat -> CGM ([(Id RWCExp,Loc)],Node,Node,Loc) -- bindings, entry node, exit node, match bit loc
cfgPat lscr tscr (RWCPatCon dci ps) = do -- ntm: rtm <- tag match?
                                        (ntm,rtm) <- mkTagCheck dci lscr
                                        
                                        case ps of
                                          [] -> return ([],ntm,ntm,rtm)
                                          _  -> do
                                            -- nfi: rfi <- field i
                                            tfs             <- getFieldTys dci tscr
                                            (rfs,nfs)       <- liftM unzip $ mapM (mkGetField dci lscr tscr) [0..(length tfs - 1)]
                                            stringNodes (map (\ n -> (n,n)) nfs)
                                            -- bsi: bindings for subpat i
                                            -- npi_i~>npo_i: entry, exit nodes for subpat i
                                            -- rmi: whether match for subpat i
                                            bsnpinporms       <- zipWithM3 cfgPat rfs tfs ps
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
cfgPat lscr tscr RWCPatWild         = do rtm <- freshLocBool
                                         ntm <- addFreshNode (Assign rtm (BoolRHS (BoolConst True)))
                                         return ([],ntm,ntm,rtm)
cfgPat lscr tscr (RWCPatVar x _)    = do rtm <- freshLocBool
                                         ntm <- addFreshNode (Assign rtm (BoolRHS (BoolConst True)))
                                         return ([(x,lscr)],ntm,ntm,rtm)
cfgPat _ _ (RWCPatLiteral _)        = fail "cfgPat: encountered literal"

cfgLastAlt :: Loc -> RWCTy -> Loc -> RWCAlt -> CGM (Node,Node,Node,Loc)
cfgLastAlt lscr tscr lres (RWCAlt p e) = do (bds,nip,nop,_) <- cfgLastPat lscr tscr p
                                            foldr (uncurry binding) (do
                                              (nie,noe,le) <- cfgExpr e
                                              no           <- addFreshNode (Assign lres (LocRHS le))
                                              addEdge noe no (Conditional (BoolConst True))
                                              addEdge nop nie (Conditional (BoolConst True))
                                              return (nip,no,no,le))
                                             bds

cfgAlt :: Loc -> RWCTy -> Loc -> RWCAlt -> CGM (Node,Node,Node,Loc) -- entry node, true exit node, false exit node, result reg if true
cfgAlt lscr tscr lres (RWCAlt p e) = do (bds,nip,nop,rp) <- cfgPat lscr tscr p
                                        foldr (uncurry binding) (do
                                          (nie,noe,le) <- cfgExpr e
                                          no_t         <- addFreshNode (Assign lres (LocRHS le))
                                          no_f         <- addFreshNode (Rem "alt exit (no match)")
                                          addEdge noe no_t (Conditional (BoolConst True))
                                          addEdge nop nie (Conditional (BoolVar rp))
                                          addEdge nop no_f (Conditional (Not (BoolVar rp)))
                                          return (nip,no_t,no_f,le))
                                         bds

cfgLastAcAlt :: Loc -> RWCTy -> Loc -> RWCAlt -> CGM (Node,Node,Node,Loc)
cfgLastAcAlt lscr tscr lres (RWCAlt p e) = do (bds,nip,nop,rp) <- cfgLastPat lscr tscr p
                                              foldr (uncurry binding) (do
                                                (nie,noe,le) <- cfgAcExpr e
                                                no           <- addFreshNode (Assign lres (LocRHS le))
                                                addEdge noe no (Conditional (BoolConst True))
                                                addEdge nop nie (Conditional (BoolConst True))
                                                return (nip,no,no,le))
                                               bds

cfgAcAlt :: Loc -> RWCTy -> Loc -> RWCAlt -> CGM (Node,Node,Node,Loc) -- entry node, true exit node, false exit node, result reg if true
cfgAcAlt lscr tscr lres (RWCAlt p e) = do (bds,nip,nop,rp) <- cfgPat lscr tscr p
                                          foldr (uncurry binding) (do
                                            (nie,noe,le) <- cfgAcExpr e
                                            no_t         <- addFreshNode (Assign lres (LocRHS le))
                                            no_f         <- addFreshNode (Rem "alt exit (no match)")
                                            addEdge noe no_t (Conditional (BoolConst True))
                                            addEdge nop nie (Conditional (BoolVar rp))
                                            addEdge nop no_f (Conditional (Not (BoolVar rp)))
                                            return (nip,no_t,no_f,le))
                                           bds

cfgAcExpr :: RWCExp -> CGM (Node,Node,Loc) -- entry node, exit node, result reg
cfgAcExpr e = case ef of
               RWCApp _ _                    -> fail "cfgAcExpr: app in function position (can't happen)"
               -- I suppose strictly speaking we *could* handle lambdas,
               -- but not for now.
               RWCLam _ _ _                  -> fail "cfgAcExpr: encountered lambda"
               RWCLiteral _                  -> fail "cfgAcExpr: encountered literal"
               -- Can't use data constructors to construct a resumption.
               RWCCon _ _                    -> fail "cfgAcExpr: encountered con"
               
               RWCLet x el eb -> do
                 -- Expression being bound.
                 (niel,noel,lel) <- cfgExpr el
                 -- Body expression.
                 (nieb,noeb,leb) <- binding x lel $ cfgAcExpr eb
                 -- Connect the two in sequence.
                 addEdge noel nieb (Conditional (BoolConst True))
                 
                 return (niel,noeb,leb)
                 
               RWCVar x _ | x == mkId "bind" -> do
                 -- Bind is only allowed with a lambda on RHS.
                 case eargs of
                   [el,RWCLam x _ er] -> do
                     -- Process is essentially identical to let.
                     (entl,exl,regl)  <- cfgAcExpr el
                     (entr,exr,regr)  <- binding x regl $ cfgAcExpr er
                     addEdge exl entr (Conditional (BoolConst True))
                     return (entl,exr,regr)
                   _ -> fail "wrong rhs for bind"
                   
               RWCVar x _ | x == mkId "return" -> do
                 case eargs of
                   [e] -> cfgExpr e
                   _   -> fail "cfgAcExpr: wrong number of arguments for return"
                   
               RWCVar x _ | x == mkId "signal" -> do
                 case eargs of
                   [e] -> do
                     -- First we compute the signal value.
                     (ni,no,re) <- cfgExpr e
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
                   _  -> fail "cfgAcExpr: wrong number of arguments for signal"
               
               RWCVar x _ | x == mkId "lift" -> do
                 case eargs of
                   [e] -> localStateLayer (+1) (cfgAcExpr e)
                   _   -> fail "cfgAcExpr: wrong number of arguments for lift"

               RWCVar x _ | x == mkId "get" -> do
                 case eargs of
                   [] -> do l   <- askStateLayer
                            tss <- askStateTys
                            if l < 0
                              then fail "cfgAcExpr: not in a state monad (can't happen)"
                              else if l >= length tss
                                then fail $ "cfgAcExpr: current state layer " ++ show l ++ " exceeds number of state transformers (can't happen)"
                                else do
                                  tss <- askStateTys
                                  r   <- freshLocTy (tss!!l)
                                  n   <- addFreshNode (Assign r (LocRHS $ "statevar" ++ show l))
                                  return (n,n,r)
                   _  -> fail "cfgAcExpr: wrong number of arguments for get"

               RWCVar x _ | x == mkId "put" -> do
                 case eargs of
                   [e] -> do l <- askStateLayer
                             if l < 0
                               then fail "cfgAcExpr: not in a state monad (can't happen)"
                               else do
                                 (nie,noe,re) <- cfgExpr e
                                 no           <- addFreshNode (Assign ("statevar" ++ show l) (LocRHS re))
                                 r            <- freshLocSize 0
                                 addEdge noe no (Conditional (BoolConst True))
                                 return (nie,no,r)
                   _   -> fail "cfgAcExpr: wrong number of arguments for get"

               RWCVar x _                      -> do
                 -- This is required to be a tail call! Look up info for the
                 -- callee.
                 (rs_f,ni_f,no_f,rr_f) <- cfgAcDefn x
                 case eargs of
                   [] -> return (ni_f,no_f,rr_f)
                   _ -> do
                     -- Generate code for argument expressions.
                     ninor_args            <- mapM cfgExpr eargs
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
                     (ni_scr,no_scr,r_scr) <- cfgExpr escr
                     -- Pre-allocate result register (it's up to cfgAcAlt to
                     -- copy the result to this).
                     r_res                 <- freshLocTy (compBase $ typeOf e)
                     -- Landing node.
                     nl                    <- addFreshNode (Rem "end case")
                     -- Compile each alt *except* the last. (Note that we're
                     -- assuming there is at least one; a case with zero alts
                     -- is always undefined anyway.)
                     ninotnoers_init       <- mapM (cfgAcAlt r_scr (typeOf escr) r_res) (init alts)
                     -- Special treatment for the last alt.
                     ninotnoers_last       <- cfgLastAcAlt r_scr (typeOf escr) r_res (last alts)
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
                   _  -> fail "cfgAcExpr: encountered case expression in function position"
                     
   where (ef:eargs) = flattenApp e  

peelLambdas (RWCLam n t e) = ((n,t):nts,e')
                             where (nts,e') = peelLambdas e
peelLambdas e              = ([],e)

-- Generate code for an action function.
cfgAcDefn :: Id RWCExp -> CGM ([Loc],Node,Node,Loc)
cfgAcDefn n = do
                am <- getActionMap
                case Map.lookup n am of
                  -- If the name is already in the map, codegen for the
                  -- function is already completed or in progress.
                  Just x  -> return x
                  Nothing -> do 
                    md <- lift $ lift $ queryG n
                    case md of
                      Nothing               -> fail $ "cfgAcDefn: " ++ show n ++ " not defined"
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
                        (nie,noe,rre) <- foldr (uncurry binding) (cfgAcExpr e) xrs
                        -- Connect in and out nodes.
                        addEdge ni nie (Conditional (BoolConst True))
                        addEdge noe no (Conditional (BoolConst True))
                        -- Return function info (identical to what we
                        -- inserted into the map before).
                        return (rs,ni,no,rr)

mkStateRegDecls :: CGM ()
mkStateRegDecls = do ts <- askStateTys
                     ws <- mapM tyWidth ts
                     let rs =  zipWith (\ w n -> RegDecl { regDeclName = "statevar" ++ show n, regDefnTy = TyBits w }) ws [0..]
                     h      <- getHeader
                     putHeader (h { regDecls = rs ++ regDecls h })

cfgStart :: RWCExp -> CGM ()
cfgStart (RWCApp (RWCApp (RWCVar x _) e) _) | x == mkId "extrude" = cfgStart e -- FIXME: fill in state expression!
cfgStart (RWCVar x t) = local buildEnv $ do
                         si  <- tyWidth ti
                         so  <- tyWidth to
                         h   <- getHeader
                         putHeader (h { inputSize  = si,
                                        outputSize = so })
                         mkStateRegDecls
                         n_start    <- addFreshNode (Rem "START")
                         (_,ni,_,_) <- cfgAcDefn x
                         addEdge n_start ni (Conditional (BoolConst True))
 where buildEnv env = env { inputTy  = ti,
                            outputTy = to,
                            stateTys = tss }
       (ti,to,tss) =  
         case t of
           RWCTyComp (RWCTyApp (RWCTyApp (RWCTyApp (RWCTyCon (TyConId "ReT")) ti) to) tsm) _ ->
             let
               getStateTys (RWCTyApp (RWCTyApp (RWCTyCon (TyConId "StT")) tst) ts) = tst : getStateTys ts
               getStateTys (RWCTyCon (TyConId "I"))                                = []
               getStateTys _                                                       = error "cfgStart: start has malformed type (inner monad stack is not of form (StT (StT ... (StT I))))"
               tss                                                                 = getStateTys tsm
             in (ti,to,tss)
           _ -> error "cfgStart: start has malformed type (not a computation with outer monad ReT)"
cfgStart _ = fail "cfgStart: malformed start expression"

cfgProg :: CGM ()
cfgProg = do md <- lift $ lift $ queryG (mkId "start")
             case md of
              Nothing              -> fail "cfgProg: `start' not defined"
              Just (RWCDefn _ _ e) -> cfgStart e

cfgFromRW :: RWCProg -> CFG
cfgFromRW p_ = fst $ runRW ctr p (runStateT (runReaderT doit env0) s0)
  where doit    = do cfgProg
                     h <- getHeader
                     g <- getGraph
                     return (CFG { cfgHeader = h, cfgGraph = g })
        env0    = Env { stateLayer = -1,
                        inputTy = error "input type not set",
                        outputTy = error "output type not set",
                        stateTys = [],
                        varMap = Map.empty }
        s0      = (0,CFG { cfgHeader = Header { funDefns   = [],
                                                regDecls   = [],
                                                stateNames = [],
                                                startState = "", 
                                                inputSize  = 999,
                                                outputSize = 999 },
                           cfgGraph = empty },
                     Map.empty,
                     Map.empty)
        (p,ctr) = uniquify 0 p_

eu gr = gr { cfgGraph = elimUnreachable 0 (cfgGraph gr) }

cmdToSCFG :: TransCommand
cmdToSCFG _ p = (Nothing,Just (mkDot $ gather $ eu $ cfgFromRW p))

cmdToCFG :: TransCommand
cmdToCFG _ p = (Nothing,Just (mkDot $ gather $ linearize $ cfgFromRW p))

cmdToPreG :: TransCommand
cmdToPreG _ p = (Nothing,Just (show (cfgToProg (cfgFromRW p))))

cmdToPre :: TransCommand
cmdToPre _ p = (Nothing,Just (show (gotoElim $ cfgToProg (cfgFromRW p))))

cmdToVHDL :: TransCommand
cmdToVHDL _ p = (Nothing,Just (toVHDL (elimEmpty $ gotoElim $ cfgToProg (cfgFromRW p))))

mkFunTagCheck :: DataConId -> Loc -> CGM (Cmd,Loc)
mkFunTagCheck dci lscr = do rtm  <- freshLocBool
                            tci  <- getDataConTyCon dci
                            tagw <- getTagWidth tci
                            case tagw of
                              0 -> return (Assign rtm (BoolRHS (BoolConst True)),rtm)
                              _ -> do tagv <- liftM ConstRHS (getTag dci)
                                      return (Assign rtm (BoolRHS (BoolEq tagv (SliceRHS 0 (tagw-1) lscr))),rtm)

mkFunGetField :: DataConId -> Loc -> RWCTy -> Int -> CGM (Loc,Cmd)
mkFunGetField dci lscr tscr n = do tci         <- getDataConTyCon dci
                                   tagWidth    <- getTagWidth tci
                                   fieldTys    <- getFieldTys dci tscr
                                   fieldWidths <- mapM tyWidth fieldTys
                                   rf          <- freshLocSize (fieldWidths !! n)
                                   let fieldOffsets    = scanl (+) tagWidth fieldWidths
                                       ranges []       = []
                                       ranges [n]      = []
                                       ranges (n:m:ns) = (n,m-1) : ranges (m:ns)
                                       fieldRanges     = ranges fieldOffsets
                                       (lo,hi)         = fieldRanges !! n
                                   return (rf,Assign rf (SliceRHS lo hi lscr))

funPat :: Loc -> RWCTy -> RWCPat -> CGM ([(Id RWCExp,Loc)],Cmd,Loc)
funPat lscr tscr (RWCPatCon dci ps) = do (ctm,rtm) <- mkFunTagCheck dci lscr
                                         case ps of
                                           [] -> return ([],ctm,rtm)
                                           _  -> do tfs       <- getFieldTys dci tscr
                                                    (rfs,cfs) <- liftM unzip $ mapM (mkFunGetField dci lscr tscr) [0..(length tfs - 1)]
                                                    bdscsrms  <- zipWithM3 funPat rfs tfs ps
                                                    let bdss  =  map (\ (bds,_,_) -> bds) bdscsrms
                                                        cs    =  map (\ (_,c,_) -> c) bdscsrms
                                                        rms   =  map (\ (_,_,rm) -> rm) bdscsrms
                                                    rm        <- freshLocBool
                                                    return (concat bdss,
                                                            foldr1 mkSeq ([ctm] ++ cfs ++ cs ++ [Assign rm (BoolRHS (foldr1 And (map BoolVar rms)))]),
                                                            rm)
funPat lscr tscr RWCPatWild         = do rm <- freshLocBool
                                         return ([],Assign rm (BoolRHS (BoolConst True)),rm)
funPat lscr tscr (RWCPatVar x _)    = do rm <- freshLocBool
                                         return ([(x,lscr)],Assign rm (BoolRHS (BoolConst True)),rm)
funPat _ _ (RWCPatLiteral _)        = fail "funPat: encountered literal"
                                         
funAlt :: Loc -> RWCTy -> Loc -> RWCAlt -> CGM Cmd
funAlt lscr tscr lres (RWCAlt p e) = do (bds,cmatch,rmatch) <- funPat lscr tscr p
                                        foldr (uncurry binding) (do
                                          (ce,le) <- funExpr e
                                          return (cmatch `mkSeq` If (BoolVar rmatch) (ce `mkSeq` Assign lres (LocRHS le))))
                                         bds

{-
funLastAlt :: Loc -> RWCTy -> Loc -> RWCAlt -> CGM Cmd
funLastAlt lscr tscr lres (RWCAlt p e) = do (bds,cmatch,rmatch) <- funPat lscr tscr p
                                            foldr (uncurry binding) (do
                                              (ce,le) <- funExpr e
                                              return (cmatch `mkSeq` ce `mkSeq` Assign lres (LocRHS le)))
                                             bds
-}

funExpr :: RWCExp -> CGM (Cmd,Loc)
funExpr e = case ef of
             RWCApp _ _     -> fail "cfgExpr: app in function position (can't happen)"
             RWCLiteral _   -> fail "cfgExpr: encountered literal"
             RWCLam _ _ _   -> fail "cfgExpr: encountered lambda"
             RWCLet x el eb -> do
               (cel,lel) <- funExpr el
               (ceb,leb) <- binding x lel $ funExpr eb
               return (cel `mkSeq` ceb,leb)
             RWCVar x _     -> do
               mr <- askBinding x
               case mr of
                 Just r  -> return (Skip,r)
                 Nothing -> do
                   -- If this is not a locally bound variable then it must
                   -- be a non-monadic global, which tranlates to a FunCall.
                   nf          <- funDefn x
                   crs         <- mapM funExpr eargs
                   let (cs,rs) =  unzip crs
                   r           <- freshLocTy (typeOf e)
                   return (foldr1 mkSeq (cs++[Assign r (FunCallRHS nf rs)]),r)
             RWCCon dci _ -> do
               -- Tag.
               t            <- getTag dci
               rt           <- freshLocSize (length t)
               -- Allocate result reg.
               r            <- freshLocTy (typeOf e)
               res_size     <- tyWidth (typeOf e)
               -- Padding.
               let targs    =  map typeOf eargs
               args_size    <- liftM sum (mapM tyWidth targs)
               let pad_size =  res_size - args_size - length t
               r_pad        <- freshLocSize pad_size
               case eargs of
                 [] -> -- No arguments, so tag is all we need.
                       return (foldr1 mkSeq [Assign rt (ConstRHS t),
                                             Assign r_pad (ConstRHS (replicate pad_size Zero)),
                                             Assign r (ConcatRHS [rt,r_pad])],r
                                             )
                 _  -> do -- Compile argument expressions.
                          crs          <- mapM funExpr eargs
                          let (cs,rs)  =  unzip crs
                          -- Concatenate tag and args, result in r.
                          return (foldr1 mkSeq (cs++[Assign r (ConcatRHS ([rt]++rs++[r_pad]))]),r)
             RWCCase escr alts               -> do
               case eargs of
                 [] -> do
                   (c_scr,r_scr) <- funExpr escr
                   r_res         <- freshLocTy (typeOf e)
                   cs_init       <- mapM (funAlt r_scr (typeOf escr) r_res) (init alts)
                   c_last        <- funAlt r_scr (typeOf escr) r_res (last alts) -- was funLastAlt
                   return (foldr1 mkSeq ([c_scr]++cs_init++[c_last]),r_res)
                 _  -> fail "funExpr: encountered case expression in function position"
  where (ef:eargs) = flattenApp e

funDefn :: Id RWCExp -> CGM String
funDefn n = do ms <- askFun n
               case ms of
                 Just s  -> return s
                 Nothing -> do
                   md <- lift $ lift $ queryG n
                   case md of
--                     Nothing               -> fail $ "funDefn: " ++ show n ++ " not defined"
                     Nothing               -> return (show n) -- FIXME: in this case it should be a VHDL-defined function
                     Just (RWCDefn _ _ e_) -> do
                       fn          <- freshFunName n
                       let (xts,e) =  peelLambdas e_
                           xs      =  map fst xts
                           ts      =  map snd xts
                       pns         <- mapM freshLocTy ts
                       psizes      <- mapM tyWidth ts
                       rr          <- freshLocTy (typeOf e)
                       let xrs     =  zip xs pns
                       h           <- getHeader
                       putHeader (h { regDecls = [] })
                       (ce,re)     <- foldr (uncurry binding) (funExpr e) xrs
                       h'          <- getHeader
                       let rds     =  regDecls h'
                           pds     =  zipWith RegDecl pns (map TyBits psizes)
                           fd      =  FunDefn fn pds rds ce re
                       putHeader (h' { regDecls = regDecls h,
                                       funDefns = fd : funDefns h' })
                       fm          <- getFunMap                                  
                       putFunMap (Map.insert n fn fm)
                       return fn
