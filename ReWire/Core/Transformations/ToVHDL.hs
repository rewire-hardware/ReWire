module ReWire.Core.Transformations.ToVHDL where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Monad
import ReWire.Core.Transformations.CheckNF
import ReWire.Core.Transformations.Types
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (intercalate,findIndex,find,nub)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Unbound.LocallyNameless

type Declaration = (String,Int)
data Bit = Zero | One deriving (Eq,Show)
data Condition = CondEq String String | CondAnd Condition Condition | CondTrue deriving Show
type Assignment = (String,AssignmentRHS)
data AssignmentRHS  = FunCall String [String]
                    | LocalVariable String
                    | Concat [String]
                    | BitConst [Bit]
                    | Slice String Int Int
                    | Conditional [(Condition,String)]
                    deriving Show
data VMState = VMState { assignments   :: [Assignment],
                         declarations  :: [Declaration],
                         signalCounter :: Int } deriving Show
data VMEnv   = VMEnv   { bindings :: Map (Name RWCExp) NameInfo } deriving Show
type VM = ReaderT VMEnv (StateT VMState RW)

data NameInfo = BoundK Int | BoundVar String | BoundFun String | NotBound deriving (Eq,Show)

getSignalCounter = get >>= return . signalCounter
modifySignalCounter f = modify (\ s -> s { signalCounter = f (signalCounter s) })
putSignalCounter = modifySignalCounter . const

getAssignments = get >>= return . assignments
modifyAssignments f = modify (\ s -> s { assignments = f (assignments s) })
putAssignments = modifyAssignments . const

getDeclarations = get >>= return . declarations
modifyDeclarations f = modify (\ s -> s { declarations = f (declarations s) })
putDeclarations = modifyDeclarations . const

tellAssignments o = modifyAssignments (++o)
tellDeclarations o = modifyDeclarations (++o)

askBindings :: VM (Map (Name RWCExp) NameInfo)
askBindings = ask >>= return . bindings

localBindings :: (Map (Name RWCExp) NameInfo -> Map (Name RWCExp) NameInfo) -> VM a -> VM a
localBindings f = local (\ e -> e { bindings = f (bindings e) })

askNameInfo :: Name RWCExp -> VM NameInfo
askNameInfo n = do bindings <- askBindings
                   return $ maybe NotBound id (Map.lookup n bindings)

freshName :: String -> VM String
freshName n = do ctr <- getSignalCounter
                 putSignalCounter (ctr+1)
                 return (n++"_"++show ctr)
               
freshTmp :: String -> Int -> VM String
freshTmp n 0 = return "\"\"" -- FIXME: hack hack hack?
freshTmp n i = do sn <- freshName n
                  emitDeclaration (sn,i)
                  return sn

freshTmpTy :: String -> RWCTy -> VM String
freshTmpTy n = freshTmp n <=< tyWidth

emitAssignment :: Assignment -> VM ()
emitAssignment ("\"\"",_) = return () -- FIXME: hack hack hack?
emitAssignment o         = tellAssignments [o]

emitDeclaration :: Declaration -> VM ()
emitDeclaration = tellDeclarations . (:[])

--genBittyLiteral :: RWCLit -> VM String
--genBittyLiteral (RWCLitInteger 

getStateTagWidth :: VM Int
getStateTagWidth = do bdgs        <- askBindings
                      let nstates =  length [() | (_,BoundK _) <- Map.toList bdgs]
                      return (nBits (nstates-1))

getThisStateWidth :: Name RWCExp -> VM Int
getThisStateWidth n = do Just (RWCDefn _ (Embed b)) <- lift $ lift $ askDefn n
                         lunbind b $ \ (tvs,(t,e)) -> do
                           let (targs,_) = flattenArrow t
                           liftM (sum . init) $ mapM tyWidth targs -- Last argument is the input, not part of state

getThisOutputWidth :: Name RWCExp -> VM Int
getThisOutputWidth n = do Just (RWCDefn _ (Embed b)) <- lift $ lift $ askDefn n
                          lunbind b $ \ (tvs,(t,e)) -> do
                            let (_,tres) = flattenArrow t
                            case tres of
                              (RWCTyApp (RWCTyApp (RWCTyApp (RWCTyCon "React") _) t) _) -> tyWidth t
                              _ -> fail $ "getThisOutputWidth: malformed result type for state (shouldn't happen)"

getOutputWidth :: VM Int
getOutputWidth = do bdgs    <- askBindings
                    let sns =  [n | (n,BoundK _) <- Map.toList bdgs]
                    ows     <- mapM getThisOutputWidth sns
                    case nub ows of
                      []  -> fail $ "getOutputWidth: no states defined?"
                      [n] -> return n
                      _   -> fail $ "getOutputWidth: inconsistent output widths (shouldn't happen)"

getThisInputWidth :: Name RWCExp -> VM Int
getThisInputWidth n = do Just (RWCDefn _ (Embed b)) <- lift $ lift $ askDefn n
                         lunbind b $ \ (tvs,(t,e)) -> do
                            let (_,tres) = flattenArrow t
                            case tres of
                              (RWCTyApp (RWCTyApp (RWCTyApp (RWCTyCon "React") t) _) _) -> tyWidth t
                              _ -> fail $ "getThisInputWidth: malformed result type for state (shouldn't happen)"

getInputWidth :: VM Int
getInputWidth = do bdgs    <- askBindings
                   let sns =  [n | (n,BoundK _) <- Map.toList bdgs]
                   ows     <- mapM getThisInputWidth sns
                   case nub ows of
                      []  -> fail $ "getInputWidth: no states defined?"
                      [n] -> return n
                      _   -> fail $ "getInputWidth: inconsistent intput widths (shouldn't happen)"

getStateWidth :: VM Int
getStateWidth = do ow      <- getOutputWidth
                   tw      <- getStateTagWidth
                   bdgs    <- askBindings
                   let sns =  [n | (n,BoundK _) <- Map.toList bdgs]
                   sws     <- mapM getThisStateWidth sns
                   return (ow+tw+maximum sws)

getStateTag :: Int -> VM [Bit]
getStateTag n = do tw <- getStateTagWidth
                   return (bitConstFromIntegral tw n)

genExp :: RWCExp -> VM String
genExp e@(RWCApp t _ _)   = let (ef:es) = flattenApp e
                            in case ef of
                              RWCCon _ "P" -> case es of
                                [eo,ek] -> do s_o   <- genExp eo
                                              s_k   <- genExp ek
                                              w     <- getStateWidth
                                              s     <- freshTmp "P" w
                                              emitAssignment (s,Concat [s_o,s_k])
                                              return s
                                _       -> fail $ "genExp: malformed pause expression"
                              RWCVar _ i -> do n_i  <- askNameInfo i
                                               case n_i of
                                                 BoundK n   -> do tag      <- getStateTag n
                                                                  tagWidth <- getStateTagWidth
                                                                  s_tag    <- freshTmp "tag" tagWidth
                                                                  emitAssignment (s_tag,BitConst tag)
                                                                  s_es     <- mapM genExp es
                                                                  sw       <- getStateWidth
                                                                  ow       <- getOutputWidth
                                                                  let kw   =  sw-ow
                                                                  s        <- freshTmp "k" kw
                                                                  emitAssignment (s,Concat (s_tag:s_es))
                                                                  return s
                                                 BoundFun n -> do s_es <- mapM genExp es
                                                                  s    <- freshTmpTy "funcall" t
                                                                  emitAssignment (s,FunCall n s_es)
                                                                  return s
                                                 BoundVar _ -> fail $ "genExp: locally bound variable is applied as function: " ++ show i
                                                 NotBound   -> fail $ "genExp: unbound variable: " ++ show i
                              RWCCon _ i -> do tci      <- getDataConTyCon i
                                               tagWidth <- getTagWidth tci
                                               s_tag    <- freshTmp "tag" tagWidth
                                               tag      <- getTag i
                                               emitAssignment (s_tag,BitConst tag)
                                               s        <- freshTmpTy "conapp" t
                                               s_es     <- mapM genExp es
                                               emitAssignment (s,Concat (s_tag:s_es))
                                               return s
                              _          -> fail $ "genExp: malformed application head: " ++ show e
genExp (RWCCon t i)       = do tag <- getTag i
                               s   <- freshTmpTy "con" t
                               emitAssignment (s,BitConst tag)
                               return s
genExp (RWCVar t i)       = do n_i <- askNameInfo i
                               case n_i of
                                 BoundK n   -> do tag      <- getStateTag n
                                                  tagWidth <- getStateTagWidth
                                                  s_tag    <- freshTmp "k" tagWidth
                                                  emitAssignment (s_tag,BitConst tag)
                                                  return s_tag
                                 BoundFun n -> do s <- freshTmpTy "funcall" t
                                                  emitAssignment (s,FunCall n [])
                                                  return s
                                 BoundVar n -> do s <- freshTmpTy "var" t
                                                  emitAssignment (s,LocalVariable n)
                                                  return s
                                 NotBound   -> fail $ "genExp: unbound variable: " ++ show i
--genExp (RWCLiteral _ l)   = genBittyLiteral l
genExp (RWCCase t e alts) = do s_scrut     <- genExp e
                               scond_alts  <- mapM (genBittyAlt s_scrut (typeOf e)) alts
                               s           <- freshTmpTy "case" t
                               emitAssignment (s,Conditional scond_alts)
                               return s
genExp e                  = fail $ "genExp: malformed bitty expression: " ++ show e

getTagWidth :: Identifier -> VM Int
getTagWidth i = do dds <- lift $ lift $ askDataDecls
                   case find (\(RWCData i' _) -> i==i') dds of
                     Nothing            -> fail $ "getTagWidth: unknown constructor " ++ i
                     Just (RWCData _ b) -> lunbind b $ \ (_,dcs) -> return (nBits (length dcs-1))

nBits 0 = 0
nBits n = nBits (n `quot` 2) + 1

getDataConTyCon :: Identifier -> VM Identifier
getDataConTyCon dci = do dds   <- lift $ lift $ askDataDecls
                         let tryThisOne (RWCData i b) = lunbind b $ \ (_,dcs) -> if any (\ (RWCDataCon dci' _) -> dci==dci') dcs then return (Just i) else return Nothing
                         res   <- mapM tryThisOne dds
                         let r =  msum res
                         case r of
                           Just tci -> return tci
                           Nothing  -> fail $ "getDataConTyCon: unknown data constructor" ++ dci

breakData :: Identifier -> String -> RWCTy -> VM (String,[(String,RWCTy)])
breakData i s_scrut t_scrut = do tci              <- getDataConTyCon i
                                 tagWidth         <- getTagWidth tci
                                 s_tag            <- freshTmp "tag" tagWidth
                                 emitAssignment (s_tag,Slice s_scrut 0 (tagWidth-1))
                                 fieldTys         <- getFieldTys i t_scrut
                                 fieldWidths      <- mapM tyWidth fieldTys
                                 s_fields         <- mapM (freshTmp "field") fieldWidths
                                 let fieldOffsets    =  scanl (+) tagWidth fieldWidths
                                     ranges []       =  []
                                     ranges [n]      =  []
                                     ranges (n:m:ns) =  (n,m-1) : ranges (m:ns)
                                     fieldRanges     =  ranges fieldOffsets
                                     emitOne s (l,h) =  emitAssignment (s,Slice s_scrut l h)
                                 zipWithM_ emitOne s_fields fieldRanges
                                 return (s_tag,zip s_fields fieldTys)

bitConstFromIntegral :: Integral a => Int -> a -> [Bit]
bitConstFromIntegral 0 _     = []
bitConstFromIntegral width n = bitConstFromIntegral (width-1) (n`div`2) ++ thisBit where thisBit = if odd n then [One] else [Zero]

getTag :: Identifier -> VM [Bit]
getTag i = do tci      <- getDataConTyCon i
              tagWidth <- getTagWidth tci
              mdd      <- lift $ lift $ askConDataDecl i
              case mdd of
                Nothing            -> fail $ "getTag: unknown constructor " ++ i
                Just (RWCData _ b) -> lunbind b $ \ (_,dcs) ->
                  case findIndex (\ (RWCDataCon i' _) -> i==i') dcs of
                    Just pos -> return (bitConstFromIntegral tagWidth pos)
                    Nothing  -> fail $ "getTag: unknown constructor " ++ i

-- NB: This will not terminate if called on a recursive type! (Could fix
-- that...)
tyWidth :: RWCTy -> VM Int
tyWidth (RWCTyVar _) = fail $ "tyWidth: type variable encountered"
tyWidth t            = do let (th:_) = flattenTyApp t
                          case th of
                            RWCTyCon "React" -> getStateWidth
                            RWCTyVar _ -> fail $ "tyWidth: type variable encountered"
                            RWCTyCon i -> do
                              dds <- lift $ lift $ askDataDecls
                              case find (\(RWCData i' _) -> i==i') dds of
                                Just (RWCData _ b) -> lunbind b $ \ (_,dcs) ->
                                  do tagWidth <- getTagWidth i
                                     cws      <- mapM (dataConWidth i) dcs
                                     return (tagWidth + maximum cws)
                                Nothing             -> fail $ "tyWidth: unknown type constructor " ++ i
                where dataConWidth di (RWCDataCon i _) = do
                        fts <- getFieldTys i t
                        liftM sum (mapM tyWidth fts)

getFieldTys :: Identifier -> RWCTy -> VM [RWCTy]
getFieldTys i t = do mdd <- lift $ lift $ askConDataDecl i
                     case mdd of
                       Nothing             -> fail $ "getFieldTys: unknown constructor " ++ i
                       Just (RWCData di b) -> lunbind b $ \ (tvs,dcs) ->
                         do let pt   = foldl RWCTyApp (RWCTyCon di) (map RWCTyVar tvs)
                                msub = matchty [] pt t
                            case msub of
                              Nothing  -> fail $ "getFieldTys: type matching failed (type was " ++ show t ++ " and datacon was " ++ i ++ ")"
                              Just sub -> do let (RWCDataCon _ targs) = fromJust $ find (\(RWCDataCon i' _) -> i==i') dcs
                                             return (substs sub targs)

zipWithM3 :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWithM3 _ [] _ _               = return []
zipWithM3 _ _ [] _               = return []
zipWithM3 _ _ _ []               = return []
zipWithM3 f (a:as) (b:bs) (c:cs) = do d  <- f a b c
                                      ds <- zipWithM3 f as bs cs
                                      return (d:ds)

genBittyAlt :: String -> RWCTy -> RWCAlt -> VM (Condition,String)
genBittyAlt s_scrut t_scrut (RWCAlt b) = lunbind b $ \ (p,e) -> do (cond,bdgs) <- genBittyPat s_scrut t_scrut p
                                                                   s           <- localBindings (Map.union bdgs) $ genExp e
                                                                   return (cond,s)

genBittyPat :: String -> RWCTy -> RWCPat -> VM (Condition,Map (Name RWCExp) NameInfo) -- (condition for match, resulting bindings)
genBittyPat s_scrut t_scrut (RWCPatCon i pats)      = do (s_tag,st_fields) <- breakData i s_scrut t_scrut
                                                         let s_fields      =  map fst st_fields
                                                             t_fields      =  map snd st_fields
                                                         tagValue          <- getTag i
                                                         condbinds_pats    <- zipWithM3 genBittyPat s_fields t_fields pats
                                                         let cond_pats     =  map fst condbinds_pats
                                                             binds_pats    =  map snd condbinds_pats
                                                         tci               <- getDataConTyCon i
                                                         tagWidth          <- getTagWidth tci
                                                         s_tagtest         <- freshTmp "test" tagWidth
                                                         emitAssignment(s_tagtest,BitConst tagValue)
                                                         return (foldr CondAnd (CondEq s_tag s_tagtest) cond_pats,
                                                                 foldr Map.union Map.empty binds_pats)
--genBittyPat s_scrut (RWCPatLiteral l) = 
genBittyPat s_scrut t_scrut (RWCPatVar (Embed t) n) = do s <- freshTmpTy "patvar" t_scrut
                                                         emitAssignment (s,LocalVariable s_scrut)
                                                         return (CondTrue,Map.singleton n (BoundVar s))

bitToChar Zero = '0'
bitToChar One  = '1'

renderAssignmentForVariables :: Assignment -> String
renderAssignmentForVariables (s,FunCall f ss)    = s ++ " := " ++ f ++ "(" ++ intercalate "," ss ++ ");"
renderAssignmentForVariables (s,LocalVariable x) = s ++ " := " ++ x ++ ";"
renderAssignmentForVariables (s,Concat ss)       = s ++ " := " ++ intercalate " & " ss ++ ";"
renderAssignmentForVariables (s,BitConst bs)     = s ++ " := \"" ++ map bitToChar bs ++ "\";"
renderAssignmentForVariables (s,Slice t l h)     = s ++ " := " ++ t ++ "(" ++ show l ++ " to " ++ show h ++ ");"
renderAssignmentForVariables (s,Conditional cs)  = let (b:bs) = map renderOneCase cs
                                                   in b ++ concatMap (" els"++) bs ++ " else " ++ s ++ " := (others => '0'); end if;"
  where renderOneCase :: (Condition,String) -> String
        renderOneCase (c,sc) = "if " ++ renderCond c ++ " then " ++ s ++ " := " ++ sc ++ ";"
        renderCond (CondEq s1 s2)  = s1 ++ " = " ++ s2
        renderCond (CondAnd c1 c2) = "(" ++ renderCond c1 ++ " and " ++ renderCond c2 ++ ")"
        renderCond CondTrue        = "true"

renderAssignmentForSignals :: Assignment -> String
renderAssignmentForSignals (s,FunCall f ss)    = s ++ " <= " ++ f ++ "(" ++ intercalate "," ss ++ ");"
renderAssignmentForSignals (s,LocalVariable x) = s ++ " <= " ++ x ++ ";"
renderAssignmentForSignals (s,Concat ss)       = s ++ " <= " ++ intercalate " & " ss ++ ";"
renderAssignmentForSignals (s,BitConst bs)     = s ++ " <= \"" ++ map bitToChar bs ++ "\";"
renderAssignmentForSignals (s,Slice t l h)     = s ++ " <= " ++ t ++ "(" ++ show l ++ " to " ++ show h ++ ");"
renderAssignmentForSignals (s,Conditional cs)  = s ++ " <= " ++ concatMap renderOneCase cs ++ "(others => '0');"
  where renderOneCase (c,sc) = sc ++ " when " ++ renderCond c ++ " else "
        renderCond (CondEq s1 s2)  = s1 ++ " = " ++ s2
        renderCond (CondAnd c1 c2) = "(" ++ renderCond c1 ++ " and " ++ renderCond c2 ++ ")"
        renderCond CondTrue        = "true"

renderDeclarationForVariables :: Declaration -> String
renderDeclarationForVariables (s,i) = "variable " ++ s ++ " : std_logic_vector(0 to " ++ show (i-1) ++ ") := (others => '0');"

renderDeclarationForSignals :: Declaration -> String
renderDeclarationForSignals (s,i) = "signal " ++ s ++ " : std_logic_vector(0 to " ++ show (i-1) ++ ") := (others => '0');"

-- FIXME: Lots of duplicated code here (genBittyDefn).
genMain :: VM String
genMain =
  do md <- lift $ lift $ askDefn (s2n "main")
     case md of
       Just (RWCDefn n (Embed b)) ->
         hideAssignments $ hideDeclarations $
          lunbind b $ \(tvs,(t,e)) ->
           do wr  <- tyWidth (typeOf e)
              s   <- genExp e
              as  <- getAssignments
              ds  <- getDeclarations
              return ("  pure function sm_state_initial\n" ++
                      "    return std_logic_vector\n" ++
                      "  is\n" ++
                      concatMap (("    "++) . (++"\n") . renderDeclarationForVariables) ds ++
                      "  begin\n" ++
                      concatMap (("    "++) . (++"\n") . renderAssignmentForVariables) as ++
                      "    return " ++ s ++ ";\n" ++
                      "  end sm_state_initial;\n")
       Nothing -> fail $ "genMain: No definition for main"

genBittyDefn :: RWCDefn -> VM String
genBittyDefn (RWCDefn n_ (Embed b)) =
  hideAssignments $ hideDeclarations $
  lunbind b $ \(tvs,(t,e_)) ->
  flattenLambda e_ $ \ (nts,e) ->
    do (BoundFun n) <- askNameInfo n_
       let freshArgumentTy (n,t) = do s <- freshName "arg"
                                      return ((n,BoundVar s),s ++ " : std_logic_vector")
       wr  <- tyWidth (typeOf e)
       bps <- mapM freshArgumentTy nts
       let (bdgs,ps) = unzip bps
       s   <- localBindings (Map.union $ Map.fromList bdgs) (genExp e)
       as  <- getAssignments
       ds  <- getDeclarations
       return ("  pure function " ++ n ++ "(" ++ intercalate " ; " ps ++ ")\n" ++
               "    return std_logic_vector\n" ++
               "  is\n" ++
               concatMap (("    "++) . (++"\n") . renderDeclarationForVariables) ds ++
               "  begin\n" ++
               concatMap (("    "++) . (++"\n") . renderAssignmentForVariables) as ++
               "    return " ++ s ++ ";\n" ++
               "  end " ++ n ++ ";\n")

genContDefn :: RWCDefn -> VM ()
genContDefn (RWCDefn n_ (Embed b)) =
  lunbind b $ \(tvs,(t,e_)) ->
  flattenLambda e_ $ \ (nts_,e) ->
    do (BoundK nk)         <- askNameInfo n_
       let nts             =  init nts_
           (nin,tin)       =  last nts_
       tagWidth            <- getStateTagWidth
       outputWidth         <- getOutputWidth
       fieldWidths         <- mapM (tyWidth . snd) nts
       s_fields            <- mapM (freshTmp "statefield") fieldWidths
       let fieldOffsets    =  scanl (+) (tagWidth+outputWidth) fieldWidths
           ranges []       =  []
           ranges [n]      =  []
           ranges (n:m:ns) =  (n,m-1) : ranges (m:ns)
           fieldRanges     =  ranges fieldOffsets
           emitOne s (l,h) =  emitAssignment (s,Slice "sm_state" l h)
       zipWithM_ emitOne s_fields fieldRanges
       let mkNI n s = (n,BoundVar s)
           bdgs_    =  zipWith mkNI (map fst nts) s_fields
       let bdgs      = (nin,BoundVar "sm_input"):bdgs_
       s   <- localBindings (Map.union $ Map.fromList bdgs) (genExp e)
       stateWidth <- getStateWidth
       emitDeclaration("next_state_"++show nk,stateWidth)
       emitAssignment("next_state_"++show nk,LocalVariable s)

hideAssignments :: VM a -> VM a
hideAssignments m = do as <- getAssignments
                       putAssignments []
                       v  <- m
                       putAssignments as
                       return v

hideDeclarations :: VM a -> VM a
hideDeclarations m = do ds <- getDeclarations
                        putDeclarations []
                        v  <- m
                        putDeclarations ds
                        return v

-- Note: here we can't use the fresh name supply for conts because the
-- synth. tools need them numbered 0 and up in order for FSM to be
-- recognized. (I think. I may be totally wrong about that.)
initBindings :: Map (Name RWCExp) DefnSort -> VM (Map (Name RWCExp) NameInfo)
initBindings m = do let kvs =  Map.toList m
                        sns =  [0..]
                    kvs'    <- doEm sns kvs
                    return (Map.fromList kvs')
  where doEm sns ((k,DefnBitty):kvs)    = do n    <- freshName "func"
                                             rest <- doEm sns kvs
                                             return ((k,BoundFun n):rest)
        doEm (n:sns) ((k,DefnCont):kvs) = do rest <- doEm sns kvs
                                             return ((k,BoundK n):rest)
        doEm _ []                       = return []
  
nextstate_sig_assign :: (a,NameInfo) -> VM [String]
nextstate_sig_assign (_,BoundK n) = do tag <- getStateTag n
                                       ss  <- getOutputWidth
                                       sw  <- getStateTagWidth
                                       -- special case when only one state!
                                       if sw==0
                                          then return $ ["if true then sm_state <= next_state_" ++ show n ++ ";"]
                                          else return $ ["if sm_state(" ++ show ss ++ " to " ++ show (ss+sw-1) ++ ") = \"" ++ map bitToChar tag ++ "\" then sm_state <= next_state_" ++ show n ++ ";"]
nextstate_sig_assign _            = return []

genProg :: VM String
genProg = do res <- lift $ lift $ checkProg'
             case res of
               Left e  -> fail $ "genProg: Error in checking normal form: " ++ e
               Right m -> do
                bdgs <- initBindings m
                localBindings (Map.union bdgs) $ do
                 v_main  <- genMain
                 sw      <- getStateWidth
                 iw      <- getInputWidth
                 ow      <- getOutputWidth
                 let kts =  Map.toList m
                 v_funs  <- mapM genOne kts
                 as      <- getAssignments
                 ds      <- getDeclarations
                 (assn:assns) <- liftM concat $ mapM nextstate_sig_assign (Map.toList bdgs)
                 let entity_name = "rewire" -- FIXME: customizable?
                 return ("library ieee;\n" ++
                         "use ieee.std_logic_1164.all;\n" ++
                         "entity " ++ entity_name ++ " is\n" ++
                         "  port (clk : in std_logic;\n" ++
                         "        sm_input : in std_logic_vector(0 to " ++ show (iw-1) ++ ");\n" ++
                         "        sm_output : out std_logic_vector(0 to " ++ show (ow-1) ++ "));\n" ++
                         "end rewire;\n" ++
                         "architecture behavioral of " ++ entity_name ++ " is\n" ++
                         v_main ++
                         concat v_funs ++
                         "  signal sm_state : std_logic_vector(0 to " ++ show (sw-1) ++ ") := sm_state_initial;\n" ++
                         concatMap (("  "++) . (++"\n") . renderDeclarationForSignals) ds ++
                         "begin\n" ++
                         "  sm_output <= sm_state(0 to " ++ show (ow-1) ++ ");\n" ++
                         concatMap (("  "++) . (++"\n") . renderAssignmentForSignals) as ++
                         "  process(clk)\n" ++
                         "  begin\n" ++
                         "    if rising_edge(clk) then\n" ++
                         "        " ++ assn ++ concatMap (" els"++) assns ++ " else sm_state <= sm_state_initial; end if;\n" ++
                         "    end if;\n" ++
                         "  end process;\n" ++
                         "end behavioral;\n")
   where genOne (k,DefnBitty) = do md <- lift $ lift $ askDefn k
                                   case md of
                                     Just d  -> genBittyDefn d
                                     Nothing -> fail $ "genProg: No definition for bitty function " ++ show k
         genOne (k,DefnCont)  = do md <- lift $ lift $ askDefn k
                                   case md of
                                     Just d  -> genContDefn d >> return ""
                                     Nothing -> fail $ "genProg: No definition for cont function " ++ show k

runVM :: RWCProg -> VM a -> (a,VMState)
runVM p phi = runRW p $
               (runStateT $ runReaderT phi env0) state0
  where state0 = VMState { signalCounter = 0,
                           assignments   = [],
                           declarations  = [] }
        env0   = VMEnv { bindings = Map.empty }
        
cmdToVHDL :: TransCommand
cmdToVHDL _ p = (Nothing,Just s)
  where s = fst $ runVM p genProg