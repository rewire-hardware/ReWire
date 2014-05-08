module ReWire.Core.Transformations.ToVHDL where

import ReWire.Scoping
import ReWire.Core.Syntax
import ReWire.Core.Transformations.Monad
import ReWire.Core.Transformations.CheckNF
import ReWire.Core.Transformations.Types
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (intercalate,findIndex,find,nub,foldl',isPrefixOf)
import Data.Maybe (fromJust,catMaybes)
import Data.Tuple (swap)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Debug.Trace (trace)
import Control.DeepSeq

type Declaration = (String,Int)
data Bit = Zero | One deriving (Eq,Show)
data Condition = CondEq AssignmentRHS AssignmentRHS | CondAnd Condition Condition | CondTrue deriving (Eq,Show)
type Assignment = (String,AssignmentRHS)
data AssignmentRHS  = FunCall String [AssignmentRHS]
                    | LocalVariable String
                    | Concat [AssignmentRHS]
                    | BitConst [Bit]
                    | Slice AssignmentRHS Int Int
                    | Conditional [(Condition,AssignmentRHS)]
                    deriving (Eq,Show)
data VMState = VMState { assignments        :: [Assignment],
                         declarations       :: [Declaration],
                         signalCounter      :: Int, 
                         tyWidthCache       :: Map RWCTy Int, 
                         inputWidthCache    :: Maybe Int,
                         outputWidthCache   :: Maybe Int,
                         stateWidthCache    :: Maybe Int, 
                         stateTagWidthCache :: Maybe Int } deriving Show
data VMEnv   = VMEnv   { bindings :: Map (Id RWCExp) NameInfo } deriving Show
type VM = RWT (ReaderT VMEnv (StateT VMState Identity))

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

getTyWidthCache = get >>= return . tyWidthCache
modifyTyWidthCache f = modify (\ s -> s { tyWidthCache = f (tyWidthCache s) })
--putTyWidthCache = modifyTyWidthCache . const

getOutputWidthCache = get >>= return . outputWidthCache
modifyOutputWidthCache f = modify (\ s -> s { outputWidthCache = f (outputWidthCache s) })
putOutputWidthCache = modifyOutputWidthCache . const

getInputWidthCache = get >>= return . inputWidthCache
modifyInputWidthCache f = modify (\ s -> s { inputWidthCache = f (inputWidthCache s) })
putInputWidthCache = modifyInputWidthCache . const

getStateWidthCache = get >>= return . stateWidthCache
modifyStateWidthCache f = modify (\ s -> s { stateWidthCache = f (stateWidthCache s) })
putStateWidthCache = modifyStateWidthCache . const

getStateTagWidthCache = get >>= return . stateTagWidthCache
modifyStateTagWidthCache f = modify (\ s -> s { stateTagWidthCache = f (stateTagWidthCache s) })
putStateTagWidthCache = modifyStateTagWidthCache . const

tellAssignments o = modifyAssignments (o++)
tellDeclarations o = modifyDeclarations (o++)

askBindings :: VM (Map (Id RWCExp) NameInfo)
askBindings = ask >>= return . bindings

localBindings :: (Map (Id RWCExp) NameInfo -> Map (Id RWCExp) NameInfo) -> VM a -> VM a
localBindings f = local (\ e -> e { bindings = f (bindings e) })

askNameInfo :: Id RWCExp -> VM NameInfo
askNameInfo n = do bindings <- askBindings
                   return $ maybe NotBound id (Map.lookup n bindings)

freshName :: String -> VM String
freshName n = do ctr <- getSignalCounter
                 putSignalCounter (ctr+1)
                 return (n++"_"++show ctr)
               
freshTmp :: String -> Int -> VM String
freshTmp n 0 = return "NIL" -- FIXME: hack hack hack?
freshTmp n i = do sn <- freshName n
                  emitDeclaration (sn,i)
                  return sn

freshTmpTy :: String -> RWCTy -> VM String
freshTmpTy n = freshTmp n <=< tyWidth

--isNil = (=="\"\"")

--removeNils :: AssignmentRHS -> AssignmentRHS
--removeNils (FunCall f ss)    = FunCall f (filter (not . isNil) ss)
--removeNils (LocalVariable v) = LocalVariable v
--removeNils (Concat ss)       = Concat (filter (not . isNil) ss)
--removeNils (BitConst bs)     = BitConst bs
--removeNils (Slice s i j)     = Slice s i j
--removeNils (Conditional cs)  = Conditional (map removeNilsCase cs)

--removeNilsCase :: (Condition,String) -> (Condition,String)
--removeNilsCase (c,s) = (removeNilsCond c,s)

--removeNilsCond :: Condition -> Condition
--removeNilsCond (CondEq s1 s2) | isNil s1 && isNil s2 = CondTrue
--                              | otherwise            = CondEq s1 s2
--removeNilsCond (CondAnd c1 c2)                       = CondAnd (removeNilsCond c1) (removeNilsCond c2)
--removeNilsCond CondTrue                              = CondTrue

emitAssignment :: Assignment -> VM ()
emitAssignment ("NIL",_) = return ()
emitAssignment (s,rhs)   = tellAssignments [(s,rhs)]

emitDeclaration :: Declaration -> VM ()
emitDeclaration d = tellDeclarations [d]

--genBittyLiteral :: RWCLit -> VM String
--genBittyLiteral (RWCLitInteger 

getStateTagWidth :: VM Int
getStateTagWidth = do mc <- getStateTagWidthCache
                      case mc of
                        Just n -> return n
                        Nothing -> do bdgs        <- askBindings
                                      let nstates =  length [() | (_,BoundK _) <- Map.toList bdgs]
                                          n       =  nBits (nstates-1)
                                      putStateTagWidthCache (Just n)
                                      return n

getThisStateWidth :: Id RWCExp -> VM Int
getThisStateWidth n = do Just (RWCDefn n (tvs :-> t) e) <- queryG n
                         let (targs,_) = flattenArrow t
                         -- Last argument is the input, not part of state, hence init
                         liftM (sum . init) $ mapM tyWidth targs

getThisOutputWidth :: Id RWCExp -> VM Int
getThisOutputWidth n = do Just (RWCDefn n (tvs :-> t) e) <- queryG n
                          let (_,tres) = flattenArrow t
                          case tres of
                            (RWCTyApp (RWCTyApp (RWCTyApp (RWCTyCon (TyConId "React")) _) t) _) -> tyWidth t
                            _ -> fail $ "getThisOutputWidth: malformed result type for state (shouldn't happen)"

getOutputWidth :: VM Int
getOutputWidth = do mc <- getOutputWidthCache
                    case mc of
                      Just n  -> return n
                      Nothing -> do bdgs    <- askBindings
                                    let sns =  [n | (n,BoundK _) <- Map.toList bdgs]
                                    ows     <- mapM getThisOutputWidth sns
                                    case nub ows of
                                      []  -> fail $ "getOutputWidth: no states defined?"
                                      [n] -> putOutputWidthCache (Just n) >> return n
                                      _   -> fail $ "getOutputWidth: inconsistent output widths (shouldn't happen)"

getThisInputWidth :: Id RWCExp -> VM Int
getThisInputWidth n = do Just (RWCDefn n (tvs :-> t) e) <- queryG n
                         let (_,tres) = flattenArrow t
                         case tres of
                           (RWCTyApp (RWCTyApp (RWCTyApp (RWCTyCon (TyConId "React")) t) _) _) -> tyWidth t
                           _ -> fail $ "getThisInputWidth: malformed result type for state (shouldn't happen)"

getInputWidth :: VM Int
getInputWidth = do mc <- getInputWidthCache
                   case mc of
                     Just n  -> return n
                     Nothing -> do bdgs    <- askBindings
                                   let sns =  [n | (n,BoundK _) <- Map.toList bdgs]
                                   ows     <- mapM getThisInputWidth sns
                                   case nub ows of
                                     []  -> fail $ "getInputWidth: no states defined?"
                                     [n] -> putInputWidthCache (Just n) >> return n
                                     _   -> fail $ "getInputWidth: inconsistent intput widths (shouldn't happen)"

getStateWidth :: VM Int
getStateWidth = do mc <- getStateWidthCache
                   case mc of
                     Just n -> return n
                     Nothing -> do ow      <- getOutputWidth
                                   tw      <- getStateTagWidth
                                   bdgs    <- askBindings
                                   let sns =  [n | (n,BoundK _) <- Map.toList bdgs]
                                   sws     <- mapM getThisStateWidth sns
                                   let n = ow+tw+maximum sws
                                   putStateWidthCache (Just n)
                                   return n

getStateTag :: Int -> VM [Bit]
getStateTag n = do tw <- getStateTagWidth
                   return (bitConstFromIntegral tw n)

genExp :: RWCExp -> VM String
genExp e@(RWCApp _ _)     = let (ef:es) = flattenApp e
                                t       = typeOf e
                            in case ef of
                              RWCCon (DataConId "P") _ -> case es of
                                [eo,ek] -> do s_o   <- genExp eo
                                              s_k   <- genExp ek
                                              w     <- getStateWidth
                                              s     <- freshTmp "P" w
                                              emitAssignment (s,Concat [LocalVariable s_o,LocalVariable s_k])
                                              return s
                                _       -> fail $ "genExp: malformed pause expression"
                              RWCVar i _ -> do n_i  <- askNameInfo i
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
                                                                  emitAssignment (s,Concat (LocalVariable s_tag:map LocalVariable s_es))
                                                                  return s
                                                 BoundFun n -> do s_es <- mapM genExp es
                                                                  s    <- freshTmpTy "funcall" t
                                                                  emitAssignment (s,FunCall n (map LocalVariable s_es))
                                                                  return s
                                                 BoundVar _ -> fail $ "genExp: locally bound variable is applied as function: " ++ show i
                                                 NotBound   -> fail $ "genExp@RWCApp: unbound variable: " ++ show i
                              RWCCon i _ -> do tci      <- getDataConTyCon i
                                               tagWidth <- getTagWidth tci
                                               s_tag    <- freshTmp "tag" tagWidth
                                               tag      <- getTag i
                                               emitAssignment (s_tag,BitConst tag)
                                               s        <- freshTmpTy "conapp" t
                                               s_es     <- mapM genExp es
                                               emitAssignment (s,Concat (LocalVariable s_tag:map LocalVariable s_es))
                                               return s
                              _          -> fail $ "genExp: malformed application head: " ++ show e
genExp (RWCCon i t)       = do tag <- getTag i
                               s   <- freshTmpTy "con" t
                               emitAssignment (s,BitConst tag)
                               return s
genExp (RWCVar i t)       = do n_i <- askNameInfo i
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
                                 NotBound   -> fail $ "genExp@RWCVar: unbound variable: " ++ show i
--genExp (RWCLiteral _ l)   = genBittyLiteral l
genExp e_@(RWCCase e alts) = do s_scrut     <- genExp e
                                scond_alts  <- mapM (genBittyAlt s_scrut (typeOf e)) alts
                                s           <- freshTmpTy "case" (typeOf e_)
                                emitAssignment (s,Conditional scond_alts)
                                return s
genExp e                   = fail $ "genExp: malformed bitty expression: " ++ show e

getTagWidth :: TyConId -> VM Int
getTagWidth i = do Just (TyConInfo (RWCData _ _ cs)) <- queryT i
                   return (nBits (length cs-1))

nBits 0 = 0
nBits n = nBits (n `quot` 2) + 1

getDataConTyCon :: DataConId -> VM TyConId
getDataConTyCon dci = do Just (DataConInfo n _) <- queryD dci
                         return n

breakData :: DataConId -> String -> RWCTy -> [Bool] -> VM (String,[(String,RWCTy)])
breakData i s_scrut t_scrut used = do tci               <- getDataConTyCon i
                                      tagWidth          <- getTagWidth tci
                                      s_tag             <- freshTmp "tag" tagWidth
                                      emitAssignment (s_tag,Slice (LocalVariable s_scrut) 0 (tagWidth-1))
                                      fieldTys          <- getFieldTys i t_scrut
                                      fieldWidths       <- mapM tyWidth fieldTys
                                      let mkField False _ = return ""
                                          mkField True w  = freshTmp "field" w
                                      s_fields          <- zipWithM mkField used fieldWidths
                                      let fieldOffsets    =  scanl (+) tagWidth fieldWidths
                                          ranges []       =  []
                                          ranges [n]      =  []
                                          ranges (n:m:ns) =  (n,m-1) : ranges (m:ns)
                                          fieldRanges     =  ranges fieldOffsets
                                          emitOne "" _    =  return ()
                                          emitOne s (l,h) =  emitAssignment (s,Slice (LocalVariable s_scrut) l h)
                                      fieldRanges `deepseq` zipWithM_ emitOne s_fields fieldRanges
                                      return (s_tag,zip s_fields fieldTys)

bitConstFromIntegral :: Integral a => Int -> a -> [Bit]
bitConstFromIntegral 0 _     = []
bitConstFromIntegral width n = bitConstFromIntegral (width-1) (n`div`2) ++ thisBit where thisBit = if odd n then [One] else [Zero]

getTag :: DataConId -> VM [Bit]
getTag i = do tci                 <- getDataConTyCon i
              Just (TyConInfo dd) <- queryT tci 
              tagWidth            <- getTagWidth tci
              case findIndex (\ (RWCDataCon i' _) -> i==i') (dataCons dd) of
                Just pos -> return (bitConstFromIntegral tagWidth pos)
                Nothing  -> fail $ "getTag: unknown constructor " ++ deDataConId i

-- NB: This will not terminate if called on a recursive type! (Could fix
-- that...)
tyWidth :: RWCTy -> VM Int
tyWidth (RWCTyVar _) = fail $ "tyWidth: type variable encountered"
tyWidth t            = do twc <- getTyWidthCache
                          case Map.lookup t twc of
                            Just size -> return size
                            Nothing   -> do
                              let (th:_) = flattenTyApp t
                              case th of
                                RWCTyCon (TyConId "React") -> getStateWidth
                                RWCTyVar _ -> fail $ "tyWidth: type variable encountered"
                                RWCTyCon i -> do
                                  Just (TyConInfo (RWCData _ _ dcs)) <- queryT i
                                  tagWidth <- getTagWidth i
                                  cws      <- mapM (dataConWidth i) dcs
                                  let size =  tagWidth + maximum cws
                                  modifyTyWidthCache (Map.insert t size)
                                  return size
                where dataConWidth di (RWCDataCon i _) = do
                        fts <- getFieldTys i t
                        liftM sum (mapM tyWidth fts)

getFieldTys :: DataConId -> RWCTy -> VM [RWCTy]
getFieldTys i t = do Just (DataConInfo tci _)             <- queryD i
                     Just (TyConInfo (RWCData _ tvs dcs)) <- queryT tci
                     let pt   = foldl' RWCTyApp (RWCTyCon tci) (map RWCTyVar tvs)
                         msub = matchty Map.empty pt t
                     case msub of
                       Nothing  -> fail $ "getFieldTys: type matching failed (type was " ++ show t ++ " and datacon was " ++ deDataConId i ++ ")"
                       Just sub -> do let (RWCDataCon _ targs) = fromJust $ find (\(RWCDataCon i' _) -> i==i') dcs
                                      return (subst sub targs)

zipWithM3 :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWithM3 _ [] _ _               = return []
zipWithM3 _ _ [] _               = return []
zipWithM3 _ _ _ []               = return []
zipWithM3 f (a:as) (b:bs) (c:cs) = do d  <- f a b c
                                      ds <- zipWithM3 f as bs cs
                                      return (d:ds)

genBittyAlt :: String -> RWCTy -> RWCAlt -> VM (Condition,AssignmentRHS)
genBittyAlt s_scrut t_scrut a = inAlt a $ \ p e -> do
                                             (cond,bdgs) <- genBittyPat s_scrut t_scrut p
                                             s           <- localBindings (Map.union bdgs) $ genExp e
                                             return (cond,LocalVariable s)

isWild RWCPatWild = True
isWild _          = False

genBittyPat :: String -> RWCTy -> RWCPat -> VM (Condition,Map (Id RWCExp) NameInfo) -- (condition for match, resulting bindings)
genBittyPat s_scrut t_scrut (RWCPatCon i pats)      = do let used = map (not . isWild) pats
                                                         (s_tag,st_fields) <- used `deepseq` breakData i s_scrut t_scrut used
                                                         let s_fields      =  map fst st_fields
                                                             t_fields      =  map snd st_fields
                                                         tagValue          <- getTag i
                                                         condbinds_pats    <- s_fields `deepseq` t_fields `deepseq` zipWithM3 genBittyPat s_fields t_fields pats
                                                         let cond_pats     =  map fst condbinds_pats
                                                             binds_pats    =  map snd condbinds_pats
                                                         tci               <- getDataConTyCon i
                                                         tagWidth          <- getTagWidth tci
                                                         s_tagtest         <- freshTmp "test" tagWidth
                                                         emitAssignment (s_tagtest,BitConst tagValue)
                                                         return (foldr CondAnd (CondEq (LocalVariable s_tag) (LocalVariable s_tagtest)) cond_pats,
                                                                 foldr Map.union Map.empty binds_pats)
--genBittyPat s_scrut (RWCPatLiteral l) = 
genBittyPat s_scrut t_scrut (RWCPatVar n _) = do s <- freshTmpTy "patvar" t_scrut
                                                 emitAssignment (s,LocalVariable s_scrut)                                            
                                                 return (CondTrue,Map.singleton n (BoundVar s))
genBittyPat _ _ RWCPatWild                  = return (CondTrue,Map.empty)

bitToChar Zero = '0'
bitToChar One  = '1'

renderRHS (FunCall f [])    = f
renderRHS (FunCall f rs)    = f ++ "(" ++ intercalate "," (map renderRHS rs) ++ ")"
renderRHS (LocalVariable x) = x
renderRHS (Concat rs)       = "(" ++ intercalate " & " (map renderRHS rs) ++ ")"
renderRHS (BitConst bs)     = "\"" ++ map bitToChar bs ++ "\""
renderRHS (Slice t l h)     = "(" ++ renderRHS t ++ "(" ++ show l ++ " to " ++ show h ++ "))"
renderRHS r@(Conditional _) = error $ "renderRHS: encountered nested condition: " ++ show r

renderAssignmentForVariables :: Assignment -> String
renderAssignmentForVariables (s,Conditional [])  = s ++ " := (others => '0');" -- shouldn't happen
renderAssignmentForVariables (s,Conditional cs)  = let (b:bs) = map renderOneCase cs
                                                   in b ++ concatMap (" els"++) bs ++ " else " ++ s ++ " := (others => '0'); end if;"
  where renderOneCase :: (Condition,AssignmentRHS) -> String
        renderOneCase (c,sc) = "if " ++ renderCond c ++ " then " ++ s ++ " := " ++ renderRHS sc ++ ";"
        renderCond (CondEq s1 s2)  = "(" ++ renderRHS s1 ++ " = " ++ renderRHS s2 ++ ")"
        renderCond (CondAnd c1 c2) = "(" ++ renderCond c1 ++ " and " ++ renderCond c2 ++ ")"
        renderCond CondTrue        = "true"
renderAssignmentForVariables (s,r)               = s ++ " := " ++ renderRHS r ++ ";"

renderAssignmentForSignals :: Assignment -> String
renderAssignmentForSignals (s,Conditional [])  = s ++ " <= (others => '0');"
renderAssignmentForSignals (s,Conditional cs)  = s ++ " <= " ++ concatMap renderOneCase cs ++ "(others => '0');"
  where renderOneCase (c,sc) = renderRHS sc ++ " when " ++ renderCond c ++ " else "
        renderCond (CondEq s1 s2)  = "(" ++ renderRHS s1 ++ " = " ++ renderRHS s2 ++ ")"
        renderCond (CondAnd c1 c2) = "(" ++ renderCond c1 ++ " and " ++ renderCond c2 ++ ")"
        renderCond CondTrue        = "true"
renderAssignmentForSignals (s,r)               = s ++ " <= " ++ renderRHS r ++ ";"

renderDeclarationForVariables :: Declaration -> String
renderDeclarationForVariables (s,i) = "variable " ++ s ++ " : std_logic_vector(0 to " ++ show (i-1) ++ ") := (others => '0');"

renderDeclarationForSignals :: Declaration -> String
renderDeclarationForSignals (s,i) = "signal " ++ s ++ " : std_logic_vector(0 to " ++ show (i-1) ++ ") := (others => '0');"

-- FIXME: Lots of duplicated code here (genBittyDefn).
genStart :: VM String
genStart =
  do md <- queryG (mkId "start")
     case md of
       Just (RWCDefn n (tvs :-> t) e) ->
         hideAssignments $ hideDeclarations $
           do wr    <- tyWidth (typeOf e)
              s     <- genExp e
              s_ret <- freshTmp "ret" wr
              emitAssignment (s_ret,LocalVariable s)
              optimize
              as  <- getAssignments
              ds  <- getDeclarations
              return  ("  pure function sm_state_initial\n" ++
                       "    return std_logic_vector\n" ++
                       "  is\n" ++
                       concatMap (("    "++) . (++"\n") . renderDeclarationForVariables) (reverse ds) ++
                       "  begin\n" ++
                       concatMap (("    "++) . (++"\n") . renderAssignmentForVariables) (reverse as) ++
                       "    return " ++ s_ret ++ ";\n" ++
                       "  end sm_state_initial;\n")
       Nothing -> fail $ "genStart: No definition for start"

genBittyDefn :: RWCDefn -> VM String
genBittyDefn (RWCDefn n_ (tvs :-> t) e_) =
  hideAssignments $ hideDeclarations $
  inLambdas e_ $ \ nts e ->
    do (BoundFun n) <- askNameInfo n_
       let freshArgumentTy pos (n,t) = do let s = "arg_" ++ show pos
                                          ws    <- tyWidth t
                                          sUse  <- freshTmp "arg_use" ws
                                          emitAssignment (sUse,LocalVariable s)
                                          return ((n,BoundVar sUse),s ++ " : std_logic_vector") 
       wr  <- tyWidth (typeOf e)
       bps <- zipWithM freshArgumentTy [0..] nts
       let (bdgs,ps) = unzip bps
       s   <- localBindings (Map.union $ Map.fromList bdgs) (genExp e)
       s_r <- freshTmp "ret" wr
       emitAssignment (s_r,LocalVariable s)
       optimize
       as  <- getAssignments
       ds  <- getDeclarations
       return  ("  -- bitty: " ++ deId n_ ++ "\n" ++
                "  pure function " ++ n ++ (if null ps then "" else "(" ++ intercalate " ; " ps ++ ")") ++ "\n" ++
                "    return std_logic_vector\n" ++
                "  is\n" ++
                concatMap (("    "++) . (++"\n") . renderDeclarationForVariables) (reverse ds) ++
                "  begin\n" ++
                concatMap (("    "++) . (++"\n") . renderAssignmentForVariables) (reverse as) ++
                "    return " ++ s_r ++ ";\n" ++
                "  end " ++ n ++ ";\n")

genBittyDefnProto :: RWCDefn -> VM String
genBittyDefnProto (RWCDefn n_ (tvs :-> t) e_) =
  hideAssignments $ hideDeclarations $
  inLambdas e_ $ \ nts e ->
    do (BoundFun n) <- askNameInfo n_
       let freshArgumentTy pos (n,t) = let s = "arg_" ++ show pos in return ((n,BoundVar s),s ++ " : std_logic_vector") 
       wr  <- tyWidth (typeOf e)
       bps <- zipWithM freshArgumentTy [0..] nts
       let (bdgs,ps) = unzip bps
       return ("  -- bitty: " ++ deId n_ ++ "\n" ++
               "  pure function " ++ n ++ (if null ps then "" else "(" ++ intercalate " ; " ps ++ ")") ++ "\n" ++
               "    return std_logic_vector;\n")

genContDefn :: RWCDefn -> VM ()
genContDefn (RWCDefn n_ (tvs :-> t) e_) =
  inLambdas e_ $ \ nts_ e ->
   trace ("genContDefn: " ++ show n_) $
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
           emitOne s (l,h) =  emitAssignment (s,Slice (LocalVariable "sm_state") l h)
       zipWithM_ emitOne s_fields fieldRanges
       let mkNI n s = (n,BoundVar s)
           bdgs_    =  zipWith mkNI (map fst nts) s_fields
       let bdgs      = (nin,BoundVar "sm_input"):bdgs_
       s     <- localBindings (Map.union $ Map.fromList bdgs) (genExp e)
       stateWidth <- getStateWidth
       s_ret <- freshTmp "ret" stateWidth
       emitAssignment (s_ret,LocalVariable s)
       let decl = ("next_state_"++show nk,stateWidth)
       emitDeclaration decl
       let assignment = ("next_state_"++show nk,LocalVariable s_ret)
       emitAssignment ("next_state_"++show nk,LocalVariable s_ret)

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
initBindings :: Map (Id RWCExp) DefnSort -> VM (Map (Id RWCExp) NameInfo)
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

optimize :: VM ()
optimize = do as  <- liftM reverse getAssignments
              as' <- op (Map.singleton "NIL" (BitConst [])) as
              putAssignments (reverse as')
  where op :: Map String AssignmentRHS -> [Assignment] -> VM [Assignment]
        op m ((s,r):as) | isSimple r && not (isSpecial s) =
                                       do unDecl s
                                          let m'  = Map.map (applymap (Map.singleton s r)) m
                                              m'' = Map.insert s r m'
                                          op m'' as
                        | otherwise  = do let r' = simplifyRHS (applymap m r)
                                          if r'==r then do rest <- op m as
                                                           return ((s,r'):rest)
                                                   else op m ((s,r'):as)
        op _ []                      = return []
        isSpecial s = "ret_" `isPrefixOf` s || "next_state" `isPrefixOf` s || "sm_" `isPrefixOf` s || "P" `isPrefixOf` s || "k_" `isPrefixOf` s || "arg_use_" `isPrefixOf` s
        isSimple (FunCall _ [])    = True
        isSimple (FunCall _ _)     = False
        isSimple (LocalVariable _) = True
        isSimple (Concat rs)       = all isSimple rs
        isSimple (BitConst _)      = True
        isSimple (Slice _ _ _)     = False
        isSimple (Conditional _)   = False
        unDecl s = modifyDeclarations (filter ((/= s) . fst))
        applymap m (FunCall s rs)    = FunCall s (map (applymap m) rs)
        applymap m (LocalVariable s) = case Map.lookup s m of
                                         Just r  -> let r' = applymap m r in if r==r' then r' else applymap m r'
                                         Nothing -> LocalVariable s
        applymap m (Concat rs)       = Concat (map (applymap m) rs)
        applymap m (BitConst bs)     = BitConst bs
        applymap m (Slice r l h)     = Slice (applymap m r) l h
        applymap m (Conditional cs)  = Conditional $ map (\ (c,r) -> (applymapC m c,applymap m r)) cs
        applymapC m (CondEq r1 r2)  = CondEq (applymap m r1) (applymap m r2)
        applymapC m (CondAnd c1 c2) = CondAnd (applymapC m c1) (applymapC m c2)
        applymapC m CondTrue        = CondTrue
        simplifyRHS (FunCall s rs)    = FunCall s (map simplifyRHS rs)
        simplifyRHS (LocalVariable s) = LocalVariable s
        simplifyRHS r@(Concat rs)     = let rs'  = map simplifyRHS rs
                                            rs'' = smashConcat rs'
                                            r'   = case rs'' of
                                                     []  -> BitConst []
                                                     [r] -> r
                                                     _   -> Concat rs''
                                        in if r == r' then r' else simplifyRHS r'
        simplifyRHS (BitConst bs)     = BitConst bs
        simplifyRHS (Slice (BitConst bs) l h) = simplifyRHS $ BitConst $ reverse $ drop (length bs - (h+1)) $ reverse $ drop l bs
        simplifyRHS (Slice r l h)             = let r' = simplifyRHS r
                                                in if r /= r' then simplifyRHS (Slice r' l h)
                                                              else Slice r' l h
        simplifyRHS (Conditional [(CondTrue,r)]) = simplifyRHS r
        simplifyRHS (Conditional cs)             = let trimCs ((CondTrue,r):cs)           = [(CondTrue,r)]
                                                       trimCs ((c,r):cs) | necessarilyFalse c = trimCs cs
                                                                         | otherwise          = (c,r) : trimCs cs
                                                       trimCs []                          = []
                                                       cs'  = trimCs cs
                                                       cs'' = map (\ (c,r) -> (simplifyCond c,simplifyRHS r)) cs'
                                                       in if cs'' == cs then Conditional cs''
                                                                        else simplifyRHS $ Conditional cs''
        simplifyCond c@(CondEq r1 r2) | r1 == r2  = CondTrue
                                      | otherwise = let c' = CondEq (simplifyRHS r1) (simplifyRHS r2)
                                                    in  if c' /= c then simplifyCond c' else c'
        simplifyCond (CondAnd c CondTrue)         = simplifyCond c
        simplifyCond (CondAnd CondTrue c)         = simplifyCond c
        simplifyCond c@(CondAnd c1 c2)            = let c' = CondAnd (simplifyCond c1) (simplifyCond c2)
                                                    in  if c' /= c then simplifyCond c' else c'
        simplifyCond CondTrue                     = CondTrue
        necessarilyFalse (CondEq (BitConst b1) (BitConst b2)) | b1 /= b2 = True
        necessarilyFalse _                                               = False
        smashConcat (BitConst bs1:BitConst bs2:rs) = smashConcat (BitConst (bs1++bs2):rs)
        smashConcat (BitConst []:rs)               = smashConcat rs
        smashConcat (Concat rs:rs')                = smashConcat (rs++rs')
        smashConcat (r:rs)                         = r:smashConcat rs
        smashConcat []                             = []

genProg :: Map (Id RWCExp) DefnSort -> VM String
genProg m = do bdgs <- initBindings m
               trace (show bdgs) $ localBindings (Map.union bdgs) $ do
                 v_start  <- genStart
                 sw       <- getStateWidth
                 iw       <- getInputWidth
                 ow       <- getOutputWidth
                 let kts  =  Map.toList m
                 v_funs   <- mapM genOne kts
                 optimize
                 as       <- getAssignments
                 ds       <- getDeclarations
                 (assn:assns) <- liftM concat $ mapM nextstate_sig_assign (Map.toList bdgs)
                 let entity_name = "rewire" -- FIXME: customizable?
                 v_protos <- mapM genOneProto kts
                 return ("library ieee;\n" ++
                         "use ieee.std_logic_1164.all;\n" ++
                         "entity " ++ entity_name ++ " is\n" ++
                         "  port (clk : in std_logic;\n" ++
                         "        sm_input : in std_logic_vector(0 to " ++ show (iw-1) ++ ");\n" ++
                         "        sm_output : out std_logic_vector(0 to " ++ show (ow-1) ++ "));\n" ++
                         "end rewire;\n" ++
                         "architecture behavioral of " ++ entity_name ++ " is\n" ++
                         concat v_protos ++
                         v_start ++
                         concat v_funs ++
                         "  signal sm_state : std_logic_vector(0 to " ++ show (sw-1) ++ ") := sm_state_initial;\n" ++
                         concatMap (("  "++) . (++"\n") . renderDeclarationForSignals) (reverse ds) ++
                         "begin\n" ++
                         "  sm_output <= sm_state(0 to " ++ show (ow-1) ++ ");\n" ++
                         concatMap (("  "++) . (++"\n") . renderAssignmentForSignals) (reverse as) ++
                         "  process(clk)\n" ++
                         "  begin\n" ++
                         "    if rising_edge(clk) then\n" ++
                         "        " ++ assn ++ concatMap (" els"++) assns ++ " else sm_state <= sm_state_initial; end if;\n" ++
                         "    end if;\n" ++
                         "  end process;\n" ++
                         "end behavioral;\n")
   where genOne (k,DefnBitty) = do md <- queryG k
                                   case md of
                                     Just d  -> genBittyDefn d
                                     Nothing -> fail $ "genProg: No definition for bitty function " ++ show k
         genOne (k,DefnCont)  = do md <- queryG k
                                   case md of
                                     Just d  -> genContDefn d >> return ""
                                     Nothing -> fail $ "genProg: No definition for cont function " ++ show k
         genOneProto (k,DefnBitty) = do md <- queryG k
                                        case md of
                                          Just d  -> genBittyDefnProto d
                                          Nothing -> fail $ "genProg: No definition for bitty function " ++ show k
         genOneProto _ = return ""

runVM :: RWCProg -> VM a -> (a,VMState)
runVM p phi = runIdentity $
               runStateT (runReaderT (runRWT p phi) env0) state0
  where state0 = VMState { signalCounter      = 0,
                           assignments        = [],
                           declarations       = [], 
                           tyWidthCache       = Map.empty, 
                           inputWidthCache    = Nothing,
                           outputWidthCache   = Nothing,
                           stateWidthCache    = Nothing,
                           stateTagWidthCache = Nothing }
        env0   = VMEnv { bindings = Map.empty }
        
cmdToVHDL :: TransCommand
cmdToVHDL _ p = case checkProg' p of
                  Left e  -> (Nothing,Just $ "failed normal-form check: " ++ show e)
                  Right m -> (Nothing,Just $ fst $ runVM p (genProg m))
