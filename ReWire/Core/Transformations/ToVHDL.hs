module ReWire.Core.Transformations.ToVHDL where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Monad
import ReWire.Core.Transformations.CheckNF
import ReWire.Core.Transformations.Types
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (intercalate,findIndex,find)
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
data VMState = VMState { signalCounter :: Int } deriving Show
data VMEnv   = VMEnv   { bindings :: Map (Name RWCExp) NameInfo } deriving Show
data VMOut   = VMOut   { assignments  :: [Assignment],
                         declarations :: [Declaration] } deriving Show
type VM = ReaderT VMEnv (WriterT VMOut (StateT VMState RW))

data NameInfo = Global String | Local String | NotBound deriving (Eq,Show)

getSignalCounter = get >>= return . signalCounter
modifySignalCounter f = modify (\ s -> s { signalCounter = f (signalCounter s) })
putSignalCounter = modifySignalCounter . const

instance Monoid VMOut where
  mempty  = VMOut { assignments = [], declarations = [] }
  mappend (VMOut { assignments = sa1, declarations = sd1 })
          (VMOut { assignments = sa2, declarations = sd2 })
            = VMOut { assignments  = sa1 `mappend` sa2,
                      declarations = sd1 `mappend` sd2 }

tellAssignments o = tell (VMOut { assignments = o, declarations = [] })
tellDeclarations o = tell (VMOut { assignments = [], declarations = o })

askBindings :: VM (Map (Name RWCExp) NameInfo)
askBindings = ask >>= return . bindings

localBindings :: (Map (Name RWCExp) NameInfo -> Map (Name RWCExp) NameInfo) -> VM a -> VM a
localBindings f = local (\ e -> e { bindings = f (bindings e) })

askNameInfo :: Name RWCExp -> VM NameInfo
askNameInfo n = do bindings <- askBindings
                   return $ maybe NotBound id (Map.lookup n bindings)

freshName :: VM String
freshName = do ctr <- getSignalCounter
               putSignalCounter (ctr+1)
               return ("tmp_"++show ctr)
               
freshTmp :: Int -> VM String
freshTmp i = do sn <- freshName
                emitDeclaration (sn,i)
                return sn

freshTmpTy :: RWCTy -> VM String
freshTmpTy = freshTmp <=< tyWidth

emitAssignment :: Assignment -> VM ()
emitAssignment = tellAssignments . (:[])

emitDeclaration :: Declaration -> VM ()
emitDeclaration = tellDeclarations . (:[])

--genBittyLiteral :: RWCLit -> VM String
--genBittyLiteral (RWCLitInteger 

genBittyExp :: RWCExp -> VM String
genBittyExp e@(RWCApp t _ _)   = let (ef:es) = flattenApp e
                                 in case ef of
                                   RWCVar _ i -> do n_i  <- askNameInfo i
                                                    s_es <- mapM genBittyExp es
                                                    s    <- freshTmpTy t
                                                    case n_i of
                                                      Global n -> emitAssignment (s,FunCall n s_es)
                                                      Local _  -> fail $ "genBittyExp: locally bound variable is applied as function: " ++ show i
                                                      NotBound -> fail $ "genBittyExp: unbound variable: " ++ show i
                                                    return s
                                   RWCCon _ i -> do tci      <- getDataConTyCon i
                                                    tagWidth <- getTagWidth tci
                                                    s_tag    <- freshTmp tagWidth
                                                    s_es     <- mapM genBittyExp es
                                                    tag      <- getTag i
                                                    emitAssignment (s_tag,BitConst tag)
                                                    s        <- freshTmpTy t
                                                    emitAssignment (s,Concat (s_tag:s_es))
                                                    return s
                                   _          -> fail $ "genBittyExp: malformed application head: " ++ show e
genBittyExp (RWCCon t i)       = do tag <- getTag i
                                    s   <- freshTmpTy t
                                    emitAssignment (s,BitConst tag)
                                    return s
genBittyExp (RWCVar t i)       = do n_i <- askNameInfo i
                                    s   <- freshTmpTy t
                                    case n_i of
                                      Global n -> emitAssignment (s,FunCall n [])
                                      Local n  -> emitAssignment (s,LocalVariable n)
                                      NotBound -> fail $ "genBittyExp: unbound variable: " ++ show i
                                    return s
--genBittyExp (RWCLiteral _ l)   = genBittyLiteral l
genBittyExp (RWCCase t e alts) = do s_scrut     <- genBittyExp e
                                    scond_alts  <- mapM (genBittyAlt s_scrut (typeOf e)) alts
                                    s           <- freshTmpTy t
                                    emitAssignment (s,Conditional scond_alts)
                                    return s
genBittyExp e                  = fail $ "genBittyExp: malformed bitty expression: " ++ show e

getTagWidth :: Identifier -> VM Int
getTagWidth i = do dds <- lift $ lift $ askDataDecls
                   case find (\(RWCData i' _) -> i==i') dds of
                     Nothing            -> fail $ "getTagWidth: unknown constructor " ++ i
                     Just (RWCData _ b) -> lunbind b $ \ (_,dcs) -> return (nBits (length dcs-1))
  where nBits 0 = 0
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
                                 s_tag            <- freshTmp tagWidth
                                 emitAssignment (s_tag,Slice s_scrut 0 (tagWidth-1))
                                 fieldTys         <- getFieldTys i t_scrut
                                 fieldWidths      <- mapM tyWidth fieldTys
                                 s_fields         <- mapM freshTmp fieldWidths
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
                                                                   s           <- localBindings (Map.union bdgs) $ genBittyExp e
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
                                                         s_tagtest         <- freshTmp tagWidth
                                                         emitAssignment(s_tagtest,BitConst tagValue)
                                                         return (foldr CondAnd (CondEq s_tag s_tagtest) cond_pats,
                                                                 foldr Map.union Map.empty binds_pats)
--genBittyPat s_scrut (RWCPatLiteral l) = 
genBittyPat s_scrut t_scrut (RWCPatVar (Embed t) n) = do s <- freshTmpTy t_scrut
                                                         emitAssignment (s,LocalVariable s_scrut)
                                                         return (CondTrue,Map.singleton n (Local s))

chainSconds :: [(String,String)] -> String
chainSconds ((cond,s):sconds) = s ++ " when " ++ cond ++ " else " ++ chainSconds sconds
chainSconds []                = "(others => '0')"

bitToChar Zero = '0'
bitToChar One  = '1'

renderAssignmentForVariables :: Assignment -> String
renderAssignmentForVariables (s,FunCall f ss)    = s ++ " := " ++ f ++ "(" ++ intercalate "," ss ++ ");"
renderAssignmentForVariables (s,LocalVariable x) = s ++ " := " ++ x ++ ";"
renderAssignmentForVariables (s,Concat ss)       = s ++ " := " ++ intercalate " & " ss ++ ";"
renderAssignmentForVariables (s,BitConst bs)     = s ++ " := \"" ++ map bitToChar bs ++ "\";"
renderAssignmentForVariables (s,Slice t l h)     = s ++ " := " ++ t ++ "[" ++ show l ++ ":" ++ show h ++ "];"
renderAssignmentForVariables (s,Conditional cs)  = s ++ " := " ++ concatMap renderOneCase cs ++ "(others => '0');"
  where renderOneCase (c,sc) = sc ++ " when " ++ renderCond c ++ " else "
        renderCond (CondEq s1 s2)  = s1 ++ " = " ++ s2
        renderCond (CondAnd c1 c2) = "(" ++ renderCond c1 ++ " and " ++ renderCond c2 ++ ")"
        renderCond CondTrue        = "true"

renderDeclarationForVariables :: Declaration -> String
renderDeclarationForVariables (s,i) = "variable " ++ s ++ " : std_logic_vector(0 to " ++ show (i-1) ++ ") := (others => '0');"

-- FIXME: don't just show name, look up in table
genBittyDefn :: RWCDefn -> VM String
genBittyDefn (RWCDefn n (Embed b)) =
  lunbind b $ \(tvs,(t,e_)) ->
  flattenLambda e_ $ \ (nts,e) ->
    do let freshArgumentTy (n,t) = do s <- freshName
                                      w <- tyWidth t
                                      return ((n,Local s),s ++ " : std_logic_vector(0 to " ++ show (w-1) ++")")
       wr  <- tyWidth (typeOf e)
       bps <- mapM freshArgumentTy nts
       let (bdgs,ps) = unzip bps
       (s,w) <- listen $ localBindings (Map.union $ Map.fromList bdgs) (genBittyExp e)
       return ("function " ++ show n ++ "(" ++ intercalate " ; " ps ++ ")\n" ++
               "  returns std_logic_vector(0 to " ++ show (wr-1) ++ ")\n" ++
               "is\n" ++
               concatMap (("  "++) . (++"\n") . renderDeclarationForVariables) (declarations w) ++
               "begin\n" ++
               concatMap (("  "++) . (++"\n") . renderAssignmentForVariables) (assignments w) ++
               "  return " ++ s ++ ";\n" ++
               "end " ++ show n ++ ";\n")

genProg :: VM [String]
genProg = do res <- lift $ lift $ lift $ checkProg'
             case res of
               Left e -> fail $ "genProg: Error in checking normal form: " ++ e
               Right m -> do
                 let kts =  Map.toList m
                 mapM genOne kts
   where genOne (k,DefnBitty) = do md <- lift $ lift $ askDefn k
                                   case md of
                                     Just d  -> genBittyDefn d
                                     Nothing -> fail $ "genProg: No definition for bitty function " ++ show k
         genOne _             = return ""

runVM :: RWCProg -> VM a -> ((a,VMOut),VMState)
runVM p phi = runRW p $
               (runStateT
                 (runWriterT
                   (runReaderT phi env0)) state0)
  where state0 = VMState { signalCounter = 0 }
        env0   = VMEnv { bindings = Map.empty }
        
cmdToVHDL :: TransCommand
cmdToVHDL _ p = (Nothing,Just s)
  where s = intercalate "\n\n" $ fst $ fst $ runVM p genProg