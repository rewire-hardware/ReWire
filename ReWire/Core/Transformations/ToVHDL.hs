module ReWire.Core.Transformations.ToVHDL where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Monad
import ReWire.Core.Transformations.CheckNF
import ReWire.Core.Transformations.Types
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (intercalate,findIndex,find)
import Data.Maybe (fromJust)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Unbound.LocallyNameless

data VMState = VMState { signalCounter      :: Int } deriving Show
data VMEnv   = VMEnv   { bindings           :: Map (Name RWCExp) NameInfo } deriving Show
data VMOut   = VMOut   { signalAssignments  :: [String],
                         signalDeclarations :: [String] } deriving Show
type VM = ReaderT VMEnv (WriterT VMOut (StateT VMState RW))

data NameInfo = Global String | Local String | NotBound deriving (Eq,Show)

getSignalCounter = get >>= return . signalCounter
modifySignalCounter f = modify (\ s -> s { signalCounter = f (signalCounter s) })
putSignalCounter = modifySignalCounter . const

instance Monoid VMOut where
  mempty  = VMOut { signalAssignments = [], signalDeclarations = [] }
  mappend (VMOut { signalAssignments = sa1, signalDeclarations = sd1 })
          (VMOut { signalAssignments = sa2, signalDeclarations = sd2 })
            = VMOut { signalAssignments  = sa1 `mappend` sa2,
                      signalDeclarations = sd1 `mappend` sd2 }

tellSignalAssignments o = tell (VMOut { signalAssignments = o, signalDeclarations = [] })
tellSignalDeclarations o = tell (VMOut { signalAssignments = [], signalDeclarations = o })

askBindings :: VM (Map (Name RWCExp) NameInfo)
askBindings = ask >>= return . bindings

localBindings :: (Map (Name RWCExp) NameInfo -> Map (Name RWCExp) NameInfo) -> VM a -> VM a
localBindings f = local (\ e -> e { bindings = f (bindings e) })

askNameInfo :: Name RWCExp -> VM NameInfo
askNameInfo n = do bindings <- askBindings
                   return $ maybe NotBound id (Map.lookup n bindings)

freshSignal :: Int -> VM String
freshSignal i = do ctr <- getSignalCounter
                   putSignalCounter (ctr+1)
                   let sn = "tmp_" ++ show ctr
                   tellSignalDeclarations ["signal " ++ sn ++ " : std_logic_vector (0 to " ++ show (i-1) ++ ") := \"" ++ stdlConstFromIntegral i 0 ++ "\";"]
                   return sn

freshSignalTy :: RWCTy -> VM String
freshSignalTy = freshSignal <=< tyWidth

emitSignalAssignment s = tellSignalAssignments [s]

--genBittyLiteral :: RWCLit -> VM String
--genBittyLiteral (RWCLitInteger 

genBittyExp :: RWCExp -> VM String
genBittyExp e@(RWCApp t _ _)   = let (ef:es) = flattenApp e
                                 in case ef of
                                   RWCVar _ i -> do n_i  <- askNameInfo i
                                                    s    <- freshSignalTy t
                                                    s_es <- mapM genBittyExp es
                                                    case n_i of
                                                      Global n -> emitSignalAssignment (s ++ " <= " ++ n ++ "(" ++ intercalate "," s_es ++ ");")
                                                      Local _  -> fail $ "genBittyExp: locally bound variable is applied as function: " ++ show i
                                                      NotBound -> fail $ "genBittyExp: unbound variable: " ++ show i
                                                    return s
                                   RWCCon _ i -> do s    <- freshSignalTy t
                                                    s_es <- mapM genBittyExp es
                                                    emitSignalAssignment (s ++ " <= " ++ i ++ "(" ++ intercalate "," s_es ++ ");")
                                                    return s
                                   _          -> fail $ "genBittyExp: malformed application head: " ++ show e
genBittyExp (RWCCon t i)       = do s <- freshSignalTy t
                                    emitSignalAssignment (s ++ " <= " ++ i ++ "();")
                                    return s
genBittyExp (RWCVar t i)       = do n_i <- askNameInfo i
                                    s   <- freshSignalTy t
                                    case n_i of
                                      Global n -> return (s ++ " <= " ++ n ++ "();")
                                      Local n  -> return (s ++ " <= " ++ n ++ ";")
                                      NotBound -> fail $ "genBittyExp: unbound variable: " ++ show i
--genBittyExp (RWCLiteral _ l)   = genBittyLiteral l
genBittyExp (RWCCase t e alts) = do s_scrut     <- genBittyExp e
                                    s           <- freshSignalTy t
                                    scond_alts  <- mapM (genBittyAlt s_scrut (typeOf e)) alts
                                    emitSignalAssignment (s ++ " <= " ++ chainSconds scond_alts ++ ";")
                                    return s
genBittyExp e                  = fail $ "genBittyExp: malformed bitty expression: " ++ show e

getTagWidth :: Identifier -> VM Int
getTagWidth i = do dds <- lift $ lift $ askDataDecls
                   case find (\(RWCData i' _) -> i==i') dds of
                     Nothing            -> fail $ "getTagWidth: unknown constructor " ++ i
                     Just (RWCData _ b) -> lunbind b $ \ (_,dcs) -> return (nBits (length dcs-1))
  where nBits 0 = 0
        nBits n = nBits (n `quot` 2) + 1

breakData :: Identifier -> String -> RWCTy -> VM (String,[(String,RWCTy)])
breakData i s_scrut t_scrut = do tagWidth         <- getTagWidth i
                                 s_tag            <- freshSignal tagWidth
                                 emitSignalAssignment (s_tag ++ " <= " ++ s_scrut ++ "[0:" ++ show (tagWidth-1) ++ "];")
                                 fieldTys         <- getFieldTys i t_scrut
                                 fieldWidths      <- mapM tyWidth fieldTys
                                 s_fields         <- mapM freshSignal fieldWidths
                                 let fieldOffsets    =  scanl (+) tagWidth fieldWidths
                                     ranges []       =  []
                                     ranges [n]      =  []
                                     ranges (n:m:ns) =  (n,m-1) : ranges (m:ns)
                                     fieldRanges     =  ranges fieldOffsets
                                     emitOne s (l,h) =  emitSignalAssignment (s ++ " <= " ++ s_scrut ++ "[" ++ show l ++ ":" ++ show h ++ "];")
                                 zipWithM_ emitOne s_fields fieldRanges
                                 return (s_tag,zip s_fields fieldTys)

stdlConstFromIntegral :: Integral a => Int -> a -> String
stdlConstFromIntegral 0 _     = ""
stdlConstFromIntegral width n = stdlConstFromIntegral (width-1) (n`div`2) ++ thisBit where thisBit = if odd n then "1" else "0"

getTag :: Identifier -> VM String
getTag i = do tagWidth <- getTagWidth i
              mdd <- lift $ lift $ askConDataDecl i
              case mdd of
                Nothing            -> fail $ "getTag: unknown constructor " ++ i
                Just (RWCData _ b) -> lunbind b $ \ (_,dcs) ->
                  case findIndex (\ (RWCDataCon i' _) -> i==i') dcs of
                    Just pos -> return (stdlConstFromIntegral tagWidth pos)
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

genBittyAlt :: String -> RWCTy -> RWCAlt -> VM (String,String)
genBittyAlt s_scrut t_scrut (RWCAlt b) = lunbind b $ \ (p,e) -> do (cond,bdgs) <- genBittyPat s_scrut t_scrut p
                                                                   s           <- localBindings (Map.union bdgs) $ genBittyExp e
                                                                   return (cond,s)

genBittyPat :: String -> RWCTy -> RWCPat -> VM (String,Map (Name RWCExp) NameInfo) -- (condition for match, resulting bindings)
genBittyPat s_scrut t_scrut (RWCPatCon i pats)      = do (s_tag,st_fields) <- breakData i s_scrut t_scrut
                                                         let s_fields      =  map fst st_fields
                                                             t_fields      =  map snd st_fields
                                                         tagValue          <- getTag i
                                                         condbinds_pats    <- zipWithM3 genBittyPat s_fields t_fields pats
                                                         let cond_pats     =  map fst condbinds_pats
                                                             binds_pats    =  map snd condbinds_pats
                                                         return (s_tag ++ " = " ++ tagValue ++ " and " ++ intercalate " and " cond_pats,
                                                                 foldr Map.union Map.empty binds_pats)
--genBittyPat s_scrut (RWCPatLiteral l) = 
genBittyPat s_scrut t_scrut (RWCPatVar (Embed t) n) = do s <- freshSignalTy t_scrut
                                                         emitSignalAssignment (s ++ " <= " ++ s_scrut ++ ";")
                                                         return ("true",Map.singleton n (Local s))

chainSconds :: [(String,String)] -> String
chainSconds ((s,cond):sconds) = s ++ " when " ++ cond ++ " else " ++ chainSconds sconds
chainSconds []                = "(others => '0')"

genBittyDefn :: RWCDefn -> VM String
genBittyDefn (RWCDefn n (Embed b)) = lunbind b $ \(tvs,(t,e_)) ->
  do (s,w) <- listen $ censor (const mempty) $
                flattenLambda e_ $ \ (nts,e) ->
                  do bdgs <- mapM (\ (n,t) -> freshSignalTy t >>= \ s -> return (n,Local s)) nts
                     localBindings (Map.union $ Map.fromList bdgs) (genBittyExp e)
     return s -- FIXME

genProg :: VM ()
genProg = do res <- lift $ lift $ lift $ checkProg'
             case res of
               Left e -> fail $ "genProg: Error in checking normal form: " ++ e
               Right m -> do
                 let kts =  Map.toList m
                 mapM_ genOne kts
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
  where s = show (runVM p genProg)