module ReWire.Core.Transformations.ToVHDL where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Monad
import ReWire.Core.Transformations.CheckNF
import ReWire.Core.Transformations.Types
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (intercalate)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Unbound.LocallyNameless

data VMState = VMState { signalCounter :: Int }
data VMEnv   = VMEnv   { bindings :: Map (Name RWCExp) NameInfo }
type VM = ReaderT VMEnv (WriterT [String] (StateT VMState RW))

data NameInfo = Global String | Local String | NotBound deriving (Eq,Show)

getSignalCounter = get >>= return . signalCounter
modifySignalCounter f = modify (\ s -> s { signalCounter = f (signalCounter s) })
putSignalCounter = modifySignalCounter . const

askBindings :: VM (Map (Name RWCExp) NameInfo)
askBindings = ask >>= return . bindings

localBindings :: (Map (Name RWCExp) NameInfo -> Map (Name RWCExp) NameInfo) -> VM a -> VM a
localBindings f = local (\ e -> e { bindings = f (bindings e) })

askNameInfo :: Name RWCExp -> VM NameInfo
askNameInfo n = do bindings <- askBindings
                   return $ maybe NotBound id (Map.lookup n bindings)

-- FIXME: the following don't do at all what they're supposed to
freshSignal :: Int -> VM String
freshSignal _ = do ctr <- getSignalCounter
                   putSignalCounter (ctr+1)
                   return $ "tmp_" ++ show ctr

freshSignalTy :: RWCTy -> VM String
freshSignalTy _ = do ctr <- getSignalCounter
                     putSignalCounter (ctr+1)
                     return $ "tmp_" ++ show ctr

emitSignalAssignment s = tell [s]

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
getTagWidth i = do mdd <- lift $ lift $ askConDataDecl i -- FIXME: need to lift MonadReWire thru WriterT
                   case mdd of
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
                                 
getTag = undefined -- FIXME
tyWidth = undefined -- FIXME
getFieldTys = undefined -- FIXME

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