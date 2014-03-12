module ReWire.Core.Transformations.CheckNF where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.Monad
import Unbound.LocallyNameless hiding (empty)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Graph
import Data.List (nub)
import Data.Maybe (fromJust)
import Control.Monad.State
import Control.Monad.Error
import Prelude hiding (lookup)

data DefnSort = DefnBitty | DefnPause | DefnCont deriving Show
type NFM = ErrorT NFMError (StateT NFMState RW)
data NFMState = NFM { visited :: Map (Name RWCExp) DefnSort, 
                      cpx     :: Set Identifier }         -- really this should be env not state, but whatever
type NFMError = String

getVisited = get >>= return . visited
getCpxTys = get >>= return . cpx
modifyVisited f = modify (\ s -> s { visited = f (visited s) })
modifyCpxTys f = modify (\ s -> s { cpx = f (cpx s) })

type TyGraph = [(Identifier,Identifier,[Identifier])]

buildTyGraph :: LFresh m => [RWCData] -> m TyGraph
buildTyGraph = liftM (("(->)","(->)",[]):) . mapM mkNode
  where mkNode (RWCData i b)     = lunbind b (\(tvs,dcs) -> return (i,i,concatMap dcOuts dcs))
        dcOuts (RWCDataCon i ts) = concatMap tyOuts ts
        tyOuts (RWCTyApp t1 t2)  = tyOuts t1 ++ tyOuts t2
        tyOuts (RWCTyCon i)      = [i]
        tyOuts (RWCTyVar _)      = []

-- Find "complex" type constructors: that is, all type constructors for types
-- that either are recursive, functional, or contain a complex type.
cpxTys :: [RWCData] -> Set Identifier
cpxTys dds = let edges                  = runLFreshM $ buildTyGraph dds
                 tcs                    = map (\(i,_,_) -> i) edges
                 (graph,vxmap,kmap)     = graphFromEdges edges
                 sccs                   = stronglyConnComp edges
                 sccRecs (AcyclicSCC _) = []
                 sccRecs (CyclicSCC ts) = ts
                 recs                   = "(->)" : concatMap sccRecs sccs
                 cpxs                   = nub $ recs ++ filter (\ i -> any (\ i' -> path graph (fromJust (kmap i)) i') (map (fromJust . kmap) recs)) tcs
             in Set.fromList cpxs

checkDefnIsPause :: Name RWCExp -> NFM ()
checkDefnIsPause n = do vset <- getVisited
                        case Map.lookup n vset of
                           Just DefnPause -> return ()
                           Just DefnBitty -> throwError $ "checkDefnIsPause: " ++ show n ++ " has to be bitty"
                           Just DefnCont  -> throwError $ "checkDefnIsPause: " ++ show n ++ " has to be a continuer"
                           Nothing        -> do
                             md <- askDefn n
                             case md of
                               Nothing ->
                                 throwError $ "checkDefnIsPause: " ++ show n ++ " is undefined"
                               Just (RWCDefn _ (Embed b)) -> lunbind b $ \(tvs,(t,e)) -> do
                                 modifyVisited (Map.insert n DefnPause)
                                 let (targs,tres) = flattenArrow t
                                 mapM_ checkTyIsBitty targs
                                 checkTyIsPause tres
                                 flattenLambda e $ \(xts,eb) -> do
                                   when (length xts /= length targs)
                                        (throwError $ "checkDefnIsPause: number of lambda-bound variables does not match arity of function (" ++ show (length xts) ++ " vars, arity " ++ show (length targs) ++ ")")
                                   checkExprIsPause eb

checkTyIsPause :: RWCTy -> NFM ()
checkTyIsPause (RWCTyApp (RWCTyApp (RWCTyApp (RWCTyCon "React") ti) to) ta) =
  do checkTyIsBitty ti
     checkTyIsBitty to
     checkTyIsBitty ta
     -- FIXME: check to make sure ti, to, ta match what's been seen so far.
checkTyIsPause t =
  throwError $ "checkTyIsPause: type is not of form `React t1 t2 t3': " ++ show t

checkExprIsPause :: RWCExp -> NFM ()
checkExprIsPause (RWCCase t e alts)                           = do checkTyIsPause t
                                                                   checkExprIsBitty e
                                                                   mapM_ checkAltIsPause alts
checkExprIsPause (RWCApp t (RWCApp _ (RWCCon _ "P") esig) ek) = do checkTyIsPause t
                                                                   checkExprIsBitty esig
                                                                   checkExprIsContApp ek
checkExprIsPause e                                            = throwError $ "checkExprIsPause: malformed pause expression: " ++ show e

checkAltIsPause :: RWCAlt -> NFM ()
checkAltIsPause (RWCAlt b) = lunbind b (\(_,e) -> checkExprIsPause e)

checkExprIsContApp :: RWCExp -> NFM ()
checkExprIsContApp e = do checkTyIsCont (typeOf e)
                          let (ef:eargs) = flattenApp e
                          mapM_ checkExprIsBitty eargs
                          case ef of
                            RWCVar _ n -> checkDefnIsCont n
                            _          -> throwError $ "checkExprIsContApp: malformed continuation application: " ++ show e

checkDefnIsCont :: Name RWCExp -> NFM ()
checkDefnIsCont n = do vset <- getVisited
                       case Map.lookup n vset of
                           Just DefnCont  -> return ()
                           Just DefnPause -> throwError $ "checkDefnIsCont: " ++ show n ++ " has to be a pauser"
                           Just DefnBitty -> throwError $ "checkDefnIsCont: " ++ show n ++ " has to be bitty"
                           Nothing        -> do
                             md <- askDefn n
                             case md of
                               Nothing ->
                                 throwError $ "checkDefnIsCont: " ++ show n ++ " is undefined"
                               Just (RWCDefn _ (Embed b)) -> lunbind b $ \(tvs,(t,e)) -> do
                                 modifyVisited (Map.insert n DefnCont)
                                 let (targs,tres) = flattenArrow t
                                 when (length targs < 1)
                                      (throwError $ "checkDefnIsCont: not enough arguments in type")
                                 mapM_ checkTyIsBitty targs
                                 -- FIXME: check that last element of targs matches what's been seen so far
                                 checkTyIsPause tres
                                 flattenLambda e $ \(xts,eb) -> do
                                   when (length xts /= length targs)
                                        (throwError $ "checkDefnIsCont: number of lambda-bound variables does not match arity of function (" ++ show (length xts) ++ " vars, arity " ++ show (length targs) ++ ")")
                                   checkExprIsCont eb

checkTyIsCont :: RWCTy -> NFM ()
checkTyIsCont (RWCTyApp (RWCTyApp (RWCTyCon "(->)") t1) t2) = do checkTyIsBitty t1
                                                                 checkTyIsPause t2
                                                                 -- FIXME: make sure t1 matches what's been seen so far
checkTyIsCont t                                             = throwError $ "checkTyIsCont: malformed continuation type: " ++ show t

checkExprIsCont :: RWCExp -> NFM ()
checkExprIsCont (RWCCase t e alts) = do checkTyIsPause t
                                        checkExprIsBitty e
                                        mapM_ checkAltIsCont alts
checkExprIsCont e@(RWCApp {})      = do checkTyIsPause (typeOf e)
                                        let (ef:eargs) = flattenApp e
                                        mapM_ checkExprIsBitty eargs
                                        case ef of
                                          RWCVar _ n -> checkDefnIsPause n
                                          _          -> throwError $ "checkExprIsCont: malformed application head " ++ show ef
checkExprIsCont e                  = throwError $ "checkExprIsCont: malformed pause expression: " ++ show e

checkAltIsCont :: RWCAlt -> NFM ()
checkAltIsCont (RWCAlt b) = lunbind b (\(_,e) -> checkExprIsCont e)

checkDefnIsBitty :: Name RWCExp -> NFM ()
checkDefnIsBitty n = do vset <- getVisited
                        case Map.lookup n vset of
                           Just DefnCont  -> throwError $ "checkDefnIsBitty: " ++ show n ++ " has to be a continuer"
                           Just DefnPause -> throwError $ "checkDefnIsBitty: " ++ show n ++ " has to be a pauser"
                           Just DefnBitty -> return ()
                           Nothing        -> do
                             md <- askDefn n
                             case md of
                               Nothing -> return () -- FIXME: I think this happens ONLY when it's locally bound
--                                 throwError $ "checkDefnIsBitty: " ++ show n ++ " is undefined"
                               Just (RWCDefn _ (Embed b)) -> lunbind b $ \(tvs,(t,e)) -> do
                                 modifyVisited (Map.insert n DefnBitty)
                                 let (targs,tres) = flattenArrow t
                                 mapM_ checkTyIsBitty targs
                                 checkTyIsBitty tres
                                 flattenLambda e $ \(xts,eb) -> do
                                   when (length xts /= length targs)
                                        (throwError $ "checkDefnIsBitty: number of lambda-bound variables does not match arity of function (" ++ show (length xts) ++ " vars, arity " ++ show (length targs) ++ ")")
                                   checkExprIsBitty eb

checkTyIsBitty :: RWCTy -> NFM ()
checkTyIsBitty t@(RWCTyApp {}) = mapM_ checkTyIsBitty (flattenTyApp t)
checkTyIsBitty (RWCTyVar v)    = throwError $ "checkTyIsBitty: encountered polymorphic type"
checkTyIsBitty (RWCTyCon i)    = do cpx <- getCpxTys
                                    if i `Set.member` cpx
                                       then throwError $ "checkTyIsBitty: encountered complex type: " ++ i
                                       else return ()

checkExprIsBitty :: RWCExp -> NFM ()
checkExprIsBitty e@(RWCApp {})      = do checkTyIsBitty (typeOf e)
                                         let (ef:eargs) = flattenApp e
                                         mapM_ checkExprIsBitty eargs
                                         case ef of
                                           RWCVar _ n     -> checkDefnIsBitty n
                                           RWCCon _ i     -> return ()
                                           RWCLiteral _ l -> checkLiteralIsBitty l
                                           _              -> throwError $ "checkExprIsBitty: malformed application head " ++ show ef
checkExprIsBitty (RWCVar t n)       = checkTyIsBitty t >> checkDefnIsBitty n -- checkTy is redundant I think
checkExprIsBitty (RWCCon t _)       = checkTyIsBitty t
checkExprIsBitty (RWCLiteral _ l)   = checkLiteralIsBitty l
checkExprIsBitty (RWCCase t e alts) = do checkTyIsBitty t
                                         checkExprIsBitty e
                                         mapM_ checkAltIsBitty alts

checkAltIsBitty :: RWCAlt -> NFM ()
checkAltIsBitty (RWCAlt b) = lunbind b (\(_,e) -> checkExprIsBitty e)

checkLiteralIsBitty :: RWCLit -> NFM ()
checkLiteralIsBitty (RWCLitInteger _) = return ()
checkLiteralIsBitty (RWCLitChar _)    = return ()
checkLiteralIsBitty (RWCLitFloat _)   = throwError $ "checkLiteralIsBitty: floating point literal encountered"

checkProg :: NFM (Map (Name RWCExp) DefnSort)
checkProg = do dds <- askDataDecls
               modifyCpxTys (const (cpxTys dds))
               checkDefnIsPause (s2n "main")
               getVisited

runNFM :: RWCProg -> NFM a -> Either NFMError a
runNFM p phi = fst $ runRW p (runStateT (runErrorT phi) (NFM { visited = Map.empty, cpx = Set.empty }))

cmdCheckNF :: TransCommand
cmdCheckNF _ p = (Nothing,Just s)
  where s = show (runNFM p checkProg)
