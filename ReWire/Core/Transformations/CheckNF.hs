{-# OPTIONS -fwarn-incomplete-patterns #-}

-- FIXME: Need to make sure bitty defns are not recursive.
-- FIXME: Need to make sure pattern matching is exhaustive.
module ReWire.Core.Transformations.CheckNF where

import ReWire.Scoping
import ReWire.Core.Syntax
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.Monad
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Graph
import Data.List (nub)
import Data.Maybe (fromJust)
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity
import Prelude hiding (lookup)
import Debug.Trace (trace)

data DefnSort = DefnPrim | DefnBitty | DefnCont deriving (Eq,Show)
type NFM = RWT (ErrorT NFMError (StateT NFMState Identity))
data NFMState = NFM { visited :: Map (Id RWCExp) DefnSort, 
                      cpx     :: Set TyConId }         -- really this should be env not state, but whatever
type NFMError = String

getVisited = get >>= return . visited
getCpxTys = get >>= return . cpx
modifyVisited f = modify (\ s -> s { visited = f (visited s) })
modifyCpxTys f = modify (\ s -> s { cpx = f (cpx s) })

type TyGraph = [(TyConId,TyConId,[TyConId])]

buildTyGraph :: [RWCData] -> TyGraph
buildTyGraph = ((TyConId "(->)",TyConId "(->)",[]):) . map mkNode
  where mkNode (RWCData i _ dcs) = (i,i,concatMap dcOuts dcs)
        dcOuts (RWCDataCon i ts) = concatMap tyOuts ts
        tyOuts (RWCTyApp t1 t2)  = tyOuts t1 ++ tyOuts t2
        tyOuts (RWCTyCon i)      = [i]
        tyOuts (RWCTyVar _)      = []

-- Find "complex" type constructors: that is, all type constructors for types
-- that are recursive, are functional, or contain a complex type.
cpxTys :: [RWCData] -> Set TyConId
cpxTys dds = let edges                  = buildTyGraph dds
                 tcs                    = map (\(i,_,_) -> i) edges
                 (graph,vxmap,kmap)     = graphFromEdges edges
                 sccs                   = stronglyConnComp edges
                 sccRecs (AcyclicSCC _) = []
                 sccRecs (CyclicSCC ts) = ts
                 recs                   = TyConId "(->)" : concatMap sccRecs sccs
                 cpxs                   = nub $ recs ++ filter (\ i -> any (\ i' -> path graph (fromJust (kmap i)) i') (map (fromJust . kmap) recs)) tcs
             in Set.fromList cpxs

checkTyIsReact :: RWCTy -> NFM ()
checkTyIsReact (RWCTyApp (RWCTyApp (RWCTyApp (RWCTyCon (TyConId "React")) ti) to) ta) =
  do checkTyIsBitty ti
     checkTyIsBitty to
     checkTyIsBitty ta
     -- FIXME: check to make sure ti, to, ta match what's been seen so far.
checkTyIsReact t =
  throwError $ "checkTyIsReact: type is not of form `React t1 t2 t3': " ++ show t

checkDefnIsCont :: Id RWCExp -> NFM ()
checkDefnIsCont n = trace ("cdic: " ++ show n) $
                    do vset <- getVisited
                       case Map.lookup n vset of
                           Just DefnCont  -> return ()
                           Just DefnPrim  -> throwError $ "checkDefnIsCont: " ++ show n ++ " is a primitive"
                           Just DefnBitty -> throwError $ "checkDefnIsCont: " ++ show n ++ " has to be bitty"
                           Nothing        -> do
                             md <- queryG n
                             case md of
                               Nothing ->
                                 throwError $ "checkDefnIsCont: " ++ show n ++ " is undefined"
                               Just (RWCDefn _ (tvs :-> t) e) -> do
                                 when (length tvs > 0)
                                      (throwError $ "checkDefnIsCont: type is not monomorphic")
                                 modifyVisited (Map.insert n DefnCont)
                                 let (targs,tres) = flattenArrow t
                                 when (length targs < 1)
                                      (throwError $ "checkDefnIsCont: not enough arguments in type")
                                 mapM_ checkTyIsBitty targs
                                 -- FIXME: check that last element of targs matches what's been seen so far
                                 checkTyIsReact tres
                                 inLambdas e $ \ xts eb -> do
                                   when (length xts /= length targs)
                                        (throwError $ "checkDefnIsCont: number of lambda-bound variables does not match arity of function (" ++ show (length xts) ++ " vars, arity " ++ show (length targs) ++ ")")
                                   checkExprIsCont eb

checkTyIsCont :: RWCTy -> NFM ()
checkTyIsCont (RWCTyApp (RWCTyApp (RWCTyCon (TyConId "(->)")) t1) t2) = do checkTyIsBitty t1
                                                                           checkTyIsReact t2
                                                                          -- FIXME: make sure t1 matches what's been seen so far
checkTyIsCont t                                             = throwError $ "checkTyIsCont: malformed continuation type: " ++ show t

checkExprIsCont :: RWCExp -> NFM ()
checkExprIsCont e_@(RWCCase e alts) = do checkTyIsReact (typeOf e_)
                                         checkExprIsBitty e
                                         mapM_ checkAltIsCont alts
checkExprIsCont e@(RWCApp {})        = do checkTyIsReact (typeOf e)
                                          let es = flattenApp e
                                          case es of
                                            [RWCCon (DataConId "P") _,e1,e2] -> checkExprIsBitty e1 >> checkExprIsContCall e2
                                            _                                -> throwError $ "checkExprIsCont: malformed continuation expression: " ++ show e
checkExprIsCont e                    = throwError $ "checkExprIsCont: malformed pause expression: " ++ show e

checkExprIsContCall :: RWCExp -> NFM ()
checkExprIsContCall e = do checkTyIsCont (typeOf e)
                           let (ef:eargs) = flattenApp e
                           mapM_ checkExprIsBitty eargs
                           case ef of
                             RWCVar n _ -> checkDefnIsCont n
                             _          -> throwError $ "checkExprIsContCall: malformed continuation application: " ++ show e

checkAltIsCont :: RWCAlt -> NFM ()
checkAltIsCont a = inAlt a (\ _ -> checkExprIsCont)

checkDefnIsBitty :: Id RWCExp -> NFM ()
checkDefnIsBitty n = trace ("cdib: " ++ show n) $
                     do vset <- getVisited
                        case Map.lookup n vset of
                           Just DefnCont  -> throwError $ "checkDefnIsBitty: " ++ show n ++ " has to be a continuer"
                           Just DefnPrim  -> trace ("cdib prim: " ++ show n) $ return ()
                           Just DefnBitty -> trace ("cdib bail: " ++ show n) $ return ()
                           Nothing        -> trace ("cdib looking for: " ++ show n) $ do
                             md <- queryG n
                             case md of
                               Nothing -> do
                                 mp <- queryP n
                                 case mp of
                                   Just _  -> trace ("cdib: putting prim: " ++ show n) $ modifyVisited (Map.insert n DefnPrim)
                                   Nothing -> trace ("cdib didn't find: " ++ show n) $ return () -- FIXME: I think this happens ONLY when it's locally bound
                               Just (RWCDefn _ (tvs :-> t) e) -> trace ("cdib did find: " ++ show n) $ do
                                 when (length tvs > 0)
                                      (throwError $ "checkDefnIsBitty: type is not monomorphic")
                                 trace ("cdib: putting: " ++ show n) $ modifyVisited (Map.insert n DefnBitty)
                                 let (targs,tres) = flattenArrow t
                                 mapM_ checkTyIsBitty targs
                                 checkTyIsBitty tres
                                 inLambdas e $ \ xts eb -> do
                                   when (length xts /= length targs)
                                        (throwError $ "checkDefnIsBitty: number of lambda-bound variables does not match arity of function (" ++ show (length xts) ++ " vars, arity " ++ show (length targs) ++ ")")
                                   checkExprIsBitty eb

checkTyIsBitty :: RWCTy -> NFM ()
checkTyIsBitty t@(RWCTyApp {}) = trace "ctib app" $ mapM_ checkTyIsBitty (flattenTyApp t)
checkTyIsBitty (RWCTyVar v)    = trace "ctib var" $ throwError $ "checkTyIsBitty: encountered polymorphic type"
checkTyIsBitty (RWCTyCon i)    = trace ("ctib: " ++ show i) $
                                 do cpx <- getCpxTys
                                    if i `Set.member` cpx
                                       then throwError $ "checkTyIsBitty: encountered complex type: " ++ deTyConId i
                                       else return ()
checkTyIsBitty (RWCTyComp m t) = trace "ctib comp" $ throwError $ "checkTyIsBitty: encountered computation type"

checkExprIsBitty :: RWCExp -> NFM ()
checkExprIsBitty e@(RWCApp {})      = trace "ceib app" $
                                      do checkTyIsBitty (typeOf e)
                                         let (ef:eargs) = flattenApp e
                                         mapM_ checkExprIsBitty eargs
                                         case ef of
                                           RWCVar n _   -> checkDefnIsBitty n
                                           RWCCon i _   -> return ()
                                           RWCLiteral l -> checkLiteralIsBitty l
                                           _            -> throwError $ "checkExprIsBitty: malformed application head " ++ show ef
checkExprIsBitty (RWCVar n t)       = trace ("ceib: " ++ show n) $ do
                                        checkTyIsBitty t -- this is redundant I think
                                        checkDefnIsBitty n
checkExprIsBitty (RWCCon i t)       = trace ("ceib: " ++ show i) $ checkTyIsBitty t
checkExprIsBitty (RWCLiteral l)     = trace ("ceib: " ++ show l) $ checkLiteralIsBitty l
checkExprIsBitty e_@(RWCCase e alts) = trace ("ceib case") $ 
                                       do checkTyIsBitty (typeOf e_)
                                          checkExprIsBitty e
                                          mapM_ checkAltIsBitty alts
checkExprIsBitty e                   = throwError $ "checkExprIsBitty: malformed expression: " ++ show e

checkAltIsBitty :: RWCAlt -> NFM ()
checkAltIsBitty a = trace "caib" $ inAlt a (\ _ -> checkExprIsBitty)

checkLiteralIsBitty :: RWCLit -> NFM ()
checkLiteralIsBitty (RWCLitInteger _) = return ()
checkLiteralIsBitty (RWCLitChar _)    = return ()
checkLiteralIsBitty (RWCLitFloat _)   = throwError $ "checkLiteralIsBitty: floating point literal encountered"

checkStart :: NFM ()
checkStart = do md <- queryG (mkId "start")
                case md of
                 Just (RWCDefn _ (tvs :-> t) e) -> do
                   when (length tvs > 0) $
                     throwError $ "checkStart: type of start is not monomorphic"
                   case e of
                     RWCApp (RWCApp (RWCCon (DataConId "P") _) e1) e2 -> do
                       checkExprIsBitty e1
                       checkExprIsContCall e2
                     _ -> throwError $ "checkStart: malformed body: " ++ show e
                 Nothing -> throwError "checkStart: start is not defined"

checkProg :: NFM ()
checkProg = do dds_    <- getAssumptionsT
               let dds =  map (\ (TyConInfo t) -> t) $ Map.elems dds_
               modifyCpxTys (const (cpxTys dds))
               checkStart

checkProg' :: RWCProg -> Either NFMError (Map (Id RWCExp) DefnSort)
checkProg' p = runNFM p (checkProg >> getVisited)

runNFM :: RWCProg -> NFM a -> Either NFMError a
runNFM p phi = fst $ runIdentity $ runStateT (runErrorT (runRWT p phi)) s0
  where s0 = NFM { visited = Map.empty, cpx = Set.empty }

checkNF :: RWCProg -> Either NFMError ()
checkNF p = runNFM p checkProg

cmdCheckNF :: TransCommand
cmdCheckNF _ p = (Nothing,Just s)
  where s = show (runNFM p $ checkProg >> getVisited)
