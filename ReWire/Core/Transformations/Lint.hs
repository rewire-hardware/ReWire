module ReWire.Core.Transformations.Lint where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.CBN
import Data.Graph
import Unbound.LocallyNameless
import Control.Monad
import Control.Monad.Reader
import Data.List (nub)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

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
cpxTys :: RWCProg -> [Identifier]
cpxTys p = let edges                  = runLFreshM $ buildTyGraph (dataDecls p)
               tcs                    = map (\(i,_,_) -> i) edges
               (graph,vxmap,kmap)     = graphFromEdges edges
               sccs = stronglyConnComp $ runLFreshM (buildTyGraph (dataDecls p))
               sccRecs (AcyclicSCC _) = []
               sccRecs (CyclicSCC ts) = ts
               recs                   = "(->)" : concatMap sccRecs sccs
               cpxs                   = nub $ recs ++ filter (\ i -> any (\ i' -> path graph (fromJust (kmap i)) i') (map (fromJust . kmap) recs)) tcs
           in cpxs

--flattenApp :: RWCExp -> [RWCExp]
--flattenApp (RWCApp _ e e') = flattenApp e++[e']
--flattenApp e               = [e]

typeOf (RWCApp t _ _)   = t
typeOf (RWCLam t _)     = t
typeOf (RWCVar t _)     = t
typeOf (RWCCon t _)     = t
typeOf (RWCLiteral t _) = t
typeOf (RWCCase t _ _)  = t

flattenTyApp :: RWCTy -> [RWCTy]
flattenTyApp (RWCTyApp t1 t2) = flattenTyApp t1++[t2]
flattenTyApp t                = [t]

verbotenArgType :: LFresh m => [Identifier] -> RWCExp -> m Bool
verbotenArgType rts e = case t of
                          RWCTyCon i -> return (i `elem` rts || i == "(->)")
                          _          -> return False
   where (t:ts) = flattenTyApp (typeOf e)

lintAlt :: [Identifier] -> RWCAlt -> M RWCAlt
lintAlt rts (RWCAlt b) = lunbind b (\(p,e_) -> do e <- lint rts e_
                                                  return (RWCAlt (bind p e)))

arrowRight (RWCTyApp (RWCTyApp (RWCTyCon "(->)") _) t) = t

mkApp :: RWCExp -> RWCExp -> RWCExp
mkApp e1 e2 = RWCApp (arrowRight (typeOf e1)) e1 e2

lint :: [Identifier] -> RWCExp -> M RWCExp
lint rts e__@(RWCApp _ _ _)    = do let (e_:es_) =  flattenApp e__
                                    e           <- lint rts e_   
                                    es          <- mapM (lint rts) es_
                                    case e of
                                     RWCVar _ n -> do argsHO <- liftM or (mapM (verbotenArgType rts) es)
                                                      if argsHO
                                                         then evalexpr (foldl mkApp e es) -- >>= lint rts
                                                         else return (foldl mkApp e es)
                                     _          -> evalexpr (foldl mkApp e es) -- >>= lint rts
lint rts (RWCLam t b)         = lunbind b (\(x,e_) -> do e <- lint rts e_
                                                         return (RWCLam t (bind x e)))
lint rts e@(RWCCon {})        = return e
lint rts e@(RWCVar t n)       = return e -- askvar t n -- >>= lint rts
lint rts e@(RWCLiteral {})    = return e
lint rts (RWCCase t e alts_)  = do alts <- mapM (lintAlt rts) alts_
                                   eHO  <- verbotenArgType rts e
                                   if eHO
                                      then evalexpr (RWCCase t e alts) -- >>= lint rts
                                      else return (RWCCase t e alts)

lintDefn :: [Identifier] -> RWCDefn -> M RWCDefn
lintDefn rts d@(RWCDefn n (Embed b)) | n == s2n "main" = lunbind b (\(tvs,(t,e_)) ->
                                                          do e <- lint rts e_
                                                             return (RWCDefn n (embed $ setbind tvs (t,e))))
                                     | otherwise       = return d

lintProg :: RWCProg -> M RWCProg
lintProg p = do let rts = cpxTys p
                ds  <- luntrec (defns p)
                ds' <- avoid (map defnName ds) (local (const ds) (mapM (lintDefn rts) ds))
                return (p { defns = trec ds' })
   where defnName (RWCDefn n _) = AnyName n

cmdLint :: TransCommand
cmdLint _ p = let p' = runM (lintProg p)
              in (Just p',Nothing)
                  
