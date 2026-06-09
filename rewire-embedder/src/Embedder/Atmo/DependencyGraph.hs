{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}

module Embedder.Atmo.DependencyGraph where

import safe Embedder.Atmo.Syntax as C
    ( FreeProgram,
      TypeSynonym(..), DataDefn(..), Defn(..),
      Exp(..), Ty(..), Poly(..),
      DataCon(..), Pat (..), FunBinding (..), PatBind (..), RecDefn (..), -- Rhs (..), GuardedRhs (..), 
      )
import ReWire.Orphans ()

import Data.List ((\\), groupBy)
import Data.Text (Text, splitOn, pack, empty)
import Data.Graph (Graph,graphFromEdges,scc, Vertex, Tree (..))
import Data.Maybe (mapMaybe)




--------------------------------------------------------------
-- Atmo Dependency Graph
--------------------------------------------------------------

-- Graph with vertices that are free variables
-- The variables we care about are of type:
      -- Name TyConId (global type variables)
            -- defined in type synonyms and datatypes
            -- occur in type signatures everywhere
      -- Name DataConId (global term variables)
            -- defined in datatypes
            -- occur in Exps in Defns
      -- Name Exp (global term variables)
            -- defined in Defns
            -- occur in Exps in Defns
-- Edges point from a function to its free variables
-- This means that a function depends on X if it points to X
-- This means that dependency is a 'forward' topological sort
-- And so we want our file to use a reverse topological sort


-- scc: the strongly connected components of a graph, reverse topological sort
-- scc (0 > 1 > 2 > 0, 3 > 1) ==
--      [[root = 0, [root = 1, [root = 2]]],
--       [root = 3, []]]


data Def = DDef DataDefn | RDef RecDefn | DCon DataCon | TDef TypeSynonym | Def Defn

-- Isabelle allows mutually recursive datatype definitions, but ReWire doesn't
-- ReWire does allow mutual recursion of functions; only of type ReacT
-- Atmo will have mutual recursion between things of type ReacT and perhaps lifted lambdas
data Declaration = DDecl DataDefn | RecDecl RecDefn | TDecl TypeSynonym | FDecl Defn | RDecl [Defn]

mkRDecl :: [Defn] -> Declaration
mkRDecl ds = RDecl (map combineGroup groupedDefns)
      where
      groupedDefns = groupBy (\ d1 d2 -> defnName d1 == defnName d2) ds
      combineDefns :: Defn -> Defn -> Defn
      combineDefns def1 def2 = Defn (defnAnnote def1) (defnName def1) (defnPolyTy def1) (defnAttr def1) (defnBinds def1 ++ defnBinds def2)
      combineGroup :: [Defn] -> Defn
      combineGroup (d:ds') = foldl combineDefns d ds'
        

def2Decl :: Def -> Maybe Declaration
def2Decl = \ case
      DDef d -> Just $ DDecl d
      RDef d -> Just $ RecDecl d
      DCon _ -> Nothing
      TDef d -> Just $ TDecl d
      Def d  -> Just $ FDecl d

fvt :: Ty -> [Text]
fvt = \ case
      TyApp _a t ts -> fvt t ++ concatMap fvt ts
      TyCon _a n -> [n]
      TyTuple _a ts -> concatMap fvt ts
      _ -> []



getNodeDefn :: Defn -> (Def,Text,[Text])
getNodeDefn d@(Defn _ n (Poly _ t) _ bs) =
      let tvs :: [Text] = fvt t -- [Name TyConId]
          dvs :: [Text] = concatMap (fbConcatMap datacons) bs -- [Name DataConId]
          vs :: [Text] = concatMap (fbConcatMap fve) bs -- [Name Exp]
          tvs_e :: [Text] = concatMap (fbConcatMap tycons) bs -- [Name TyConId]
      in (Def d, n, tvs ++ dvs ++ tvs_e ++ vs)
      where
      fbConcatMap :: (Exp -> [Text]) -> FunBinding -> [Text]
      fbConcatMap f (FunBinding _ ps rhs) = concatMap dataconsPat ps ++ f rhs -- rhsConcatMap f rhs
      -- rhsConcatMap :: (Exp -> [Text]) -> Rhs -> [Text]
      -- rhsConcatMap f (UnGuardedRhs _ e) = f e
      -- rhsConcatMap f (GuardedRhss _ grhss) = concatMap (grhsConcatMap f) grhss
      -- grhsConcatMap f (GuardedRhs _ gs e) = concatMap f gs ++ f e


tycons :: Exp -> [Text] -- Name TyConId
tycons = \ case
      C.App _a mp _mt e es -> tycons e ++ concatMap tycons es ++ mp_fvt mp
      Lam _a mp _mt _vs e -> tycons e ++ mp_fvt mp
      Var _a mp _mt _n -> mp_fvt mp
      Con _a mp _mt _n -> mp_fvt mp
      C.Case _a mp _mt e pbs -> tycons e ++ concatMap pbTycons pbs ++ mp_fvt mp
      RWUser _a mp _mt _b -> mp_fvt mp
      LitInt {} -> []
      LitStr {} -> []
      C.LitVec _a mp _mt es -> concatMap tycons es ++ mp_fvt mp
      LitList _a mp _mt es -> concatMap tycons es ++ mp_fvt mp
      Tuple _a mp _mt es -> concatMap tycons es ++ mp_fvt mp
      If _ mp _mt t c a -> tycons t ++ tycons c ++ tycons a ++ mp_fvt mp
      Let _ mp _mt pbs e -> concatMap pbTycons pbs ++ tycons e ++ mp_fvt mp
      RecVal _ mp _ fs   -> concatMap (tycons . snd) fs ++ mp_fvt mp
      RecUpd _ mp _ e fs -> tycons e ++ concatMap (tycons . snd) fs ++ mp_fvt mp
      RecSel _ mp _ _ e  -> tycons e ++ mp_fvt mp
      where
            mp_fvt :: Maybe Poly -> [Text]
            mp_fvt = \ case
                  Just (Poly _ t) -> fvt t
                  _ -> []

pbTycons :: PatBind -> [Text]
pbTycons (PatBind _ e) = tycons e

fve :: Exp -> [Text] -- Name Exp
fve = \ case
      C.App _ _ _ e es -> fve e ++ concatMap fve es
      Lam _ _ _ vs e -> fve e \\ vs
      Var _ _ _ n -> [n]
      Con _ _ _ _n -> []
      C.Case _ _ _ e pbs -> fve e ++ concatMap fvePb pbs
      RWUser _ _ _ _b -> []
      LitInt {} -> []
      LitStr {} -> []
      C.LitVec _ _ _ es -> concatMap fve es
      LitList _ _ _ es -> concatMap fve es
      Tuple _ _ _ es -> concatMap fve es
      If _ _ _ t c a -> fve t ++ fve c ++ fve a
      Let _ _ _ pbs e -> concatMap fvePb pbs ++ fve e
      RecVal _ _ _ fs    -> concatMap (fve . snd) fs
      RecUpd _ _ _ e fs  -> fve e ++ concatMap (fve . snd) fs
      RecSel _ _ _ _ e   -> fve e

fvePb :: PatBind -> [Text]
fvePb (PatBind p e) = fvePat p ++ fve e

fvePat :: Pat -> [Text] -- Name Exp
fvePat = \ case
           PatCon      _ _ _ _n ps -> concatMap fvePat ps
           PatVar      _ _ _ n -> [n]
           PatWildCard {} -> []
           PatTuple    _ _ _ ps -> concatMap fvePat ps
           PatAs       _ _ _ n p -> n : fvePat p
           PatRec _ _ _ fs -> concatMap (fvePat . snd) fs

datacons :: Exp -> [Text] -- Name DataConId
datacons = \ case
      C.App _ _ _ e es -> datacons e ++ concatMap datacons es
      Lam _ _ _ _vs e -> datacons e
      Var _ _ _ _n -> []
      Con _ _ _ n -> [n]
      C.Case _ _ _ e pbs -> datacons e ++ concatMap dataconsPb pbs
      RWUser _ _ _ _b -> []
      LitInt {} -> []
      LitStr {} -> []
      C.LitVec _ _ _ es -> concatMap datacons es
      LitList _ _ _ es -> concatMap datacons es
      Tuple _ _ _ es -> concatMap datacons es
      If _ _ _ t c a -> datacons t ++ datacons c ++ datacons a
      Let _ _ _ pbs e -> concatMap dataconsPb pbs ++ datacons e
      RecVal _ _ _ fs    -> concatMap (datacons . snd) fs
      RecUpd _ _ _ e fs  -> datacons e ++ concatMap (datacons . snd) fs
      RecSel _ _ _ _ e   -> datacons e


dataconsPb :: PatBind -> [Text]
dataconsPb (PatBind p e) = dataconsPat p ++ datacons e

dataconsPat :: Pat -> [Text] -- Name DataConId
dataconsPat = \ case
           PatCon      _ _ _ n ps -> n : concatMap dataconsPat ps
           PatVar      _ _ _ _n -> []
           PatWildCard {} -> []
           PatTuple    _ _ _ ps -> concatMap dataconsPat ps
           PatAs       _ _ _ _ p -> dataconsPat p
           PatRec      _ _ _ fs -> concatMap (dataconsPat . snd) fs

getNodeTSyns :: TypeSynonym -> (Def,Text,[Text])
getNodeTSyns d@(C.TypeSynonym _ n (Poly _ t)) = (TDef d, n, fvt t :: [Text]) -- Name TyConId

getNodeData :: DataDefn -> [(Def,Text,[Text])]
getNodeData d@(DataDefn _ n _ cons) =
      (DDef d, ddef_name, ddef_fvs) : map getNodeDataCon cons
      where
            ddef_name = n
            ddef_fvs = concatMap dcon_fvs cons
            dcon_fvs :: DataCon -> [Text]
            dcon_fvs (DataCon _ _ (Poly _ t)) = fvt t :: [Text]
            getNodeDataCon :: DataCon -> (Def,Text,[Text])
            getNodeDataCon c@(DataCon _ name _) =
                  (DCon c, name, ddef_name : dcon_fvs c)

getNodeRec :: RecDefn -> (Def,Text,[Text])
getNodeRec d@(RecDefn _ n _ poly fs) =
      (RDef d, n, deps)
      where
            deps = fvt (case poly of Poly _ t -> t) ++ concatMap (fvt . snd) fs

-- Function to convert a qualified name to an unqualified name
unqualify :: Text -> Text
unqualify qualifiedName =
  case splitOn (pack ".") qualifiedName of
    [] -> empty  -- handle unexpected empty case gracefully
    parts -> last parts  -- take the last part after splitting by '.'

-- Function to transform a node to use unqualified names in the second and third components
unqualifyNode :: (Def, Text, [Text]) -> (Def, Text, [Text])
unqualifyNode (def, qualifiedName, qualifiedList) =
  let unqualifiedName = unqualify qualifiedName
      unqualifiedList = map unqualify qualifiedList
  in (def, unqualifiedName, unqualifiedList)


sortFreeProgram :: FreeProgram -> ([Declaration], Graph, [Tree Vertex])
sortFreeProgram (datadefs,recdefs,tysns,defs) =
      let   nodes :: [(Def,Text,[Text])] = concatMap getNodeData datadefs ++ map getNodeRec recdefs ++  map getNodeTSyns tysns ++ map getNodeDefn defs
            nodes' = map unqualifyNode nodes
            (graph,getV,_) = graphFromEdges nodes'
            tree = scc graph
      in
            (tree2decls ((\ (d,_,_) -> d) . getV) tree, graph, tree)
            where
            toDecl :: Tree Def -> Maybe Declaration
            toDecl = \ case
                  Node d []                    -> def2Decl d
                  Node (Def d) (toDecls -> ds) -> Just $ mkRDecl (d : ds)
                  _ -> error "ERROR: toDecl found non-fun mutual recursion"
            toDecls :: [Tree Def] -> [Defn]
            toDecls = \ case
                 [] -> []
                 (toDecl -> Just (FDecl d)) : (toDecls -> ds) -> d : ds
                 (toDecl -> Just (RDecl ds)) : (toDecls -> ds')    -> ds ++ ds'
                 _ -> error "ERROR: toDecls found non-fun mutual recursion"
            tree2decls :: (Vertex -> Def) -> [Tree Vertex] -> [Declaration]
            tree2decls f = mapMaybe (toDecl . fmap f)