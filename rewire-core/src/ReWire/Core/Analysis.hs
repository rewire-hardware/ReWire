{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Safe #-}
-- | Target-independent analyses over the Core IR shared by the RTL backends.
module ReWire.Core.Analysis (DefnMap, Uses, IsPure, defnMap, defnUses, pureDefns, isPure) where

import ReWire.Core.Syntax
import ReWire.Fix (fix')

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified Data.Text           as T

type Uses    = Natural
type IsPure  = Bool
type DefnMap = HashMap GId (Exp, (Uses, IsPure))

-- | Map each defn (including loop and state0) to its body, use count, and
--   purity (purity meaning: no implicit clock/reset required).
defnMap :: Device -> DefnMap
defnMap p@Device { loop, state0, defns } = foldl' defnInfo mempty defns'
      where defnInfo :: DefnMap -> Defn -> DefnMap
            defnInfo m (Defn _ g _ e) = Map.insert g (e, (Map.findWithDefault 0 g uses, Set.member g pures)) m

            uses :: HashMap GId Uses
            uses = defnUses p

            pures :: HashSet GId
            pures = pureDefns p

            defns' :: [Defn]
            defns' = loop : state0 : defns

-- | Defns that do not require an implicit clock/reset.
pureDefns :: Device -> HashSet GId
pureDefns Device { loop, state0, defns } = fix' purity mempty
      where purity :: HashSet GId -> HashSet GId
            purity m = foldl' purity' m defns'

            purity' :: HashSet GId -> Defn -> HashSet GId
            purity' ps (Defn _ g _ e) = if isPure ps e then Set.insert g ps else ps

            defns' :: [Defn]
            defns' = loop : state0 : defns

defnUses :: Device -> HashMap GId Uses
defnUses Device { loop, state0, defns } = Map.fromList [(defnName loop, 1), (defnName state0, 1)]
      <+> foldr (<+>) Map.empty (expUses . defnBody <$> loop : defns) -- drop state0 body.
      where expUses :: Exp -> HashMap GId Uses
            expUses = \ case
                  Concat _ e1 e2              -> expUses e1 <+> expUses e2
                  Call _ _ (Global g) e _ els -> Map.singleton g 1 <+> expUses e <+> expUses els
                  Call _ _ _          e _ els ->                       expUses e <+> expUses els
                  _                           -> Map.empty

            (<+>) :: HashMap GId Uses -> HashMap GId Uses -> HashMap GId Uses
            (<+>) = Map.unionWith (+)

isPure :: HashSet GId -> Exp -> Bool
isPure m = \ case
      Call _ _ (Extern (ExternSig _ _ c r _ _) _ _) a _ b
                                -> T.null c && T.null r && pur a && pur b
      Call _ _ (Global g) a _ b -> purG g && pur a && pur b
      Call _ _ _ a _ b          -> pur a && pur b
      Concat _ a b              -> pur a && pur b
      LVar {}                   -> True
      Lit {}                    -> True
      where pur :: Exp -> Bool
            pur = isPure m

            purG :: GId -> Bool
            purG = flip Set.member m
