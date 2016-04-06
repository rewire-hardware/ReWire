module ReWire.FrontEnd.PrimBasis (addPrims) where

import ReWire.Annotation
import ReWire.FrontEnd.Syntax

import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Unbound.Generics.LocallyNameless (string2Name, name2String)

import qualified Data.Map.Strict as Map

--
-- The "primitive basis" has some magical voodoo that cannot be expressed in
-- the concrete syntax... so here we are.

addPrims :: Fresh m => RWMProgram -> m RWMProgram
addPrims (RWMProgram ts vs) = do
      vs'   <- untrec vs
      -- pdats <- mapM mkPrimData $ fv vs'
      let pdefs = catMaybes $ map mkPrimDefn $ fvs vs'
      return $ RWMProgram (ts ++ primDatas) $ trec $ vs' ++ pdefs

fvs :: [RWMDefn] -> [Name RWMExp]
fvs = nub . concatMap fv'
      where fv' :: RWMDefn -> [Name RWMExp]
            fv' (RWMDefn _ _ _ _ (Embed e)) = fv e

mkPrimDefn :: Name RWMExp -> Maybe RWMDefn
mkPrimDefn n = do
      t <- Map.lookup (name2String n) prims
      return $ RWMDefn (msg $ "Builtin: " ++ name2String n) n t False $ Embed $ bind [] $ RWMVar noAnn tblank n

mkId :: String -> Name b
mkId = string2Name

msg :: String -> Annote
msg m = MsgAnnote m

primDatas :: [RWMData]
primDatas = [ RWMData (msg "Builtin: ->")  (TyConId "->")  [mkId "a", mkId "b"]           (KStar `KFun` (KStar `KFun` KStar))                  []
            , RWMData (msg "Builtin: ReT") (TyConId "ReT") [mkId "i", mkId "o", mkId "m"] (KStar `KFun` (KStar `KFun` (KMonad `KFun` KMonad))) []
            , RWMData (msg "Builtin: StT") (TyConId "StT") [mkId "s", mkId "m"]           (KStar `KFun` (KMonad `KFun` KMonad))                []
            , RWMData (msg "Builtin: I")   (TyConId "I")   []                             KMonad                                               []
            , RWMData (msg "Builtin: ()")  (TyConId "()")  []                             KStar                                                [RWMDataCon noAnn (DataConId "()") []]
            ] ++ map mkTuple [2..62] -- why 62? 'cause that's what ghc does!

mkTuple :: Int -> RWMData
mkTuple n = RWMData (msg "Builtin: tuple") (TyConId i) tvs k [ctor]
      where i    = "(" ++ replicate (n-1) ',' ++ ")"
            tvs  = map mkId $ take n $ [[c] | c <- ['a'..'z']] ++ map (('t':) . show) [0::Integer ..]
            k    = foldr KFun KStar $ replicate n KStar
            ctor = RWMDataCon noAnn (DataConId i) $ map (RWMTyVar noAnn) tvs

v :: String -> RWMTy
v = RWMTyVar noAnn . mkId

c :: String -> RWMTy
c = RWMTyCon noAnn . TyConId

tyApp :: RWMTy -> RWMTy -> RWMTy
tyApp = RWMTyApp noAnn

reT :: RWMTy -> RWMTy -> RWMTy -> RWMTy
reT i o m = c "ReT" `tyApp` i `tyApp` o `tyApp` m

stT :: RWMTy -> RWMTy -> RWMTy
stT s m   = c "StT" `tyApp` s `tyApp` m

prims :: Map String (Embed Poly)
prims = Map.fromList
      [ ( "return"
        , [mkId "a", mkId "m"]
        |-> v "a" `mkArrow` RWMTyComp noAnn (v "m") (v "a")
        )
      , ( ">>="
        , [mkId "m", mkId "a", mkId "b"]
        |-> RWMTyComp noAnn (v "m") (v "a")
            `mkArrow` (v "a" `mkArrow` RWMTyComp noAnn (v "m") (v "b"))
            `mkArrow` RWMTyComp noAnn (v "m") (v "b")
        )
      , ( "get"
        , [mkId "s", mkId "m"]
        |-> RWMTyComp noAnn (stT (v "s") (v "m")) (v "s")
        )
      , ( "put"
        , [mkId "s", mkId "m"]
        |-> v "s" `mkArrow` RWMTyComp noAnn (stT (v "s") (v "m")) (c "()")
        )
      , ( "signal"
        , [mkId "o", mkId "i", mkId "m"]
        |-> v "o" `mkArrow` RWMTyComp noAnn (reT (v "i") (v "o") (v "m")) (v "i")
        )
      , ( "lift"
        , [mkId "m", mkId "a", mkId "t"]
        |-> RWMTyComp noAnn (v "m") (v "a") `mkArrow` RWMTyComp noAnn (v "t" `tyApp` v "m") (v "a")
        )
      , ( "extrude"
        , [mkId "i", mkId "o", mkId "s", mkId "m", mkId "a"]
        |-> RWMTyComp noAnn (reT (v "i") (v "o") (stT (v "s") (v "m"))) (v "a")
            `mkArrow` v "s"
            `mkArrow` RWMTyComp noAnn (reT (v "i") (v "o") (v "m")) (c "(,)" `tyApp` v "a" `tyApp` v "s")
        )
      ]
