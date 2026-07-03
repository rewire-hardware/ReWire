{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The synthesized base vocabulary: Crust definitions and datatypes for
--   the small closed set of Prelude entities the deleted HSE front end
--   used to silently rebind to rewire-user definitions -- ($), (.), id,
--   not, (&&), (||), fst, snd, and the Maybe/Either datatypes (which can
--   occur in width-bearing device types, so they must reach ToHyle). The
--   definitions are INLINE so they are spliced away before partial
--   evaluation; use sites are recognized via the tables in
--   "ReWire.GHC.Recognize".
module ReWire.GHC.Vocab (vocabDatas, vocabDefns) where

import ReWire.Annotation (Annote (MsgAnnote))
import ReWire.Crust.Types (poly, arr, pairTy, (|->))
import ReWire.GHC.Recognize (maybeTyName, eitherTyName)
import ReWire.Unbound (s2n, Embed (..), bind)

import Data.Text (Text)

import qualified ReWire.Crust.Syntax as M

va :: Annote
va = MsgAnnote "ghc-frontend: base vocabulary"

tv :: Text -> M.Ty
tv = M.TyVar va M.KStar . s2n

boolT :: M.Ty
boolT = M.TyCon va $ s2n "Bool"

conE :: Text -> M.Ty -> M.Exp
conE n t = M.Con va Nothing (Just t) $ s2n n

varE :: M.Exp -> M.Exp -> M.Exp
varE f x = M.App va Nothing Nothing f x

lamE :: Text -> M.Ty -> M.Exp -> M.Exp
lamE x t body = M.Lam va Nothing (Just t) $ bind (s2n x) body

vE :: Text -> M.Ty -> M.Exp
vE x t = M.Var va Nothing (Just t) $ s2n x

vocabDefn :: Text -> M.Poly -> M.Exp -> M.Defn
vocabDefn n pt body = M.Defn va (s2n n) (Embed pt) (Just M.Inline) (Embed $ bind [] body)

vocabDatas :: [M.DataDefn]
vocabDatas =
      [ M.DataDefn va (s2n maybeTyName) (M.KFun M.KStar M.KStar)
            [ M.DataCon va (s2n "GHC.Internal.Maybe.Nothing") ([s2n "a"] |-> maybeT)
            , M.DataCon va (s2n "GHC.Internal.Maybe.Just")    ([s2n "a"] |-> arr a maybeT)
            ]
      , M.DataDefn va (s2n eitherTyName) (M.KFun M.KStar $ M.KFun M.KStar M.KStar)
            [ M.DataCon va (s2n "GHC.Internal.Data.Either.Left")  ([s2n "a", s2n "b"] |-> arr a eitherT)
            , M.DataCon va (s2n "GHC.Internal.Data.Either.Right") ([s2n "a", s2n "b"] |-> arr b eitherT)
            ]
      ]
      where a, b, maybeT, eitherT :: M.Ty
            a       = tv "a"
            b       = tv "b"
            maybeT  = M.TyApp va (M.TyCon va $ s2n maybeTyName) a
            eitherT = M.TyApp va (M.TyApp va (M.TyCon va $ s2n eitherTyName) a) b

vocabDefns :: [M.Defn]
vocabDefns =
      [ vocabDefn "GHC.Internal.Base.$" (poly [s2n "a", s2n "b"] $ arr (arr a b) $ arr a b)
            $ lamE "f" (arr a b) $ lamE "x" a $ varE (vE "f" $ arr a b) (vE "x" a)
      , vocabDefn "GHC.Internal.Base.." (poly [s2n "a", s2n "b", s2n "c"] $ arr (arr b c) $ arr (arr a b) $ arr a c)
            $ lamE "f" (arr b c) $ lamE "g" (arr a b) $ lamE "x" a
            $ varE (vE "f" $ arr b c) $ varE (vE "g" $ arr a b) (vE "x" a)
      , vocabDefn "GHC.Internal.Base.id" (poly [s2n "a"] $ arr a a)
            $ lamE "x" a $ vE "x" a
      , vocabDefn "GHC.Classes.not" (poly [] $ arr boolT boolT)
            $ lamE "b" boolT
            $ M.Case va Nothing (Just boolT) (vE "b" boolT) (bind patTrue $ conE "False" boolT) (Just $ conE "True" boolT)
      , vocabDefn "GHC.Classes.&&" (poly [] $ arr boolT $ arr boolT boolT)
            $ lamE "x" boolT $ lamE "y" boolT
            $ M.Case va Nothing (Just boolT) (vE "x" boolT) (bind patTrue $ vE "y" boolT) (Just $ conE "False" boolT)
      , vocabDefn "GHC.Classes.||" (poly [] $ arr boolT $ arr boolT boolT)
            $ lamE "x" boolT $ lamE "y" boolT
            $ M.Case va Nothing (Just boolT) (vE "x" boolT) (bind patTrue $ conE "True" boolT) (Just $ vE "y" boolT)
      , vocabDefn "GHC.Internal.Data.Tuple.fst" (poly [s2n "a", s2n "b"] $ arr (pairTy va a b) a)
            $ lamE "p" (pairTy va a b)
            $ M.Case va Nothing (Just a) (vE "p" $ pairTy va a b)
                  (bind (M.PatCon va (Embed Nothing) (Embed Nothing) (Embed $ s2n "(,)")
                        [ M.PatVar va (Embed Nothing) (Embed $ Just a) $ s2n "x"
                        , M.PatVar va (Embed Nothing) (Embed $ Just b) $ s2n "y" ])
                        $ vE "x" a)
                  Nothing
      , vocabDefn "GHC.Internal.Data.Tuple.snd" (poly [s2n "a", s2n "b"] $ arr (pairTy va a b) b)
            $ lamE "p" (pairTy va a b)
            $ M.Case va Nothing (Just b) (vE "p" $ pairTy va a b)
                  (bind (M.PatCon va (Embed Nothing) (Embed Nothing) (Embed $ s2n "(,)")
                        [ M.PatVar va (Embed Nothing) (Embed $ Just a) $ s2n "x"
                        , M.PatVar va (Embed Nothing) (Embed $ Just b) $ s2n "y" ])
                        $ vE "y" b)
                  Nothing
      ]
      where a, b, c :: M.Ty
            a = tv "a"
            b = tv "b"
            c = tv "c"

            patTrue :: M.Pat
            patTrue = M.PatCon va (Embed Nothing) (Embed Nothing) (Embed $ s2n "True") []
