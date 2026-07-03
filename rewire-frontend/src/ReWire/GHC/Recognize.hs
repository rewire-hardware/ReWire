{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Recognition for the Core-to-Crust bridge: classifying GHC names and
--   types (primitives, the RWC.Primitives mirror module, user classes,
--   erasable evidence), the external tycon and base-vocabulary tables,
--   Crust naming conventions for Vars and constructors, and source
--   locations. Everything here answers "what is this Core thing to
--   ReWire?"; the translation itself lives in "ReWire.GHC.ToCrust".
module ReWire.GHC.Recognize
      ( uKey
      , spanAnnote, varAnnote
      , isPrimModule, isPrimVar, homeishMod
      , qualName, conName, tupleName, splitStart
      , erasedArg, erasedEv, userPred
      , tyConModule, tyConKey, tyConTable
      , vocabTable, maybeTyName, eitherTyName
      ) where

import ReWire.Annotation (Annote (MsgAnnote), srcAnnote)

import Data.Text (Text, pack)

import qualified Data.Text as T

import GHC (ModuleName, moduleName, moduleNameString)
import GHC.Builtin.Types (trueDataCon, falseDataCon, unitDataCon)
import GHC.Core (CoreExpr, isTyCoArg)
import GHC.Core.DataCon (DataCon, dataConName, isTupleDataCon, dataConSourceArity)
import GHC.Core.Predicate (isEvVarType)
import GHC.Core.TyCo.Rep (Type)
import GHC.Core.TyCon (TyCon, tyConName, isClassTyCon)
import GHC.Core.Type (expandTypeSynonyms, tyConAppTyCon_maybe)
import GHC.Core.Utils (exprType)
import GHC.Data.FastString (unpackFS)
import GHC.Types.Name (getOccString, nameModule_maybe, nameSrcSpan)
import GHC.Types.SrcLoc (SrcSpan (..), srcSpanFile, srcSpanStartLine, srcSpanStartCol, srcSpanEndLine, srcSpanEndCol)
import GHC.Types.Unique (getKey)
import GHC.Types.Var (Var, varName, varUnique)

-- | IntMap key for a binder (uniques are 64-bit as of GHC 9.10; low bits
--   suffice as map keys within one compilation).
uKey :: Var -> Int
uKey = fromIntegral . getKey . varUnique

spanAnnote :: SrcSpan -> Annote
spanAnnote = \ case
      RealSrcSpan rs _ -> srcAnnote (unpackFS $ srcSpanFile rs)
                                    (srcSpanStartLine rs, srcSpanStartCol rs)
                                    (srcSpanEndLine rs, srcSpanEndCol rs)
      UnhelpfulSpan _  -> MsgAnnote "ghc-frontend"

varAnnote :: Var -> Annote
varAnnote = spanAnnote . nameSrcSpan . varName

-- | Arguments the translation erases: types, coercions, and evidence --
--   except evidence for /user/ classes, which is kept as ordinary data
--   (dictionaries as values; the specializer + case-of-known-constructor
--   eliminate them).
erasedArg :: CoreExpr -> Bool
erasedArg a = isTyCoArg a || erasedEv (exprType a)

-- | Evidence to erase: everything but user-class dictionaries.
erasedEv :: Type -> Bool
erasedEv t = isEvVarType t && not (userPred t)

-- | Is this a user-class predicate type? A class defined in a home module
--   (approximated by defining-module namespace, like the tycon table's
--   fallback): built-in evidence (KnownNat, Monad, HasCallStack, ...) is
--   external and erased; classes in the user's own modules are data.
userPred :: Type -> Bool
userPred t = case tyConAppTyCon_maybe $ expandTypeSynonyms t of
      Just tc -> isClassTyCon tc && homeishMod (tyConModule tc)
      _       -> False

-- | Home modules (loaded from source) get qualified names; known external
--   entities are in the tables; anything else external is out of
--   vocabulary. There is no unit map here, so approximate: well-known
--   external namespace prefixes are rejected, everything else is assumed
--   to be a home module.
homeishMod :: Maybe ModuleName -> Bool
homeishMod = \ case
      Nothing -> False
      Just mn -> not (isPrimModule mn) && not (any (`T.isPrefixOf` pack (moduleNameString mn))
            (["GHC.", "Data.", "Control.", "System.", "Foreign.", "Text.", "Unsafe."] :: [Text]))

isPrimVar :: Var -> Bool
isPrimVar v = "rwPrim" `T.isPrefixOf` pack (getOccString v)
      && maybe False (isPrimModule . moduleName) (nameModule_maybe $ varName v)

-- | In the RWC.Primitives module (the GHC-visible mirror of PrimBasis),
--   type and constructor names map to their bare occurrence names.
isPrimModule :: ModuleName -> Bool
isPrimModule = (== "RWC.Primitives") . moduleNameString

qualName :: ModuleName -> Var -> Text
qualName mn b
      | isPrimModule mn = pack $ getOccString b
      | otherwise       = pack (moduleNameString mn) <> "." <> pack (getOccString b)

-- | Split a qualified start symbol ("Main.start") into module and
--   occurrence parts.
splitStart :: Text -> (String, String)
splitStart s = case T.breakOnEnd "." s of
      ("", occ) -> ("Main", T.unpack occ)
      (m, occ)  -> (T.unpack $ T.dropEnd 1 m, T.unpack occ)

-- | Crust names for constructors.
conName :: DataCon -> Text
conName dc
      | dc == trueDataCon  = "True"
      | dc == falseDataCon = "False"
      | dc == unitDataCon  = "()"
      | isTupleDataCon dc  = tupleName $ dataConSourceArity dc
      | Just mn <- moduleName <$> nameModule_maybe (dataConName dc)
      , isPrimModule mn    = pack $ getOccString $ dataConName dc
      | Just mn <- moduleName <$> nameModule_maybe (dataConName dc)
                           = pack (moduleNameString mn) <> "." <> pack (getOccString $ dataConName dc)
      | otherwise          = pack $ getOccString $ dataConName dc

tupleName :: Int -> Text
tupleName n = "(" <> T.replicate (n - 1) "," <> ")"

tyConModule :: TyCon -> Maybe ModuleName
tyConModule = fmap moduleName . nameModule_maybe . tyConName

-- | Key for the external tycon table: defining module and occurrence.
tyConKey :: TyCon -> (String, String)
tyConKey tc = ( maybe "?" (moduleNameString . moduleName) $ nameModule_maybe $ tyConName tc
              , getOccString $ tyConName tc )

-- | External tycons mapped by (defining module, occurrence) to
--   (Crust name, number of leading type args to drop).
tyConTable :: [((String, String), (Text, Int))]
tyConTable =
      [ (("Data.Vector.Generic.Sized.Internal", "Vector"),   ("Vec", 1))    -- drop the unsized-vector arg
      , (("Data.Finite.Internal.Integral", "Finite"),        ("Finite", 1)) -- drop the rep (Integer) arg
      , (("Data.Finite.Internal", "Finite"),                 ("Finite", 0)) -- older finite-typelits
      , (("Control.Monad.Resumption.Reactive", "ReacT"),     ("ReacT", 0))
      , (("Control.Monad.Trans.State.Lazy", "StateT"),       ("StateT", 0))
      , (("GHC.Internal.Data.Functor.Identity", "Identity"), ("Identity", 0))
      , (("GHC.Internal.Maybe", "Maybe"),                    (maybeTyName, 0))
      , (("GHC.Internal.Data.Either", "Either"),             (eitherTyName, 0))
      ]

-- | Base combinators supported via synthesized (INLINE) Crust definitions
--   (see "ReWire.GHC.Vocab"); outer key: defining module; inner:
--   occurrence -> Crust name.
vocabTable :: [(Text, [(Text, Text)])]
vocabTable =
      [ ("GHC.Internal.Base",       [ ("$", "GHC.Internal.Base.$"), (".", "GHC.Internal.Base.."), ("id", "GHC.Internal.Base.id") ])
      , ("GHC.Classes",             [ ("not", "GHC.Classes.not"), ("&&", "GHC.Classes.&&"), ("||", "GHC.Classes.||") ])
      , ("GHC.Internal.Data.Tuple", [ ("fst", "GHC.Internal.Data.Tuple.fst"), ("snd", "GHC.Internal.Data.Tuple.snd") ])
      ]

maybeTyName, eitherTyName :: Text
maybeTyName  = "GHC.Internal.Maybe.Maybe"
eitherTyName = "GHC.Internal.Data.Either.Either"
