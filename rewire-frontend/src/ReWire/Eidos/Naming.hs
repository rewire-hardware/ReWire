{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The shared naming and ordering conventions for compiler-minted
--   definitions and (later) machine-level labels: one function per naming
--   decision, so independently-minted artifacts that must correspond
--   (stage 1's lifted join continuations; stage 2's procify block labels)
--   agree by construction, and regenerated goldens drift minimally.
--
--   Names are /occurrence-stable/: derived from the enclosing
--   definition's display name, the bound occurrence's display name, and a
--   deterministic per-name ordinal — never from a global fresh counter,
--   which renumbers wholesale on any upstream change.
module ReWire.Eidos.Naming (liftedJoinName, blockLabel, labelBase, defnBase, originTag) where

import ReWire.Pretty (showt)

import Data.Bits (xor)
import Data.Char (isAlphaNum, ord)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word32)
import Numeric (showHex)

import qualified Data.Text as T

-- | The label for a procify-minted block: @$L.\<source>@ for a source
--   name's first block, @$L.\<source>\<ordinal>@ from the second on. The
--   ordinal is per source name, so an upstream edit renumbers only that
--   name's later blocks, never the whole process. (The machine fold
--   derives the blocks' Hyle definition names from these via 'labelBase',
--   disambiguating collisions there; dispatch order and tag values key on
--   the label's unique, not its text.)
blockLabel :: Text -> Int -> Text
blockLabel src i | i <= 1    = "$L." <> src
                 | otherwise = "$L." <> src <> showt i

-- | The top-level name for a join continuation lifted out of a
--   definition: @$LL.\<defn>.\<join>\<ordinal>@. The @$LL.@ prefix marks
--   the definition as compiler-lifted; it is display-only (the machine
--   fold strips it via 'defnBase' when naming the Hyle definition).
liftedJoinName :: Text -> Text -> Int -> Text
liftedJoinName defn joinOcc i = "$LL." <> defn <> "." <> joinOcc <> showt i

-- | The display base of a machine-label occurrence, for naming the
--   block's Hyle definition: label prefixes and qualification stripped,
--   the per-name ordinal kept (@$L.Main.getIns2@ -> @getIns2@,
--   @$L.arm@ -> @arm@, @$L.$ds@ -> @$ds@ — compiler-marked binders stay
--   marked).
labelBase :: Text -> Text
labelBase = T.takeWhileEnd (/= '.') . stripPrefixes
      where stripPrefixes :: Text -> Text
            stripPrefixes t
                  | Just t' <- T.stripPrefix "$L."  t = stripPrefixes t'
                  | Just t' <- T.stripPrefix "$LL." t = stripPrefixes t'
                  | otherwise                         = t

-- | The display base of a top-level definition's occurrence for its Hyle
--   global name: the @$LL.@ lifted-definition marker stripped
--   (@$LL.Main.foo.j1@ -> @Main.foo.j1@), other occurrences unchanged.
defnBase :: Text -> Text
defnBase occ = fromMaybe occ $ T.stripPrefix "$LL." occ

-- | A short display tag for an instantiation, from the pre-rendered
--   argument texts: sanitized to identifier characters (separators
--   collapse to @_@) and @$@-joined when short — @Vec 8 Bool@ becomes
--   @Vec_8_Bool@ — otherwise a stable 32-bit FNV-1a hash of the full
--   rendering. Stability across platforms and compiler versions is the
--   point: instance names derived from these must not churn between
--   compiles (which is why this is not 'Data.Hashable.hash').
originTag :: [Text] -> Text
originTag args
      | not (T.null joined), T.length joined <= 24 = joined
      | otherwise                                  = fnv1a $ T.intercalate "|" args
      where joined :: Text
            joined = T.intercalate "$" $ map sanitize args

            sanitize :: Text -> Text
            sanitize = T.intercalate "_" . filter (not . T.null) . T.split (not . keep)

            keep :: Char -> Bool
            keep c = isAlphaNum c || c == '\''

            fnv1a :: Text -> Text
            fnv1a = T.pack . flip showHex "" . T.foldl' step (2166136261 :: Word32)
                  where step :: Word32 -> Char -> Word32
                        step h c = (h `xor` fromIntegral (ord c)) * 16777619
