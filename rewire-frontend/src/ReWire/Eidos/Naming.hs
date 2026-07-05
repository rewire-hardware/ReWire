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
--   deterministic per-definition ordinal (pre-order traversal position) —
--   never from a global fresh counter, which renumbers wholesale on any
--   upstream change.
module ReWire.Eidos.Naming (liftedJoinName, blockLabel) where

import ReWire.Pretty (showt)

import Data.Text (Text)

-- | The label for a procify-minted block: @$L.\<source>\<ordinal>@, where
--   the source name is the continuation's, callee's, or join's display
--   name (the machine fold later derives label-enumeration constructor
--   names from these, so they fix dispatch order and tag values).
blockLabel :: Text -> Int -> Text
blockLabel src i = "$L." <> src <> showt i

-- | The top-level name for a join continuation lifted out of a
--   definition: @$LL.\<defn>.\<join>\<ordinal>@. The @$LL.@ prefix is the
--   pipeline's lifted-definition convention (the duplicate-merge pass
--   matches on it, so INLINE-duplicated continuations still merge).
liftedJoinName :: Text -> Text -> Int -> Text
liftedJoinName defn joinOcc i = "$LL." <> defn <> "." <> joinOcc <> showt i
