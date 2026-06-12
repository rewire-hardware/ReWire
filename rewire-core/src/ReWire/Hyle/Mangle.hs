{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Hyle.Mangle (mangle, mangleFresh, mangleMod) where

import GHC.Utils.Encoding (zEncodeString)   -- this is from the ghc package
import Data.Char (isAlphaNum)
import Data.Text (Text, pack, unpack)

import qualified Data.Text as T

-- TODO: text version of this?
mangle :: Text -> Text
mangle = pack . zEncodeString . unpack

-- | Mangle a Hyle name into an identifier acceptable to the RTL backends
--   (when it isn't one already): alphanumerics plus underscores and dollar
--   signs (the VHDL pretty-printer further escapes names as needed).
mangleFresh :: Text -> Text
mangleFresh x = if isRtlId x' then x' else mangle x'
      where subDots :: Text -> Text
            subDots = T.replace "." "_"

            subDollar :: Text -> Text
            subDollar = \ case
                  (T.stripPrefix "$" -> Just x'') -> "Z" <> x''
                  x''                             -> x''

            subTick :: Text -> Text
            subTick = T.replace "'" "$"

            isRtlId :: Text -> Bool
            isRtlId = T.all isRtlId'

            isRtlId' :: Char -> Bool
            isRtlId' c = isAlphaNum c || c == '_' || c == '$'

            x' :: Text
            x' = subTick $ subDollar $ subDots x

-- | Module names need to be de-conflicted because they aren't immediately
--   freshened.
mangleMod :: Text -> Text
mangleMod x = mangleFresh x'
      where subDots :: Text -> Text
            subDots = T.replace "_" "__"

            subDollar :: Text -> Text
            subDollar = \ case
                  (T.stripPrefix "Z" -> Just x'') -> "ZZ" <> x''
                  x''                             -> x''

            x' :: Text
            x' = subDollar $ subDots x
