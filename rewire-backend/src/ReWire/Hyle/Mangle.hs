{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Trustworthy #-}
module ReWire.Hyle.Mangle (mangle, mangleFresh, mangleMod, pickFresh, seedNames, stripFreshTag, svReserved) where

import ReWire.Pretty (showt)

import GHC.Utils.Encoding (zEncodeString)   -- this is from the ghc package
import Data.Char (isAlphaNum, isDigit)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified Data.Text           as T

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

-- | Strip the freshening suffixes stacked onto a name by the Hyle inliner
--   (@$i<digits>@) and the fold-time instance hoist (@$h<digits>@),
--   recovering the original base for display. Suffix groups stack (deep
--   inline chains re-freshen already-freshened names, and hoisting re-lets
--   inliner names), so stripping repeats; a name that is nothing but suffix
--   (e.g. a bare legacy @$i0@) is kept unstripped rather than emptied.
stripFreshTag :: Text -> Text
stripFreshTag x = maybe x stripFreshTag $ stripOne x
      where stripOne :: Text -> Maybe Text
            stripOne t
                  | T.null digits = Nothing
                  | otherwise     = case (T.stripSuffix "$i" pre, T.stripSuffix "$h" pre) of
                        (Just base, _) | not (T.null base) -> Just base
                        (_, Just base) | not (T.null base) -> Just base
                        _                                  -> Nothing
                  where pre, digits :: Text
                        pre    = T.dropWhileEnd isDigit t
                        digits = T.takeWhileEnd isDigit t

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

-- | Pick an unused RTL identifier from an already-mangled seed, updating the
--   used map: the seed itself when free, otherwise the first free
--   suffix-numbered variant. Every picked (or 'seedNames'-seeded) name is a
--   key of the map, so a suffixed pick can never collide with a name issued
--   later under its own seed; the value is the seed's next suffix number,
--   keeping repeated requests O(1).
pickFresh :: Text -> HashMap Text Int -> Text -> (Text, HashMap Text Int)
pickFresh sep used s = go $ fromMaybe 0 $ Map.lookup s used
      where go :: Int -> (Text, HashMap Text Int)
            go k | Map.member cand used = go $ k + 1
                 | otherwise            = (cand, Map.insert s (k + 1) $ if cand == s then used else Map.insert cand 1 used)
                  where cand :: Text
                        cand | k <= 0    = s
                             | otherwise = s <> sep <> showt k

-- | Seed a 'pickFresh' used map with ambient names (ports, registers) that
--   are emitted verbatim, without passing through the backend's fresh-name
--   supply: generated names must not collide with them.
seedNames :: [Text] -> HashMap Text Int
seedNames = Map.fromList . map (, 1)

-- | SystemVerilog reserved words (IEEE 1800-2012, Annex B): generated
--   Verilog identifiers must dodge them. (The VHDL backend needs no
--   analogue: its pretty-printer escapes reserved words as extended
--   identifiers.)
svReserved :: Text -> Bool
svReserved = (`Set.member` svKeywords)

svKeywords :: HashSet Text
svKeywords = Set.fromList
      [ "accept_on", "alias", "always", "always_comb", "always_ff", "always_latch", "and"
      , "assert", "assign", "assume", "automatic", "before", "begin", "bind", "bins"
      , "binsof", "bit", "break", "buf", "bufif0", "bufif1", "byte", "case", "casex"
      , "casez", "cell", "chandle", "checker", "class", "clocking", "cmos", "config"
      , "const", "constraint", "context", "continue", "cover", "covergroup", "coverpoint"
      , "cross", "deassign", "default", "defparam", "design", "disable", "dist", "do"
      , "edge", "else", "end", "endcase", "endchecker", "endclass", "endclocking"
      , "endconfig", "endfunction", "endgenerate", "endgroup", "endinterface"
      , "endmodule", "endpackage", "endprimitive", "endprogram", "endproperty"
      , "endspecify", "endsequence", "endtable", "endtask", "enum", "event"
      , "eventually", "expect", "export", "extends", "extern", "final", "first_match"
      , "for", "force", "foreach", "forever", "fork", "forkjoin", "function", "generate"
      , "genvar", "global", "highz0", "highz1", "if", "iff", "ifnone", "ignore_bins"
      , "illegal_bins", "implements", "implies", "import", "incdir", "include"
      , "initial", "inout", "input", "inside", "instance", "int", "integer"
      , "interconnect", "interface", "intersect", "join", "join_any", "join_none"
      , "large", "let", "liblist", "library", "local", "localparam", "logic", "longint"
      , "macromodule", "matches", "medium", "modport", "module", "nand", "negedge"
      , "nettype", "new", "nexttime", "nmos", "nor", "noshowcancelled", "not", "notif0"
      , "notif1", "null", "or", "output", "package", "packed", "parameter", "pmos"
      , "posedge", "primitive", "priority", "program", "property", "protected", "pull0"
      , "pull1", "pulldown", "pullup", "pulsestyle_ondetect", "pulsestyle_onevent"
      , "pure", "rand", "randc", "randcase", "randsequence", "rcmos", "real", "realtime"
      , "ref", "reg", "reject_on", "release", "repeat", "restrict", "return", "rnmos"
      , "rpmos", "rtran", "rtranif0", "rtranif1", "s_always", "s_eventually"
      , "s_nexttime", "s_until", "s_until_with", "scalared", "sequence", "shortint"
      , "shortreal", "showcancelled", "signed", "small", "soft", "solve", "specify"
      , "specparam", "static", "string", "strong", "strong0", "strong1", "struct"
      , "super", "supply0", "supply1", "sync_accept_on", "sync_reject_on", "table"
      , "tagged", "task", "this", "throughout", "time", "timeprecision", "timeunit"
      , "tran", "tranif0", "tranif1", "tri", "tri0", "tri1", "triand", "trior", "trireg"
      , "type", "typedef", "union", "unique", "unique0", "unsigned", "until"
      , "until_with", "untyped", "use", "uwire", "var", "vectored", "virtual", "void"
      , "wait", "wait_order", "wand", "weak", "weak0", "weak1", "while", "wildcard"
      , "wire", "with", "within", "wor", "xnor", "xor"
      ]
