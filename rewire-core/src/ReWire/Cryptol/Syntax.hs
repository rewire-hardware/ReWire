{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
-- | Abstract syntax for the fragment of Cryptol emitted by the Cryptol
--   backend (ReWire.Core.ToCryptol). The generated file is a self-contained
--   anonymous module (no module header, so it can be loaded under any file
--   name): a few helper definitions implementing ReWire Core primitives, an
--   optional @parameter@ block declaring externs as uninterpreted functions,
--   and one function per Core defn. Helper names contain a tick (@rw'resize@)
--   so they can never collide with generated names (see 'cryptolName').
module ReWire.Cryptol.Syntax where

import ReWire.BitVector (BV, width, nat)
import qualified ReWire.BitVector as BV
import ReWire.Pretty (empty, text, Pretty (..), parens, (<+>), vsep, hsep, punctuate, comma, nest, Doc, brackets)

import Data.Char (isAlphaNum, intToDigit)
import Data.List (intersperse)
import Data.Text (Text)
import Numeric (showHex, showIntAtBase)
import Numeric.Natural (Natural)

import qualified Data.Text as T

type Name = Text
type Size = Word

-- | Maps a Core name to a valid Cryptol identifier body: any character
--   outside @[A-Za-z0-9_]@ becomes an underscore. Callers are expected to
--   prepend a prefix (@rw_@), so the first character doesn't matter and the
--   result can never collide with a Cryptol keyword, a Prelude name, or the
--   tick-containing helper names.
cryptolName :: Text -> Name
cryptolName = T.map sub
      where sub :: Char -> Char
            sub c | isAlphaNum c || c == '_' = c
                  | otherwise                = '_'

data Module = Module
      { modName     :: !Name    -- ^ Cryptol allows loading a file whose name doesn't match.
      , modComments :: ![Text]  -- ^ Header comment lines.
      , modParams   :: ![Param] -- ^ Externs, as uninterpreted functions.
      , modDefns    :: ![Defn]
      }
      deriving (Eq, Show)

instance Pretty Module where
      pretty (Module n comments params defns) = vsep $ intersperse empty
            $  [vsep $ map (\ c -> text "//" <+> text c) comments | not $ null comments]
            <> [text "module" <+> text n <+> text "where"]
            <> [helpers]
            <> [ppParams params | not $ null params]
            <> map pretty defns

-- | Helper definitions emitted with every module, implementing Core
--   primitives that don't map directly to a Cryptol operator. @rw'resize@ is
--   Verilog assignment semantics: truncate or zero-extend to the new width.
helpers :: Doc an
helpers = vsep $ map text
      [ "// Helpers implementing ReWire Core primitive semantics."
      , "rw'resize : {m, n} (fin m, fin n) => [n] -> [m]"
      , "rw'resize x = drop`{n} ((zero : [m]) # x)"
      , ""
      , "rw'repl : {n, w} (fin n, fin w) => [w] -> [n * w]"
      , "rw'repl x = join (repeat`{n} x)"
      , ""
      , "rw'parity : {n} (fin n) => [n] -> Bit"
      , "rw'parity x = foldr (^) False x"
      ]

-- | An uninterpreted function declared in a @parameter@ block: name, argument
--   widths, result width.
data Param = Param !Name ![Size] !Size
      deriving (Eq, Show)

ppParams :: [Param] -> Doc an
ppParams ps = nest 2 $ vsep $ text "parameter" : map ppParam ps
      where ppParam :: Param -> Doc an
            ppParam (Param n args res) = text n <+> text ":" <+> ppFun (map TBits args) (TBits res)

-- | Sequence lengths in types: the device's quantified variable @n@, @1 + n@,
--   or a literal.
data Len = LenVar | LenSucc
      deriving (Eq, Show)

instance Pretty Len where
      pretty = \ case
            LenVar  -> text "n"
            LenSucc -> text "1 + n"

data Ty = TBits !Size    -- ^ > [w]
        | TSeq !Len !Ty  -- ^ > [n]t
      deriving (Eq, Show)

instance Pretty Ty where
      pretty = \ case
            TBits w  -> brackets $ pretty $ toInteger w
            TSeq l t -> brackets (pretty l) <> pretty t

ppFun :: [Ty] -> Ty -> Doc an
ppFun args res = hsep $ punctuate (text " ->") $ map pretty $ args <> [res]

data Defn = Defn
      { defnComments :: ![Text]
      , defnName     :: !Name
      , defnQuantN   :: !Bool -- ^ Prefix the signature with @{n} (fin n) =>@.
      , defnArgs     :: ![(Name, Ty)]
      , defnResTy    :: !Ty
      , defnBody     :: !Exp
      , defnWhere    :: ![Bind]
      }
      deriving (Eq, Show)

instance Pretty Defn where
      pretty (Defn comments n quantN args res body whr) = vsep
            $  map (\ c -> text "//" <+> text c) comments
            <> [ text n <+> text ":" <+> quant <> ppFun (map snd args) res
               , nest 2 $ vsep
                     $ [hsep ([text n] <> map (text . fst) args <> [text "="]) <+> pretty body]
                    <> [nest 2 $ vsep $ text "where" : map pretty whr | not $ null whr]
               ]
            where quant :: Doc an
                  quant | quantN    = text "{n} (fin n) => "
                        | otherwise = empty

-- | A @where@-binding with an explicit type annotation.
data Bind = Bind !Name !Ty !Exp
      deriving (Eq, Show)

instance Pretty Bind where
      pretty (Bind n t e) = vsep
            [ text n <+> text ":" <+> pretty t
            , text n <+> text "=" <+> pretty e
            ]

data Exp = Lit !BV               -- ^ Sized hex/binary literal (or @(zero : [0])@).
         | Var !Name
         | Call !Name ![Exp]     -- ^ Prefix application: @f e1 e2@.
         | TCall !Name !Natural ![Exp] -- ^ Application with a type argument: @f`{k} e1 e2@.
         | BinOp !Name !Exp !Exp
         | UnOp !Name !Exp
         | Index !Exp !Natural   -- ^ > e @ k
         | If !Exp !Exp !Exp
         | Sing !Exp             -- ^ Bit to @[1]@: @[e]@.
         | Comp !Exp ![(Name, Exp)] -- ^ > [ e | x <- e1 | y <- e2 ]
         | SeqLit ![Exp]         -- ^ > [e1, e2]
      deriving (Eq, Show)

class Parenless a where
      parenless :: a -> Bool

instance Parenless Exp where
      parenless = \ case
            Lit    {} -> True -- zero-width literals print their own parens.
            Var    {} -> True
            Sing   {} -> True
            Comp   {} -> True
            SeqLit {} -> True
            _         -> False

mparens :: (Pretty a, Parenless a) => a -> Doc an
mparens a | parenless a = pretty a
          | otherwise   = parens $ pretty a

instance Pretty Exp where
      pretty = \ case
            Lit bv         -> ppLit bv
            Var n          -> text n
            Call f es      -> hsep $ text f : map mparens es
            TCall f k es   -> hsep $ (text f <> text "`{" <> pretty (toInteger k) <> text "}") : map mparens es
            BinOp op e1 e2 -> mparens e1 <+> text op <+> mparens e2
            UnOp op e      -> text op <+> mparens e
            Index e k      -> mparens e <+> text "@" <+> pretty (toInteger k)
            If c e1 e2     -> text "if" <+> pretty c <+> text "then" <+> mparens e1 <+> text "else" <+> mparens e2
            Sing e         -> brackets $ pretty e
            Comp e arms    -> brackets $ pretty e <+> hsep (map arm arms)
                  where arm :: (Name, Exp) -> Doc an
                        arm (x, e') = text "|" <+> text x <+> text "<-" <+> pretty e'
            SeqLit es      -> brackets $ hsep $ punctuate comma $ map pretty es

-- | Hex when the width is a multiple of four (Cryptol hex literals have a
--   width of exactly four bits per digit), binary otherwise.
ppLit :: BV -> Doc an
ppLit bv
      | w <= 0         = parens $ text "zero : [0]"
      | w `mod` 4 == 0 = text $ "0x" <> pad (w `div` 4) (T.pack $ showHex (nat bv) "")
      | otherwise      = text $ "0b" <> pad w (T.pack $ showIntAtBase 2 intToDigit (nat bv) "")
      where w :: Int
            w = width bv

            pad :: Int -> Text -> Text
            pad n s = T.replicate (n - T.length s) "0" <> s

-- | Slicing, MSB-first (Cryptol's bit order): @k@ bits starting at MSB offset
--   @off@ out of a @w@-bit expression. Skips @take@/@drop@ when they would be
--   the identity.
slice :: Size -> Size -> Size -> Exp -> Exp
slice w off k e = takeE $ dropE e
      where takeE :: Exp -> Exp
            takeE e' | off + k == w = e'
                     | otherwise    = TCall "take" (fromIntegral k) [e']

            dropE :: Exp -> Exp
            dropE e' | off == 0  = e'
                     | otherwise = TCall "drop" (fromIntegral off) [e']

-- | Truncate-or-zero-extend to the given width (Verilog assignment
--   semantics), as implemented by the emitted @rw'resize@ helper.
resize :: Size -> Exp -> Exp
resize sz = \ case
      TCall "rw'resize" _ [e] -> TCall "rw'resize" (fromIntegral sz) [e]
      Lit bv | width bv == 0, sz == 0 -> Lit bv
      e                       -> TCall "rw'resize" (fromIntegral sz) [e]

-- | Concatenation, filtering zero-width parts.
cat :: [Exp] -> Exp
cat es = case filter (not . isNil) es of
      []  -> nil
      es' -> foldr1 (BinOp "#") es'

nil :: Exp
nil = Lit BV.nil

isNil :: Exp -> Bool
isNil = \ case
      Lit bv -> width bv <= 0
      _      -> False
