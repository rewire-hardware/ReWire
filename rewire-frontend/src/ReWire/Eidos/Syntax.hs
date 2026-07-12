{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
-- | The abstract syntax of Eidos, the typed IR between GHC Core and Hyle.
--   Specified in doc/eidos.md; this module is the P level (system-F-lite
--   mirror of GHC Core). Conventions the rest of the compiler relies on:
--
--   * Binders are globally unique ('Uniq'): every binding site in a program
--     carries a distinct unique, established by the bridge and preserved by
--     every pass (duplication goes through a refreshing clone). 'Id' equality
--     and hashing are by unique only.
--   * Types are plain data (no binders): 'Ty' has no forall; quantification
--     appears only in 'Sig', as a plain type-variable list. Every binder
--     carries its full signature, so 'typeOf' is total and synthesizing.
--   * There is no term equality instance: comparing terms for alpha
--     equivalence is an explicit operation (see ReWire.Eidos.Types), never
--     accidental structural comparison (which would be annotation- and
--     unique-sensitive).
--   * Annotations ('Annote') are semantically inert and never compared.
module ReWire.Eidos.Syntax
      ( Uniq, TyConId, DataConId
      , Kind (..), TyVar (..), Ty (..), Sig (..), monoSig
      , Id (..), JoinId (..)
      , Exp (..), Arg (..), Bind (..), Alt (..), AltCon (..)
      , Defn (..), DefnAttr (..), SpecOrigin (..)
      , DataDefn (..), DataCon (..)
      , Proc (..), Cell (..), Block (..), Cmd (..), Term (..), TAlt (..)
      , Program (..)
      ) where

import ReWire.Annotation (Annote, Annotated (..))
import ReWire.Builtins (Builtin)

import Control.DeepSeq (NFData)
import Data.Data (Typeable, Data)
import Data.Hashable (Hashable (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- | Binder uniques. Globally unique per program by construction (bridge) and
--   by maintenance (every duplicating pass refreshes them).
type Uniq = Int

-- | Type constructor and data constructor names are stable global text
--   (dotted, never shadowed); they carry no uniques.
type TyConId  = Text
type DataConId = Text

data Kind = KStar
          | KNat
          | KFun !Kind !Kind
      deriving (Eq, Ord, Show, Generic, Typeable, Data, NFData, Hashable)

-- | A type variable: display text, unique, kind. Equality and hashing by
--   unique only.
data TyVar = TyVar
      { tvOcc  :: !Text
      , tvUniq :: !Uniq
      , tvKind :: !Kind
      }
      deriving (Show, Generic, Typeable, Data, NFData)

instance Eq TyVar where
      a == b = tvUniq a == tvUniq b
instance Ord TyVar where
      compare a b = compare (tvUniq a) (tvUniq b)
instance Hashable TyVar where
      hashWithSalt s = hashWithSalt s . tvUniq

-- | Types: plain first-order data, no binders, no forall. Type-level
--   naturals appear literally ('TyNat') or as applications of the built-in
--   arithmetic type constructors (see ReWire.Eidos.Types.natNorm).
--   Annotations are inert; the Eq/Ord/Hashable instances ignore them.
data Ty = TyCon  Annote !TyConId
        | TyApp  Annote !Ty !Ty
        | TyVarT Annote !TyVar
        | TyNat  Annote !Natural
        | Arrow  Annote !Ty !Ty
      deriving (Show, Generic, Typeable, Data, NFData)

instance Annotated Ty where
      ann = \ case
            TyCon  a _   -> a
            TyApp  a _ _ -> a
            TyVarT a _   -> a
            TyNat  a _   -> a
            Arrow  a _ _ -> a

instance Eq Ty where
      TyCon  _ c    == TyCon  _ c'     = c == c'
      TyApp  _ t u  == TyApp  _ t' u'  = t == t' && u == u'
      TyVarT _ v    == TyVarT _ v'     = v == v'
      TyNat  _ n    == TyNat  _ n'     = n == n'
      Arrow  _ t u  == Arrow  _ t' u'  = t == t' && u == u'
      _             == _               = False

instance Hashable Ty where
      hashWithSalt s = \ case
            TyCon  _ c   -> s `hashWithSalt` (0 :: Int) `hashWithSalt` c
            TyApp  _ t u -> s `hashWithSalt` (1 :: Int) `hashWithSalt` t `hashWithSalt` u
            TyVarT _ v   -> s `hashWithSalt` (2 :: Int) `hashWithSalt` v
            TyNat  _ n   -> s `hashWithSalt` (3 :: Int) `hashWithSalt` toInteger n
            Arrow  _ t u -> s `hashWithSalt` (4 :: Int) `hashWithSalt` t `hashWithSalt` u

-- | A signature: quantified type variables (empty for every local binder;
--   possibly non-empty for top-level definitions, constructors, and
--   primitive references) over a forall-free type.
data Sig = Sig
      { sigTVs :: ![TyVar]
      , sigTy  :: !Ty
      }
      deriving (Eq, Show, Generic, Typeable, Data, NFData, Hashable)

-- | A monomorphic signature.
monoSig :: Ty -> Sig
monoSig = Sig []

-- | A term-level name: display text, unique, signature. Locals always have
--   monomorphic signatures ('monoSig'); top-level definition names may be
--   polymorphic until specialization. Equality and hashing by unique only.
data Id = Id
      { idOcc  :: !Text
      , idUniq :: !Uniq
      , idSig  :: !Sig
      }
      deriving (Show, Generic, Typeable, Data, NFData)

instance Eq Id where
      a == b = idUniq a == idUniq b
instance Ord Id where
      compare a b = compare (idUniq a) (idUniq b)
instance Hashable Id where
      hashWithSalt s = hashWithSalt s . idUniq

-- | A join point name: an 'Id' plus the number of value parameters every
--   'Jump' to it must supply (jumps are saturated).
data JoinId = JoinId
      { jpId    :: !Id
      , jpArity :: !Int
      }
      deriving (Eq, Show, Generic, Typeable, Data, NFData, Hashable)

-- | Expressions. Type occurrences: 'Var' reads its type off the 'Id'
--   signature; 'Con', 'Prim', and the literals carry their (instantiated)
--   type at the occurrence; everything else synthesizes (see
--   ReWire.Eidos.Types.typeOf).
data Exp = Var     Annote !Id
         | Con     Annote !Ty !DataConId
         | Prim    Annote !Ty !Builtin
         | LitInt  Annote !Ty !Integer
         | LitStr  Annote !Text
         | LitList Annote !Ty ![Exp]
         | LitVec  Annote !Ty ![Exp]
         | App     Annote !Exp !Arg
         | Lam     Annote !Id !Exp
         | Let     Annote !Bind !Exp
         | Jump    Annote !JoinId ![Exp]
         | Case    Annote !Ty !Exp !Id ![Alt]
      deriving (Show, Generic, Typeable, Data, NFData)

instance Annotated Exp where
      ann = \ case
            Var     a _     -> a
            Con     a _ _   -> a
            Prim    a _ _   -> a
            LitInt  a _ _   -> a
            LitStr  a _     -> a
            LitList a _ _   -> a
            LitVec  a _ _   -> a
            App     a _ _   -> a
            Lam     a _ _   -> a
            Let     a _ _   -> a
            Jump    a _ _   -> a
            Case    a _ _ _ _ -> a

-- | An application argument: a term or a type. Type arguments instantiate
--   the head's signature variables in order; the specializer is driven by
--   them.
data Arg = EArg !Exp
         | TArg !Ty
      deriving (Show, Generic, Typeable, Data, NFData)

-- | Local bindings. 'Join' declares a join point whose body ends the
--   enclosing scope's continuation ('Jump' is the only way to reach it);
--   the parameter list length must equal the 'JoinId' arity.
data Bind = NonRec !Id !Exp
          | Rec ![(Id, Exp)]
          | Join !JoinId ![Id] !Exp
      deriving (Show, Generic, Typeable, Data, NFData)

-- | Case alternatives. The default alternative, if present, comes first
--   (the Core convention); constructor alternatives bind their fields.
data Alt = Alt Annote !AltCon ![Id] !Exp
      deriving (Show, Generic, Typeable, Data, NFData)

data AltCon = DataAlt !DataConId
            | LitAlt !Integer
            | DefaultAlt
      deriving (Eq, Show, Generic, Typeable, Data, NFData, Hashable)

data DefnAttr = Inline | NoInline
      deriving (Eq, Ord, Show, Generic, Typeable, Data, NFData, Hashable)

-- | Provenance of a compiler-minted definition clone: the origin
--   definition's display name and — for specializer clones — the type
--   arguments it was instantiated at ('BakeOrigin' marks the partial
--   evaluator's value-baked clones, whose baked arguments are terms, not
--   types). Rides into dumps, error messages, and (stably) generated HDL
--   names.
data SpecOrigin = SpecOrigin
      { originName :: !Text
      , originArgs :: ![Ty]
      }
      | BakeOrigin !Text
      deriving (Eq, Show, Generic, Typeable, Data, NFData, Hashable)

-- | A top-level definition: name (whose signature is the definition's
--   signature), a parameter telescope (arity is structural: length matches
--   the signature's arrow spine prefix), and a body.
data Defn = Defn
      { defnAnnote :: Annote
      , defnId     :: !Id
      , defnParams :: ![Id]
      , defnBody   :: !Exp
      , defnAttr   :: !(Maybe DefnAttr)
      , defnOrigin :: !(Maybe SpecOrigin)
      }
      deriving (Show, Generic, Typeable, Data, NFData)

-- | A datatype declaration. Constructor signatures quantify the datatype's
--   parameters: @C :: forall as. t1 -> ... -> tk -> T as@.
data DataDefn = DataDefn
      { dataAnnote :: Annote
      , dataName   :: !TyConId
      , dataKind   :: !Kind
      , dataCons   :: ![DataCon]
      }
      deriving (Show, Generic, Typeable, Data, NFData)

data DataCon = DataCon Annote !DataConId !Sig
      deriving (Show, Generic, Typeable, Data, NFData)

---
--- The M level (doc/eidos.md §7): processes.
---

-- | A process (doc/eidos.md §7.1): input/output types, an optional clock
--   name, named state cells (one per retired state layer), the reset
--   block (@entry@: parameterless, implicitly labeled), and labeled
--   blocks. Cell and process names are their own (Text) namespaces;
--   labels are 'Id's in the binder-unique discipline.
data Proc = Proc
      { procAnnote :: Annote
      , procName   :: !Text
      , procInTy   :: !Ty
      , procOutTy  :: !Ty
      , procClock  :: !(Maybe Text)
      , procCells  :: ![Cell]
      , procEntry  :: !Block
      , procBlocks :: ![(Id, Block)]
      }
      deriving (Show, Generic, Typeable, Data, NFData)

-- | A state cell: name, type, and initial value — a closed pure
--   expression evaluated at compile time, or 'Nothing' (@undef@) for a
--   cell first written before any read on every path from entry.
data Cell = Cell
      { cellAnnote :: Annote
      , cellName   :: !Text
      , cellTy     :: !Ty
      , cellInit   :: !(Maybe Exp)
      }
      deriving (Show, Generic, Typeable, Data, NFData)

-- | A block: parameters (a pause target's *last* parameter is the
--   resumed input, typed by the process input type), commands, and a
--   terminator.
data Block = Block
      { blkAnnote :: Annote
      , blkParams :: ![Id]
      , blkCmds   :: ![Cmd]
      , blkTerm   :: !Term
      }
      deriving (Show, Generic, Typeable, Data, NFData)

-- | A command: a pure computation, a cell read, or a cell write.
data Cmd = CmdBind Annote !Id !Exp
         | CmdGet  Annote !Id !Text
         | CmdPut  Annote !Text !Exp
      deriving (Show, Generic, Typeable, Data, NFData)

-- | A block terminator (doc/eidos.md §7.1). A @pause@ supplies all of
--   its target's parameters except the last (the resumed input, supplied
--   by the machine); a @goto@ supplies all of them.
data Term = Pause Annote !Exp !Id ![Exp]
          | Goto  Annote !Id ![Exp]
          | Halt  Annote !Exp
          | TCase Annote !Exp ![TAlt]
      deriving (Show, Generic, Typeable, Data, NFData)

-- | A terminator-case alternative (default first, as at the P level).
data TAlt = TAlt Annote !AltCon ![Id] !Term
      deriving (Show, Generic, Typeable, Data, NFData)

-- | A whole program: datatypes, definitions, processes (the M level;
--   empty until procification), and the designated device root.
data Program = Program
      { progDatas :: ![DataDefn]
      , progDefns :: ![Defn]
      , progProcs :: ![Proc]
      , progTop   :: !Id
      }
      deriving (Show, Generic, Typeable, Data, NFData)
