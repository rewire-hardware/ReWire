{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
-- | The abstract syntax of Synolon, the machine-level IR between Eidos and
--   Hyle (doc/eidos.md §7): the
--   process calculus — state cells, labeled blocks with commands, and
--   @pause@/@goto@/@halt@ terminators — over the Eidos expression language,
--   which this module re-exports: command right-hand sides, cell initials,
--   and terminator operands are Eidos expressions, and the pure
--   definitions a machine calls are Eidos definitions. Only the process
--   constructs and the program are new; the conventions of
--   'ReWire.Eidos.Syntax' (unique-keyed names, inert annotations, no term
--   equality) carry over unchanged.
module ReWire.Synolon.Syntax
      ( module ReWire.Eidos.Syntax
      , Proc (..), Cell (..), Block (..), Cmd (..), Term (..), TAlt (..)
      , Program (..)
        -- * Traversals
      , allBlocks, blockExps, cmdExps, termExps, cmdBinders
      , termTargets, pauseTargets, haltSites, mapTermTargets
      ) where

import ReWire.Annotation (Annote, Annotated (..))
import ReWire.Eidos.Syntax hiding (Program (..))

import Control.DeepSeq (NFData)
import Data.Data (Typeable, Data)
import Data.Text (Text)
import GHC.Generics (Generic)


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

-- | A terminator-case alternative (default first, as in an expression case).
data TAlt = TAlt Annote !AltCon ![Id] !Term
      deriving (Show, Generic, Typeable, Data, NFData)


-- | A whole program: datatypes, the definitions the machine calls (pure,
--   monomorphic, first-order — 'ReWire.Eidos.Types.machineDefn'), and the
--   processes, which are its roots. There is no @top@: procification
--   consumed the device root into the process.
data Program = Program
      { progDatas :: ![DataDefn]
      , progDefns :: ![Defn]
      , progProcs :: ![Proc]
      }
      deriving (Show, Generic, Typeable, Data, NFData)

instance Annotated Proc where
      ann = procAnnote

instance Annotated Cell where
      ann = cellAnnote

instance Annotated Block where
      ann = blkAnnote

instance Annotated Cmd where
      ann = \ case
            CmdBind a _ _ -> a
            CmdGet  a _ _ -> a
            CmdPut  a _ _ -> a

instance Annotated Term where
      ann = \ case
            Pause a _ _ _ -> a
            Goto  a _ _   -> a
            Halt  a _     -> a
            TCase a _ _   -> a

instance Annotated TAlt where
      ann (TAlt a _ _ _) = a

---
--- Traversals: the projections every consumer of a process needs, defined
--- once.
---

-- | Every block of a process, the entry block first (paired with no label).
allBlocks :: Proc -> [Block]
allBlocks pr = procEntry pr : map snd (procBlocks pr)

-- | The expressions a block holds: command right-hand sides and put
--   payloads, then the terminator's operands.
blockExps :: Block -> [Exp]
blockExps b = concatMap cmdExps (blkCmds b) <> termExps (blkTerm b)

cmdExps :: Cmd -> [Exp]
cmdExps = \ case
      CmdBind _ _ e -> [e]
      CmdGet {}     -> []
      CmdPut _ _ e  -> [e]

-- | A terminator's operands: the pause output and arguments, goto
--   arguments, the halt answer, and (recursively) a terminator case's
--   scrutinee and alternatives.
termExps :: Term -> [Exp]
termExps = \ case
      Pause _ a _ as -> a : as
      Goto _ _ as    -> as
      Halt _ a       -> [a]
      TCase _ a alts -> a : concat [ termExps t | TAlt _ _ _ t <- alts ]

-- | The binders a block's commands introduce (in order).
cmdBinders :: Block -> [Id]
cmdBinders b = [ x | CmdBind _ x _ <- blkCmds b ] <> [ x | CmdGet _ x _ <- blkCmds b ]

-- | The labels a terminator transfers to (by pause or goto).
termTargets :: Term -> [Id]
termTargets = \ case
      Pause _ _ l _  -> [l]
      Goto _ l _     -> [l]
      TCase _ _ alts -> concat [ termTargets t | TAlt _ _ _ t <- alts ]
      Halt {}        -> []

-- | The labels a terminator pauses to: the machine states.
pauseTargets :: Term -> [Id]
pauseTargets = \ case
      Pause _ _ l _  -> [l]
      TCase _ _ alts -> concat [ pauseTargets t | TAlt _ _ _ t <- alts ]
      _              -> []

-- | The locations of a terminator's halts.
haltSites :: Term -> [Annote]
haltSites = \ case
      Halt an _      -> [an]
      TCase _ _ alts -> concat [ haltSites t | TAlt _ _ _ t <- alts ]
      _              -> []

-- | Rewrite the transfer targets (and their argument lists) of a
--   terminator, leaving halts alone.
mapTermTargets :: (Id -> [Exp] -> (Id, [Exp])) -> Term -> Term
mapTermTargets f = \ case
      Pause an a l as -> let (l', as') = f l as in Pause an a l' as'
      Goto an l as    -> let (l', as') = f l as in Goto an l' as'
      TCase an a alts -> TCase an a [ TAlt aan c xs (mapTermTargets f t) | TAlt aan c xs t <- alts ]
      t               -> t
