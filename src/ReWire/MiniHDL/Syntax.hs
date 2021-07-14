{-# LANGUAGE Safe #-}
module ReWire.MiniHDL.Syntax where

import Prelude hiding ((<>))
import Prettyprinter (Pretty (..), parens, (<>), (<+>), vcat, hcat, semi, colon, punctuate, comma, nest)
import ReWire.Pretty (($$))
import Data.Text (Text)

newtype Program = Program { programUnits :: [Unit] }
      deriving (Eq, Show)

instance Pretty Program where
      pretty (Program units) = vcat (map pretty units)

data Unit = Unit !Entity !Architecture
      deriving (Eq, Show)

instance Pretty Unit where
      pretty (Unit ent arch) = pretty "library ieee;"
                           $$ pretty "use ieee.std_logic_1164.all;"
                           $$ pretty ent
                           $$ pretty arch

type Name = Text

data Entity = Entity
      { entityName  :: !Name
      , entityPorts :: ![Port]
      } deriving (Eq, Show)

instance Pretty Entity where
      pretty (Entity n ps) = pretty "entity" <+> pretty n <+> pretty "is"
                         $$ nest 2 (pretty "port" <+> parens (vcat (punctuate semi (map pretty ps))) <> semi)
                         $$ pretty "end" <+> pretty n <> semi

data Architecture = Architecture
      { archName       :: !Name
      , archEntityName :: !Name
      , archSignals    :: ![Signal]
      , archComponents :: ![Component]
      , archStmts      :: ![Stmt]
      } deriving (Eq, Show)

instance Pretty Architecture where
      pretty (Architecture n1 n2 sigs comps ss) = pretty "architecture" <+> pretty n1 <+> pretty "of" <+> pretty n2 <+> pretty "is"
                                              $$ nest 2 (vcat (map pretty sigs))
                                              $$ nest 2 (vcat (map pretty comps))
                                              $$ pretty "begin"
                                              $$ nest 2 (vcat (map pretty ss))
                                              $$ pretty "end" <+> pretty n1 <> semi

data Component = Component
      { componentName  :: !Name
      , componentPorts :: ![Port]
      } deriving (Eq, Show)

instance Pretty Component where
      pretty (Component n ps) = pretty "component" <+> pretty n
                            $$ nest 2 (pretty "port" <+> parens (vcat (punctuate semi (map pretty ps))) <> semi)
                            $$ pretty "end component;"

data Port = Port
      { portName      :: !Name
      , portDirection :: !Direction
      , portType      :: !Ty
      } deriving (Eq, Show)

instance Pretty Port where
      pretty (Port n d t) = pretty n <> colon <+> pretty d <+> pretty t

data Signal = Signal
      { signalName :: !Name
      , signalType :: !Ty
      } deriving (Eq, Show)

instance Pretty Signal where
      pretty (Signal n t) = pretty "signal" <+> pretty n <> colon <+> pretty t <> semi

data Direction = In | Out deriving (Eq, Show)

instance Pretty Direction where
      pretty In  = pretty "in"
      pretty Out = pretty "out"

data Ty = TyStdLogic
        | TyStdLogicVector !Int
        | TyBool
        | TyClock -- not a real VHDL type, represented as std_logic.
        | TyRegister !Name !Int -- not real VHDL, just a hack.
        deriving (Eq, Show)

instance Pretty Ty where
      pretty TyStdLogic           = pretty "std_logic"
      pretty (TyStdLogicVector n) = pretty "std_logic_vector" <+> parens (pretty "0 to" <+> pretty (n - 1))
      pretty TyBool               = pretty "boolean"
      pretty TyClock              = pretty "std_logic"
      pretty (TyRegister _ n)     = pretty "std_logic_vector" <+> parens (pretty "0 to" <+> pretty (n - 1))

data Stmt = Assign !LHS !Expr
          | WithAssign !Expr !LHS ![(Expr, Expr)] !(Maybe Expr)
          | Instantiate !Name !Name !PortMap
          | ClkProcess !Name ![Stmt]
      deriving (Eq, Show)

instance Pretty Stmt where
      pretty = \ case
            Assign lhs e           -> pretty lhs <+> pretty "<=" <+> pretty e <> semi
            WithAssign e lhs bs mb -> pretty "with" <+> pretty e <+> pretty "select" <+> pretty lhs <+> pretty "<=" <+> vcat (punctuate comma branches) <> semi
                  where branches = map (\ (e1, e2) -> pretty e1 <+> pretty "when" <+> pretty e2) bs
                                ++ maybe [] (\ e -> [pretty e <+> pretty "when others"]) mb
            Instantiate n1 n2 pm   -> pretty n1 <> colon <+> pretty n2 <+> pretty "port map" <+> parens (pretty pm) <> semi
            ClkProcess n ss        -> pretty "process" <> parens (pretty n)
                                  $$ pretty "begin"
                                  $$ nest 2 (pretty "if" <+> pretty n <> pretty "'event and" <+> pretty n <+> pretty "= '1' then")
                                  $$ nest 4 (vcat (map pretty ss))
                                  $$ nest 2 (pretty "end if;")
                                  $$ pretty "end process;"

newtype PortMap = PortMap [(Name, Expr)]
      deriving (Eq, Show)

instance Pretty PortMap where
      pretty (PortMap ps) = vcat (punctuate comma (map (\ (n, e) -> pretty n <+> pretty "=>" <+> pretty e) ps))

newtype LHS = LHSName Name
      deriving (Eq, Show)

instance Pretty LHS where
      pretty (LHSName n) = pretty n

data Expr = ExprName !Name
          | ExprBit !Bit
          | ExprBitString ![Bit]
          | ExprConcat !Expr !Expr
          | ExprSlice !Expr !Int !Int
          | ExprIsEq !Expr !Expr
          | ExprBoolConst !Bool
          | ExprAnd !Expr !Expr
      deriving (Eq, Show)

instance Pretty Expr where
      pretty = \ case
            ExprName n          -> pretty n
            ExprBit b           -> pretty "'" <> pretty b <> pretty "'"
            ExprBitString bs    -> pretty "\"" <> hcat (map pretty bs) <> pretty "\""
            ExprConcat e1 e2    -> parens (pretty e1 <+> pretty "&" <+> pretty e2)
            ExprSlice e l h     -> pretty e <> parens (pretty l <+> pretty "to" <+> pretty h)
            ExprIsEq e1 e2      -> parens (pretty e1 <+> pretty "=" <+> pretty e2)
            ExprBoolConst True  -> pretty "TRUE"
            ExprBoolConst False -> pretty "FALSE"
            ExprAnd e1 e2       -> parens (pretty e1 <+> pretty "AND" <+> pretty e2)

data Bit = Zero | One deriving (Eq, Show)

instance Pretty Bit where
      pretty = \ case
            Zero -> pretty "0"
            One  -> pretty "1"

simplifyConcat :: Expr -> Expr
simplifyConcat = \ case
      ExprConcat e1 e2 -> simplify' (simplifyConcat e1) (simplifyConcat e2)
      e                -> e
      where simplify' :: Expr -> Expr -> Expr
            simplify' (ExprBitString []) e                  = e
            simplify' e (ExprBitString [])                  = e
            simplify' (ExprBitString s1) (ExprBitString s2) = ExprBitString (s1 ++ s2)
            simplify' e1 e2                                 = ExprConcat e1 e2


