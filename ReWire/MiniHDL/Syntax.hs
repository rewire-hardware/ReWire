module ReWire.MiniHDL.Syntax where

import ReWire.Pretty
import Text.PrettyPrint

data Program = Program { programUnits :: [Unit] }
             deriving (Eq,Show)

instance Pretty Program where
  pretty (Program units) = vcat (map pretty units)

data Unit = Unit Entity Architecture
          deriving (Eq,Show)

instance Pretty Unit where
  pretty (Unit ent arch) = pretty ent $+$ pretty arch

type Name = String

data Entity = Entity { entityName  :: Name,
                       entityPorts :: [Port] }
              deriving (Eq,Show)

instance Pretty Entity where
  pretty (Entity n ps) = text "entity" <+> text n <+> text "is"
                     $+$ nest 2 (text "port" <+> parens (vcat (punctuate semi (map pretty ps))) <> semi)
                     $+$ text "end" <+> text n <> semi

data Architecture = Architecture { archName       :: Name,
                                   archEntityName :: Name,
                                   archSignals    :: [Signal],
                                   archComponents :: [Component],
                                   archStmts      :: [Stmt] }
                    deriving (Eq,Show)

instance Pretty Architecture where
  pretty (Architecture n1 n2 sigs comps ss) = text "architecture" <+> text n1 <+> text "of" <+> text n2 <+> text "is"
                                          $+$ nest 2 (vcat (map pretty sigs))
                                          $+$ nest 2 (vcat (map pretty comps))
                                          $+$ text "begin"
                                          $+$ nest 2 (vcat (map pretty ss))
                                          $+$ text "end" <+> text n1 <> semi

data Component = Component { componentName  :: Name,
                             componentPorts :: [Port] }
                 deriving (Eq,Show)

instance Pretty Component where
  pretty (Component n ps) = text "component" <+> text n
                        $+$ nest 2 (text "port" <+> parens (vcat (punctuate semi (map pretty ps))) <> semi)
                        $+$ text "end component;"

data Port = Port { portName      :: Name,
                   portDirection :: Direction,
                   portType      :: Ty }
            deriving (Eq,Show)

instance Pretty Port where
  pretty (Port n d t) = text n <> colon <+> pretty d <+> pretty t

data Signal = Signal { signalName :: Name,
                       signalType :: Ty }
              deriving (Eq,Show)

instance Pretty Signal where
  pretty (Signal n t) = text "signal" <+> text n <> colon <+> pretty t <> semi

data Direction = In | Out deriving (Eq,Show)

instance Pretty Direction where
  pretty In  = text "in"
  pretty Out = text "out"

data Ty = TyStdLogic | TyStdLogicVector Int | TyBool deriving (Eq,Show)

instance Pretty Ty where
  pretty TyStdLogic           = text "std_logic"
  pretty (TyStdLogicVector n) = text "std_logic_vector" <+> parens (text "0 to" <+> int (n-1))
  pretty TyBool               = text "boolean"

data Stmt = Assign LHS Expr
          | WithAssign Expr LHS [(Expr,Expr)] (Maybe Expr)
          | Instantiate Name Name PortMap
          | ClkProcess Name [Stmt]
          deriving (Eq,Show)

instance Pretty Stmt where
  pretty (Assign lhs e)           = pretty lhs <+> text "<=" <+> pretty e <> semi
  pretty (WithAssign e lhs bs mb) = text "with" <+> pretty e <+> text "select" <+> pretty lhs <+> text "<=" <+> vcat (punctuate comma branches) <> semi
      where branches = map (\(e1,e2) -> pretty e1 <+> text "when" <+> pretty e2) bs
                       ++ case mb of
                            Nothing -> []
                            Just e  -> [pretty e <+> text "when others"]
  pretty (Instantiate n1 n2 pm)   = text n1 <> colon <+> text n2 <+> text "port map" <+> parens (pretty pm) <> semi
  pretty (ClkProcess n ss)        = text "process" <> parens (text n)
                                $+$ text "begin"
                                $+$ nest 2 (text "if" <+> text n <> text "'event and" <+> text n <+> text "= '1' then")
                                $+$ nest 4 (vcat (map pretty ss))
                                $+$ nest 2 (text "end if;")
                                $+$ text "end process;"

data PortMap = PortMap [(Name,Name)]
             deriving (Eq,Show)

instance Pretty PortMap where
  pretty (PortMap ps) = vcat (punctuate comma (map (\(n1,n2) -> text n1 <+> text "=>" <+> text n2) ps))

data LHS = LHSName Name
         deriving (Eq,Show)

instance Pretty LHS where
  pretty (LHSName n) = text n

data Expr = ExprName Name
          | ExprBit Bit
          | ExprBitString [Bit]
          | ExprConcat Expr Expr
          | ExprSlice Expr Int Int
          | ExprIsEq Expr Expr
          | ExprBoolConst Bool
          deriving (Eq,Show)

instance Pretty Expr where
  pretty (ExprName n)          = text n
  pretty (ExprBit b)           = text "'" <> pretty b <> text "'"
  pretty (ExprBitString bs)    = text "\"" <> hcat (map pretty bs) <> text "\""
  pretty (ExprConcat e1 e2)    = parens (pretty e1 <+> text "&" <+> pretty e2)
  pretty (ExprSlice e l h)     = pretty e <> parens (int l <+> text "to" <+> int h)
  pretty (ExprIsEq e1 e2)      = parens (pretty e1 <+> text "=" <+> pretty e2)
  pretty (ExprBoolConst True)  = text "TRUE"
  pretty (ExprBoolConst False) = text "FALSE"

data Bit = Zero | One deriving (Eq,Show)

instance Pretty Bit where
  pretty Zero = text "0"
  pretty One  = text "1"
