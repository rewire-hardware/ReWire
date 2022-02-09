{-# LANGUAGE Safe, OverloadedStrings #-}
module ReWire.VHDL.Syntax where

import Prettyprinter (Pretty (..), parens, (<+>), vsep, hcat, hsep, semi, colon, punctuate, comma, nest, align, Doc)
import ReWire.Pretty (($$), empty, text)
import Data.Text (Text, splitOn)
import Data.List (intersperse)
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (listToMaybe, mapMaybe)

type Size  = Word
type Index = Int

newtype Program = Program { programUnits :: [Unit] }
      deriving (Eq, Show)

instance Pretty Program where
      pretty (Program units) = vsep (intersperse empty $ map pretty units)

data Unit = Unit ![Text] !Entity !Architecture
      deriving (Eq, Show)

instance Pretty Unit where
      pretty (Unit uses ent arch) = prettyImports "library" (getLibs uses)
            $$ prettyImports "use" uses
            $$ pretty ent
            $$ pretty arch

            where getLibs :: [Text] -> [Text]
                  getLibs = filter (/= "work") . nubOrd . mapMaybe (listToMaybe . splitOn ".")

                  prettyImports :: Text -> [Text] -> Doc ann
                  prettyImports pre = vsep . map ((pretty pre <+>) . (<> semi) . pretty)

type Name = Text

data Entity = Entity
      { entityName  :: !Name
      , entityPorts :: ![Port]
      } deriving (Eq, Show)

instance Pretty Entity where
      pretty (Entity n ps) = nest 2 (text "entity" <+> text n <+> text "is" $$ text "port" <+> parens (align $ vsep $ punctuate semi $ map pretty ps) <> semi)
            $$ text "end" <+> text n <> semi

data Architecture = Architecture
      { archName       :: !Name
      , archEntityName :: !Name
      , archSignals    :: ![Signal]
      , archComponents :: ![Component]
      , archStmts      :: ![Stmt]
      } deriving (Eq, Show)

instance Pretty Architecture where
      pretty (Architecture n1 n2 sigs comps ss) = nest 2 (vsep $ [ text "architecture" <+> text n1 <+> text "of" <+> text n2 <+> text "is" ]
                ++ map pretty sigs
                ++ map pretty comps)
            $$ nest 2 (vsep $ text "begin" : map pretty ss)
            $$ text "end" <+> text n1 <> semi

data Component = Component
      { componentName  :: !Name
      , componentPorts :: ![Port]
      } deriving (Eq, Show)

instance Pretty Component where
      pretty (Component n ps) = nest 2 (text "component" <+> text n $$ text "port" <+> parens (align $ vsep (punctuate semi (map pretty ps))) <> semi)
            $$ text "end component;"

data Port = Port
      { portName      :: !Name
      , portDirection :: !Direction
      , portType      :: !Ty
      } deriving (Eq, Show)

instance Pretty Port where
      pretty (Port n d t) = text n <> colon <+> pretty d <+> pretty t

data Signal = Signal
      { signalName :: !Name
      , signalType :: !Ty
      } deriving (Eq, Show)

instance Pretty Signal where
      pretty (Signal n t) = text "signal" <+> text n <> colon <+> pretty t <> semi

data Direction = In | Out deriving (Eq, Show)

instance Pretty Direction where
      pretty In  = text "in"
      pretty Out = text "out"

data Ty = TyStdLogic
        | TyStdLogicVector !Size
        | TyBool
        | TyClock -- not a real VHDL type, represented as std_logic.
        | TyRegister !Name !Size -- not real VHDL, just a hack.
        deriving (Eq, Show)

instance Pretty Ty where
      pretty TyStdLogic           = text "std_logic"
      pretty (TyStdLogicVector n) = text "std_logic_vector" <+> parens (text "0 to" <+> pretty (toInteger n - 1))
      pretty TyBool               = text "boolean"
      pretty TyClock              = text "std_logic"
      pretty (TyRegister _ n)     = text "std_logic_vector" <+> parens (text "0 to" <+> pretty (toInteger n - 1))

data Stmt = Assign !LHS !Expr
          | WithAssign !Expr !LHS ![(Expr, Expr)] !(Maybe Expr)
          | Instantiate !Name !Name !PortMap
          | ClkProcess !Name ![Stmt]
      deriving (Eq, Show)

instance Pretty Stmt where
      pretty = \ case
            Assign lhs e           -> pretty lhs <+> text "<=" <+> pretty e <> semi
            WithAssign e lhs bs mb -> nest 2 ((text "with" <+> pretty e)
                        $$ nest 2 (text "select" <+> pretty lhs <+> text "<=" <+> align (vsep $ punctuate comma branches) <> semi))
                  where branches = map (\ (e1, e2) -> pretty e1 <+> text "when" <+> pretty e2) bs
                                ++ maybe [] (\ e -> [pretty e <+> text "when others"]) mb
            Instantiate n1 n2 pm   -> pretty n1 <> colon <+> pretty n2 <+> text "port map" <+> parens (align $ pretty pm) <> semi
            ClkProcess n ss        -> text "process" <> parens (pretty n)
                  $$ nest 2 (vsep
                        [ text "begin"
                        , nest 2 (vsep $ (text "if" <+> text n <> text "'event and" <+> text n <+> text "= '1' then") : map pretty ss)
                        , text "end if;"
                        ])
                  $$ text "end process;"

newtype PortMap = PortMap [(Name, Expr)]
      deriving (Eq, Show)

instance Pretty PortMap where
      pretty (PortMap ps) = vsep (punctuate comma (map (\ (n, e) -> text n <+> text "=>" <+> pretty e) ps))

newtype LHS = LHSName Name
      deriving (Eq, Show)

instance Pretty LHS where
      pretty (LHSName n) = text n

data Expr = ExprName !Name
          | ExprBit !Bit
          | ExprBitString ![Bit]
          | ExprConcat !Expr !Expr
          | ExprSlice !Expr !Index !Index
          | ExprIsEq !Expr !Expr
          | ExprBoolConst !Bool
          | ExprAnd !Expr !Expr
          | ExprFunCall !Name ![Expr]
      deriving (Eq, Show)

instance Pretty Expr where
      pretty = \ case
            ExprName n          -> text n
            ExprBit b           -> text "'" <> pretty b <> text "'"
            ExprBitString bs    -> text "\"" <> hcat (map pretty bs) <> text "\""
            ExprConcat e1 e2    -> parens (pretty e1 <+> text "&" <+> pretty e2)
            ExprSlice e l h     -> pretty e <> parens (pretty l <+> text "to" <+> pretty h)
            ExprIsEq e1 e2      -> parens (pretty e1 <+> text "=" <+> pretty e2)
            ExprBoolConst True  -> text "TRUE"
            ExprBoolConst False -> text "FALSE"
            ExprAnd e1 e2       -> parens (pretty e1 <+> text "AND" <+> pretty e2)
            ExprFunCall n args  -> text n <> parens (hsep $ punctuate comma $ map pretty args)

data Bit = Zero | One deriving (Eq, Show)

instance Pretty Bit where
      pretty = \ case
            Zero -> text "0"
            One  -> text "1"

simplifyConcat :: Expr -> Expr
simplifyConcat = \ case
      ExprConcat e1 e2 -> simplify' (simplifyConcat e1) (simplifyConcat e2)
      e                -> e
      where simplify' :: Expr -> Expr -> Expr
            simplify' (ExprBitString []) e                  = e
            simplify' e (ExprBitString [])                  = e
            simplify' (ExprBitString s1) (ExprBitString s2) = ExprBitString (s1 ++ s2)
            simplify' e1 e2                                 = ExprConcat e1 e2

