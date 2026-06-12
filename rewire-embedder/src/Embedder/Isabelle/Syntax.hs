{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}


module Embedder.Isabelle.Syntax where

import Data.Data ( Typeable, Data(..) )
import ReWire.Pretty
    ( (<+>), punctuate, vsep, brackets, comma, dquotes,
      parens, space, Doc, Pretty(..), text, empty, TextShow (..) )
import Data.List (intersperse)
import Data.Text as T (Text, pack, isPrefixOf, replicate, length, intercalate, unpack)
import qualified Prettyprinter as P
import Embedder.Builtins (Builtin (..), TyBuiltin (..), RWUserOp(..), rwu2s,
                        tb2s)

import Debug.Trace (trace)
import Data.Maybe (fromMaybe)

{- helpers -}

textS :: String -> Doc ann
textS = text . pack

bar :: [Doc ann] -> Doc ann
bar = P.encloseSep (space <> space <> space) mempty (text " | ")

andS :: String
andS = "and"

($+$) :: Doc ann -> Doc ann -> Doc ann
a $+$ b = P.vcat [a, b]

tyAnn :: Doc ann -> Doc ann -> Doc ann
tyAnn d t = parens $ d <+> "::" <+> t

printCon :: Text -> [Doc ann] -> Doc ann
printCon "->" [a,b] = parens $ a <+> "\\<Rightarrow>" <+> b
printCon "(,)" [a,b] = parens $ a <> "," <+> b
printCon "@" [] = "@"
printCon "Tuple" ls = parens $ P.hsep $ punctuate comma ls
printCon "Nothing" [] = "None"
printCon "Just" [a] = parens $ "Some" <+> a
printCon "Left" [a] = parens $ "inj\\<^sub>1" <+> a
printCon "Right" [a] = parens $ "inj\\<^sub>2" <+> a
printCon s [] = text s
printCon s ls = parens $ text s <+> P.hsep ls

{- ------------------- Types ---------------------------------------- -}

-- TODO: refactor String ~> Text
-- TODO: Add type primitives? [prodS, sProdS, funS, cFunS, lFunS, sSumS]
-- TODO: Handle Modules?

-- | Names
type TName = Text

-- Types
data Typ = Type { typeId :: TName,
                  typeArgs :: [Typ] }
         | TBuiltin { typeB :: TyBuiltin,
                      typeArgs :: [Typ] }
         | TNum { typeN :: Int }
         | TVar { typeId :: TName }
         deriving (Eq, Ord, Show, Typeable, Data)

data TypSig = TypSig {typSigArgs :: [Typ], typeSigCod :: Typ } deriving (Eq, Ord, Show, Typeable, Data)

instance Pretty TypSig where
  pretty = printTypSig

instance Pretty Typ where
  pretty = printType

tyAtomic :: Typ -> Bool
tyAtomic = \ case
  Type _ [] -> True
  Type _ _ -> False
  TBuiltin _ [] -> True
  TBuiltin _ _ -> False
  TNum _ -> True
  TVar _ -> True

printType :: Typ -> Doc ann
printType t = case t of
  TVar name -> text $ if isPrefixOf "\'" name || isPrefixOf "?\'" name
                      then name else "\'" <> name
  TNum nvar -> text $ showt nvar
  TBuiltin b args -> printTBuiltin b args
  Type name args -> case name of
    -- ReWire base types:
    "()"   -> "unit"
    "Bool" -> "bool"
    "Maybe" -> case args of 
      [t1] -> printType t1 <+> "option"
      _ -> error "printType: incorrect Maybe type"
    "Either" -> case args of
      [t1,t2] -> pTyArg t1 <+> "\\<uplus>" <+> pTyArg t2
      _ -> error "printType: incorrect Either type"
    "Vec" -> case args of
              (TNum n:Type "Bool" []:_) -> text (showt n) <+> "word"
              (TNum n:t:_) -> parens (text (showt n) <> comma <+> printType t) <+> "cvec"
              _ -> error "printType: incorrect Vec type"
    -- infix notations
    "->" -> case args of
             [t1, t2] -> parens (pTyArg t1 <+> "\\<Rightarrow>" <+> printType t2)
             _ -> error "printType: incorrect arrow type"
    nm | isTuple nm -> parens $ P.sep $ punctuate " \\<times>" $ map printType args
    -- standard isabelle `('a,'b) Type` notation
    _ -> printType' name args

printTyTuple :: [Typ] -> Doc ann
printTyTuple [] = mempty
printTyTuple (t:ts) = printType t <+>  "\\<times>" <+> parens (printTyTuple ts)

pTyArg :: Typ -> Doc ann
pTyArg t = if tyAtomic t then
  pretty t else parens $ pretty t

printType' :: TName -> [Typ] -> Doc ann
printType' name args = case args of
           [] -> text name
           [arg] -> let d = pTyArg arg in
                      d <+> text name
           _ -> parens (P.sep $ punctuate comma $
                       map pretty args) <+> text name

printTBuiltin :: TyBuiltin -> [Typ] -> Doc ann
printTBuiltin TyUnit [] = text "unit"
printTBuiltin TyInteger [] = text "int"
printTBuiltin TyString [] = text "string"
printTBuiltin TyBool [] = text "bool"
printTBuiltin TyFun [a,b] = pretty a <+> text "\\<Rightarrow>" <+> pretty b
printTBuiltin TyReacT _ = trace "printTB: uneliminated monad transformer TyReacT" $ text "TyReacT"
printTBuiltin TyStateT _ = trace "printTB: uneliminated monad transformer TyStateT" $ text "TyStateT"
printTBuiltin TyIdentity _ = trace "printTB: uneliminated monad transformer Identity" $ text "TyIdentity"
printTBuiltin b@TyState args@[_,_] = printType' (tb2s b) args
printTBuiltin b@TyRe args@[_,_,_,_] = printType' (tb2s b) args
printTBuiltin b@TyDev args@[_,_] = printType' (tb2s b) args
printTBuiltin b@TyStateDev args@[_,_,_] = printType' (tb2s b) args
printTBuiltin TyProd [a,b] = pretty a <+> text "\\<times>" <+> pretty b
printTBuiltin TyList [a] = pTyArg a <+> text "list"
printTBuiltin b@TyVec args@[_,_] = printType' (tb2s b) args
printTBuiltin TyProxy [_] = text "nat"
printTBuiltin TyFin [n] = pTyArg n <+> text "fin"
printTBuiltin TyPlus [a,b] = pTyArg a <+> "+" <+> pTyArg b
printTBuiltin TyNeg [a] = text "-" <+> pTyArg a
printTBuiltin TyRef [a] = pTyArg a <+> text "Ref"
printTBuiltin _ _ = text "??TyBuiltin??"

isTuple :: Text -> Bool
isTuple c = c == "(" <> T.replicate (T.length c - 2) "," <> ")"

{-
instance Pretty Ty where
      pretty t = case flattenTyApp t of
            (TyCon _ (n2s -> c)     : ts)
                  | isTupleCtor c               -> parens $ hsep $ punctuate comma $ map pretty ts
            (TyCon _ (n2s -> "->")  : [t1, t2])
                  | needsParens t1              -> parens (pretty t1) <+> text "->" <+> pretty t2
            (TyCon _ (n2s -> "->")  : [t1, t2]) -> pretty t1 <+> text "->" <+> pretty t2
            (TyCon _ (n2s -> "[_]") : [t'])     -> brackets $ pretty t'
            [TyCon _ n]                         -> text $ n2s n
            [TyVar _ _ n]                       -> text $ showt n
            [TyNat _ n]                         -> text $ showt n
            ts                                  -> hsep $ map mparens ts
            where needsParens :: Ty -> Bool
                  needsParens t = case flattenTyApp t of
                        (TyCon _ (n2s -> "->") : _) -> True
                        _                           -> False

-}


printTypSig :: TypSig -> Doc ann
printTypSig (TypSig args cod) =  P.fillSep $ punctuate " \\<Rightarrow>" $ map printType args ++ [printType cod]


-- Types for Terms
data DTyp = Hide { typ :: Typ }
          | Disp { typ :: Typ }
      deriving (Eq, Ord, Show, Typeable, Data)



{- ------------------- Expressions ---------------------------------- -}

-- | Names
type VName = Text

-- | Names with Qualifiers
data QName = QName
    { qname :: Text
    , qualifiers :: [Text] }
  deriving (Eq, Ord, Typeable, Data)

instance Show QName where
    show q = unpack $ intercalate "." $ qualifiers q <> [qname q]


data Term =
        LitString Text
      | LitNum Integer
      | LitWord Int Integer
      | LitVec [Term]
      | Free { termName :: VName }
      | Prim { primId :: RWUserOp }
      | Abs { absVars :: [VName],
                termId :: Term }  -- lambda abstraction
      | App { funId :: Term,
               argIds :: [Term] }    -- application
      | If { ifId :: Term,
             thenId :: Term,
             elseId :: Term }
      | Case { termId :: Term,
               caseSubst :: [(Pttrn, Term)] }
      | Let { letSubst :: [(Pttrn, Term)],
              inId :: Term }
      | IsaEq { firstTerm :: Term,
                secondTerm :: Term }
      | Tuplex [Term]
      | List   [Term]
      | RecordVal [(Text, Term)]
      | RecordUpdate Term [(Text, Term)]
      | RecordSel Text Term
      | TypAnnTerm { termId :: Term, 
                     typAnn :: Typ}
      deriving (Eq, Ord, Show, Typeable, Data)

atomic :: Term -> Bool
atomic = \ case
       LitString _ -> True
       LitNum _    -> True
       LitWord _ _ -> True
       LitVec _    -> True
       Free _      -> True
       Prim _      -> True
       Abs _ e     -> atomic e
       App _ _     -> False
       If {}       -> False
       Case _ _    -> False
       Let bs e    -> atomic e && all (atomic . snd) bs
       IsaEq _ _   -> False
       Tuplex es   -> all atomic es
       List es     -> all atomic es
       TypAnnTerm e _ -> atomic e
       RecordVal fields -> Prelude.length fields <= 4 && all (atomic . snd) fields 
       RecordUpdate {} -> True
       RecordSel {} -> True


selfGroup :: Term -> Bool
selfGroup = \ case
       LitString _ -> True
       LitNum _    -> True
       LitWord _ _ -> True
       LitVec _    -> True
       Free _      -> True
       Prim _      -> True
       Abs {}      -> False
       App _ _     -> False
       If {}       -> True
       Case _ _    -> True
       Let {}      -> True
       IsaEq _ _   -> False
       Tuplex _    -> True
       List _      -> True
       RecordVal {}     -> True
       RecordUpdate {}  -> True
       RecordSel {}     -> False
       TypAnnTerm {} -> True


--              | Put | Get
--              | Add | Sub | Mul | Div | Mod | Pow
--              | Eq | Gt | GtEq | Lt | LtEq
----------------------------------------------
--                Error | Extern
--              | Bind | Return
--              | Signal | Lift | Extrude | Unfold
--              | VecFromList | VecReplicate | VecReverse | VecSlice | VecRSlice
--              | VecIndex | VecIndexProxy
--              | VecConcat
--              | VecMap | VecFoldR | VecFoldL | VecGenerate
--              | Finite | FiniteMinBound | FiniteMaxBound | ToFinite | ToFiniteMod | FromFinite
--              | NatVal
--              | Bits | Resize | BitSlice | BitIndex
--              | LAnd | LOr
--              | And | Or
--              | XOr | XNor
--              | LShift | RShift | RShiftArith
--              | LNot | Not
--              | RAnd | RNAnd | ROr | RNor | RXOr | RXNor
--              | MSBit

pArg :: Term -> Doc ann
pArg a = if selfGroup a then pretty a else parens (pretty a)

infixBuiltinList :: [Builtin]
infixBuiltinList = [
  Add, Sub, Mul, Pow, Div, Mod,
  Eq, Gt, GtEq, Lt, LtEq,
  Bind,
  VecIndex, VecIndexProxy, VecConcat,
  LAnd, LOr,
  And, Or, XOr, XNor,
  LShift, RShift, RShiftArith
  ]

infixRWUserOpList :: [RWUserOp]
infixRWUserOpList = [
  CompDot,
  NEq,
  BAnd, BOr, BXOr,
  BindI, BindS, BindR, BindRInf,
  Seq, SeqI, SeqS, SeqR, SeqRInf,
  RBindI, RBindS, RBindR, RBindRInf,
  FinAdd, FinSub, FinMul, FinDiv, FinEq, FinLt,
  WordSlice, WordIndex, WordIndexProxy, WordIndexFin
  ]

prefixBuiltinList :: [Builtin]
prefixBuiltinList = [Not]

prefixRWUserOpList :: [RWUserOp]
prefixRWUserOpList = []

isInfix :: RWUserOp -> Bool
isInfix (RWBuiltin b) = b `elem` infixBuiltinList
isInfix op = op `elem` infixRWUserOpList

isPrefix :: RWUserOp -> Bool
isPrefix (RWBuiltin b) = b `elem` prefixBuiltinList
isPrefix op = op `elem` prefixRWUserOpList

textListRWOp :: [(RWUserOp,Text)]
textListRWOp = [
  (RWBuiltin Add, "+"), (RWBuiltin Sub, "-"), (RWBuiltin Mul, "*"),
  (RWBuiltin Pow, "**"), (RWBuiltin Div, "div"), (RWBuiltin Mod, "mod"),
  (RWBuiltin Gt, ">"), (RWBuiltin GtEq, ">="), (RWBuiltin Lt, "<"), (RWBuiltin LtEq, "<="),
  (RWBuiltin Bind, "\\<bind>"),
  (RWBuiltin VecIndex, "!vf"), (RWBuiltin VecIndexProxy, "!v"),
  (RWBuiltin VecConcat, "++"), (RWBuiltin VecSlice, "slice"),
  (RWBuiltin VecRSlice, "rslice"),
  (RWBuiltin VecGenerate, "generate"), (RWBuiltin VecMap, "map"),
  (RWBuiltin VecReplicate, "replicate"), (RWBuiltin VecReverse, "reverse"),
  (RWBuiltin LAnd, "\\<and>w"), (RWBuiltin LOr, "\\<or>w"), (RWBuiltin LNot, "\\<not>w"),
  (RWBuiltin And, "&&"), (RWBuiltin Or, "||"), (RWBuiltin Not, "~~"),
  (RWBuiltin XOr, "xor"), (RWBuiltin XNor, "~xor"),
  (RWBuiltin LShift, "<<"), (RWBuiltin RShift, ">>"), (RWBuiltin RShiftArith, ">>>"),
  (CompDot, "o"),
  (BXOr, "x\\<or>"),
  (RBindI, "=<<I"), (RBindS, "=<<S"), (RBindR, "=<<R"), (RBindRInf, "=<<<R"),
  (BindI, "\\<bind>I"), (BindS, "\\<bind>S"), (BindR, "\\<bind>R"), (BindRInf, ">>>=R"),
  (Seq, "\\<then>"), (SeqI, "\\<then>I"), (SeqS, "\\<then>S"), (SeqR, "\\<then>R"), (SeqRInf, ">>>R"),
  (FinAdd, "+%"), (FinSub, "-%"), (FinMul, "*%"), (FinDiv, "div%"),
  (FinEq, "="), (FinLt, "<"),
  (WordIndex, "@!"), (WordSlice, "@@"),
  (VecLastIndexProxy, "lastIndexNat"),
  (WordIndexProxy,"!w"), (WordIndexFin,"!wf"),
  (NEq, "\\<noteq>"),
  (Update, "update")
  ]

{-
      (VecEmpty  , "empty"), (VecSingleton, "singleton"),
      (VecCons   , "cons"),  (VecSnoc     , "snoc"),
      (VecHead   , "head"),  (VecLastIndex, "lastIndex"),
      (VecLastIndexProxy, "lastIndex'"), (VecTake, "take"),
      (VecDrop   , "drop"),  (VecInit     , "init"),
      (VecTail   , "tail"),  (VecZipWith  , "zipWith"),
      (VecZipWith3, "zipWith3"), (VecPackLo, "packlo"),
      (VecPackHi , "packhi"), (VecUnpackLo, "unpacklo"),
      (VecUnpackHi, "unpackhi")]
-}


-- TODO: Need to handle vector-concat and word-concat separately
-- TODO: Similarly: liftS and liftR, bindS and bindR,
printApp :: Term -> [Term] -> Doc ann
-- handles operators with special formatting/parens
printApp (Prim (RWBuiltin Eq)) [a,b] = parens $ pArg a <+> "=" <+> pArg b
printApp (Prim BAnd) (a:as) = parens $ pArg a <+> "\\<and>" <+> P.hsep (map pArg as)
printApp (Prim BOr) (a:as) = parens $ pArg a <+> "\\<or>" <+> P.hsep (map pArg as)
printApp (Prim CompDol) (a:bs) = pArg a <+> parens (P.align (P.hsep (map pretty bs)))
printApp (Prim (RWBuiltin Bind)) (a:bs) = parens $ pArg a <+> "\\<bind>" <+> P.hsep (map pArg bs)
-- handle standard operator forms (infix or not) uniformly
printApp (Prim op) args = pOp op args
printApp (Free "not") [a] = parens $ "\\<not>" <+> pArg a
printApp (Free "not") [] = parens "\\<lambda> x. \\<not> x"
printApp (Free "Nothing") [] = "None"
printApp (Free "Just") [a] = "Some" <+> pArg a
printApp (Free "Left") [a] = "inj\\<^sub>1" <+> pArg a
printApp (Free "Right") [a] = "inj\\<^sub>2" <+> pArg a
printApp (Free op) args | isTuple op = printTuple args
printApp (Free op) args = printApp' (Free op) args
printApp e args = printApp' e args


rwOpText :: RWUserOp -> Text
rwOpText op = fromMaybe (fromMaybe "rwOpText: unimplemented op" (rwu2s op)) (lookup op textListRWOp)

pOp :: RWUserOp -> [Term] -> Doc ann
pOp op args@(arg:args') | isInfix op = case args' of
  [] -> P.group $ parens (text (rwOpText op)) <+> pArg arg
  (_:_) -> if all atomic args
           then P.group $ P.hsep (pArg arg : text (rwOpText op) : map pArg args')
           else pArg arg <+> text (rwOpText op) <+> P.align (P.hsep (map pArg args'))
pOp op args@(_:_) = if all atomic args
  then P.group $ P.hsep (text (rwOpText op) : map pArg args)
  else text (rwOpText op) <+> P.align (P.sep (map pArg args))
pOp op [] | isPrefix op = parens $ text $ "\\<lambda> x. " <> rwOpText op <> " x"
pOp op [] = text (rwOpText op)


printApp' :: Term -> [Term] ->  Doc ann
printApp' e args = if all atomic args
                  then P.group $ P.hsep (map pArg (e : args))
                  else pretty e <+> P.align (P.sep (map pArg args))

printTuple :: Pretty a => [a] -> Doc ann
printTuple es =  parens $ P.align $ P.fillSep $ punctuate comma (map pretty es)

instance Pretty Term where
  pretty = prettyTerm



      -- -- Additional notation: Bit=Bool, W n=Vec n Bit, extern, modify, length, len, fromList
      -- -- Additional notation: iter, iterSt
      -- -- Additional notation: id, const, (.), flip, ($), (&&), (||), not, otherwise, maybe, either, fst, snd, curry, uncurry, undefined, (=<<), (>>)
      -- Additional notation: empty, singleton, cons, snoc, head, lastIndex, lastIndex', take, drop, init, tail, zipWith, zipWith3, packlo, packhi, unpacklo, unpackhi, (!=) ; update
      --  -- Additional notation: Embedder.FiniteComp: +, -, *, div, ==, <, even, odd
      -- Additional notation: Embedder.Bits: zero, one, bit, lit, `xor`, (@@) ; bitSlice, (@.) ; bitIndex, rotR, rotL, (/=), even, odd, Lit=W128
      -- 


      -- | Primitives
      -- -- Defined Types/Data structs: Monad, MonadTrans, A_=, R_=, PuRe s o=Done(A_,s)|Pause(o,(R_,s)), Ref a=Ref String, Proxy (n::Nat)=Proxy
      -- -- Imported Types/Data structs: type(+),type(Nat),Identity, ReacT, StateT, Integer, String, Bool, Vec=Vector, KnownNat, Finite
      -- | Prelude
      -- -- Data structures: Maybe a = Nothing | Just a, Either a b = Left a | Right b, Bool = True | False
      -- | Vectors
      -- -- Missing prims: VecUpdate, VecBulkUpdate, VecIterate, VecZip, VecFromList
      -- -- Missing Builtins: VecFoldR, VecFoldL,
      -- | Bits 
      -- -- Missing prims: ToInteger,
      -- -- Not translated (directly): Bits, 
      -- ]


prettyTerm :: Term -> Doc ann
prettyTerm = \ case
     Free "not" -> parens "\\<lambda> x. \\<not> x"
     Free "Nothing" -> "None"
     Free v -> -- trace ("print Free: " <> unpack v) $ 
                text v
     App e args -> --trace ("print App: " <> show e  <> " " <> show args) $ 
                   printApp e args
     Tuplex es -> printTuple es
     List es -> brackets $ P.align $ P.fillSep $ punctuate comma (map pretty es)
     LitString txt -> P.squotes $ P.squotes $ text txt
     LitNum i -> pretty i
     LitWord n i -> parens $ pretty n <> text "w." <> pretty i
     Abs vs e -> parens $ "\\<lambda>" <+> P.hsep (map pretty vs) <> "." <+> pretty e
     If t c a  -> parens $ P.align $ P.fillSep ["if" <+> pretty t,
                                                "then" <+> pretty c,
                                                "else" <+> pretty a]
     Prim b | isInfix b -> parens $ text $ rwOpText b
     Prim b | isPrefix b -> parens $ text $ "\\<lambda> x. " <> rwOpText b <> " x"
     Prim b -> text (rwOpText b)
     Case e ps -> parens $ text "case" <+> pretty e <+> text "of"
        $+$ P.align (bar $ map (\ (p, t) ->
               P.fillSep [ pretty p <+> text "\\<Rightarrow>"
                    , parens $ pretty t]) ps)
     TypAnnTerm _e (TBuiltin TyProxy [TNum n]) -> pretty n
     TypAnnTerm e t -> tyAnn (pretty e) (pretty t)
                -- TODO: Could use conditional 'width' to align cases
     LitVec _es -> text "LitVec" -- something like 3w.[ a, b, c ]
     Let _bs _e -> text "Let" -- not needed yet
     IsaEq _e1 _e2 -> text "IsaEq" -- not needed yet
     RecordVal fields -> text "(|" <> P.hsep (punctuate comma (map ppField fields)) <> text "|)"
          where
          ppField (f, t) = text f <+> text "=" <+> pretty t

     RecordUpdate rec fields -> pretty rec <> text "(|" <> P.hsep (punctuate comma (map ppUpd fields)) <> text "|)"
          where
          ppUpd (f, t) = text f <+> text ":=" <+> pretty t

     RecordSel f t -> pretty t <> text "." <> text f



{- ------------------- Patterns --------------------------------------- -}

data Pttrn = PttrnWildCard (Maybe Typ)
           | PttrnVar VName (Maybe Typ)
           | PttrnCon VName (Maybe Typ) [Pttrn]
           | PttrnTuple (Maybe Typ) [Pttrn]
           | PttrnAs (Maybe Typ) Pttrn VName
           | PttrnRecord (Maybe Typ) [(Text, Pttrn)]
        deriving (Eq, Ord, Show, Typeable, Data)

instance Pretty Pttrn where
  pretty = \ case
    PttrnWildCard Nothing -> text "_"
    PttrnWildCard (Just t) -> tyAnn (text "_") (pretty t)
    PttrnVar name Nothing -> text name
    PttrnVar name (Just t) -> tyAnn (text name) (pretty t)
    PttrnCon name Nothing ps -> printCon name (map pretty ps)
    PttrnCon name (Just t) ps -> tyAnn (printCon name (map pretty ps)) (pretty t)
    PttrnTuple Nothing ps -> printTuple ps
    PttrnTuple (Just t) ps -> tyAnn (printTuple ps) (pretty t)
    PttrnAs Nothing p n -> parens $ pretty p <+> text "=:" <+> text n
    PttrnAs (Just t) p n -> tyAnn (parens (pretty p <+> text "=:" <+> text n)) (pretty t)
    PttrnRecord Nothing fields -> text "(|" <> P.hsep (punctuate comma (map ppField fields)) <> text "|)"
      where
        ppField (f, p) = text f <+> text "=" <+> pretty p
    PttrnRecord (Just t) fields -> tyAnn (text "(|" <> P.hsep (punctuate comma (map ppField fields)) <> text ")|)") (pretty t)
      where
        ppField (f, p) = text f <+> text "=" <+> pretty p






{- ------------------- Definitions ------------------------------------ -}



{- ------------------- Funs/Primrecs ---------------------------------- -}



{- ------------------- Datatypes and Type Synonyms -------------------- -}


data DatatypeConstructor = DatatypeConstructor {
      constructorName :: Text,
      constructorArgs :: [Typ] }
  deriving (Eq, Ord, Show, Typeable, Data)


instance Pretty DatatypeConstructor where
  pretty = printDatatypeConstructor

printDatatypeConstructor :: DatatypeConstructor -> Doc ann
printDatatypeConstructor (DatatypeConstructor name args) =
  text name <+> P.hsep (map (dquotes . pretty) args)

{-
datatype ('a,'b) tuplist = 
      ConsA "'a" "('a,'b) tuplist"  
      | TailB
      | TailA "'a \\<Rightarrow> ('a,'b) tuplist"
      | Empty
-}



{- --------------- Toplevel Declarations ----------------------------- -}


data Decl =
    Datatype {
      datatypeName :: Text,
      datatypeTVars :: [TName],
      datatypeConstructors :: [DatatypeConstructor] }
  | Record { 
      recordName :: Text, 
      recordTVars :: [Text], 
      recordFields :: [(Text, Typ)] }
  | TypeSynonym Text [Text] Typ
  | Definition {
      definitionName :: Text,
      definitionType :: TypSig,
      definitionVars :: [Term],
      definitionTerm :: Term }
  | Fun { funEquations :: [(Text, TypSig, [([Pttrn], Term)])] }
    deriving (Eq, Ord, Show, Typeable, Data)

instance Pretty Decl where
  pretty = \ case
    Datatype name tvs cons ->
      text "datatype" <+> pretty (Type name (map TVar tvs)) <+> P.align ("="
          <+> P.vsep (punctuate " |" (map pretty cons))) <> P.line
    Record name tvs fields ->
      P.nest 2 $ text "record" <+> pretty (Type name (map TVar tvs)) <+> text "="
          $+$ P.vsep (map (\(f, t) -> text f <+> "::" <+> dquotes (pretty t)) fields) <> P.line
    TypeSynonym name args ty ->
      text "type_synonym" <+> pretty (Type name (map TVar args)) <+> "=" <+> dquotes (pretty ty) <> P.line
    Definition name ty vs t ->
        (text "definition" <+> text name <+>
          P.align ("::" <+> P.fillSep [ dquotes (pretty ty) , text "where"]) $+$
         P.nest 2 (dquotes (text name <+> P.sep (map pretty vs) <+> text "=" $+$
                     pretty t)))
        <> P.line
    f@(Fun {}) -> text "fun"
          <+> P.vcat (intersperse (text "and") $
                    map (\ (name, tp, _) -> text name <+> text "::"
                          <+> dquotes (pretty tp) <+> empty)
                        (funEquations f))
          <+> text "where" $+$
       P.nest 2 (let eqs = concatMap (\ (name, _, e) -> map (\ e' -> (name, e')) e)
                            (funEquations f)
                     eqs' = map (\ (n, (vs, t)) -> dquotes (text n <+>
                           P.sep (map pretty vs) <+>
                           text "=" <+> pretty t)) eqs
                 in bar eqs')
          <> P.line
    -- f@(Fun {}) -> text "function"
    --       <+> P.vcat (intersperse (text "and") $
    --                 map (\ (name, tp, _) -> text name <+> text "::"
    --                       <+> dquotes (pretty tp) <+> empty)
    --                     (funEquations f))
    --       <+> text "where" $+$
    --    P.nest 2 (let eqs = concatMap (\ (name, _, e) -> map (\ e' -> (name, e')) e)
    --                         (funEquations f)
    --                  eqs' = map (\ (n, (vs, t)) -> dquotes (text n <+>
    --                        P.sep (map pretty vs) <+>
    --                        text "=" <+> pretty t)) eqs
    --              in bar eqs')
    --       <> P.line <> "  by auto"
    --       <> P.line



{- --------------- Theory File --------------------------------------- -}

data Theory = Theory
  { thyName :: Text
  , imports :: [Text]
  , decls :: [Decl]
  -- , graph :: Graph
  -- , tree :: [Tree Vertex]
  } deriving (Eq, Ord, Show, Typeable)


instance Pretty Theory where
  pretty thy@(Theory {}) =
        mkHeader thy
    $+$ printDecls thy
    $+$ mkFooter thy

printDecls :: Theory -> Doc ann
printDecls t@(Theory{}) = vsep (map pretty $ decls t)

mkHeader :: Theory -> Doc ann
mkHeader t@(Theory {}) =
      P.sep [text "theory", text $ thyName t]
  $+$ P.nest 2 (P.vcat (text "imports" : map text (imports t)))
  $+$ text "begin" $+$ mempty


mkFooter :: Theory -> Doc ann
mkFooter _ = mempty $+$ text "end"
