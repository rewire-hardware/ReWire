module ReWire.Core where

-- Some scribbles as I try to figure out what form ReWire Core will take. Note
-- that the term ReWire Core as used here is different from the term as used
-- in the ICFPT paper; rather than being a normal form of Haskell it is a
-- separate language like GHC Core. This is the input to the partial
-- evaluator.

data RWCProg = RWCProg { dataDecls     :: [RWCData],
                         classDecls    :: [RWCClass],
                         instanceDecls :: [RWCInstance],
                         defaultDecls  :: [RWCDefaults], 
                         bindings      :: [RWCBinding] }
                       deriving (Eq,Show)

data RWCPat = RWCPatCon Id [RWCPat]
            | RWCPatLiteral Literal
            | RWCPatVar Id
            deriving (Eq,Show)

data RWCTy = ...

data RWCExp = RWCApp RWCTy Id [RWCExp]
            | RWCVar RWCTy Id
            | RWCCon RWCTy Id
            | RWCLiteral RWCTy Literal
            | RWCCase RWCTy RWCExp [RWCAlt]
            deriving (Eq,Show)

data RWCAlt = RWCAlt RWCPat RWCExp deriving (Eq,Show)

data RWCBinding = RWCNonRecBinding RWCDefn
                | RWCRecBinding [RWCDefn]
                deriving (Eq,Show)

data RWCDefn = RWCDefn Id [(Id,RWCTy)] RWCExp
               deriving (Eq,Show)
