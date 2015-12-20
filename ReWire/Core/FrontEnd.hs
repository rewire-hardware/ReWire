{-# LANGUAGE ViewPatterns, LambdaCase, NamedFieldPuns, FlexibleInstances #-}
module ReWire.Core.FrontEnd
      ( parseFile
      , ParseResult(..)
      , SrcLoc(..)
      , prettyPrint
      ) where

import ReWire.Core.Kinds
import ReWire.Core.Syntax
import ReWire.Scoping (mkId, Id, fv)
import ReWire.SYB

import Control.Applicative ((<*>))
import Control.Arrow ((&&&))
import Control.Monad (foldM, replicateM, (>=>), mzero, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (runStateT, StateT, get, put)
import Data.Data (Data, cast)
import Data.Foldable (foldl', foldrM)
import Data.Functor ((<$>))
import Data.Functor.Identity
import Data.List (nub, find, foldl1)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat)
import qualified Data.Map.Strict as Map
import System.FilePath (joinPath, (<.>))

import qualified Language.Haskell.Exts as Haskell (parseFile)
import           Language.Haskell.Exts hiding (parseFile, loc, name, binds, op, Kind)

import ReWire.Core.PrettyPrintHaskell

ppRWC :: ModMeta -> Trans ()
ppRWC (ModMeta m exps) = do
      liftIO $ putStrLn "\nEXPORTS:"
      liftIO $ print exps
      liftIO $ putStrLn "\nMOD:"
      liftIO $ print $ ppHaskell m

pp :: Module -> Trans ()
pp = liftIO . putStrLn . prettyPrint

-- | Recursively opens and parses files.
parseFile :: FilePath -> IO (ParseResult RWCModule)
parseFile = runTrans . getModule
      where runTrans :: Trans a -> IO (ParseResult a)
            runTrans = except2PR . (flip runStateT initState >=> return . fst)

            except2PR :: ExceptT (SrcLoc, String) IO a -> IO (ParseResult a)
            except2PR (runExceptT -> e) = e >>= return . \case
                  Left (loc, msg) -> ParseFailed loc msg
                  Right a         -> return a

toFilePath :: ModuleName -> FilePath
toFilePath (ModuleName n) = joinPath (splitOn "." n) <.> "hs"

-- | Desugar into lambdas then normalize the lambdas.
desugar :: Module -> Trans Module
desugar = runT (normIds <> deparenify <> desugarInfix <> wheresToLets)
      >=> runT desugarFuns
      >=> runT (desugarTuples <> desugarDos)
      >=> runT desugarLets
      >=> runT (desugarIfs <> desugarNegs <> desugarWildCards)
      -- *** Normalize lambdas:
      >=> runT flattenLambdas
      >=> runT depatLambdas
      >=> runT desugarAsPats
      >=> runT lambdasToCases

unknownLoc :: SrcLoc
unknownLoc = SrcLoc "" 0 0

initState :: TransState
initState = TS 0 Map.empty

data FQName = FQName ModuleName Name
      deriving (Eq, Show)

class ToQName a where
      toQName :: a -> QName
instance ToQName QName where
      toQName = id
instance ToQName FQName where
      toQName (FQName m x) = Qual m x
instance ToQName Name where
      toQName = UnQual

class FromQName a where
      fromQName :: QName -> a
instance FromQName QName where
      fromQName = id
instance FromQName FQName where
      fromQName (Qual m x) = FQName m x
instance FromQName Name where
      fromQName (Qual _ x) = x
      fromQName (UnQual x) = x
instance FromQName String where
      fromQName (Qual m x)         = qual (FQName m x)
      fromQName (UnQual (Ident x)) = x

tySigil :: (ToQName a, FromQName b) => a -> b
tySigil x = case toQName x of
      Qual m (Ident x') -> fromQName $ Qual m $ Ident $ "#" ++ x'
      UnQual (Ident x') -> fromQName $ UnQual $ Ident $ "#" ++ x'

data TransState = TS
      { tsNextFresh :: Int
      , tsModCache  :: Map.Map FilePath ModMeta
      }
      deriving Show

data ModMeta = ModMeta
      { mmModule  :: RWCModule
      , mmExports :: [FQName]
      }
      deriving Show

getModule :: FilePath -> Trans RWCModule
getModule fp = do
      ModMeta m _ <- getCached fp
      return m

getExports :: FilePath -> Trans [FQName]
getExports fp = do
      ModMeta _ exps <- getCached fp
      return exps

getCached :: FilePath -> Trans ModMeta
getCached fp = do
      mods <- tsModCache <$> get
      case Map.lookup fp mods of
            Just mm  -> return mm
            Nothing -> do
                  m <- justParse fp
                  m' <- desugar m
                  pp m'
                  rn <- mkRenamer m'
                  m'' <- trans rn m'
                  ppRWC m''
                  cache fp m''
                  return m''

      where justParse :: FilePath -> Trans Module
            justParse = (liftIO . Haskell.parseFile) >=> pr2Trans
            pr2Trans :: ParseResult a -> Trans a
            pr2Trans = \case
                  ParseOk p           -> return p
                  ParseFailed loc msg -> pFail loc msg

mkRenamer :: Module -> Trans Renamer
mkRenamer (Module loc m _ _ _ imps ds) = do
      rns <- mapM toRenamer imps
      return $ mconcat rns

toRenamer :: ImportDecl -> Trans Renamer
toRenamer (ImportDecl loc m quald _ _ _ as specs) = do
      exps <- getExports $ toFilePath m
      callMrt exps as specs
      where callMrt exps Nothing   Nothing          = mkTable m Nothing exps
            callMrt exps (Just m') Nothing          = mkTable m' Nothing exps
            callMrt exps (Just m') (Just (h, imps)) = mkTable m' (Just (h, map getImp imps)) exps
            callMrt exps Nothing   (Just (h, imps)) = mkTable m (Just (h, map getImp imps)) exps

            getImp (IVar n)              = n
            getImp (IAbs _ n)            = n
            getImp (IThingAll _n)        = error "wtf"
            getImp (IThingWith _n _cons) = error "well now my fucking map got to be a fold"

            mkTable :: ModuleName -> Maybe (Bool, [Name]) -> [FQName] -> Trans Renamer
            -- No list of imports -- so import everything.
            mkTable m' Nothing exps = mkTable m' (Just (False, map getUnQual exps)) exps
                  where getUnQual :: FQName -> Name
                        getUnQual (FQName _ n) = n
            -- List of imports, no "hiding".
            mkTable m' (Just (False, imps)) exps = foldM ins Map.empty imps
                  where ins table imp = case find (cmp imp) exps of
                              Just exp -> return $
                                    let tab' = Map.insert (Qual m' imp) exp table
                                    in if quald then tab' else Map.insert (UnQual imp) exp tab'
                              Nothing  -> pFail loc $ "importing an unexported symbol from " ++ prettyPrint m
            -- List of imports with "hiding" -- import everything, then delete
            -- the items from the list.
            mkTable m' (Just (True, imps)) exps = do
                  tab <- mkTable m' Nothing exps
                  foldM del tab imps
                  where del table imp = case find (cmp imp) exps of
                              Just exp -> return
                                    $ Map.delete (Qual m' imp)
                                    $ Map.delete (UnQual imp) table
                              Nothing  -> pFail loc $ "importing an unexported symbol from " ++ prettyPrint m

            cmp :: Name -> FQName -> Bool
            cmp imp (FQName _ exp) = imp == exp

type Renamer = Map.Map QName FQName

-- | Parameters: the renamer, the name to rename, a module to use as the default qualifier.
--   Returns: the fully qualified name.
rename :: (ToQName a, FromQName b) => Renamer -> a -> b
rename rn x = fromQName . maybe (toQName x) toQName $ Map.lookup (toQName x) rn

extend :: ToQName a => [(a, FQName)] -> Renamer -> Renamer
extend kvs = Map.union $ Map.fromList $ map ((toQName . fst) &&& snd) kvs

qual :: FQName -> String
qual (FQName (ModuleName m) (Ident n)) = m ++ "." ++ n

unqual :: FQName -> QName
unqual (FQName _ n) = UnQual n

exclude :: ToQName a => [a] -> Renamer -> Renamer
exclude = foldr ((.) . Map.delete . toQName) id

-- | True iff an entry for the name exists in the renamer.
finger :: Renamer -> QName -> Bool
finger = flip Map.member

type Trans = StateT TransState (ExceptT (SrcLoc, String) IO)

pFail :: SrcLoc -> String -> Trans a
pFail loc msg = lift $ throwE (loc, msg)

update :: (TransState -> TransState) -> Trans ()
update = (<$> get) >=> put

cache :: FilePath -> ModMeta -> Trans ()
cache fp mm = do
      mods <- tsModCache <$> get
      update (\s -> s {tsModCache = Map.insert fp mm $ tsModCache s})

fresh :: Trans Name
fresh = do
      x <- tsNextFresh <$> get
      update $ \s -> s {tsNextFresh = x + 1}
      return $ Ident $ "$" ++ show x

-- | Removes parens in types, expressions, and patterns so they don't confuddle
--   everything.
deparenify :: Transform Trans
deparenify = match (\(Paren n)   -> return n)
          <> match (\(PParen n)  -> return n)
          <> match (\(TyParen n) -> return n)

mkTuple :: Int -> Name
mkTuple n = Ident $ "(" ++ replicate (n-1) ',' ++ ")"

-- | Turns Symbols and Specials into normal identifiers and adds a prefix to
--   all names in the type namespace.
normIds :: Transform Trans
normIds = match (\case
            Symbol n               -> return $ Ident n)
       <> match (\case
            Special UnitCon        -> return $ UnQual $ Ident "()"
            Special ListCon        -> return $ UnQual $ Ident "List"
            Special FunCon         -> return $ UnQual $ Ident "->"
            -- I think this is only for the prefix constructor.
            Special (TupleCon _ i) -> return $ UnQual $ mkTuple i
            Special Cons           -> return $ UnQual $ Ident "Cons")
       <> match (\case
            TyCon x                -> return $ TyCon $ tySigil x)
       <> match (\case
            DataDecl a b c x d e f -> return $ DataDecl a b c (tySigil x) d e f)

-- | Turns sections and infix ops into regular applications and lambdas.
desugarInfix :: Transform Trans
desugarInfix = match $ \case
      LeftSection e (QVarOp op)  -> return $ App (Var op) e
      LeftSection e (QConOp op)  -> return $ App (Con op) e
      RightSection (QVarOp op) e -> do
            x <- fresh
            return $ Lambda unknownLoc [PVar x] $ App (App (Var op) $ Var $ UnQual x) e
      RightSection (QConOp op) e -> do
            x <- fresh
            return $ Lambda unknownLoc [PVar x] $ App (App (Con op) $ Var $ UnQual x) e
      InfixApp e1 (QVarOp op) e2 -> return $ App (App (Var op) e1) e2
      InfixApp e1 (QConOp op) e2 -> return $ App (App (Con op) e1) e2

-- | Turns wildcard patterns into variable patterns.
desugarWildCards :: Transform Trans
desugarWildCards = match $ \PWildCard -> return $ PVar $ Ident "$_"

-- | Turns piece-wise function definitions into a single PatBind with a lambda
--   and case expression on the RHS. E.g.:
-- > f p1 p2 = rhs1
-- > f q1 q2 = rhs2
-- becomes
-- > f = \$1 $2 -> case ($1, $2) of { (p1, p2) -> rhs1; (q1, q2) -> rhs2 }
desugarFuns :: Transform Trans
desugarFuns = match $ \case
      FunBind ms@(Match loc name pats _ _ Nothing:_) -> do
            e <- buildLambda loc ms $ length pats
            return $ PatBind loc (PVar name) (UnGuardedRhs e) Nothing
      n@(FunBind _)                                  -> pFail unknownLoc $ "unsupported decl syntax: " ++ prettyPrint n
      where buildLambda :: SrcLoc -> [Match] -> Int -> Trans Exp
            buildLambda loc ms 1 = do
                  alts <- mapM toAlt ms
                  x <- fresh
                  return $ Lambda loc [PVar x] $ Case (Var $ UnQual x) alts
            buildLambda loc ms arrity = do
                  alts <- mapM toAlt ms
                  xs <- replicateM arrity fresh
                  return $ Lambda loc (map PVar xs) $ Case (Tuple Boxed (map (Var . UnQual) xs)) alts
            toAlt :: Match -> Trans Alt
            toAlt (Match loc' _ [p] Nothing rhs binds) = return $ Alt loc' p rhs binds
            toAlt (Match loc' _ ps Nothing rhs binds)  = return $ Alt loc' (PTuple Boxed ps) rhs binds
            toAlt m@(Match loc' _ _ _ _ _)             = pFail loc' $ "unsupported decl syntax: " ++ prettyPrint m

-- | Turns tuples into applications of a TupleN constructor (also in types and pats):
-- > (x, y, z)
-- becomes
-- > (Tuple3 x y z)
desugarTuples :: Transform Trans
desugarTuples = match (\(Tuple _ es)   -> return $ foldl' App (Con $ UnQual $ mkTuple $ length es) es)
             <> match (\(TyTuple _ ts) -> return $ foldl' TyApp (TyCon $ UnQual $ mkTuple $ length ts) ts)
             <> match (\(PTuple _ ps)  -> return $ PApp (UnQual $ mkTuple $ length ps) ps)

-- | Turns do-notation into a series of >>= \x ->. Turns LetStmts into Lets.
--   Should run before Let and Lambda desugarage. E.g.:
-- > do p1 <- m
-- >    let p2 = e
-- >    return e
-- becomes
-- > m >>= (\p1 -> (let p2 = e in return e))
desugarDos :: Transform Trans
desugarDos = match $ \(Do stmts) -> transDo stmts
      where transDo :: [Stmt] -> Trans Exp
            transDo = \case
                  Generator loc p e : stmts -> App (App (Var $ UnQual $ Ident ">>=") e) . Lambda loc [p] <$> transDo stmts
                  [Qualifier e]             -> return e
                  Qualifier e : stmts       -> App (App (Var $ UnQual $ Ident ">>=") e) . Lambda unknownLoc [PWildCard] <$> transDo stmts
                  LetStmt binds : stmts     -> Let binds <$> transDo stmts
                  s : _                     -> pFail unknownLoc $ "unsupported syntax in do-block: " ++ prettyPrint s
                  _                         -> pFail unknownLoc "something went wrong while translating a do-block."

-- | Turns where clauses into lets. Only valid because we're disallowing
--   guards, so this pass also raises an error if it encounters a guard.
wheresToLets :: Transform Trans
wheresToLets = match (\case
                  Match loc name ps t (UnGuardedRhs e) (Just binds) -> return $ Match loc name ps t (UnGuardedRhs $ Let binds e) Nothing
                  n@(Match loc _ _ _ (GuardedRhss _) _)             -> pFail loc $ "guards are not supported: " ++ prettyPrint n)
            <> match (\case
                  PatBind loc p (UnGuardedRhs e) (Just binds) -> return $ PatBind loc p (UnGuardedRhs $ Let binds e) Nothing
                  n@(PatBind loc _ (GuardedRhss _) _)         -> pFail loc $ "guards are not supported: " ++ prettyPrint n)
            <> match (\case
                  Alt loc p (UnGuardedRhs e) (Just binds) -> return $ Alt loc p (UnGuardedRhs $ Let binds e) Nothing
                  n@(Alt loc _ (GuardedRhss _) _)         -> pFail loc $ "guards are not supported: " ++ prettyPrint n)

-- | Turns Lets into Cases. Assumes functions in Lets are already desugared.
--   E.g.:
-- > let p = e1
-- >     q = e2
-- > in e3
-- becomes
-- > case e1 of { p -> (case e2 of { q -> e3 } }
desugarLets :: Transform Trans
desugarLets = match $ \case
      Let (BDecls ds) e -> foldrM transLet e ds
      n@(Let _ _)       -> pFail unknownLoc $ "unsupported let syntax: " ++ prettyPrint n
      where transLet :: Decl -> Exp -> Trans Exp
            transLet (PatBind loc p (UnGuardedRhs e1) Nothing) inner = return $ Case e1 [Alt loc p (UnGuardedRhs inner) Nothing]
            transLet n@(PatBind loc _ _ _)                     _     = pFail loc $ "unsupported let syntax: " ++ prettyPrint n
            transLet n                                         _     = pFail unknownLoc $ "unsupported syntax: " ++ prettyPrint n

-- | Turns ifs into cases and unary minus.
-- > if e1 then e2 else e3
-- becomes
-- > case e1 of { True -> e2; False -> e3 }
desugarIfs :: Transform Trans
desugarIfs = match $
      \(If e1 e2 e3) -> return $ Case e1
            [ Alt unknownLoc (PApp (UnQual $ Ident "True")  []) (UnGuardedRhs e2) Nothing
            , Alt unknownLoc (PApp (UnQual $ Ident "False") []) (UnGuardedRhs e3) Nothing
            ]

desugarNegs :: Transform Trans
desugarNegs = match $
      \(NegApp e) -> return $ App (App (Var $ UnQual $ Ident "-") $ Lit $ Int 0) e

-- | Turns Lambdas with several bindings into several lambdas with single
--   bindings. E.g.:
-- > \p1 p2 -> e
-- becomes
-- > \p1 -> \p2 -> e
flattenLambdas :: Transform Trans
flattenLambdas = match $
      \(Lambda loc ps e) -> return $ foldr (Lambda loc . return) e ps

-- | Replaces non-var patterns in lambdas with a fresh var and a case. E.g.:
-- > \(a,b) -> e
-- becomes
-- > \$x -> case $x of { (a,b) -> e }
depatLambdas :: Transform Trans
depatLambdas = match $ \case
      n@(Lambda _ [PVar _] _) -> return n
      Lambda loc [p] e        -> do
            x <- fresh
            return $ Lambda loc [PVar x] (Case (Var $ UnQual x) [Alt loc p (UnGuardedRhs e) Nothing])

-- | Desugars as-patterns in (and only in) cases into more cases. Should run
--   after depatLambdas. E.g.:
-- > case e1 of
-- >   x@(C y@p) -> e2
-- becomes
-- > case e1 of { C p -> (\x -> ((\y -> e2) p)) (C p) }
desugarAsPats :: Transform Trans
desugarAsPats = match $
      \(Alt loc p (UnGuardedRhs e) Nothing) -> do
            app <- foldrM (mkApp loc) e $ getAses p
            return $ Alt loc (deAs p) (UnGuardedRhs app) Nothing
      where mkApp :: SrcLoc -> (Pat, Pat) -> Exp -> Trans Exp
            mkApp loc (p, p') e = App (Lambda loc [p] e) <$> patToExp p'
            getAses :: Pat -> [(Pat, Pat)]
            getAses = runQ $ query' $ \case
                  PAsPat n p -> [(PVar n, p)]
                  _          -> []
            deAs :: Pat -> Pat
            deAs = runIdentity . runT (match' $
                  \case PAsPat _ p -> return p
                        n          -> return n)
            patToExp :: Pat -> Trans Exp
            patToExp = \case
                  PVar n            -> return $ Var $ UnQual n
                  PLit s n          -> return $ Lit $ deSign s n
                  -- PNPlusK _name _int ->
                  PApp n ps         -> foldl' App (Con n) <$> mapM patToExp ps
                  PList ps          -> List <$> mapM patToExp ps
                  -- PRec _qname _patfields ->
                  PAsPat _ p        -> patToExp p
                  PIrrPat p         -> patToExp p
                  PatTypeSig _ p _  -> patToExp p
                  -- PViewPat _exp _pat ->
                  PBangPat p        -> patToExp p
                  p                 -> pFail unknownLoc $ "unsupported pattern: " ++ prettyPrint p

-- | Turns beta-redexes into cases. E.g.:
-- > (\x -> e2) e1
-- becomes
-- > case e1 of { x -> e2 }
lambdasToCases :: Transform Trans
lambdasToCases = match $
      \(App (Lambda loc [p] e2) e1) -> return $ Case e1 [Alt loc p (UnGuardedRhs e2) Nothing]

-- | Translate a Haskell module into the ReWire abstract syntax.
trans :: Renamer -> Module -> Trans ModMeta
trans rn (Module loc m _pragmas _ exps imps (reverse -> ds)) = do
      let rn' = extend (zip (getGlobs ds) $ map (FQName m) $ getGlobs ds) rn
      tyDefs <- foldM (transData rn') [] ds
      tySigs <- foldM (transTySig rn') [] ds
      inls   <- foldM transInlineSig [] ds
      fnDefs <- foldM (transDef rn' tySigs inls) [] ds
      exps'  <- maybe (return $ map (FQName m) $ getGlobs ds) (foldM (transExport loc m rn') []) exps
      imps'  <- mapM (getModule . toFilePath . importModule) imps
      return $ ModMeta (mergeMods $ RWCModule tyDefs fnDefs : imps') exps'
      where getGlobs :: [Decl] -> [Name]
            getGlobs = foldl' (flip getGlobs') []
                  where getGlobs' :: Decl -> [Name] -> [Name]
                        getGlobs' = \case
                              DataDecl _ _ _ n _ cons _     -> ((n : foldr getCtors [] cons) ++)
                              PatBind _ (PVar n) _ _        -> (n :)
                              _                             -> id
                        getCtors :: QualConDecl -> [Name] -> [Name]
                        getCtors = \case
                              QualConDecl _ _ _ (ConDecl n _) -> (n :)
                              _                               -> id
            mergeMods :: [RWCModule] -> RWCModule
            mergeMods = foldl1 mergeMods'
                  where mergeMods' (RWCModule ts fs) (RWCModule ts' fs') = RWCModule (ts ++ ts') (fs ++ fs')

transExport :: SrcLoc -> ModuleName -> Renamer -> [FQName] -> ExportSpec -> Trans [FQName]
transExport loc m rn exps = \case
      EVar x              -> if finger rn x
            then return $ rename rn x : exps
            else pFail loc $ "unknown name in export list: " ++ prettyPrint x
      EAbs _ x -> if finger rn x
            then return $ rename rn x : exps
            else pFail loc $ "unknown name in export list: " ++ prettyPrint x
      EThingAll _       -> pFail unknownLoc "TODO: EThingAll"
      EThingWith _ _    -> pFail unknownLoc "TODO: EThingWith"
      EModuleContents m -> do
            exps' <- getExports $ toFilePath m
            return $ exps' ++ exps

transData :: Renamer -> [RWCData] -> Decl -> Trans [RWCData]
transData rn datas (DataDecl loc _ _ x tyVars cons _deriving) = do
      tyVars' <- mapM (transTyVar loc) tyVars
      cons' <- mapM (transCon rn) cons
      return $ RWCData (TyConId $ rename rn x) tyVars' kblank cons' : datas
transData _  datas _                                          = return datas

transTySig :: Renamer -> [(Name, RWCTy)] -> Decl -> Trans [(Name, RWCTy)]
transTySig rn sigs (TypeSig loc names t) = do
      t' <- transTy loc rn [] t
      return $ zip names (repeat t') ++ sigs
transTySig _ sigs _                      = return sigs

-- I guess this doesn't need to be in the monad, really, but whatever...  --adam
-- Not sure what the boolean field means here, so we ignore it!  --adam
transInlineSig :: [Name] -> Decl -> Trans [Name]
transInlineSig inls = \case
      (InlineSig _ _ AlwaysActive (Qual _ x)) -> return $ x : inls
      (InlineSig _ _ AlwaysActive (UnQual x)) -> return $ x : inls
      _                                       -> return inls

transDef :: Renamer -> [(Name, RWCTy)] -> [Name] -> [RWCDefn] -> Decl -> Trans [RWCDefn]
transDef rn tys inls defs (PatBind loc (PVar x) (UnGuardedRhs e) Nothing) = case lookup x tys of
      Just t -> (:defs) . RWCDefn (mkId $ rename rn x) (nub (fv t) :-> t) (x `elem` inls) <$> transExp loc rn e
      _      -> pFail loc $ "no type signature for " ++ prettyPrint x
transDef _  _   _    defs _                                               = return defs

transTyVar :: SrcLoc -> TyVarBind -> Trans (Id RWCTy)
transTyVar loc = \case
      UnkindedVar (Ident x) -> return $ mkId x
      tv                    -> pFail loc $ "unsupported type syntax: " ++ prettyPrint tv

transCon :: Renamer -> QualConDecl -> Trans RWCDataCon
transCon rn = \case
      QualConDecl loc [] _ (ConDecl x tys) -> (RWCDataCon $ DataConId $ rename rn x) <$> mapM (transTy loc rn []) tys
      d@(QualConDecl loc _ _ _)            -> pFail loc $ "unsupported ctor syntax: " ++ prettyPrint d

transTy :: SrcLoc -> Renamer -> [Name] -> Type -> Trans RWCTy
transTy loc rn ms = \case
      TyForall Nothing cs t    -> do
           ms' <- mapM (getNad loc) cs
           transTy loc rn (ms ++ ms') t
      TyFun a b                -> mkArrow <$> transTy loc rn ms a <*> transTy loc rn ms b
      TyApp a b | isMonad ms a -> RWCTyComp <$> transTy loc rn ms a <*> transTy loc rn ms b
                | otherwise    -> RWCTyApp <$> transTy loc rn ms a <*> transTy loc rn ms b
      TyCon x                  -> return $ RWCTyCon (TyConId $ rename rn x)
      TyVar x                  -> return $ RWCTyVar (mkId $ prettyPrint x)
      t                        -> pFail loc $ "unsupported type syntax: " ++ prettyPrint t

getNad :: SrcLoc -> Asst -> Trans Name
getNad loc = \case
      ClassA (UnQual (Ident "Monad")) [TyVar x] -> return x
      a                                         -> pFail loc $ "unsupported typeclass constraint: " ++ prettyPrint a

isMonad :: [Name] -> Type -> Bool
isMonad ms = \case
      TyApp (TyApp (TyApp (TyCon (UnQual (Ident "ReT"))) _) _) t -> isMonad ms t
      TyApp (TyApp (TyCon (UnQual (Ident "StT"))) _) t           -> isMonad ms t
      TyCon (UnQual (Ident "I"))                                 -> True
      TyVar x                                                    -> x `elem` ms
      _                                                          -> False

kblank :: Kind
kblank = Kstar

tblank :: RWCTy
tblank = RWCTyCon (TyConId "_")

transExp :: SrcLoc -> Renamer -> Exp -> Trans RWCExp
transExp loc rn = \case
      App (App (Var (UnQual (Ident "nativeVhdl"))) (Lit (String f))) e
                            -> RWCNativeVHDL f <$> transExp loc rn e
      App e1 e2             -> RWCApp <$> transExp loc rn e1 <*> transExp loc rn e2
      Lambda loc [PVar x] e -> RWCLam (mkId $ prettyPrint x) tblank <$> transExp loc (exclude [x] rn) e
      Var x                 -> return $ RWCVar (mkId $ rename rn x) tblank
      Con x                 -> return $ RWCCon (DataConId $ rename rn x) tblank
      Lit lit               -> RWCLiteral <$> transLit loc lit
      Case e alts           -> RWCCase <$> transExp loc rn e <*> mapM (transAlt rn) alts
      e                     -> pFail loc $ "unsupported expression syntax: " ++ prettyPrint e

-- Not entirely sure this is right...
deSign :: Sign -> Literal -> Literal
deSign Negative = \case
      Int        l -> Int (-l)
      Frac       l -> Frac (-l)
      PrimInt    l -> PrimInt (-l)
      PrimWord   l -> PrimWord (-l)
      PrimFloat  l -> PrimFloat (-l)
      PrimDouble l -> PrimDouble (-l)
deSign _ = id

transLit :: SrcLoc -> Literal -> Trans RWCLit
transLit loc = \case
      Int i  -> return $ RWCLitInteger i
      Frac d -> return $ RWCLitFloat (fromRational d)
      Char c -> return $ RWCLitChar c
      lit    -> pFail loc $ "unsupported syntax for a literal: " ++ prettyPrint lit

transAlt :: Renamer -> Alt -> Trans RWCAlt
transAlt rn = \case
      Alt loc p (UnGuardedRhs e) Nothing -> RWCAlt <$> transPat loc rn p <*> transExp loc (exclude (getVars p) rn) e
      a@(Alt loc _ _ _)                  -> pFail loc $ "unsupported syntax: " ++ prettyPrint a
      where getVars :: Pat -> [Name]
            getVars = runQ $ query' $ \case
                  PVar x -> [x]
                  _      -> []

transPat :: SrcLoc -> Renamer -> Pat -> Trans RWCPat
transPat loc rn = \case
      PApp x ps          -> RWCPatCon (DataConId $ rename rn x) <$> mapM (transPat loc rn) ps
      PLit s lit         -> RWCPatLiteral <$> transLit loc (deSign s lit)
      PVar x             -> return $ RWCPatVar (mkId $ prettyPrint x) tblank
      p                  -> pFail loc $ "unsupported syntax in a pattern: " ++ prettyPrint p
