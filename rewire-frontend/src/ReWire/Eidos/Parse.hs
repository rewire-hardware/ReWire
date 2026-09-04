{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Safe #-}
-- | Parser for the Eidos concrete syntax (.eir; doc/eidos.md, section 9).
--   'ReWire.Eidos.Pretty' is the other half of the round-trip contract:
--   @parse . pretty@ is the identity on programs modulo annotations, and
--   @pretty . parse . pretty == pretty@ is a tested fixpoint.
--
--   The concrete syntax does not carry a type for every binder occurrence,
--   so (as in ReWire.Hyle.Parse) parsing is followed by an elaboration pass
--   that reconstructs what the format leaves implicit:
--
--   * variable occurrences print bare (@x#12@) and receive their binder's
--     'Id' (unique, occurrence text, and signature) from a scope map; an
--     occurrence whose unique is not in scope is an error;
--   * the case binder prints bare and receives the scrutinee's synthesized
--     type;
--   * join point labels print with no signature; a label's signature is
--     reconstructed as arrows from its parameter types to its body's
--     synthesized type (doc/eidos.md section 4.2).
--
--   Join point scoping, on the other hand, is resolved during parsing
--   proper: a scope map of join binders (keyed by unique) is threaded
--   through the expression grammar, 'Jump' sites take their 'JoinId'
--   (in particular its arity) from the binding, and a jump to an unbound
--   label is a parse-time error — labels are lexically scoped and never
--   escape (section 3.4), so no forward references exist. Join points are
--   not recursive: a label is not in scope in its own body.
--
--   Two lexical devices keep the grammar newline-insensitive:
--
--   * a name in expression-atom position followed by @::@ is not an atom
--     (it starts the next definition's signature line) — the expression
--     grammar itself has no bare @::@;
--   * a @occ#uniq@ token in type-atom position is a type variable only if
--     its unique is bound by the enclosing signature's @forall@, so the
--     equation name following a signature line never extends the
--     signature's type. (Both rely on global binder uniqueness; a program
--     that reuses a signature's type-variable unique as a term binder
--     unique — ill-formed per section 2 — may misparse.)
--
--   The machine level's process declarations parse in
--   'ReWire.Synolon.Parse', which reuses this grammar and elaboration for
--   the expressions, definitions, and datatypes a Synolon program embeds.
module ReWire.Eidos.Parse
      ( parseEir, parseEirText
        -- * The grammar and elaboration, for reuse by other parsers
      , Scope (..), TVScope, JScope, pendingSig
      , kindP, tyP, sigP, param, expP, atomP, defnP, dataDefnP
      , Env (..), insertVar, elabExp, elabDefn, synthTy
      ) where

import ReWire.Annotation (Annote, noAnn)
import ReWire.Builtins (Builtin, builtins)
import ReWire.Eidos.Lexer
import ReWire.Eidos.Syntax
import ReWire.Eidos.Types (dstArrow, flattenApp, instantiate)
import ReWire.Error (failAt, MonadError, AstError)
import ReWire.Pretty (showt)

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.HashMap.Strict (HashMap)
import Data.List (find)
import Data.Text (Text)
import Text.Megaparsec (many, some, try, (<|>), (<?>), parse, sepBy, notFollowedBy, optional, eof)
import Text.Megaparsec.Char (char)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Text.Megaparsec.Char.Lexer as L

parseEir :: (MonadError AstError m, MonadIO m) => FilePath -> m Program
parseEir p = liftIO (T.readFile p) >>= flip parseEirText p

parseEirText :: MonadError AstError m => Text -> FilePath -> m Program
parseEirText txt p = either failParse elabProgram $ parse (space *> programP <* eof) p txt

---
--- Scopes threaded through the grammar.
---

-- | Type variables bound by the enclosing signature's @forall@, by unique.
type TVScope = HashMap Uniq TyVar

-- | Join points in scope, by unique.
type JScope = HashMap Uniq JoinId

data Scope = Scope
      { scTVs   :: !TVScope
      , scJoins :: !JScope
      }

-- | A placeholder signature for binders whose types the concrete syntax
--   does not carry (variable occurrences, case binders, join labels);
--   every one of them is replaced by elaboration.
pendingSig :: Sig
pendingSig = monoSig $ TyCon noAnn "()"

---
--- Kinds, types, signatures.
---

kindP :: Parser Kind
kindP = do
      k <- kindAtom
      (KFun k <$> (arrow *> kindP)) <|> pure k
      <?> "kind"

kindAtom :: Parser Kind
kindAtom = (KStar <$ symbol "*")
      <|> (KNat <$ keyword "Nat")
      <|> parens kindP

-- | The built-in type-level arithmetic constructor names, @+@ @-@ @*@
--   (kind @Nat -> Nat -> Nat@); a @-@ immediately followed by @>@ is an
--   arrow, not an operator.
natOp :: Parser Text
natOp = lexeme $ ("+" <$ char '+')
      <|> ("*" <$ char '*')
      <|> try ("-" <$ (char '-' <* notFollowedBy (char '>')))

-- | Type, at top (arrow) level: arrows are right-associative and bind
--   loosest.
tyP :: TVScope -> Parser Ty
tyP tvs = do
      an <- getAnn
      t  <- tyAppP tvs
      (Arrow an t <$> (arrow *> tyP tvs)) <|> pure t
      <?> "type"

-- | Type, at application level (left-associative). The built-in arithmetic
--   constructors are admitted in head position only (the printer emits
--   prefix form, @+ 1 2@); the parenthesized infix form is handled by
--   'tyAtomP'.
tyAppP :: TVScope -> Parser Ty
tyAppP tvs = do
      an <- getAnn
      (natHead, h) <- tyHead
      as <- many $ tyAtomP tvs
      -- An unapplied arithmetic constructor has no printable form (the
      -- printer emits prefix application); reject it.
      when (natHead && null as) $ fail "unapplied type-level operator"
      pure $ foldl (TyApp an) h as
      where tyHead :: Parser (Bool, Ty)
            tyHead = ((True, ) <$> withSpan (flip TyCon <$> natOp)) <|> ((False, ) <$> tyAtomP tvs)

-- | Type, at atom level: a constructor, a type variable bound by the
--   enclosing signature (an @occ#uniq@ whose unique is not in scope is not
--   a type atom — that is what terminates a signature line before the
--   equation that follows it), a natural, the unit/tuple constructors, or
--   a parenthesized type (possibly the infix arithmetic sugar
--   @(ty natop ty)@, which desugars to prefix applications).
tyAtomP :: TVScope -> Parser Ty
tyAtomP tvs = tyName
      <|> withSpan (flip TyNat <$> natural)
      <|> withSpan (flip TyCon <$> tupleName)
      <|> withSpan (flip TyCon <$> listName)
      <|> parenTy
      <?> "type atom"
      where tyName :: Parser Ty
            tyName = try $ do
                  an       <- getAnn
                  (occ, u) <- nameMaybeUniq
                  case u of
                        Nothing -> pure $ TyCon an occ
                        Just u' -> case Map.lookup u' tvs of
                              Just v  -> pure $ TyVarT an v
                              Nothing -> fail $ "unbound type variable: " <> T.unpack occ <> "#" <> show u'

            nameMaybeUniq :: Parser (Text, Maybe Uniq)
            nameMaybeUniq = lexeme $ try $ do
                  x <- identRaw
                  u <- optional $ char '#' *> L.signed (pure ()) L.decimal
                  when (u == Nothing && x `Set.member` reservedWords) $ fail "reserved word"
                  pure (x, u)

            -- The list type constructor names ("[_]" and "[]", the bridge
            -- conventions), written tightly.
            listName :: Parser Text
            listName = lexeme $ try ("[_]" <$ symbol "[_]") <|> try ("[]" <$ symbol "[]")

            parenTy :: Parser Ty
            parenTy = do
                  an <- getAnn
                  _  <- symbol "("
                  t  <- tyP tvs
                  r  <- optional $ (,) <$> natOp <*> tyP tvs
                  _  <- symbol ")"
                  pure $ case r of
                        Nothing       -> t
                        Just (op, t2) -> TyApp an (TyApp an (TyCon an op) t) t2

-- | A type variable binder in a @forall@: @(a#7 :: kind)@.
tyVarBinder :: Parser TyVar
tyVarBinder = parens $ do
      (occ, u) <- uniqName
      dcolon
      TyVar occ u <$> kindP

-- | A signature: @forall (a#1 :: kind) ... . ty@, or a bare type. Returns
--   the scope extended with the quantified variables, for the types that
--   follow (a definition's parameters and body, a constructor's fields).
sigP :: TVScope -> Parser (Sig, TVScope)
sigP tvs = quantified <|> ((, tvs) . monoSig <$> tyP tvs)
      <?> "signature"
      where quantified :: Parser (Sig, TVScope)
            quantified = do
                  keyword "forall"
                  vs <- some tyVarBinder
                  _  <- symbol "."
                  let tvs' = foldr (\ v -> Map.insert (tvUniq v) v) tvs vs
                  (, tvs') . Sig vs <$> tyP tvs'

---
--- Expressions.
---

-- | A term binder with an ascribed type: @(x#1 :: ty)@.
param :: TVScope -> Parser Id
param tvs = binder <?> "parameter"
      where binder :: Parser Id
            binder = parens $ do
                  (occ, u) <- uniqName
                  dcolon
                  Id occ u . monoSig <$> tyP tvs

expP :: Scope -> Parser Exp
expP sc = lamE <|> letE <|> caseE <|> jumpE <|> appP sc
      <?> "expression"
      where lamE :: Parser Exp
            lamE = do
                  an <- getAnn
                  _  <- symbol "\\"
                  ps <- some $ param $ scTVs sc
                  arrow
                  flip (foldr $ Lam an) ps <$> expP sc

            letE :: Parser Exp
            letE = do
                  an <- getAnn
                  keyword "let"
                  (b, sc') <- bindP sc
                  keyword "in"
                  Let an b <$> expP sc'

            caseE :: Parser Exp
            caseE = do
                  an <- getAnn
                  keyword "case"
                  e <- expP sc
                  keyword "of"
                  (occ, u) <- uniqName
                  alts <- braces $ altP sc `sepBy` semi
                  dcolon
                  t <- tyP $ scTVs sc
                  -- The case binder's signature (the scrutinee's type) is
                  -- reconstructed by elaboration.
                  pure $ Case an t e (Id occ u pendingSig) alts

            jumpE :: Parser Exp
            jumpE = do
                  an <- getAnn
                  keyword "jump"
                  (occ, u) <- uniqName
                  case Map.lookup u $ scJoins sc of
                        Nothing -> fail $ "jump to unbound join point: " <> T.unpack occ <> "#" <> show u
                        Just j  -> Jump an j <$> parens (expP sc `sepBy` comma)

appP, atomP :: Scope -> Parser Exp
appP sc = do
      an <- getAnn
      h  <- atomP sc
      as <- many argP
      pure $ foldl (App an) h as
      where argP :: Parser Arg
            argP = (TArg <$> (symbol "@" *> tyAtomP (scTVs sc)))
               <|> (EArg <$> atomP sc)

atomP sc = withSpan (flip LitStr <$> stringLit)
      <|> varE
      <|> parenAtom
      <?> "atom"
      where -- A name followed by @::@ is not an atom: it starts the next
            -- definition's signature line. (The expression grammar itself
            -- has no bare @::@.)
            varE :: Parser Exp
            varE = try $ withSpan $ do
                  (occ, u) <- uniqName
                  notFollowedBy dcolon
                  pure $ \ an -> Var an $ Id occ u pendingSig

            parenAtom :: Parser Exp
            parenAtom = parens inner

            inner :: Parser Exp
            inner = listVec <|> conPrim <|> litInt <|> expP sc

            -- @list [e, ...] :: ty@ and @vec [e, ...] :: ty@.
            listVec :: Parser Exp
            listVec = do
                  an  <- getAnn
                  ctr <- (LitList an <$ keyword "list") <|> (LitVec an <$ keyword "vec")
                  es  <- brackets $ expP sc `sepBy` comma
                  dcolon
                  t   <- tyP $ scTVs sc
                  pure $ ctr t es

            -- @C :: ty@ (data constructor) or @rwPrimFoo :: ty@ (builtin).
            conPrim :: Parser Exp
            conPrim = do
                  an <- getAnn
                  c  <- conName
                  dcolon
                  t  <- tyP $ scTVs sc
                  if "rwPrim" `T.isPrefixOf` c
                        then maybe (fail $ "unknown builtin: " <> T.unpack c) (pure . Prim an t) $ lookupBuiltin c
                        else pure $ Con an t c

            -- @lit :: ty@ (integer literals may be negative).
            litInt :: Parser Exp
            litInt = do
                  an <- getAnn
                  n  <- integer
                  dcolon
                  LitInt an <$> tyP (scTVs sc) <*> pure n

lookupBuiltin :: Text -> Maybe Builtin
lookupBuiltin = flip Map.lookup builtinMap
      where builtinMap :: HashMap Text Builtin
            builtinMap = Map.fromList builtins

---
--- Bindings and case alternatives.
---

-- | A local binding (the part between @let@ and @in@). Returns the scope
--   for the let body: a @join@ adds its label (join points are not
--   recursive, so the label is not in scope in its own body). Join labels
--   carry their arity from the binding's parameter count; their signatures
--   are reconstructed by elaboration.
bindP :: Scope -> Parser (Bind, Scope)
bindP sc = recB <|> joinB <|> nonRecB
      <?> "binding"
      where nonRecB :: Parser (Bind, Scope)
            nonRecB = (, sc) . uncurry NonRec <$> eqP

            recB :: Parser (Bind, Scope)
            recB = do
                  keyword "rec"
                  (, sc) . Rec <$> braces (eqP `sepBy` semi)

            joinB :: Parser (Bind, Scope)
            joinB = do
                  keyword "join"
                  (occ, u) <- uniqName
                  ps   <- parens $ param (scTVs sc) `sepBy` comma
                  equals
                  body <- expP sc
                  let j = JoinId (Id occ u pendingSig) $ length ps
                  pure (Join j ps body, sc { scJoins = Map.insert u j $ scJoins sc })

            -- One equation of a (non-recursive or recursive) let:
            -- @x#1 :: ty = e@.
            eqP :: Parser (Id, Exp)
            eqP = do
                  (occ, u) <- uniqName
                  dcolon
                  t <- tyP $ scTVs sc
                  equals
                  (Id occ u $ monoSig t, ) <$> expP sc

-- | A case alternative: @_ -> e@ (default; first, if present),
--   @C (x#1 :: ty) ... -> e@, or @lit -> e@.
altP :: Scope -> Parser Alt
altP sc = defaultAlt <|> litAlt <|> dataAlt
      <?> "case alternative"
      where defaultAlt :: Parser Alt
            defaultAlt = do
                  an <- getAnn
                  underscore
                  arrow
                  Alt an DefaultAlt [] <$> expP sc

            litAlt :: Parser Alt
            litAlt = do
                  an <- getAnn
                  n  <- integer
                  arrow
                  Alt an (LitAlt n) [] <$> expP sc

            dataAlt :: Parser Alt
            dataAlt = do
                  an <- getAnn
                  c  <- conName
                  ps <- many $ param $ scTVs sc
                  arrow
                  Alt an (DataAlt c) ps <$> expP sc

---
--- Definitions, datatypes, programs.
---

-- | A definition: the signature line, then the equation line. The
--   signature's @forall@ binders scope over the equation's parameter and
--   body types; the equation's name must repeat the signature line's.
defnP :: Parser Defn
defnP = do
      an <- getAnn
      (occ, u) <- uniqName
      dcolon
      (sig, tvs)  <- sigP mempty
      attr        <- optional attrP
      orig        <- optional $ fromP tvs
      (occ', u')  <- uniqName
      unless (u == u' && occ == occ') $
            fail $ "definition equation name " <> T.unpack occ' <> "#" <> show u'
                <> " does not match its signature line (" <> T.unpack occ <> "#" <> show u <> ")"
      ps <- many $ param tvs
      equals
      body <- expP $ Scope tvs mempty
      pure $ Defn an (Id occ u sig) ps body attr orig
      <?> "definition"
      where attrP :: Parser DefnAttr
            attrP = (Inline <$ keyword "inline") <|> (NoInline <$ keyword "noinline")

            fromP :: TVScope -> Parser SpecOrigin
            fromP tvs = specP <|> bakedP
                  where specP = do
                              keyword "from"
                              SpecOrigin <$> bareName <*> parens (tyP tvs `sepBy` comma)
                        bakedP = do
                              keyword "baked"
                              BakeOrigin <$> bareName

-- | @data T kind { C1 :: sig1; ... }@ (the constructor list may be empty;
--   each constructor signature quantifies its own type variables).
dataDefnP :: Parser DataDefn
dataDefnP = do
      an <- getAnn
      keyword "data"
      t <- conName
      k <- kindP
      DataDefn an t k <$> braces (dataConP `sepBy` semi)
      <?> "data declaration"
      where dataConP :: Parser DataCon
            dataConP = do
                  an <- getAnn
                  c  <- conName
                  dcolon
                  DataCon an c . fst <$> sigP mempty

-- | @data* defn* proc* 'top' var@. @top@ must name a parsed definition
--   (matched by unique) and takes that definition's 'Id'.
programP :: Parser Program
programP = do
      ds <- many dataDefnP
      fs <- many defnP
      keyword "top"
      (occ, u) <- uniqName
      case find ((== u) . idUniq . defnId) fs of
            Just d  -> pure $ Program ds fs $ defnId d
            Nothing -> fail $ "top: designated device root " <> T.unpack occ <> "#" <> show u
                           <> " does not name a definition"
      <?> "program"

---
--- Elaboration: reconstruct the types the concrete syntax leaves implicit.
--- Variable occurrences take their binder's 'Id'; case binders take the
--- scrutinee's synthesized type; join labels take arrows from their
--- parameter types to their body's synthesized type (and jump sites take
--- the finalized 'JoinId' of their binding).
---

data Env = Env
      { envVars  :: HashMap Uniq Id
      , envJoins :: HashMap Uniq JoinId
      }

insertVar :: Id -> Env -> Env
insertVar x env = env { envVars = Map.insert (idUniq x) x $ envVars env }

elabProgram :: MonadError AstError m => Program -> m Program
elabProgram (Program ds fs top) = do
      fs' <- mapM (elabDefn env) fs
      pure $ Program ds fs' top
      where env :: Env
            env = Env (Map.fromList [ (idUniq $ defnId d, defnId d) | d <- fs ]) mempty

elabDefn :: MonadError AstError m => Env -> Defn -> m Defn
elabDefn env (Defn an x ps body attr orig) = do
      body' <- elabExp (foldr insertVar env ps) body
      pure $ Defn an x ps body' attr orig

elabExp :: forall m. MonadError AstError m => Env -> Exp -> m Exp
elabExp env = \ case
      Var an x           -> case Map.lookup (idUniq x) $ envVars env of
            Just xB -> pure $ Var an xB
            Nothing -> failAt an $ "unbound variable: " <> idOcc x <> "#" <> showt (idUniq x)
      e@Con {}           -> pure e
      e@Prim {}          -> pure e
      e@LitInt {}        -> pure e
      e@LitStr {}        -> pure e
      LitList an t es    -> LitList an t <$> mapM (elabExp env) es
      LitVec an t es     -> LitVec an t <$> mapM (elabExp env) es
      App an e a         -> App an <$> elabExp env e <*> elabArg a
      Lam an x e         -> Lam an x <$> elabExp (insertVar x env) e
      Let an b e         -> elabLet an b e
      Jump an j es       -> do
            es' <- mapM (elabExp env) es
            case Map.lookup (idUniq $ jpId j) $ envJoins env of
                  Just j' -> pure $ Jump an j' es'
                  Nothing -> failAt an $ "unbound join point: " <> idOcc (jpId j) <> "#" <> showt (idUniq $ jpId j)
      Case an t e x alts -> do
            e' <- elabExp env e
            ts <- synthTy e'
            let x' = x { idSig = monoSig ts }
            Case an t e' x' <$> mapM (elabAlt $ insertVar x' env) alts
      where elabArg :: Arg -> m Arg
            elabArg = \ case
                  EArg e -> EArg <$> elabExp env e
                  a      -> pure a

            elabAlt :: Env -> Alt -> m Alt
            elabAlt env' (Alt an c xs e) = Alt an c xs <$> elabExp (foldr insertVar env' xs) e

            elabLet :: Annote -> Bind -> Exp -> m Exp
            elabLet an b body = case b of
                  NonRec x e  -> do
                        e' <- elabExp env e
                        Let an (NonRec x e') <$> elabExp (insertVar x env) body
                  Rec eqs     -> do
                        let env' = foldr (insertVar . fst) env eqs
                        eqs' <- mapM (\ (x, e) -> (x, ) <$> elabExp env' e) eqs
                        Let an (Rec eqs') <$> elabExp env' body
                  Join j xs e -> do
                        e' <- elabExp (foldr insertVar env xs) e
                        bt <- synthTy e'
                        let x0 = jpId j
                            j' = JoinId (x0 { idSig = monoSig $ foldr (Arrow an . sigTy . idSig) bt xs }) $ jpArity j
                        body' <- elabExp env { envJoins = Map.insert (idUniq x0) j' $ envJoins env } body
                        pure $ Let an (Join j' xs e') body'

-- | Synthesize the type of an (already elaborated) expression: the located,
--   monadic twin of 'ReWire.Eidos.Types.typeOf', used where the concrete
--   syntax omits a type that the abstract syntax carries. Fails (rather
--   than 'error's) on ill-formed spines, so grossly ill-typed input is
--   rejected with a diagnostic here; everything subtler is the linter's
--   job.
synthTy :: forall m. MonadError AstError m => Exp -> m Ty
synthTy e = case e of
      Var _ x         -> pure $ sigTy $ idSig x
      Con _ t _       -> pure t
      Prim _ t _      -> pure t
      LitInt _ t _    -> pure t
      LitStr an _     -> pure $ TyCon an "String"
      LitList _ t _   -> pure t
      LitVec _ t _    -> pure t
      Lam an x b      -> Arrow an (sigTy $ idSig x) <$> synthTy b
      Let _ _ b       -> synthTy b
      Jump an j args  -> peel an (length args) $ sigTy $ idSig $ jpId j
      Case _ t _ _ _  -> pure t
      App an _ _      -> spineTy an
      where spineTy :: Annote -> m Ty
            spineTy an = case flattenApp e of
                  (Var an' x, args) -> do
                        let (tas, eas) = span isTArg args
                            tys        = [ t | TArg t <- tas ]
                        when (any isTArg eas) $ failAt an "type arguments must form a prefix of the application spine"
                        ht <- headTy an' (idSig x) tys
                        peel an (length eas) ht
                  (h, args)
                        | not $ any isTArg args -> synthTy h >>= peel an (length args)
                        | otherwise             -> failAt an "type argument applied to a non-variable head"

            headTy :: Annote -> Sig -> [Ty] -> m Ty
            headTy an sig tys
                  | null tys                        = pure $ sigTy sig
                  | length (sigTVs sig) == length tys = pure $ instantiate sig tys
                  | otherwise                       = failAt an "unsaturated type application"

            peel :: Annote -> Int -> Ty -> m Ty
            peel an n t
                  | n <= 0                    = pure t
                  | Just (_, u) <- dstArrow t = peel an (n - 1) u
                  | otherwise                 = failAt an "term argument applied to a non-arrow type"

            isTArg :: Arg -> Bool
            isTArg = \ case
                  TArg _ -> True
                  _      -> False
