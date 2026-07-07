{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Cryptol-to-Hyle translation: the engine behind rwc's Cryptol
--   foreign-function interface. 'translate' loads and typechecks a
--   Cryptol module (with the Cryptol implementation itself -- the
--   typechecker needs z3 on the PATH), elaborates the instantiation
--   @fn : ty@ (so Cryptol's own typechecker decides whether the use-site
--   type is admissible), monomorphizes it with Cryptol's specializer,
--   and translates the resulting closure of monomorphic definitions to
--   Hyle.
--
--   The translation is the inverse of the width-preserving embedding the
--   Cryptol backend uses (doc/hyle.md, section 8.4): a Cryptol word
--   @[n]@ is a Hyle bitvector of the same numeric value, sequence and
--   tuple element zero sits at the most-significant end, and @Bit@ is
--   one bit. Cryptol's specializer erases the type arguments of
--   primitive instances (a literal's value, an index operator's
--   dimensions), so specialization runs through its lower-level
--   'S.withDeclGroups' interface, whose name map recovers each
--   primitive clone's instantiation types.
--
--   Supported fragment (v1): first-order functions over Bit, words,
--   vectors, and tuples; if-then-else; local value bindings; the scalar
--   and slicing primitives. Comprehensions, folds, local function
--   bindings, variable indexing, records, enums, and recursion are
--   rejected with (it is hoped) actionable messages.
module ReWire.Cryptol.Translate (translate) where

import ReWire.Annotation (noAnn)
import ReWire.BitVector (BV (..), bitVec, zeros, nbits)
import ReWire.Pretty (showt)

import qualified ReWire.Hyle.Syntax as A

import qualified Cryptol.Eval                 as E
import qualified Cryptol.ModuleSystem         as M
import qualified Cryptol.ModuleSystem.Env     as ME
import qualified Cryptol.ModuleSystem.Monad   as MM
import qualified Cryptol.ModuleSystem.Name    as N
import qualified Cryptol.Parser               as P
import qualified Cryptol.Transform.Specialize as S
import qualified Cryptol.TypeCheck.AST        as T
import qualified Cryptol.TypeCheck.InferTypes as TI
import qualified Cryptol.TypeCheck.Solver.SMT as SMT
import qualified Cryptol.TypeCheck.TypeMap    as TM
import qualified Cryptol.TypeCheck.TypeOf     as T (fastTypeOf)
import qualified Cryptol.Utils.Ident          as I
import qualified Cryptol.Utils.Logger         as L
import Cryptol.Utils.PP (pp)

import Control.Exception (try, SomeException)
import Control.Monad (unless, zipWithM)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError, liftEither)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlphaNum)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import System.FilePath (takeDirectory)

import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified Data.Text           as T

-- | Translate function @fn@ from Cryptol module @file@, at the
--   monomorphic Cryptol type @ty@, to a self-contained set of Hyle
--   definitions whose entry point is named @entry@ (helpers are prefixed
--   with it). Left is a (possibly multi-line, source-located) diagnostic.
translate :: FilePath -> Text -> Text -> Text -> IO (Either Text [A.Defn])
translate file fn ty entry = either bail id <$> (try go :: IO (Either SomeException (Either Text [A.Defn])))
      where bail :: SomeException -> Either Text [A.Defn]
            bail e = Left $ "cryptol: " <> T.pack (show e) <> "\n(is z3 on the PATH?)"

            go :: IO (Either Text [A.Defn])
            go = do
                  env0 <- M.initialModuleEnv
                  let env = env0 { ME.meSearchPath = takeDirectory file : ME.meSearchPath env0 }
                  SMT.withSolver (pure ()) (TI.defaultSolverConfig $ ME.meSearchPath env) $ \ solver -> runExceptT $ do
                        let minp menv = M.ModuleInput
                                    { M.minpCallStacks  = False
                                    , M.minpSaveRenamed = False
                                    , M.minpEvalOpts    = pure $ E.EvalOpts L.quietLogger E.defaultPPOpts
                                    , M.minpByteReader  = BS.readFile
                                    , M.minpModuleEnv   = menv
                                    , M.minpTCSolver    = solver
                                    }
                            run :: M.ModuleCmd a -> ME.ModuleEnv -> ExceptT Text IO (a, ME.ModuleEnv)
                            run cmd menv = do
                                  (r, _warns) <- liftIO $ cmd $ minp menv
                                  either (throwError . T.pack . show . pp) pure r
                        (_, env1)                 <- run (M.loadModuleByPath file) env
                        pe                        <- either (throwError . T.pack . show . P.ppError) pure
                                                          $ P.parseExpr $ fn <> " : " <> ty
                        ((_, texpr, _sch), env2)  <- run (M.checkExpr pe) env1
                        ((spec, _cache), _env3)   <- run (\ mi -> MM.runModuleT mi $ S.runSpecT Map.empty
                                                            $ S.withDeclGroups (ME.allDeclGroups env2)
                                                            $ S.specializeExpr texpr) env2
                        let (body, dgs, nmap) = spec
                        liftEither $ transClosure entry body dgs nmap

---
--- The specialized closure to Hyle definitions.
---

data TEnv = TEnv
      { tePrims :: HashMap Int (Text, [T.Type]) -- ^ Primitive clones: name and instantiation types.
      , teDefns :: HashMap Int (A.GId, A.Sig)   -- ^ Translated definitions: Hyle name and signature.
      , teTypes :: Map.Map N.Name T.Schema      -- ^ Schemas of everything in scope, for type reconstruction.
      , teScope :: HashMap Int A.Exp            -- ^ Local binders.
      }

transClosure :: Text -> T.Expr -> [T.DeclGroup] -> Map.Map N.Name (TM.TypesMap N.Name) -> Either Text [A.Defn]
transClosure entry body dgs nmap = do
      mapM_ noRec dgs
      entryName <- case fst (spine body) of
            T.EVar x -> pure x
            _        -> Left "cryptol: unexpected expression shape after specialization (rwcry bug)."
      let decls   = concatMap T.groupDecls dgs
          primSet = HM.fromList [ (N.nameUnique $ T.dName d, ()) | d <- decls, isPrim $ T.dDefinition d ]
          prims   = HM.fromList [ (N.nameUnique cl, (I.identText $ N.nameIdent orig, tys))
                                | (orig, tm) <- Map.toList nmap
                                , (tys, cl)  <- TM.toListTM tm
                                , N.nameUnique cl `HM.member` primSet
                                ]
          exprDs  = [ (d, e) | d <- decls, T.DExpr e <- [T.dDefinition d] ]
      unless (any ((== N.nameUnique entryName) . N.nameUnique . T.dName . fst) exprDs)
            $ Left "cryptol: the requested function is a Cryptol primitive; wrap it in a Cryptol definition."
      names <- HM.fromList <$> zipWithM (mkName entryName) [0 :: Int ..] exprDs
      let env = TEnv { tePrims = prims
                     , teDefns = names
                     , teTypes = Map.fromList [ (T.dName d, T.dSignature d) | d <- decls ]
                     , teScope = mempty
                     }
      mapM (transDecl env) exprDs
      where noRec :: T.DeclGroup -> Either Text ()
            noRec = \ case
                  T.Recursive ds -> Left $ "cryptol: recursive definitions are not supported: "
                        <> T.intercalate ", " (map (I.identText . N.nameIdent . T.dName) ds)
                  _              -> pure ()

            isPrim :: T.DeclDef -> Bool
            isPrim = \ case
                  T.DPrim -> True
                  _       -> False

            mkName :: N.Name -> Int -> (T.Decl, T.Expr) -> Either Text (Int, (A.GId, A.Sig))
            mkName entryName i (d, _) = do
                  (aszs, rsz) <- sigWidths $ T.dSignature d
                  let gid | N.nameUnique (T.dName d) == N.nameUnique entryName = entry
                          | otherwise = entry <> "." <> sanitize (I.identText $ N.nameIdent $ T.dName d) <> "$" <> showt i
                  pure (N.nameUnique $ T.dName d, (gid, A.Sig noAnn aszs rsz))

-- | Translate one definition: peel its lambdas into parameters; a
--   point-free definition (fewer lambdas than its type has arguments)
--   gets the shortfall eta-expanded on the Hyle side.
transDecl :: TEnv -> (T.Decl, T.Expr) -> Either Text A.Defn
transDecl env (d, e) = do
      (gid, sig@(A.Sig _ aszs _)) <- maybe (Left "cryptol: unnamed definition (rwcry bug)") pure
            $ HM.lookup (N.nameUnique $ T.dName d) $ teDefns env
      let (params, body) = peel e
          pnames         = [ pName i x | (i, (x, _)) <- zip [0 :: Int ..] params ]
          etas           = [ ("$eta" <> showt i, sz) | (i, sz) <- drop (length params) $ zip [0 :: Int ..] aszs ]
          scope          = HM.fromList [ (N.nameUnique x, A.Var noAnn sz n)
                                       | ((x, _), n, sz) <- zip3 params pnames aszs ]
          types          = foldr (\ (x, t) -> Map.insert x (T.tMono t)) (teTypes env) params
          env'           = env { teScope = scope <> teScope env, teTypes = types }
      body' <- transExp env' (map (uncurry (A.Var noAnn) . swap2) etas) body
      pure $ A.Defn noAnn gid sig (pnames <> map fst etas) body'
      where peel :: T.Expr -> ([(N.Name, T.Type)], T.Expr)
            peel = \ case
                  T.ELocated _ e'  -> peel e'
                  T.EProofAbs _ e' -> peel e'
                  T.EAbs x t e'    -> let (ps, b) = peel e' in ((x, t) : ps, b)
                  e'               -> ([], e')

            pName :: Int -> N.Name -> A.Name
            pName i x = sanitize (I.identText $ N.nameIdent x) <> "$" <> showt i

            swap2 :: (a, b) -> (b, a)
            swap2 (a, b) = (b, a)

-- | Translate an expression; @etas@ are extra (already-translated)
--   arguments to append to the outermost application (the eta-expansion
--   of a point-free definition).
transExp :: TEnv -> [A.Exp] -> T.Expr -> Either Text A.Exp
transExp env etas e0 = case spine e0 of
      (T.EVar x, args) -> do
            args' <- (<> etas) <$> mapM (transExp env []) args
            apply env x args'
      (e, _ : _)              -> Left $ "cryptol: unsupported application head" <> unsupported e
      (e, []) | not (null etas) -> Left $ "cryptol: unsupported point-free definition shape" <> unsupported e
      (e, [])     -> case e of
            T.ETuple es    -> A.cat <$> mapM (transExp env []) es
            T.EList es _   -> A.cat <$> mapM (transExp env []) es
            T.EIf c t f    -> do
                  c' <- transExp env [] c
                  t' <- transExp env [] t
                  f' <- transExp env [] f
                  pure $ A.If noAnn (A.sizeOf t') c' t' f'
            T.ESel e' sel  -> do
                  a'  <- transExp env [] e'
                  sel' env e' a' sel
            T.EWhere e' ds -> transWhere env e' ds
            T.EAbs {}      -> Left "cryptol: a lambda in argument or result position cannot cross the Cryptol boundary (define it as a named function)."
            T.EComp {}     -> Left "cryptol: sequence comprehensions are not supported yet."
            T.ECase {}     -> Left "cryptol: case expressions (enums) are not supported yet."
            T.ERec {}      -> Left "cryptol: records are not supported yet."
            T.ESet {}      -> Left "cryptol: record/sequence update is not supported yet."
            _              -> Left $ "cryptol: unsupported expression" <> unsupported e
      where sel' :: TEnv -> T.Expr -> A.Exp -> T.Selector -> Either Text A.Exp
            sel' env' scrut a' = \ case
                  T.TupleSel i _ -> do
                        t  <- exprTy env' scrut
                        ws <- tupleWidths t
                        unless (i < length ws) $ Left "cryptol: tuple selector out of bounds (rwcry bug)."
                        pure $ A.Slice noAnn (fromIntegral $ sum $ drop (i + 1) ws) (fromIntegral $ ws !! i) a'
                  T.ListSel i _  -> do
                        t        <- exprTy env' scrut
                        (n, we)  <- seqWidths t
                        unless (fromIntegral i < n) $ Left "cryptol: sequence selector out of bounds."
                        pure $ A.Slice noAnn (fromIntegral $ (n - 1 - fromIntegral i) * we) (fromIntegral we) a'
                  T.RecordSel {} -> Left "cryptol: records are not supported yet."

unsupported :: T.Expr -> Text
unsupported e = ": " <> T.pack (show $ pp e)

-- | Local bindings: monomorphic value bindings become Hyle lets;
--   local functions (which may capture) are not supported yet.
transWhere :: TEnv -> T.Expr -> [T.DeclGroup] -> Either Text A.Exp
transWhere env body dgs = do
      mapM_ noRec dgs
      go env $ concatMap T.groupDecls dgs
      where noRec :: T.DeclGroup -> Either Text ()
            noRec = \ case
                  T.Recursive ds -> Left $ "cryptol: recursive local definitions are not supported: "
                        <> T.intercalate ", " (map (I.identText . N.nameIdent . T.dName) ds)
                  _              -> pure ()

            go :: TEnv -> [T.Decl] -> Either Text A.Exp
            go env' []       = transExp env' [] body
            go env' (d : ds) = case T.dDefinition d of
                  T.DExpr rhs | ([], t) <- flatFun $ T.sType $ T.dSignature d -> do
                        _    <- tyWidth t -- representable check
                        rhs' <- transExp env' [] rhs
                        let x  = sanitize (I.identText $ N.nameIdent $ T.dName d) <> "$" <> showt (N.nameUnique $ T.dName d)
                            sz = A.sizeOf rhs'
                        rest <- go env' { teScope = HM.insert (N.nameUnique $ T.dName d) (A.Var noAnn sz x) $ teScope env'
                                        , teTypes = Map.insert (T.dName d) (T.dSignature d) $ teTypes env'
                                        } ds
                        pure $ A.Let noAnn (A.sizeOf rest) x rhs' rest
                  T.DExpr _ -> Left "cryptol: local function bindings are not supported yet (lift the function to the top level of the Cryptol module)."
                  _         -> Left "cryptol: unsupported local binding."

-- | A reference, applied: a primitive instance, a translated definition,
--   or a local.
apply :: TEnv -> N.Name -> [A.Exp] -> Either Text A.Exp
apply env x args
      | Just (pn, tys) <- HM.lookup u $ tePrims env = transPrim pn tys args
      | Just (g, A.Sig _ aszs rsz) <- HM.lookup u $ teDefns env =
            if length args == length aszs
                  then pure $ A.Call noAnn rsz g args
                  else Left $ "cryptol: partial application of " <> I.identText (N.nameIdent x) <> " cannot cross the Cryptol boundary."
      | Just v <- HM.lookup u $ teScope env =
            if null args
                  then pure v
                  else Left $ "cryptol: local " <> I.identText (N.nameIdent x) <> " is used as a function (higher-order locals are not supported)."
      | otherwise = Left $ "cryptol: unsupported reference: " <> I.identText (N.nameIdent x)
      where u = N.nameUnique x

-- | The primitive table: Cryptol prelude primitives at bitvector-ish
--   instances, mapped per doc/hyle.md section 8.4 (in reverse). @tys@
--   are the primitive's instantiation types, recovered from the
--   specializer's name map.
transPrim :: Text -> [T.Type] -> [A.Exp] -> Either Text A.Exp
transPrim pn tys args = case (pn, tys, args) of
      ("number", [v, rep], [])       -> do
            val <- tyNat' "number" v
            case wordWidth rep of
                  Just w  -> pure $ A.Lit noAnn $ bitVec (fromIntegral w) val
                  Nothing | isInteger rep -> pure $ A.Lit noAnn $ bitVec (max 1 $ fromIntegral $ nbits $ fromIntegral val) val
                  Nothing -> Left $ "cryptol: a numeric literal at an unsupported type: " <> tshow rep
      ("True",  _, [])               -> pure $ A.Lit noAnn $ bitVec 1 (1 :: Integer)
      ("False", _, [])               -> pure $ A.Lit noAnn $ bitVec 1 (0 :: Integer)
      ("zero", [t], [])              -> A.Lit noAnn . zeros . fromIntegral <$> tyWidth t
      ("+", [t], [a, b])             -> wordBin "+" t A.Add a b
      ("-", [t], [a, b])             -> wordBin "-" t A.Sub a b
      ("*", [t], [a, b])             -> wordBin "*" t A.Mul a b
      ("^^", [t], [a, b])            -> wordBin "^^" t A.Pow a b
      ("/", [t], [a, b])             -> wordBin "/" t A.UDiv a b
      ("%", [t], [a, b])             -> wordBin "%" t A.UMod a b
      ("negate", [t], [a])           -> do
            w <- likeWord "negate" t
            pure $ A.Prim noAnn w A.Sub [A.Lit noAnn $ zeros $ fromIntegral w, a]
      ("complement", [t], [a])       -> (\ w -> A.Prim noAnn w A.Not [a]) <$> likeWord "complement" t
      ("&&", [t], [a, b])            -> wordBin "&&" t A.And a b
      ("||", [t], [a, b])            -> wordBin "||" t A.Or a b
      ("^", [t], [a, b])             -> wordBin "^" t A.XOr a b
      ("==", [t], [a, b])            -> cmp t A.Eq a b
      ("!=", [t], [a, b])            -> cmp t A.Ne a b
      ("<",  [t], [a, b])            -> cmp t A.ULt a b
      ("<=", [t], [a, b])            -> cmp t A.ULe a b
      (">",  [t], [a, b])            -> cmp t A.UGt a b
      (">=", [t], [a, b])            -> cmp t A.UGe a b
      ("<$",  [t], [a, b])           -> cmp t A.SLt a b
      ("<=$", [t], [a, b])           -> cmp t A.SLe a b
      (">$",  [t], [a, b])           -> cmp t A.SGt a b
      (">=$", [t], [a, b])           -> cmp t A.SGe a b
      ("<<",  [n, _ix, el], [a, b])  -> shift "<<" n el A.Shl a b
      (">>",  [n, _ix, el], [a, b])  -> shift ">>" n el A.LShr a b
      (">>$", [n, _ix], [a, b])      -> (\ w -> A.Prim noAnn w A.AShr [a, b]) . fromIntegral <$> tyNat' ">>$" n
      ("#", [_f, _b, el], [a, b])    -> do
            ew <- tyWidth el
            unless (ew == 1) $ Left "cryptol: (#) at a non-word sequence type is not supported yet."
            pure $ A.cat [a, b]
      ("take", [f, b, el], [a])      -> do
            (fw, bw, ew) <- (,,) <$> tyNat' "take" f <*> tyNat' "take" b <*> tyWidth el
            pure $ A.Slice noAnn (fromIntegral $ bw * ew) (fromIntegral $ fw * ew) a
      ("drop", [f, b, el], [a])      -> do
            (_fw, bw, ew) <- (,,) <$> tyNat' "drop" f <*> tyNat' "drop" b <*> tyWidth el
            pure $ A.Slice noAnn 0 (fromIntegral $ bw * ew) a
      ("@", [n, el, _ix], [a, i])    -> index "@"  (\ nv iv -> (nv - 1 - iv)) n el a i
      ("!", [n, el, _ix], [a, i])    -> index "!"  (\ _  iv -> iv)            n el a i
      _                              -> Left $ "cryptol: unsupported primitive: " <> pn
            <> (if null tys then "" else " (at " <> T.intercalate ", " (map tshow tys) <> ")")
      where wordBin :: Text -> T.Type -> A.Op -> A.Exp -> A.Exp -> Either Text A.Exp
            wordBin nm t op a b = (\ w -> A.Prim noAnn w op [a, b]) <$> likeWord nm t

            cmp :: T.Type -> A.Op -> A.Exp -> A.Exp -> Either Text A.Exp
            cmp t op a b = do
                  _ <- tyWidth t -- representable
                  pure $ A.Prim noAnn 1 op [a, b]

            shift :: Text -> T.Type -> T.Type -> A.Op -> A.Exp -> A.Exp -> Either Text A.Exp
            shift nm n el op a b = do
                  ew <- tyWidth el
                  unless (ew == 1) $ Left $ "cryptol: (" <> nm <> ") at a non-word sequence type is not supported yet."
                  (\ w -> A.Prim noAnn (fromIntegral w) op [a, b]) <$> tyNat' nm n

            -- Constant-index selection from the front (@) or back (!).
            index :: Text -> (Integer -> Integer -> Integer) -> T.Type -> T.Type -> A.Exp -> A.Exp -> Either Text A.Exp
            index nm pos n el a i = do
                  nv <- tyNat' nm n
                  ew <- tyWidth el
                  case i of
                        A.Lit _ bv -> do
                              let iv = nat bv
                              unless (iv >= 0 && iv < nv) $ Left $ "cryptol: (" <> nm <> ") index out of bounds."
                              pure $ A.Slice noAnn (fromIntegral $ pos nv iv * ew) (fromIntegral ew) a
                        _          -> Left $ "cryptol: (" <> nm <> ") with a non-constant index is not supported yet."

            -- A word (or Bit) instance width.
            likeWord :: Text -> T.Type -> Either Text A.Size
            likeWord nm t = case wordWidth t of
                  Just w  -> pure $ fromIntegral w
                  Nothing -> Left $ "cryptol: (" <> nm <> ") at an unsupported instance type: " <> tshow t

            tyNat' :: Text -> T.Type -> Either Text Integer
            tyNat' nm t = maybe (Left $ "cryptol: (" <> nm <> "): expected a numeric type, got: " <> tshow t) pure $ tyNat t

---
--- Cryptol types to widths.
---

-- | The width of a representable Cryptol value type: Bit, words,
--   sequences, tuples.
tyWidth :: T.Type -> Either Text Integer
tyWidth t = case T.tNoUser t of
      T.TCon (T.TC T.TCBit) []      -> pure 1
      T.TCon (T.TC T.TCSeq) [n, el] -> (*) <$> maybe (Left $ "cryptol: sequence length is not a literal: " <> tshow n) pure (tyNat n)
                                           <*> tyWidth el
      T.TCon (T.TC (T.TCTuple _)) es -> sum <$> mapM tyWidth es
      _                             -> Left $ "cryptol: unrepresentable type: " <> tshow t

-- | Tuple component widths.
tupleWidths :: T.Type -> Either Text [Integer]
tupleWidths t = case T.tNoUser t of
      T.TCon (T.TC (T.TCTuple _)) es -> mapM tyWidth es
      _                              -> Left $ "cryptol: expected a tuple type, got: " <> tshow t

-- | Sequence length and element width.
seqWidths :: T.Type -> Either Text (Integer, Integer)
seqWidths t = case T.tNoUser t of
      T.TCon (T.TC T.TCSeq) [n, el] -> (,) <$> maybe (Left "cryptol: sequence length is not a literal.") pure (tyNat n)
                                           <*> tyWidth el
      _                             -> Left $ "cryptol: expected a sequence type, got: " <> tshow t

-- | A word type's width: @[n]@ is @Just n@, @Bit@ is @Just 1@.
wordWidth :: T.Type -> Maybe Integer
wordWidth t = case T.tNoUser t of
      T.TCon (T.TC T.TCBit) []      -> Just 1
      T.TCon (T.TC T.TCSeq) [n, el] | Just 1 <- wordWidth el -> tyNat n
      _                             -> Nothing

isInteger :: T.Type -> Bool
isInteger t = case T.tNoUser t of
      T.TCon (T.TC T.TCInteger) [] -> True
      _                            -> False

tyNat :: T.Type -> Maybe Integer
tyNat t = case T.tNoUser t of
      T.TCon (T.TC (T.TCNum n)) [] -> Just n
      _                            -> Nothing

-- | The type of a subexpression, reconstructed from the schemas in scope.
exprTy :: TEnv -> T.Expr -> Either Text T.Type
exprTy env e = pure $ T.fastTypeOf (teTypes env) e

tshow :: T.Type -> Text
tshow = T.pack . show . pp

-- | An application spine, stripping locations and proofs.
spine :: T.Expr -> (T.Expr, [T.Expr])
spine = go []
      where go :: [T.Expr] -> T.Expr -> (T.Expr, [T.Expr])
            go acc = \ case
                  T.ELocated _ e -> go acc e
                  T.EProofApp e  -> go acc e
                  T.EApp f a     -> go (a : acc) f
                  e              -> (e, acc)

-- | The (arguments, result) of a function type.
flatFun :: T.Type -> ([T.Type], T.Type)
flatFun t = case T.tNoUser t of
      T.TCon (T.TC T.TCFun) [a, b] -> let (as, r) = flatFun b in (a : as, r)
      _                            -> ([], t)

-- | Hyle signature widths from a (monomorphic) schema.
sigWidths :: T.Schema -> Either Text ([A.Size], A.Size)
sigWidths (T.Forall tvs props ty)
      | not (null tvs) || not (null props) = Left "cryptol: a definition failed to specialize to a monomorphic type (rwcry bug?)."
      | otherwise = do
            let (as, r) = flatFun ty
            as' <- mapM tyWidth as
            r'  <- tyWidth r
            pure (map fromIntegral as', fromIntegral r')

-- | Legal (bare) Hyle name characters.
sanitize :: Text -> Text
sanitize = T.map (\ c -> if isAlphaNum c || c `elem` ("_.$'" :: String) then c else '.')
