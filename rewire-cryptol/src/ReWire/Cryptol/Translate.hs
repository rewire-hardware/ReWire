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
--   Supported fragment: combinational functions over Bit, words,
--   vectors, tuples, records, newtypes, enums (case expressions), and
--   @Z n@ (modular arithmetic). if-then-else; local value and function
--   bindings; higher-order functions applied to statically known
--   functions; comprehensions, folds, and scans (unrolled); recursive
--   finite comprehensions (the message-schedule/key-schedule/CBC idiom,
--   unrolled element-wise); type-indexed recursion (unrolled per
--   instantiation); the scalar, slicing, sequence, indexing, rotate,
--   update, and polynomial (pmult/pdiv/pmod, constant divisor)
--   primitives. Records, enums, and @Z n@ are interior-only: the entry
--   point's type must be words/vectors/tuples (the rwc side has no
--   counterpart). Enum values are laid out tag#pad#args with the tag at
--   the most-significant end and the tag width nbits(#constructors) --
--   the same convention the Eidos fold uses for ReWire ADTs.
--   error/undefined become a zero poison constant and trace the
--   identity, both with warnings. Value recursion, infinite streams,
--   floating point, and Integer/Rational at runtime are rejected with
--   (it is hoped) actionable messages.
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
import qualified Cryptol.TypeCheck.Subst      as TS
import qualified Cryptol.TypeCheck.TypeMap    as TM
import qualified Cryptol.TypeCheck.TypeOf     as T (fastTypeOf)
import qualified Cryptol.Utils.Ident          as I
import qualified Cryptol.Utils.Logger         as L
import Cryptol.Utils.PP (pp)
import Cryptol.Utils.RecordMap (canonicalFields, recordElements)

import Control.Exception (try, SomeException)
import Control.Monad (unless, foldM)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError, liftEither)
import Control.Monad.IO.Class (liftIO)
import Data.Bits (testBit)
import Data.Char (isAlphaNum)
import Data.Graph (stronglyConnComp, SCC (..))
import Data.HashMap.Strict (HashMap)
import Data.List (genericLength)
import Data.Maybe (isJust)
import Data.Text (Text)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import Text.Read (readMaybe)

import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified Data.Text           as T

-- | Translate function @fn@ from Cryptol module @file@, at the
--   monomorphic Cryptol type @ty@, to a self-contained set of Hyle
--   definitions whose entry point is named @entry@ (helpers are prefixed
--   with it), plus any compile-time warnings (e.g. an @error@ compiled to
--   a poison constant). Left is a (possibly multi-line, source-located)
--   diagnostic.
translate :: FilePath -> Text -> Text -> Text -> IO (Either Text ([A.Defn], [Text]))
translate file fn ty entry = either bail id <$> (try go :: IO (Either SomeException (Either Text ([A.Defn], [Text]))))
      where bail :: SomeException -> Either Text ([A.Defn], [Text])
            bail e = Left $ "cryptol: " <> T.pack (show e) <> "\n(is z3 on the PATH?)"

            go :: IO (Either Text ([A.Defn], [Text]))
            go = do
                  maxNodes <- maybe defaultMaxNodes id . (>>= readMaybe) <$> lookupEnv "RWC_CRY_MAX_NODES"
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
                        liftEither $ transClosure entry body dgs nmap (ME.loadedNominalTypes env2) maxNodes

            -- Generous: comfortably fits an AES round or SHA schedule,
            -- but bounds a runaway unrolling.
            defaultMaxNodes :: Integer
            defaultMaxNodes = 2000000

---
--- The specialized closure to Hyle definitions.
---

data TEnv = TEnv
      { tePrims :: HashMap Int (Text, [T.Type]) -- ^ Primitive clones: name and instantiation types.
      , teDefns :: HashMap Int (A.GId, A.Sig)   -- ^ Translated definitions: Hyle name and signature.
      , teTypes :: Map.Map N.Name T.Schema      -- ^ Schemas of everything in scope, for type reconstruction.
      , teScope :: HashMap Int A.Exp            -- ^ Local binders.
      , teCons  :: HashMap Int (T.NominalType, ConDef) -- ^ Struct/enum constructors, by name unique.
      , teDepth :: Int                          -- ^ Case-nesting depth (uniquifies scrutinee lets).
      , teFuns  :: HashMap Int (TEnv, T.Expr)   -- ^ Inlinable bindings (higher-order or otherwise
                                                --   un-Hyle-able definitions, local functions), with
                                                --   the environment closed over at the binding.
      , teInts  :: HashMap Int Integer          -- ^ Constant Integer-typed binders (unrolled
                                                --   comprehension indices); see 'intVal'.
      , teStrms :: HashMap Int (TEnv, T.Expr)   -- ^ Infinite-stream local bindings (a recursive
                                                --   @[inf]@ definition), demand-unrolled by 'infPrefix'.
      }

-- | A nominal-type constructor: a struct/newtype's (transparent), or an
--   enum's (tagged).
type ConDef = Either T.StructCon T.EnumCon

-- | The node-count ceiling: unrolling (comprehensions x recursion x
--   folds) can explode, so a translation past this many Hyle nodes fails
--   with an actionable message rather than hanging a downstream tool.
--   Generous by default; raise it with @RWC_CRY_MAX_NODES@ (passed in as
--   the bound, so 0 disables it).
transClosure :: Text -> T.Expr -> [T.DeclGroup] -> Map.Map N.Name (TM.TypesMap N.Name) -> Map.Map N.Name T.NominalType -> Integer -> Either Text ([A.Defn], [Text])
transClosure entry body dgs nmap noms maxNodes = do
      decls <- orderDecls dgs
      entryName <- case spine body of
            (T.EVar x, _, _) -> pure x
            _                -> Left "cryptol: unexpected expression shape after specialization (rwcry bug)."
      let primSet = HM.fromList [ (N.nameUnique $ T.dName d, ()) | d <- decls, isPrim $ T.dDefinition d ]
          prims   = HM.fromList [ (N.nameUnique cl, (I.identText $ N.nameIdent orig, tys))
                                | (orig, tm) <- Map.toList nmap
                                , (tys, cl)  <- TM.toListTM tm
                                , N.nameUnique cl `HM.member` primSet
                                ]
          exprDs  = [ (d, e) | d <- decls, e <- defBody (T.dDefinition d) ]
          -- Definitions whose signature fits Hyle become Hyle definitions
          -- (numbered over the full list, for name stability); the rest
          -- (higher-order, or otherwise unrepresentable) are inlined at
          -- their (fully applied) use sites.
          fits    = [ (i, de) | (i, de@(d, _)) <- zip [0 :: Int ..] exprDs, hyleable d ]
          inls    = [ de | de@(d, _) <- exprDs, not $ hyleable d ]
          isEntry x = N.nameUnique x == N.nameUnique entryName
      unless (any (isEntry . T.dName . fst) exprDs)
            $ Left "cryptol: the requested function is a Cryptol primitive; wrap it in a Cryptol definition."
      unless (any (isEntry . T.dName . fst . snd) fits)
            $ Left "cryptol: the requested function's type is not representable at the FFI boundary (words, vectors, and tuples only)."
      names <- HM.fromList <$> mapM (uncurry $ mkName entryName) fits
      let env = TEnv { tePrims = prims
                     , teDefns = names
                     , teTypes = Map.fromList $ [ (T.dName d, T.dSignature d) | d <- decls ]
                                             <> concatMap T.nominalTypeConTypes (Map.elems noms)
                     , teScope = mempty
                     , teCons  = HM.fromList $ concatMap conEntries $ Map.elems noms
                     , teDepth = 0
                     , teFuns  = HM.fromList [ (N.nameUnique $ T.dName d, (env, e)) | (d, e) <- inls ]
                     , teInts  = mempty
                     , teStrms = mempty
                     }
      defns <- mapM (transDecl env . snd) fits
      let nodes = sum $ map (nodeCount . A.defnBody) defns
      unless (maxNodes <= 0 || nodes <= maxNodes)
            $ Left $ "cryptol: the translation of " <> entry <> " is very large ("
                  <> showt nodes <> " nodes, over the " <> showt maxNodes
                  <> "-node limit): an unrolled comprehension, fold, or recursion may be too big to realize."
                  <> " Raise RWC_CRY_MAX_NODES to allow it."
      pure (defns, warnUses prims decls)
      where isPrim :: T.DeclDef -> Bool
            isPrim = \ case
                  T.DPrim -> True
                  _       -> False

            hyleable :: T.Decl -> Bool
            hyleable = either (const False) (const True) . sigWidths . T.dSignature

            -- A translatable body: an ordinary definition, or a Cryptol
            -- foreign (C FFI) function's Cryptol fallback implementation.
            defBody :: T.DeclDef -> [T.Expr]
            defBody = \ case
                  T.DExpr e             -> [e]
                  T.DForeign _ (Just e) -> [e]
                  _                     -> []

            conEntries :: T.NominalType -> [(Int, (T.NominalType, ConDef))]
            conEntries nt = case T.ntDef nt of
                  T.Struct sc -> [ (N.nameUnique $ T.ntConName sc, (nt, Left sc)) ]
                  T.Enum ecs  -> [ (N.nameUnique $ T.ecName ec, (nt, Right ec)) | ec <- ecs ]
                  T.Abstract  -> []

            mkName :: N.Name -> Int -> (T.Decl, T.Expr) -> Either Text (Int, (A.GId, A.Sig))
            mkName entryName i (d, _) = do
                  (aszs, rsz) <- sigWidths $ T.dSignature d
                  let gid | N.nameUnique (T.dName d) == N.nameUnique entryName = entry
                          | otherwise = entry <> "." <> sanitize (I.identText $ N.nameIdent $ T.dName d) <> "$" <> showt i
                  pure (N.nameUnique $ T.dName d, (gid, A.Sig noAnn aszs rsz))

-- | Flatten declaration groups in dependency order. A recursive group is
--   re-analyzed after specialization: type-indexed recursion (each call
--   at a strictly smaller instantiation) arrives as a chain of distinct
--   clones, so an acyclic group is ordinary code; a genuine cycle (value
--   recursion) is rejected.
orderDecls :: [T.DeclGroup] -> Either Text [T.Decl]
orderDecls = fmap concat . mapM go
      where go :: T.DeclGroup -> Either Text [T.Decl]
            go = \ case
                  T.NonRecursive d -> pure [d]
                  T.Recursive ds   -> mapM unSCC $ sccDecls ds

            unSCC :: SCC T.Decl -> Either Text T.Decl
            unSCC = \ case
                  AcyclicSCC d -> pure d
                  CyclicSCC ds -> Left $ noRecMsg ds

-- | Strongly connected components of a recursive group, in dependency
--   order.
sccDecls :: [T.Decl] -> [SCC T.Decl]
sccDecls ds = stronglyConnComp
      [ (d, N.nameUnique $ T.dName d, filter (`HM.member` us) $ map N.nameUnique $ declRefs d) | d <- ds ]
      where us = HM.fromList [ (N.nameUnique $ T.dName d, ()) | d <- ds ]

noRecMsg :: [T.Decl] -> Text
noRecMsg ds = "cryptol: recursive definitions are not supported: "
      <> T.intercalate ", " (map (I.identText . N.nameIdent . T.dName) ds)

-- | Names referenced by a declaration/expression (for the post-
--   specialization recursion analysis).
declRefs :: T.Decl -> [N.Name]
declRefs d = case T.dDefinition d of
      T.DExpr e             -> refs e
      T.DForeign _ (Just e) -> refs e
      _                     -> []
      where refs :: T.Expr -> [N.Name]
            refs = \ case
                  T.EList es _       -> concatMap refs es
                  T.ETuple es        -> concatMap refs es
                  T.ERec fs          -> concatMap refs $ recordElements fs
                  T.ESel e _         -> refs e
                  T.ESet _ e _ v     -> refs e <> refs v
                  T.EIf c t f        -> refs c <> refs t <> refs f
                  T.ECase e as dl    -> refs e <> concatMap altRefs (Map.elems as) <> maybe [] altRefs dl
                  T.EComp _ _ e mss  -> refs e <> concatMap (concatMap matchRefs) mss
                  T.EVar x           -> [x]
                  T.ETAbs _ e        -> refs e
                  T.ETApp e _        -> refs e
                  T.EApp f a         -> refs f <> refs a
                  T.EAbs _ _ e       -> refs e
                  T.ELocated _ e     -> refs e
                  T.EProofAbs _ e    -> refs e
                  T.EProofApp e      -> refs e
                  T.EWhere e dgs'    -> refs e <> concatMap declRefs (concatMap T.groupDecls dgs')
                  T.EPropGuards gs _ -> concatMap (refs . snd) gs

            altRefs :: T.CaseAlt -> [N.Name]
            altRefs (T.CaseAlt _ e) = refs e

            matchRefs :: T.Match -> [N.Name]
            matchRefs = \ case
                  T.From _ _ _ e -> refs e
                  T.Let d'       -> declRefs d'

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
      pure $ A.Defn noAnn gid sig (pnames <> map fst etas) body' False (A.Blind [])
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

-- | Translate an expression; @etas@ are pending (already-translated)
--   arguments applied to it: the eta-expansion of a point-free
--   definition, or arguments awaiting a function-valued subexpression
--   (they thread through lambdas, ifs, wheres, and cases).
transExp :: TEnv -> [A.Exp] -> T.Expr -> Either Text A.Exp
transExp env etas e0
      | null etas, Just v <- intVal env e0 = pure $ intLit v
transExp env etas e0 = case spine e0 of
      (T.EVar x, tys, args)
            | Just (nt, con) <- HM.lookup (N.nameUnique x) (teCons env) -> do
                  unless (null etas) $ Left "cryptol: partial application of a constructor cannot cross the Cryptol boundary."
                  args' <- mapM (transExp env []) args
                  transCon nt con tys args'
            | Just (envF, rhs) <- HM.lookup (N.nameUnique x) (teFuns env) ->
                  inline env envF rhs args etas
            | Just (pn, tys') <- HM.lookup (N.nameUnique x) (tePrims env)
            , Just r <- infConsume env pn tys' args -> do
                  unless (null etas) $ Left $ "cryptol: partial application of " <> pn <> " over an infinite stream cannot cross the Cryptol boundary."
                  r
            | Just (pn, tys') <- HM.lookup (N.nameUnique x) (tePrims env), pn `elem` (["foldl", "foldr", "scanl"] :: [Text]), not (any (isInfSeq env) args) -> do
                  unless (null etas) $ Left $ "cryptol: partial application of " <> pn <> " cannot cross the Cryptol boundary."
                  transFold env pn tys' args
            | otherwise -> do
                  args' <- (<> etas) <$> mapM (transExp env []) args
                  apply env x args'
      (e, _, _ : _)              -> Left $ "cryptol: unsupported application head" <> unsupported e
      (e, _, [])  -> case e of
            T.EAbs x t b
                  | a : as <- etas -> transExp (bindAll env [(x, t, a)]) as b
                  | otherwise      -> Left "cryptol: a lambda in argument or result position cannot cross the Cryptol boundary (define it as a named function)."
            T.EIf c t f    -> do
                  c' <- transExp env [] c
                  t' <- transExp env etas t
                  f' <- transExp env etas f
                  pure $ A.If noAnn (A.sizeOf t') c' t' f'
            T.EWhere e' ds -> transWhere env etas e' ds
            T.ECase scrut alts dflt -> transCase env etas scrut alts dflt
            _ | not (null etas) -> Left $ "cryptol: cannot apply a value as a function" <> unsupported e
            T.ETuple es    -> A.cat <$> mapM (transExp env []) es
            T.EList es _   -> A.cat <$> mapM (transExp env []) es
            T.ERec fs      -> A.cat <$> mapM (transExp env []) (recordElements fs)
            T.ESel e' sel  -> do
                  a'  <- transExp env [] e'
                  sel' env e' a' sel
            T.ESet ty e' sel v -> transSet env ty e' sel v
            T.EComp _ ety body mss -> transComp env ety body mss
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
                  T.RecordSel f _ -> do
                        t  <- exprTy env' scrut
                        fs <- recFields t
                        case break ((== f) . fst) fs of
                              (_, [])              -> Left $ "cryptol: unknown record field: " <> I.identText f
                              (_, (_, w) : post) -> pure $ A.Slice noAnn (fromIntegral $ sum $ map snd post) (fromIntegral w) a'

unsupported :: T.Expr -> Text
unsupported e = ": " <> T.pack (show $ pp e)

-- | Evaluate a constant Integer-typed expression. Unbounded Integers at
--   runtime are unrepresentable in hardware, but the common uses --
--   comprehension indices (@[4 .. 43]@ defaults its elements to Integer)
--   flowing into indexing, arithmetic, and comparisons -- are constants
--   once comprehensions unroll: comprehension binders over constant
--   Integer sequences land in 'teInts', and this folds the arithmetic
--   over them.
intVal :: TEnv -> T.Expr -> Maybe Integer
intVal env e = case spine e of
      (T.EVar x, _, args) -> case (HM.lookup (N.nameUnique x) $ teInts env, HM.lookup (N.nameUnique x) $ teFuns env, args) of
            (Just v, _, [])           -> pure v
            (_, Just (envF, rhs), []) -> intVal envF rhs
            _                         -> do
                  (pn, tys) <- HM.lookup (N.nameUnique x) $ tePrims env
                  vs        <- mapM (intVal env) args
                  -- Only at the Integer instance: literals and arithmetic
                  -- at word types translate as words.
                  case (pn, tys, vs) of
                        ("number", [v, rep], []) | isInteger rep       -> tyNat v
                        ("+", [t], [a, b])       | isInteger t         -> pure $ a + b
                        ("-", [t], [a, b])       | isInteger t         -> pure $ a - b
                        ("*", [t], [a, b])       | isInteger t         -> pure $ a * b
                        ("/", [t], [a, b])       | isInteger t, b /= 0 -> pure $ a `div` b
                        ("%", [t], [a, b])       | isInteger t, b /= 0 -> pure $ a `mod` b
                        ("^^", [t, _], [a, b])   | isInteger t, b >= 0 -> pure $ a ^ b
                        ("negate", [t], [a])     | isInteger t         -> pure $ negate a
                        ("toInteger", _, [a])                          -> pure a
                        ("max", [t], [a, b])     | isInteger t         -> pure $ max a b
                        ("min", [t], [a, b])     | isInteger t         -> pure $ min a b
                        _                                              -> Nothing
      _ -> Nothing

-- | An Integer constant as a Hyle literal, at the width of its value
--   (consumers -- indexing, shift amounts -- extend as needed).
intLit :: Integer -> A.Exp
intLit v = A.Lit noAnn $ bitVec (max 1 $ fromIntegral $ nbits $ fromIntegral $ max 0 v + 1) v

-- | The constant value of a translated expression, folding literal
--   arithmetic (unrolled comprehension indices produce shapes like
--   @sub i 4@ over literals).
litVal :: A.Exp -> Maybe Integer
litVal = \ case
      A.Lit _ bv          -> pure $ nat bv
      A.Slice _ off w e   -> do
            v <- litVal e
            pure $ (v `div` (2 ^ toInteger off)) `mod` (2 ^ toInteger w)
      A.Cat _ l r         -> do
            lv <- litVal l
            rv <- litVal r
            pure $ lv * (2 ^ toInteger (A.sizeOf r)) + rv
      A.Prim _ w op es    -> do
            vs <- mapM litVal es
            let wrap v = v `mod` (2 ^ toInteger w)
            case (op, vs) of
                  (A.Add,  [a, b])          -> pure $ wrap $ a + b
                  (A.Sub,  [a, b])          -> pure $ wrap $ a - b
                  (A.Mul,  [a, b])          -> pure $ wrap $ a * b
                  (A.UDiv, [a, b]) | b /= 0 -> pure $ a `div` b
                  (A.UMod, [a, b]) | b /= 0 -> pure $ a `mod` b
                  (A.Shl,  [a, b])          -> pure $ wrap $ a * 2 ^ b
                  (A.LShr, [a, b])          -> pure $ a `div` (2 ^ b)
                  (A.ZExt _,  [a])          -> pure a
                  (A.Trunc _, [a])          -> pure $ wrap a
                  _                         -> Nothing
      _                   -> Nothing

-- | Inline an inlinable binding at a use site: parameters with
--   Hyle-representable types bind to their translated arguments; the
--   rest (function-valued or otherwise unrepresentable arguments) are
--   deferred -- recorded against the call-site environment and
--   re-translated at their own use sites. The body translates in the
--   binding's captured environment, at the call site's case-nesting
--   depth (capture along nesting is the only kind possible).
inline :: TEnv -> TEnv -> T.Expr -> [T.Expr] -> [A.Exp] -> Either Text A.Exp
inline site defn rhs args etas = go (defn { teDepth = teDepth site }) rhs args
      where go :: TEnv -> T.Expr -> [T.Expr] -> Either Text A.Exp
            go env e as = case (skip e, as) of
                  (T.EAbs x t b, a : as')
                        | Right _ <- tyWidth t -> do
                              a' <- transExp site [] a
                              go (bindAll env [(x, t, a')]) b as'
                        | otherwise -> go (env { teFuns  = HM.insert (N.nameUnique x) (site, a) $ teFuns env
                                               , teTypes = Map.insert x (T.tMono t) $ teTypes env }) b as'
                  (b, [])         -> transExp env etas b
                  (b, _)          -> do
                        as' <- mapM (transExp site []) as
                        transExp env (as' <> etas) b

            skip :: T.Expr -> T.Expr
            skip = \ case
                  T.ELocated _ e  -> skip e
                  T.EProofAbs _ e -> skip e
                  e               -> e

-- | Bind an inlinable function's parameters to arguments without
--   translating the body -- for beta-reducing an applied inf-producing
--   function (e.g. @iterate f z@) so 'infPrefix' can analyze the residual
--   stream expression. Value parameters bind translated; function
--   parameters defer as closures.
betaBind :: TEnv -> TEnv -> T.Expr -> [T.Expr] -> Either Text (TEnv, T.Expr)
betaBind site env0 rhs0 args0 = go (env0 { teDepth = teDepth site }) rhs0 args0
      where go :: TEnv -> T.Expr -> [T.Expr] -> Either Text (TEnv, T.Expr)
            go env e as = case (skip e, as) of
                  (T.EAbs x t b, a : as')
                        | Right _ <- tyWidth t -> do
                              a' <- transExp site [] a
                              go (bindAll env [(x, t, a')]) b as'
                        | otherwise -> go (env { teFuns  = HM.insert (N.nameUnique x) (site, a) $ teFuns env
                                               , teTypes = Map.insert x (T.tMono t) $ teTypes env }) b as'
                  (b, []) -> pure (env, b)
                  _       -> Left "cryptol: an infinite stream produced by a partially applied function is not supported."

            skip :: T.Expr -> T.Expr
            skip = \ case
                  T.ELocated _ e  -> skip e
                  T.EProofAbs _ e -> skip e
                  e               -> e

-- | A saturated constructor application (arguments already translated).
--   A struct (newtype) constructor is transparent: the value is its
--   record argument's bits. An enum constructor builds tag#pad#args: the
--   constructor's declaration index in nbits(#constructors) bits at the
--   most-significant end, zero padding up to the enum's width, then the
--   argument bits (matching the Eidos fold's ADT layout).
transCon :: T.NominalType -> ConDef -> [T.Type] -> [A.Exp] -> Either Text A.Exp
transCon nt con tys args = case con of
      Left _sc -> case args of
            [a] -> pure a
            _   -> Left $ "cryptol: a struct constructor expects exactly its record argument: " <> ntName
      Right ec -> do
            su <- paramSubst nt tys
            let nCons  = length [ () | T.Enum ecs <- [T.ntDef nt], _ <- ecs ]
            ftys <- mapM (tyWidth . TS.apSubst su) $ T.ecFields ec
            unless (length args == length ftys)
                  $ Left $ "cryptol: partial application of an enum constructor cannot cross the Cryptol boundary: " <> ntName
            w <- tyWidth $ T.TNominal nt tys
            let tagW   = fromIntegral $ nbits $ fromIntegral nCons
                szArgs = sum ftys
                padW   = w - tagW - szArgs
            unless (padW >= 0) $ Left "cryptol: enum constructor wider than its type (rwcry bug)."
            pure $ A.cat $ [ A.Lit noAnn $ bitVec (fromIntegral tagW) (toInteger $ T.ecNumber ec) | tagW > 0 ]
                        <> [ A.Lit noAnn $ zeros $ fromIntegral padW | padW > 0 ]
                        <> args
      where ntName = I.identText $ N.nameIdent $ T.ntName nt

-- | A case expression over an enum value: the scrutinee is bound once
--   (the let's name is uniquified by case-nesting depth, the only axis
--   along which capture is possible), the tag slice is compared against
--   each alternative's constructor index in declaration order, and each
--   alternative's binders are bound to slices of the payload. The
--   default alternative (if any) is the final else and may bind the
--   whole scrutinee.
transCase :: TEnv -> [A.Exp] -> T.Expr -> Map.Map I.Ident T.CaseAlt -> Maybe T.CaseAlt -> Either Text A.Exp
transCase env etas scrut alts dflt = do
      st        <- exprTy env scrut
      nt <- case T.tNoUser st of
            T.TNominal nt' _ -> pure nt'
            _                -> Left $ "cryptol: case on a non-enum type: " <> tshow st
      ecs <- case T.ntDef nt of
            T.Enum ecs -> pure ecs
            _          -> Left $ "cryptol: case on a non-enum type: " <> tshow st
      scrut' <- transExp env [] scrut
      let sw    = A.sizeOf scrut'
          tagW  = fromIntegral $ nbits $ fromIntegral $ length ecs
          sname = "case$" <> showt (teDepth env)
          sv    = A.Var noAnn sw sname
          tag   = A.Slice noAnn (sw - tagW) tagW sv
          env'  = env { teDepth = teDepth env + 1 }
      arms   <- sequence [ (toInteger $ T.ecNumber ec, ) <$> alt env' sv a
                         | ec <- ecs, Just a <- [Map.lookup (N.nameIdent $ T.ecName ec) alts] ]
      dflt'  <- traverse (dfltAlt env' sv) dflt
      chain  <- ifChain tagW tag arms dflt'
      pure $ A.Let noAnn (A.sizeOf chain) sname scrut' chain
      where -- An alternative's binders map to payload slices: the fields
            -- sit at the least-significant end, first field first.
            alt :: TEnv -> A.Exp -> T.CaseAlt -> Either Text A.Exp
            alt env' sv (T.CaseAlt bs rhs) = do
                  ws <- mapM (tyWidth . snd) bs
                  let offs  = drop 1 $ scanr (+) 0 ws
                      binds = [ (x, t, A.Slice noAnn (fromIntegral off) (fromIntegral w) sv)
                              | ((x, t), w, off) <- zip3 bs ws offs ]
                  transExp (bindAll env' binds) etas rhs

            -- The default alternative may bind the scrutinee itself.
            dfltAlt :: TEnv -> A.Exp -> T.CaseAlt -> Either Text A.Exp
            dfltAlt env' sv (T.CaseAlt bs rhs) = case bs of
                  []       -> transExp env' etas rhs
                  [(x, t)] -> transExp (bindAll env' [(x, t, sv)]) etas rhs
                  _        -> Left "cryptol: unexpected default case-alternative shape (rwcry bug)."

            ifChain :: A.Size -> A.Exp -> [(Integer, A.Exp)] -> Maybe A.Exp -> Either Text A.Exp
            ifChain tagW tag arms mDflt = case (arms, mDflt) of
                  ([], Just d)          -> pure d
                  ([], Nothing)         -> Left "cryptol: a case expression with no alternatives (rwcry bug)."
                  ((_, a) : _, _) -> do
                        let w = A.sizeOf a
                            (initArms, lastArm) = case mDflt of
                                  Just d  -> (arms, d)
                                  Nothing -> (init arms, snd $ last arms)
                        pure $ foldr (\ (i, rhs) els ->
                                    A.If noAnn w (A.Prim noAnn 1 A.Eq [tag, A.Lit noAnn $ bitVec (fromIntegral tagW) i]) rhs els)
                              lastArm initArms

-- | A record (or tuple/sequence) update @{ e | sel = v }@: the bits
--   before and after the selected field are carried over, the field's
--   bits replaced.
transSet :: TEnv -> T.Type -> T.Expr -> T.Selector -> T.Expr -> Either Text A.Exp
transSet env ty e sel v = do
      w  <- tyWidth ty
      e' <- transExp env [] e
      v' <- transExp env [] v
      (off, fw) <- case sel of
            T.RecordSel f _ -> do
                  fs <- recFields ty
                  case break ((== f) . fst) fs of
                        (_, [])              -> Left $ "cryptol: unknown record field: " <> I.identText f
                        (_, (_, fw) : post) -> pure (sum $ map snd post, fw)
            T.TupleSel i _  -> do
                  ws <- tupleWidths ty
                  unless (i < length ws) $ Left "cryptol: tuple update selector out of bounds (rwcry bug)."
                  pure (sum $ drop (i + 1) ws, ws !! i)
            T.ListSel i _   -> do
                  (n, we) <- seqWidths ty
                  unless (fromIntegral i < n) $ Left "cryptol: sequence update selector out of bounds."
                  pure ((n - 1 - fromIntegral i) * we, we)
      let hiW = w - off - fw
      pure $ A.cat $ [ A.Slice noAnn (fromIntegral $ off + fw) (fromIntegral hiW) e' | hiW > 0 ]
                  <> [ v' ]
                  <> [ A.Slice noAnn 0 (fromIntegral off) e' | off > 0 ]

-- | Local bindings: representable monomorphic value bindings become
--   Hyle lets; local functions (and unrepresentable local values, e.g.
--   Integer-typed intermediates) are recorded with the environment they
--   close over and inlined at their use sites; recursive groups that
--   survive the post-specialization SCC analysis are recursive sequence
--   definitions, unrolled element-wise by 'recSeqs'.
transWhere :: TEnv -> [A.Exp] -> T.Expr -> [T.DeclGroup] -> Either Text A.Exp
transWhere env etas body dgs = go env $ concatMap items dgs
      where items :: T.DeclGroup -> [Either T.Decl [T.Decl]]
            items = \ case
                  T.NonRecursive d -> [Left d]
                  T.Recursive ds   -> map fromSCC $ sccDecls ds

            fromSCC :: SCC T.Decl -> Either T.Decl [T.Decl]
            fromSCC = \ case
                  AcyclicSCC d -> Left d
                  CyclicSCC c  -> Right c

            go :: TEnv -> [Either T.Decl [T.Decl]] -> Either Text A.Exp
            go env' []              = transExp env' etas body
            -- A recursive group of infinite streams is registered for
            -- demand-driven unrolling (infStream); a recursive group of
            -- finite sequences is unrolled eagerly (recSeqs).
            go env' (Right c : ds)
                  | all isInfDecl c   = go (registerStreams env' c) ds
                  | otherwise         = do
                        (env'', lets) <- recSeqs env' c
                        rest <- go env'' ds
                        pure $ foldr (\ (x, rhs) b -> A.Let noAnn (A.sizeOf b) x rhs b) rest lets
            go env' (Left d : ds) = case T.dDefinition d of
                  T.DExpr rhs | ([], t) <- flatFun $ T.sType $ T.dSignature d, Right _ <- tyWidth t -> do
                        rhs' <- transExp env' [] rhs
                        let x  = sanitize (I.identText $ N.nameIdent $ T.dName d) <> "$" <> showt (N.nameUnique $ T.dName d)
                            sz = A.sizeOf rhs'
                        rest <- go env' { teScope = HM.insert (N.nameUnique $ T.dName d) (A.Var noAnn sz x) $ teScope env'
                                        , teTypes = Map.insert (T.dName d) (T.dSignature d) $ teTypes env'
                                        } ds
                        pure $ A.Let noAnn (A.sizeOf rest) x rhs' rest
                  T.DExpr rhs -> go env' { teFuns  = HM.insert (N.nameUnique $ T.dName d) (env', rhs) $ teFuns env'
                                         , teTypes = Map.insert (T.dName d) (T.dSignature d) $ teTypes env'
                                         } ds
                  _         -> Left "cryptol: unsupported local binding."

            isInfDecl :: T.Decl -> Bool
            isInfDecl d = case (flatFun $ T.sType $ T.dSignature d, T.dDefinition d) of
                  (([], t), T.DExpr _) -> case T.tNoUser t of
                        T.TCon (T.TC T.TCSeq) [n, _] -> case T.tNoUser n of
                              T.TCon (T.TC T.TCInf) [] -> True
                              _                        -> False
                        _ -> False
                  _ -> False

            -- Register a recursive group of infinite streams for
            -- demand-driven unrolling, knot-tying the environment so that
            -- (mutual) self-references resolve back to the group.
            registerStreams :: TEnv -> [T.Decl] -> TEnv
            registerStreams e0 c = e1
                  where e1 = e0 { teStrms = HM.fromList [ (N.nameUnique $ T.dName d, (e1, rhs)) | d <- c, T.DExpr rhs <- [T.dDefinition d] ] <> teStrms e0
                               , teTypes = Map.fromList [ (T.dName d, T.dSignature d) | d <- c ] <> teTypes e0 }

-- | A cyclic group of local value bindings, each a finite sequence: the
--   recursive-comprehension idiom (@ys = [iv] # [ f y x | y <- ys | x <-
--   xs ]@ -- CBC chaining, key schedules, message schedules). Each
--   definition's name is bound to the concatenation of fresh per-element
--   variables, the right-hand sides translate against that (so
--   self-references become element references once 'pev' folds the
--   slice/concat algebra), and the elements are let-bound in dependency
--   order; a genuinely cyclic element dependency is rejected.
recSeqs :: TEnv -> [T.Decl] -> Either Text (TEnv, [(A.Name, A.Exp)])
recSeqs env ds = do
      infos <- mapM info ds
      let env' = env { teScope = HM.fromList [ (u, A.cat [ A.Var noAnn (fromIntegral we) x | x <- xs ])
                                             | (u, _, we, xs, _) <- infos ] <> teScope env
                     , teTypes = Map.fromList [ (T.dName d, T.dSignature d) | d <- ds ] <> teTypes env
                     }
      elems <- concat <$> mapM (elemsOf env') infos
      let nameSet = HM.fromList [ (x, ()) | (x, _) <- elems ]
      lets <- mapM unSCC $ stronglyConnComp
            [ ((x, e), x, [ r | r <- varRefs e, r `HM.member` nameSet ]) | (x, e) <- elems ]
      pure (env', lets)
      where info :: T.Decl -> Either Text (Int, Integer, Integer, [A.Name], T.Expr)
            info d = case T.dDefinition d of
                  T.DExpr rhs | ([], t) <- flatFun $ T.sType $ T.dSignature d
                              , Right (n, we) <- seqWidths t ->
                        let base = sanitize (I.identText $ N.nameIdent $ T.dName d) <> "$" <> showt (N.nameUnique $ T.dName d)
                        in pure ( N.nameUnique $ T.dName d, n, we
                                , [ base <> "$" <> showt i | i <- [0 .. n - 1] ], rhs )
                  _ -> Left $ noRecMsg ds

            elemsOf :: TEnv -> (Int, Integer, Integer, [A.Name], T.Expr) -> Either Text [(A.Name, A.Exp)]
            elemsOf env' (_, n, we, xs, rhs) = do
                  rhs' <- transExp env' [] rhs
                  pure [ (x, pev $ elemSlice n we i rhs') | (i, x) <- zip [0 ..] xs ]

            unSCC :: SCC (A.Name, A.Exp) -> Either Text (A.Name, A.Exp)
            unSCC = \ case
                  AcyclicSCC x -> pure x
                  CyclicSCC xs -> Left $ "cryptol: a recursive sequence definition has a cyclic element dependency: "
                        <> T.intercalate ", " (map fst xs)

-- | Partial evaluation of a translated expression: slice/concat/literal
--   algebra plus constant arithmetic. Extracting the elements of a
--   recursive sequence definition relies on this: the element slice of
--   the translated right-hand side must reduce to references to only
--   the element variables it actually depends on, so that the elements
--   admit a dependency order.
pev :: A.Exp -> A.Exp
pev = \ case
      A.Cat an l r         -> A.Cat an (pev l) (pev r)
      A.Slice _ off w e    -> sliceE (toInteger off) (toInteger w) (pev e)
      A.Prim an w op es    ->
            let e' = A.Prim an w op $ map pev es
            in maybe e' (A.Lit an . bitVec (fromIntegral w)) $ litVal e'
      A.If an w c t f      -> case litVal $ pev c of
            Just v  -> if v /= 0 then pev t else pev f
            Nothing -> A.If an w (pev c) (pev t) (pev f)
      A.Call an w g es     -> A.Call an w g $ map pev es
      A.XCall an w x gs es -> A.XCall an w x gs $ map pev es
      A.Let an w x rhs b   -> A.Let an w x (pev rhs) (pev b)
      e                    -> e

-- | @sliceE off w e@: e[off +: w], pushing the slice through concats,
--   slices, literals, and muxes.
sliceE :: Integer -> Integer -> A.Exp -> A.Exp
sliceE off w e
      | w <= 0                                = A.Lit noAnn $ zeros 0
      | off == 0, w == toInteger (A.sizeOf e) = e
      | otherwise = case e of
            A.Cat _ l r ->
                  let rw = toInteger $ A.sizeOf r
                  in if off >= rw then sliceE (off - rw) w l
                     else if off + w <= rw then sliceE off w r
                     else A.Cat noAnn (sliceE 0 (off + w - rw) l) (sliceE off (rw - off) r)
            A.Slice _ off' _ e' -> sliceE (off + toInteger off') w e'
            A.Lit _ bv          -> A.Lit noAnn $ bitVec (fromIntegral w) $ (nat bv `div` (2 ^ off)) `mod` (2 ^ w)
            A.If _ _ c t f      -> A.If noAnn (fromIntegral w) c (sliceE off w t) (sliceE off w f)
            _                   -> A.Slice noAnn (fromIntegral off) (fromIntegral w) e

-- | Variable references in a translated expression.
varRefs :: A.Exp -> [A.Name]
varRefs = \ case
      A.Var _ _ x        -> [x]
      A.Cat _ l r        -> varRefs l <> varRefs r
      A.Slice _ _ _ e    -> varRefs e
      A.Prim _ _ _ es    -> concatMap varRefs es
      A.Call _ _ _ es    -> concatMap varRefs es
      A.XCall _ _ _ _ es -> concatMap varRefs es
      A.If _ _ c t f     -> varRefs c <> varRefs t <> varRefs f
      A.Let _ _ _ rhs b  -> varRefs rhs <> varRefs b
      _                  -> []

-- | The number of nodes in a translated expression (the size governor's
--   metric).
nodeCount :: A.Exp -> Integer
nodeCount = \ case
      A.Cat _ l r        -> 1 + nodeCount l + nodeCount r
      A.Slice _ _ _ e    -> 1 + nodeCount e
      A.Prim _ _ _ es    -> 1 + sum (map nodeCount es)
      A.Call _ _ _ es    -> 1 + sum (map nodeCount es)
      A.XCall _ _ _ _ es -> 1 + sum (map nodeCount es)
      A.If _ _ c t f     -> 1 + nodeCount c + nodeCount t + nodeCount f
      A.Let _ _ _ rhs b  -> 1 + nodeCount rhs + nodeCount b
      _                  -> 1

-- | Compile-time warnings from a static scan of the specialized
--   declarations: uses of primitives that translate with a semantic
--   caveat (error/assert/undefined as a zero poison; trace as identity).
warnUses :: HashMap Int (Text, [T.Type]) -> [T.Decl] -> [Text]
warnUses prims decls = concat
      [ [ errW   | hasUse ["error"] ]
      , [ traceW | hasUse ["trace"] ]
      , [ recipW | hasUse ["recip", "/."] ]
      ]
      where used   = [ pn | d <- decls, x <- declRefs d, Just (pn, _) <- [HM.lookup (N.nameUnique x) prims] ]
            hasUse ns = any (`elem` (ns :: [Text])) used
            traceW = "trace/traceVal is ignored (evaluated in hardware, it is the identity on its result)."
            errW   = "error/assert/undefined compiled to a zero constant (Hyle has no bottom); "
                  <> "the result is defined but meaningless where the error would fire."
            recipW = "recip/(/.) at Z p unrolls a Fermat inverse (~2*log2(p) modular multiplies); "
                  <> "for a large modulus this is big -- watch the node budget (RWC_CRY_MAX_NODES)."

-- | Count references to a variable (a let-binding may shadow it).
countVar :: A.Name -> A.Exp -> Int
countVar x = \ case
      A.Var _ _ y        -> if y == x then 1 else 0
      A.Cat _ l r        -> countVar x l + countVar x r
      A.Slice _ _ _ e    -> countVar x e
      A.Prim _ _ _ es    -> sum $ map (countVar x) es
      A.Call _ _ _ es    -> sum $ map (countVar x) es
      A.XCall _ _ _ _ es -> sum $ map (countVar x) es
      A.If _ _ c t f     -> countVar x c + countVar x t + countVar x f
      A.Let _ _ y rhs b  -> countVar x rhs + (if y == x then 0 else countVar x b)
      _                  -> 0

-- | Substitute an expression for a variable (a let-binding shadows it).
substVar :: A.Name -> A.Exp -> A.Exp -> A.Exp
substVar x s = go
      where go = \ case
                  A.Var _ _ y        | y == x -> s
                  A.Cat an l r        -> A.Cat an (go l) (go r)
                  A.Slice an o w e    -> A.Slice an o w (go e)
                  A.Prim an w op es   -> A.Prim an w op $ map go es
                  A.Call an w g es    -> A.Call an w g $ map go es
                  A.XCall an w n gs es -> A.XCall an w n gs $ map go es
                  A.If an w c t f     -> A.If an w (go c) (go t) (go f)
                  A.Let an w y rhs b  -> A.Let an w y (go rhs) (if y == x then b else go b)
                  e                   -> e

-- | A comprehension, fully unrolled: the lengths are concrete after
--   specialization, so each element of the result is the body translated
--   with the generator variables bound to slices of their (translated)
--   sources. Arms zip; generators within an arm nest (the last one
--   fastest), matching Cryptol's semantics.
transComp :: TEnv -> T.Type -> T.Expr -> [[T.Match]] -> Either Text A.Exp
transComp env _ety body mss
      | any (any matchIsInf) mss = transCompInf env body mss
      | otherwise = do
            arms <- mapM (armIter env) mss
            let len = case arms of
                  [] -> 0
                  _  -> minimum $ map fst arms
            parts <- mapM (part arms) [0 .. len - 1]
            pure $ A.cat parts
      where part :: [(Integer, Integer -> Either Text [(N.Name, T.Type, A.Exp)])] -> Integer -> Either Text A.Exp
            part arms k = do
                  binds <- concat <$> mapM (($ k) . snd) arms
                  transExp (bindAll env binds) [] body

            matchIsInf :: T.Match -> Bool
            matchIsInf = \ case
                  T.From _ _ _ src -> isInfSeq env src
                  T.Let _          -> False

-- | A comprehension zipping an infinite arm against a finite one (a
--   finite prefix of an infinite stream is demanded -- the min length is
--   set by the finite arm(s)). Each infinite arm is a single generator;
--   its source's prefix is realized once.
transCompInf :: TEnv -> T.Expr -> [[T.Match]] -> Either Text A.Exp
transCompInf env body mss = do
      lens <- concat <$> mapM armLen mss
      len  <- case lens of
            [] -> Left "cryptol: a comprehension with only infinite arms has unbounded length; take a finite prefix."
            _  -> pure $ minimum lens
      binders <- mapM (armBinder len) mss
      parts   <- mapM (\ k -> do
                        binds <- concat <$> mapM ($ k) binders
                        transExp (bindAll env binds) [] body) [0 .. len - 1]
      pure $ A.cat parts
      where -- The finite length contributed by an arm (empty for an
            -- infinite arm).
            armLen :: [T.Match] -> Either Text [Integer]
            armLen ms
                  | any infFrom ms = pure []
                  | otherwise      = do
                        ns <- sequence [ maybe (Left "cryptol: a comprehension source has a non-literal length.") pure $ tyNat l | T.From _ l _ _ <- ms ]
                        pure [ product ns ]

            infFrom :: T.Match -> Bool
            infFrom = \ case { T.From _ _ _ src -> isInfSeq env src ; _ -> False }

            armBinder :: Integer -> [T.Match] -> Either Text (Integer -> Either Text [(N.Name, T.Type, A.Exp)])
            armBinder len ms = case ms of
                  [T.From x _ et src] | isInfSeq env src -> do
                        es <- infPrefix env len src
                        pure $ \ k -> maybe (Left "cryptol: infinite comprehension arm too short (rwcry bug).")
                                            (pure . pure . (x, et, )) $ lookup k $ zip [0 ..] es
                  _ | any infFrom ms -> Left "cryptol: an infinite comprehension arm must be a single generator (no let or nested generators)."
                  _ -> snd <$> armIter env ms

-- | One comprehension arm: its iteration count, and the bindings its
--   matches produce at iteration @k@.
armIter :: TEnv -> [T.Match] -> Either Text (Integer, Integer -> Either Text [(N.Name, T.Type, A.Exp)])
armIter env ms = do
      radixes <- mapM radix ms
      let total = product [ n | Just n <- radixes ]
      pure (total, iter radixes)
      where radix :: T.Match -> Either Text (Maybe Integer)
            radix = \ case
                  T.From _ l _ _ -> maybe (Left "cryptol: a comprehension source has a non-literal length.") (pure . Just) $ tyNat l
                  T.Let _        -> pure Nothing

            -- Mixed-radix decomposition of @k@ over the From matches
            -- (last fastest); Lets translate in the growing scope.
            iter :: [Maybe Integer] -> Integer -> Either Text [(N.Name, T.Type, A.Exp)]
            iter radixes k = go env (zip ms digs)
                  where digs = idxs radixes k

                        go :: TEnv -> [(T.Match, Maybe Integer)] -> Either Text [(N.Name, T.Type, A.Exp)]
                        go _ [] = pure []
                        go env' ((m, dig) : rest) = do
                              b@(x, t, _) <- case (m, dig) of
                                    -- Integer-element generators (index idioms like
                                    -- @i <- [4 .. 43]@ default to Integer) bind each
                                    -- unrolled index to its constant value.
                                    (T.From x' _ et src, Just i) | isInteger et -> do
                                          vs <- seqIntVals env' src
                                          v  <- maybe (Left "cryptol: comprehension index out of range (rwcry bug).") pure
                                                    $ lookup i $ zip [0 ..] vs
                                          pure (x', et, intLit v)
                                    (T.From x' _ et src, Just i) -> do
                                          we   <- tyWidth et
                                          n    <- maybe (Left "cryptol: non-literal length (rwcry bug)") pure . tyNat $ srcLen m
                                          srcA <- transExp env' [] src
                                          pure (x', et, elemSlice n we i srcA)
                                    (T.Let d, _) -> case T.dDefinition d of
                                          T.DExpr rhs | ([], t') <- flatFun $ T.sType $ T.dSignature d ->
                                                (T.dName d, t', ) <$> transExp env' [] rhs
                                          _ -> Left "cryptol: unsupported local binding in a comprehension."
                                    _ -> Left "cryptol: malformed comprehension (rwcry bug)."
                              (b :) <$> go (bindAll env' [(x, t, thd b)]) rest

                        thd (_, _, c) = c

                        srcLen :: T.Match -> T.Type
                        srcLen = \ case
                              T.From _ l _ _ -> l
                              _              -> T.tNum (0 :: Integer)

            -- Row-major digits: positions align with the From matches.
            idxs :: [Maybe Integer] -> Integer -> [Maybe Integer]
            idxs rads k = reverse $ go' (reverse rads) k
                  where go' [] _ = []
                        go' (Nothing : rest) k' = Nothing : go' rest k'
                        go' (Just n : rest) k'  = Just (k' `mod` n) : go' rest (k' `div` n)

-- | Extend the environment with translated bindings (constant
--   Integer-typed binders also land in 'teInts' for 'intVal').
bindAll :: TEnv -> [(N.Name, T.Type, A.Exp)] -> TEnv
bindAll env binds = env
      { teScope = HM.fromList [ (N.nameUnique x, a) | (x, _, a) <- binds ] <> teScope env
      , teTypes = foldr (\ (x, t, _) -> Map.insert x (T.tMono t)) (teTypes env) binds
      , teInts  = HM.fromList [ (N.nameUnique x, v) | (x, t, a) <- binds, isInteger t, Just v <- [litVal a] ] <> teInts env
      }

-- | The constant values of an Integer-element sequence (an enumeration
--   primitive or a literal list): Integer generators only unroll over
--   constants.
seqIntVals :: TEnv -> T.Expr -> Either Text [Integer]
seqIntVals env src = case spine src of
      (T.EVar x, _, [])
            | Just (pn, tys) <- HM.lookup (N.nameUnique x) $ tePrims env
            , Just (vs, _) <- enumVals pn tys -> pure vs
      (T.EList es _, _, []) -> maybe (Left msg) pure $ mapM (intVal env) es
      _                     -> Left msg
      where msg = "cryptol: a comprehension over Integer must draw from a constant range or list."

-- | The values and element type of a constant enumeration primitive.
enumVals :: Text -> [T.Type] -> Maybe ([Integer], T.Type)
enumVals pn tys = case (pn, tys) of
      ("fromTo",                  [f, l, a])          -> range a $ (\ fv lv -> [fv .. lv])                  <$> tyNat f <*> tyNat l
      ("fromToLessThan",          [f, b, a])          -> range a $ (\ fv bv -> [fv .. bv - 1])              <$> tyNat f <*> tyNat b
      ("fromToBy",                [f, l, s, a])       -> range a $ (\ fv lv sv -> [fv, fv + sv .. lv])      <$> tyNat f <*> tyNat l <*> tyNat s
      ("fromToByLessThan",        [f, b, s, a])       -> range a $ (\ fv bv sv -> [fv, fv + sv .. bv - 1])  <$> tyNat f <*> tyNat b <*> tyNat s
      ("fromToDownBy",            [f, l, s, a])       -> range a $ (\ fv lv sv -> [fv, fv - sv .. lv])      <$> tyNat f <*> tyNat l <*> tyNat s
      ("fromToDownByGreaterThan", [f, b, s, a])       -> range a $ (\ fv bv sv -> [fv, fv - sv .. bv + 1])  <$> tyNat f <*> tyNat b <*> tyNat s
      ("fromThenTo",              [f, nx, l, a, _n])  -> range a $ (\ fv nv lv -> [fv, nv .. lv])           <$> tyNat f <*> tyNat nx <*> tyNat l
      _                                               -> Nothing
      where range :: T.Type -> Maybe [Integer] -> Maybe ([Integer], T.Type)
            range a = fmap (, a)

-- | Unrolled folds and scans; the function argument is taken
--   untranslated (it may be a reference or a lambda -- there are no
--   function values in Hyle). Each accumulator step is let-bound
--   (fold$<depth>$<k>) so a step function that uses its accumulator more
--   than once doesn't blow up the term.
transFold :: TEnv -> Text -> [T.Type] -> [T.Expr] -> Either Text A.Exp
transFold env pn tys args = case (pn, tys, args) of
      ("foldl", [n, _b, a], [f, z, xs]) -> do
            (nv, we, xsA, zA) <- setup n a xs z
            foldChain zA [ \ env' acc -> applyFn env' f [acc, elemSlice nv we i xsA] | i <- [0 .. nv - 1] ]
      ("foldr", [n, a, _b], [f, z, xs]) -> do
            (nv, we, xsA, zA) <- setup n a xs z
            foldChain zA [ \ env' acc -> applyFn env' f [elemSlice nv we i xsA, acc] | i <- [nv - 1, nv - 2 .. 0] ]
      ("scanl", [n, _a, b], [f, z, xs]) -> do
            (nv, we, xsA, zA) <- setup n b xs z
            scanChain zA [ \ env' acc -> applyFn env' f [acc, elemSlice nv we i xsA] | i <- [0 .. nv - 1] ]
      _ -> Left $ "cryptol: unsupported use of " <> pn <> " (expected a fully applied fold)."
      where setup :: T.Type -> T.Type -> T.Expr -> T.Expr -> Either Text (Integer, Integer, A.Exp, A.Exp)
            setup n el xs z = do
                  nv  <- maybe (Left $ "cryptol: (" <> pn <> "): non-literal length.") pure $ tyNat n
                  we  <- tyWidth el
                  xsA <- transExp env [] xs
                  zA  <- transExp env [] z
                  pure (nv, we, xsA, zA)

            env' :: TEnv
            env' = env { teDepth = teDepth env + 1 }

            nm :: Int -> A.Name
            nm k = "fold$" <> showt (teDepth env) <> "$" <> showt k

            -- A fold: thread the accumulator through the steps, keeping
            -- only the final value. An intermediate is let-bound only when
            -- its consuming step uses it more than once; a single-use
            -- accumulator inlines, so a plain reduction stays a single
            -- expression (as before scans were let-bound).
            foldChain :: A.Exp -> [TEnv -> A.Exp -> Either Text A.Exp] -> Either Text A.Exp
            foldChain z steps = do
                  let w = A.sizeOf z
                  (final, binds) <- foldl' (stepFold w) (pure (z, [])) (zip [0 ..] steps)
                  pure $ foldr (\ (x, rhs) b -> A.Let noAnn (A.sizeOf b) x rhs b) final $ reverse binds

            stepFold :: A.Size -> Either Text (A.Exp, [(A.Name, A.Exp)]) -> (Int, TEnv -> A.Exp -> Either Text A.Exp)
                     -> Either Text (A.Exp, [(A.Name, A.Exp)])
            stepFold w acc (k, step) = do
                  (cur, binds) <- acc
                  body <- step env' $ A.Var noAnn w $ nm k
                  if countVar (nm k) body > 1
                        then pure (body, (nm k, cur) : binds)       -- share the accumulator
                        else pure (substVar (nm k) cur body, binds) -- inline it

            -- A scan: every prefix feeds the output, so each accumulator
            -- is let-bound and referenced by name.
            scanChain :: A.Exp -> [TEnv -> A.Exp -> Either Text A.Exp] -> Either Text A.Exp
            scanChain z steps = do
                  let w = A.sizeOf z
                  binds <- goScan w 0 steps
                  let allBinds = (nm 0, z) : binds
                      out      = A.cat [ A.Var noAnn w $ nm k | k <- [0 .. length steps] ]
                  pure $ foldr (\ (x, rhs) b -> A.Let noAnn (A.sizeOf b) x rhs b) out allBinds

            goScan :: A.Size -> Int -> [TEnv -> A.Exp -> Either Text A.Exp] -> Either Text [(A.Name, A.Exp)]
            goScan _ _ []            = pure []
            goScan w k (step : rest) = do
                  body <- step env' $ A.Var noAnn w $ nm k
                  ((nm (k + 1), body) :) <$> goScan w (k + 1) rest

-- | Apply a function-position expression (a reference, possibly already
--   partially applied, or a lambda) to translated arguments.
applyFn :: TEnv -> T.Expr -> [A.Exp] -> Either Text A.Exp
applyFn env f args = case spine f of
      (T.EVar x, tys, pre)
            | Just (nt, con) <- HM.lookup (N.nameUnique x) (teCons env) -> do
                  pre' <- mapM (transExp env []) pre
                  transCon nt con tys (pre' <> args)
            | otherwise -> do
                  pre' <- mapM (transExp env []) pre
                  apply env x (pre' <> args)
      (l@(T.EAbs {}), _, []) -> lam env l args
      _ -> Left "cryptol: unsupported function argument (use a named function or a lambda)."
      where lam :: TEnv -> T.Expr -> [A.Exp] -> Either Text A.Exp
            lam env' (T.EAbs x t b) (a : as) = lam (bindAll env' [(x, t, a)]) b as
            lam env' b []                    = transExp env' [] b
            lam env' b as                    = applyFn env' b as

-- | The element type of an infinite (@[inf]a@) sequence expression, or
--   Nothing if the expression is not an infinite sequence.
seqInfElem :: TEnv -> T.Expr -> Maybe T.Type
seqInfElem env e = case T.tNoUser $ T.fastTypeOf (teTypes env) e of
      T.TCon (T.TC T.TCSeq) [n, el] | isInf n -> Just el
      _                                       -> Nothing
      where isInf t = case T.tNoUser t of
                  T.TCon (T.TC T.TCInf) [] -> True
                  _                        -> False

isInfSeq :: TEnv -> T.Expr -> Bool
isInfSeq env = isJust . seqInfElem env

-- | The element type of any sequence type.
seqElemType :: T.Type -> Maybe T.Type
seqElemType t = case T.tNoUser t of
      T.TCon (T.TC T.TCSeq) [_, el] -> Just el
      _                             -> Nothing

-- | Consumers of an infinite stream that yield a finite result: a demand
--   bound flows in and 'infPrefix' realizes only that many elements.
--   @take@{k} and a constant @\@@ are the realizable consumers; a
--   variable index, reverse index, or bare @drop@/@\@@ of an infinite
--   stream has unbounded demand and is rejected. Returns Nothing when
--   this is not an infinite-stream consumer (the normal path handles it).
infConsume :: TEnv -> Text -> [T.Type] -> [T.Expr] -> Maybe (Either Text A.Exp)
infConsume env pn ptys args = case (pn, ptys, args) of
      ("take", [front, _back, _a], [src]) | isInfSeq env src -> Just $ do
            fr <- maybe (Left "cryptol: take of an infinite stream needs a literal length.") pure $ tyNat front
            A.cat <$> infPrefix env fr src
      ("@", [_n, _a, _ix], [src, i]) | isInfSeq env src -> Just $ do
            iA <- transExp env [] i
            case litVal iA of
                  Just j  -> do
                        es <- infPrefix env (j + 1) src
                        maybe (Left "cryptol: infinite-stream index out of the demanded prefix (rwcry bug).") pure
                              $ lookup j $ zip [0 ..] es
                  Nothing -> Left "cryptol: a variable index into an infinite stream has unbounded demand; use a constant index or take a finite prefix."
      ("!",  _, src : _) | isInfSeq env src -> Just $ Left "cryptol: an infinite stream cannot be indexed from the end."
      ("drop", _, [src])   | isInfSeq env src -> Just $ Left "cryptol: drop of an infinite stream is only realizable inside a finite take or index."
      _ -> Nothing

-- | The first @k@ elements (each a translated element-width expression)
--   of an infinite -- or, where it bottoms out, finite -- sequence
--   expression. This is the demand-driven realization of the bounded
--   fragment of infinite streams: the four stream shapes (infFrom,
--   infFromThen, and the scanl/comprehension that iterate and repeat
--   desugar to), append with a finite front, drop, and recursive stream
--   definitions.
infPrefix :: TEnv -> Integer -> T.Expr -> Either Text [A.Exp]
infPrefix env k e
      | k <= 0    = pure []
      | otherwise = case spine e of
            (T.EVar x, _, args)
                  | Just (envS, rhs) <- HM.lookup (N.nameUnique x) (teStrms env), null args -> infStream env envS x rhs k
                  | Just (envF, rhs) <- HM.lookup (N.nameUnique x) (teFuns env)  -> betaBind env envF rhs args >>= \ (e', b) -> infPrefix e' k b
                  | Just (pn, ptys) <- HM.lookup (N.nameUnique x) (tePrims env)  -> infPrim env k pn ptys args
                  | otherwise -> Left $ "cryptol: unsupported infinite-stream source: " <> I.identText (N.nameIdent x)
            (T.EComp _ _ body mss, _, []) -> infComp env k body mss
            (_, _, _) | not (isInfSeq env e) -> finiteElems env k e -- a finite tail
            _ -> Left $ "cryptol: unsupported infinite-stream expression" <> unsupported e

-- | The first @k@ elements of an infinite-stream primitive application.
infPrim :: TEnv -> Integer -> Text -> [T.Type] -> [T.Expr] -> Either Text [A.Exp]
infPrim env k pn ptys args = case (pn, ptys, args) of
      ("infFrom", [a], [start]) -> do
            we <- tyWidth a
            s  <- transExp env [] start
            pure [ if i == 0 then s else A.Prim noAnn (fromIntegral we) A.Add [s, A.Lit noAnn $ bitVec (fromIntegral we) i] | i <- [0 .. k - 1] ]
      ("infFromThen", [a], [x, y]) -> do
            we <- tyWidth a
            xA <- transExp env [] x
            yA <- transExp env [] y
            let w    = fromIntegral we
                step = A.Prim noAnn w A.Sub [yA, xA]
            pure [ if i == 0 then xA
                   else if i == 1 then yA
                   else A.Prim noAnn w A.Add [xA, A.Prim noAnn w A.Mul [A.Lit noAnn $ bitVec (fromIntegral w) i, step]]
                 | i <- [0 .. k - 1] ]
      ("#", [front, _back, _a], [l, r]) -> do
            m <- maybe (Left "cryptol: (#) with a non-literal front length.") pure $ tyNat front
            ls <- finiteElems env (min k m) l
            rs <- if k > m then infPrefix env (k - m) r else pure []
            pure $ take (fromIntegral k) $ ls <> rs
      ("drop", [d, _back, _a], [src]) -> do
            dv <- maybe (Left "cryptol: drop of an infinite stream needs a literal count.") pure $ tyNat d
            drop (fromIntegral dv) <$> infPrefix env (k + dv) src
      ("take", [front, _back, _a], [src]) -> do
            fr <- maybe (Left "cryptol: take needs a literal length.") pure $ tyNat front
            take (fromIntegral $ min k fr) <$> infPrefix env (min k fr) src
      ("scanl", [_n, _ta, _tb], [f, z, xs]) -> infScan env k f z xs
      -- zero : [inf]a -- an infinite run of the zero value (the driver
      -- iterate's scanl consumes; usually a() with zero width).
      ("zero", [t], []) -> do
            el <- maybe (Left "cryptol: zero at a non-sequence infinite type.") pure $ seqElemType t
            we <- tyWidth el
            pure $ replicate (fromIntegral k) $ A.Lit noAnn $ zeros $ fromIntegral we
      _ -> Left $ "cryptol: unsupported infinite-stream primitive: " <> pn

-- | The first @k@ elements of @scanl f z xs@ (the desugaring of
--   @iterate@): the running accumulators z, f z xs0, f (f z xs0) xs1,
--   ... let-bound so a reused accumulator does not duplicate.
infScan :: TEnv -> Integer -> T.Expr -> T.Expr -> T.Expr -> Either Text [A.Exp]
infScan env k f z xs = do
      zA  <- transExp env [] z
      xse <- if k > 1 then take (fromIntegral k - 1) <$> infPrefix env (k - 1) xs else pure []
      let w    = A.sizeOf zA
          nm i = "iter$" <> showt (teDepth env) <> "$" <> showt (i :: Integer)
          env' = env { teDepth = teDepth env + 1 }
      binds <- goScan env' w nm 0 zA xse
      let refs = zA : [ A.Var noAnn w (nm i) | (i, _) <- zip [1 ..] xse ]
      -- Wrap each let around the whole list (they nest); the caller cats
      -- or selects, so wrap every element in the accumulated binders.
      pure $ map (wrapLets binds) refs
      where goScan :: TEnv -> A.Size -> (Integer -> A.Name) -> Integer -> A.Exp -> [A.Exp] -> Either Text [(A.Name, A.Exp)]
            goScan _ _ _ _ _ []           = pure []
            goScan e' w nm i acc (x : xs') = do
                  body <- applyFn e' f [acc, x]
                  let nm' = nm (i + 1)
                  ((nm', body) :) <$> goScan e' w nm (i + 1) (A.Var noAnn w nm') xs'

            wrapLets :: [(A.Name, A.Exp)] -> A.Exp -> A.Exp
            wrapLets bs body = foldr (\ (x, rhs) b -> A.Let noAnn (A.sizeOf b) x rhs b) body bs

-- | The first @k@ elements of an infinite comprehension. Supported: a
--   single parallel arm of one generator over an infinite (or finite)
--   source, plus @let@ matches -- the shape @repeat@ and simple maps
--   over a stream desugar to. Element j binds the generator variable to
--   element j of its source.
infComp :: TEnv -> Integer -> T.Expr -> [[T.Match]] -> Either Text [A.Exp]
infComp env k body mss = case mss of
      [ms] -> do
            gens <- mapM genElems ms
            let lens = [ n | Just n <- map fst gens ]
                kk   = fromIntegral $ minimum (k : lens)
            mapM (element gens) [0 .. kk - 1]
      _ -> Left "cryptol: only a single-arm infinite comprehension is supported (parallel/nested infinite comprehensions are not)."
      where -- Each match: its length (Nothing = infinite) and a function
            -- from element index to the bindings it introduces.
            genElems :: T.Match -> Either Text (Maybe Integer, Integer -> Either Text [(N.Name, T.Type, A.Exp)])
            genElems = \ case
                  T.From x _ et src
                        | isInfSeq env src -> do
                              es <- infPrefix env k src
                              pure (Nothing, \ j -> maybe (Left "cryptol: comprehension source too short (rwcry bug).") (pure . pure . (x, et, )) $ lookup j $ zip [0 ..] es)
                        | otherwise -> do
                              (n, we) <- seqWidths $ T.fastTypeOf (teTypes env) src
                              srcA    <- transExp env [] src
                              pure (Just n, \ j -> pure [ (x, et, elemSlice n we j srcA) ])
                  T.Let _ -> Left "cryptol: a let in an infinite comprehension is not supported yet."

            element :: [(Maybe Integer, Integer -> Either Text [(N.Name, T.Type, A.Exp)])] -> Integer -> Either Text A.Exp
            element gens j = do
                  binds <- concat <$> mapM (($ j) . snd) gens
                  transExp (bindAll env binds) [] body

-- | The first @k@ elements of a recursive infinite-stream definition
--   (@s = front # [ f s ... | ... ]@): unroll element by element, each
--   new element referencing earlier ones through the growing binding of
--   the stream name; the elements are let-bound in order.
infStream :: TEnv -> TEnv -> N.Name -> T.Expr -> Integer -> Either Text [A.Exp]
infStream _siteEnv defEnv sname rhs k = do
      elemT <- maybe (Left "cryptol: a recursive stream binding is not an infinite sequence (rwcry bug).") pure
            $ seqInfElem defEnv (T.EVar sname)
      we    <- tyWidth elemT
      let nm i = sanitize (I.identText $ N.nameIdent sname) <> "$" <> showt (N.nameUnique sname) <> "$" <> showt (i :: Integer)
          -- The stream name resolves to the concatenation of the
          -- per-element variables computed so far (front-padded), so a
          -- self-reference @s\@(j-d)@ slices out element (j-d).
          bindStream :: Integer -> TEnv
          bindStream have = defEnv { teStrms = HM.delete (N.nameUnique sname) $ teStrms defEnv
                                   , teScope = HM.insert (N.nameUnique sname)
                                          (A.cat [ A.Var noAnn (fromIntegral we) (nm i) | i <- [0 .. have - 1] ]) (teScope defEnv)
                                   , teTypes = Map.insert sname (T.tMono $ T.tSeq (T.tNum have) elemT) (teTypes defEnv) }
      -- Compute elements 0..k-1: element j is the j-th element of the
      -- RHS evaluated with the stream bound to elements 0..j-1 (so any
      -- self-reference must be to an earlier, lagged, element).
      let go :: Integer -> [(A.Name, A.Exp)] -> Either Text [(A.Name, A.Exp)]
          go j acc
                | j >= k    = pure $ reverse acc
                | otherwise = do
                      es <- infPrefix (bindStream j) (j + 1) rhs
                      ej <- maybe (Left "cryptol: recursive stream element out of range (rwcry bug).") pure $ lookup j $ zip [0 ..] es
                      go (j + 1) ((nm j, pev ej) : acc)
      binds <- go 0 []
      -- Reject a self-reference that isn't strictly lagged (a cycle):
      -- element j's rhs must reference only earlier element variables.
      let names = [ (nm i, i) | i <- [0 .. k - 1] ]
      mapM_ (checkLag names) $ zip [0 ..] binds
      let refs = [ A.Var noAnn (fromIntegral we) (nm i) | i <- [0 .. k - 1] ]
      pure $ map (\ r -> foldr (\ (x, rhs') b -> A.Let noAnn (A.sizeOf b) x rhs' b) r binds) refs
      where checkLag :: [(A.Name, Integer)] -> (Integer, (A.Name, A.Exp)) -> Either Text ()
            checkLag names (j, (_, ej)) =
                  case [ i | r <- varRefs ej, Just i <- [lookup r names], i >= j ] of
                        (_ : _) -> Left "cryptol: a recursive stream element depends on itself or a later element (an unbounded/ill-founded stream)."
                        []      -> pure ()

-- | The first @k@ elements of a finite sequence expression (slicing its
--   translation); fewer if the sequence is shorter than @k@.
finiteElems :: TEnv -> Integer -> T.Expr -> Either Text [A.Exp]
finiteElems env k e = do
      (n, we) <- seqWidths $ T.fastTypeOf (teTypes env) e
      eA      <- transExp env [] e
      pure [ elemSlice n we i eA | i <- [0 .. min k n - 1] ]

-- | Element @i@ (from the front -- index 0 is the most significant) of a
--   sequence of @n@ elements of width @we@.
elemSlice :: Integer -> Integer -> Integer -> A.Exp -> A.Exp
elemSlice n we i = A.Slice noAnn (fromIntegral $ (n - 1 - i) * we) (fromIntegral we)

-- | Resize an expression to a target width: zero-extend if wider,
--   truncate (keeping the low bits) if narrower.
resize :: A.Size -> A.Exp -> A.Exp
resize w e
      | A.sizeOf e == w = e
      | A.sizeOf e <  w = A.Prim noAnn w (A.ZExt w) [e]
      | otherwise       = A.Prim noAnn w (A.Trunc w) [e]

-- | A reference, applied: a primitive instance, a translated definition,
--   or a local.
apply :: TEnv -> N.Name -> [A.Exp] -> Either Text A.Exp
apply env x args
      | Just (pn, tys) <- HM.lookup u $ tePrims env = transPrim pn tys args
      | Just (g, A.Sig _ aszs rsz) <- HM.lookup u $ teDefns env =
            if length args == length aszs
                  then pure $ A.Call noAnn rsz g args
                  else Left $ "cryptol: partial application of " <> I.identText (N.nameIdent x) <> " cannot cross the Cryptol boundary."
      | Just (envF, rhs) <- HM.lookup u $ teFuns env =
            transExp (envF { teDepth = teDepth env }) args rhs
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
                  Nothing | isInteger rep -> pure $ intLit val
                  Nothing | Just n <- zN rep -> pure $ A.Lit noAnn $ bitVec (fromIntegral $ nbits $ fromIntegral n) $ val `mod` n
                  Nothing -> Left $ "cryptol: a numeric literal at an unsupported type: " <> tshow rep
      ("True",  _, [])               -> pure $ A.Lit noAnn $ bitVec 1 (1 :: Integer)
      ("False", _, [])               -> pure $ A.Lit noAnn $ bitVec 1 (0 :: Integer)
      ("zero", [t], [])              -> A.Lit noAnn . zeros . fromIntegral <$> tyWidth t
      -- Z n (integers mod n): a value in @[nbits n]@; Ring operations
      -- reduce modulo n (computed at a width wide enough to hold the
      -- unreduced result). fromInteger reduces a constant; == and the
      -- comparisons work on the representation directly.
      ("+",      [zN -> Just n], [a, b]) -> zMod n (n + n)   A.Add a b
      ("-",      [zN -> Just n], [a, b]) -> zMod n (n + n)   A.Add a $ zNeg n b
      ("*",      [zN -> Just n], [a, b]) -> zMod n (n * n)   A.Mul a b
      ("negate", [zN -> Just n], [a])    -> pure $ zNeg n a
      -- Field operations at Z p (Cryptol requires p prime): the inverse
      -- is Fermat's a^(p-2) mod p, square-and-multiply unrolled over the
      -- constant exponent's bits (~2*log p modular multiplies -- big for
      -- large p, so warnUses flags it and the size governor bounds it).
      ("recip", [zN -> Just n], [a])     -> zRecip n a
      ("/.",    [zN -> Just n], [a, b])  -> zRecip n b >>= zMod n (n * n) A.Mul a
      ("fromInteger", [zN -> Just n], [a]) -> do
            let w = fromIntegral $ nbits $ fromIntegral n
            case litVal a of
                  Just v  -> pure $ A.Lit noAnn $ bitVec (fromIntegral w) $ v `mod` n
                  -- Reduce a representable Integer argument mod n, at a
                  -- width wide enough to hold it before reduction.
                  Nothing -> do
                        let ww = max (A.sizeOf a) w
                            r  = A.Prim noAnn ww A.UMod [resize ww a, A.Lit noAnn $ bitVec (fromIntegral ww) n]
                        pure $ A.Prim noAnn w (A.Trunc w) [r]
      ("fromZ",  [_n], [a])          -> pure a -- Z n -> Integer, value-preserving
      ("toInteger", [_t], [a])       -> pure a -- word -> Integer, value-preserving
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
            _ <- tyWidth el -- representable
            pure $ A.cat [a, b]
      ("take", [f, b, el], [a])      -> do
            (fw, bw, ew) <- (,,) <$> tyNat' "take" f <*> tyNat' "take" b <*> tyWidth el
            pure $ A.Slice noAnn (fromIntegral $ bw * ew) (fromIntegral $ fw * ew) a
      ("drop", [f, b, el], [a])      -> do
            (_fw, bw, ew) <- (,,) <$> tyNat' "drop" f <*> tyNat' "drop" b <*> tyWidth el
            pure $ A.Slice noAnn 0 (fromIntegral $ bw * ew) a
      ("head", [n, el], [a])         -> do
            (nv, ew) <- (,) <$> tyNat' "head" n <*> tyWidth el
            pure $ elemSlice (nv + 1) ew 0 a
      ("last", [n, el], [a])         -> do
            (_, ew) <- (,) <$> tyNat' "last" n <*> tyWidth el
            pure $ A.Slice noAnn 0 (fromIntegral ew) a
      ("tail", [n, el], [a])         -> do
            (nv, ew) <- (,) <$> tyNat' "tail" n <*> tyWidth el
            pure $ A.Slice noAnn 0 (fromIntegral $ nv * ew) a
      ("reverse", [n, el], [a])      -> do
            (nv, ew) <- (,) <$> tyNat' "reverse" n <*> tyWidth el
            pure $ A.cat [ elemSlice nv ew i a | i <- [nv - 1, nv - 2 .. 0] ]
      ("split",   _, [a])            -> pure a -- regrouping only: the bits are unchanged
      ("join",    _, [a])            -> pure a
      ("splitAt", _, [a])            -> pure a -- the result tuple is (front, back) = the same bits
      ("min", [t], [a, b])           -> (\ w -> A.If noAnn w (A.Prim noAnn 1 A.ULt [a, b]) a b) <$> likeWord "min" t
      ("max", [t], [a, b])           -> (\ w -> A.If noAnn w (A.Prim noAnn 1 A.UGt [a, b]) a b) <$> likeWord "max" t
      ("sum", [n, el], [a])          -> reduce "sum" A.Add n el a
      ("product", [n, el], [a])      -> reduce "product" A.Mul n el a
      ("@", [n, el, _ix], [a, i])    -> index "@" True  n el a i
      ("!", [n, el, _ix], [a, i])    -> index "!" False n el a i
      -- Hyle is total: there is no bottom. error/assert/undefined (the
      -- latter two are prelude-defined via error) become a zero "poison"
      -- constant of the result width; reachability is the user's
      -- concern, as in synthesized HDL generally. A static scan
      -- (warnUses) emits a compile-time warning. trace/traceVal are the
      -- identity on their result.
      ("error", [a, _n], _)          -> A.Lit noAnn . zeros . fromIntegral <$> tyWidth a
      ("trace", [_n, _a, _b], [_s, _v, r]) -> pure r
      ("<<<", [n, _ix, el], [a, b])  -> rotate "<<<" True  n el a b
      (">>>", [n, _ix, el], [a, b])  -> rotate ">>>" False n el a b
      ("update",    [n, el, _ix], [xs, i, v]) -> update' "update"    True  n el xs i v
      ("updateEnd", [n, el, _ix], [xs, i, v]) -> update' "updateEnd" False n el xs i v
      ("transpose", [r, c, el], [a]) -> do
            (rv, cv, ew) <- (,,) <$> tyNat' "transpose" r <*> tyNat' "transpose" c <*> tyWidth el
            pure $ A.cat [ elemSlice (rv * cv) ew (i * cv + j) a | j <- [0 .. cv - 1], i <- [0 .. rv - 1] ]
      ("fromInteger", [t], [a])      -> do
            w <- likeWord "fromInteger" t
            -- A constant folds to a literal; otherwise resize the
            -- representable Integer argument (e.g. fromZ of a Z n value,
            -- whose representation is the underlying word) to the target
            -- word width -- truncating or zero-extending as Cryptol's
            -- Integer-to-word conversion does.
            case litVal a of
                  Just v  -> pure $ A.Lit noAnn $ bitVec (fromIntegral w) v
                  Nothing -> pure $ resize w a
      ("lg2", [n], [a])              -> do
            nv <- tyNat' "lg2" n
            let w = fromIntegral nv
            pure $ foldr (\ k rest -> A.If noAnn w (A.Prim noAnn 1 A.ULe [a, A.Lit noAnn $ bitVec (fromIntegral w) ((2 :: Integer) ^ k)])
                                                   (A.Lit noAnn $ bitVec (fromIntegral w) k) rest)
                         (A.Lit noAnn $ bitVec (fromIntegral w) nv) [0 .. nv - 1]
      ("/$", [n], [a, b])            -> signedDivMod True  n a b
      ("%$", [n], [a, b])            -> signedDivMod False n a b
      ("pmult", [u, v], [a, b])      -> do
            (uv, vv) <- (,) <$> tyNat' "pmult" u <*> tyNat' "pmult" v
            let wa = uv + 1                 -- dividend width
                wr = uv + vv + 1            -- result width
                -- b's coefficient of x^i, gating a shifted-by-i copy of a.
                term i = A.If noAnn (fromIntegral wr) (A.Slice noAnn (fromIntegral i) 1 b)
                              (A.cat $ [ A.Lit noAnn $ zeros $ fromIntegral $ wr - wa - i | wr - wa - i > 0 ]
                                    <> [a]
                                    <> [ A.Lit noAnn $ zeros $ fromIntegral i | i > 0 ])
                              (A.Lit noAnn $ zeros $ fromIntegral wr)
            pure $ foldr (\ i acc -> A.Prim noAnn (fromIntegral wr) A.XOr [term i, acc])
                         (A.Lit noAnn $ zeros $ fromIntegral wr) [0 .. vv]
      ("pdiv", [_u, _v], [a, b])     -> A.cat . fst <$> pdivmod a b
      ("pmod", [_u, v], [a, b])      -> do
            vv     <- tyNat' "pmod" v
            (_, r) <- pdivmod a b
            pure $ A.cat $ [ A.Lit noAnn $ zeros $ fromIntegral $ vv - genericLength r | vv > genericLength r ] <> r
      _ | Just (vs, elt) <- enumVals pn tys, null args -> do
            ew <- likeWord pn elt
            pure $ A.cat [ A.Lit noAnn $ bitVec (fromIntegral ew) v | v <- vs ]
      _                              -> Left $ "cryptol: unsupported primitive: " <> pn
            <> (if null tys then "" else " (at " <> T.intercalate ", " (map tshow tys) <> ")")
      where wordBin :: Text -> T.Type -> A.Op -> A.Exp -> A.Exp -> Either Text A.Exp
            wordBin nm t op a b = (\ w -> A.Prim noAnn w op [a, b]) <$> likeWord nm t

            cmp :: T.Type -> A.Op -> A.Exp -> A.Exp -> Either Text A.Exp
            cmp t op a b
                  -- Constant comparisons fold (also covering constant
                  -- Integer-typed operands, whose literals' widths differ).
                  | op `elem` [A.Eq, A.Ne, A.ULt, A.ULe, A.UGt, A.UGe]
                  , Just va <- litVal a, Just vb <- litVal b =
                        let r = case op of
                                    A.Eq  -> va == vb
                                    A.Ne  -> va /= vb
                                    A.ULt -> va < vb
                                    A.ULe -> va <= vb
                                    A.UGt -> va > vb
                                    _     -> va >= vb
                        in pure $ A.Lit noAnn $ bitVec 1 $ fromEnum r
                  | otherwise = do
                        _ <- tyWidth t -- representable
                        pure $ A.Prim noAnn 1 op [a, b]

            -- Rotation by a constant is re-wiring; by a variable amount,
            -- the doubled-sequence shift trick: rotate the bits of a#a
            -- by (amount mod n) elements and take the top (<<<) or
            -- bottom (>>>) half.
            rotate :: Text -> Bool -> T.Type -> T.Type -> A.Exp -> A.Exp -> Either Text A.Exp
            rotate nm left n el a b = do
                  nv <- tyNat' nm n
                  ew <- tyWidth el
                  let w = A.sizeOf a
                  if nv <= 1 then pure a else case litVal b of
                        Just k -> do
                              let k' = (if left then k else nv - k `mod` nv) `mod` nv
                              pure $ if k' == 0 then a else A.cat
                                    [ A.Slice noAnn 0 (fromIntegral $ (nv - k') * ew) a
                                    , A.Slice noAnn (fromIntegral $ (nv - k') * ew) (fromIntegral $ k' * ew) a ]
                        Nothing -> do
                              let wk   = max (fromIntegral $ A.sizeOf b) (nbits (fromIntegral nv) + 1)
                                  wamt = fromIntegral $ max wk $ nbits (fromIntegral $ (nv - 1) * ew) + 1
                                  b'   | A.sizeOf b == wamt = b
                                       | otherwise          = A.Prim noAnn wamt (A.ZExt wamt) [b]
                                  kmod = A.Prim noAnn wamt A.UMod [b', A.Lit noAnn $ bitVec (fromIntegral wamt) nv]
                                  amt  = A.Prim noAnn wamt A.Mul [kmod, A.Lit noAnn $ bitVec (fromIntegral wamt) ew]
                                  dbl  = A.Cat noAnn a a
                              pure $ if left
                                    then A.Slice noAnn w w $ A.Prim noAnn (2 * w) A.Shl  [dbl, amt]
                                    else A.Slice noAnn 0 w $ A.Prim noAnn (2 * w) A.LShr [dbl, amt]

            -- Sequence update: a constant index is re-wiring; a variable
            -- index muxes each element against an index comparison.
            update' :: Text -> Bool -> T.Type -> T.Type -> A.Exp -> A.Exp -> A.Exp -> Either Text A.Exp
            update' nm fromFront n el xs i v = do
                  nv <- tyNat' nm n
                  ew <- tyWidth el
                  case litVal i of
                        Just iv -> do
                              unless (iv >= 0 && iv < nv) $ Left $ "cryptol: (" <> nm <> ") index out of bounds."
                              let idx = if fromFront then iv else nv - 1 - iv -- position from the front (MSB)
                                  off = (nv - 1 - idx) * ew                   -- the replaced element's LSB offset
                              pure $ A.cat $ [ A.Slice noAnn (fromIntegral $ off + ew) (fromIntegral $ idx * ew) xs | idx > 0 ]
                                          <> [ v ]
                                          <> [ A.Slice noAnn 0 (fromIntegral off) xs | off > 0 ]
                        Nothing -> do
                              let wI = A.sizeOf i
                                  jlit j = let jv = if fromFront then j else nv - 1 - j
                                           in if jv < 2 ^ toInteger wI
                                                 then Just $ A.Lit noAnn $ bitVec (fromIntegral wI) jv
                                                 else Nothing -- the index can never name this element
                                  elem' j = elemSlice nv ew j xs
                              pure $ A.cat [ case jlit j of
                                                 Just jl -> A.If noAnn (fromIntegral ew) (A.Prim noAnn 1 A.Eq [i, jl]) v $ elem' j
                                                 Nothing -> elem' j
                                           | j <- [0 .. nv - 1] ]

            -- Signed division/remainder (truncated toward zero, remainder
            -- taking the dividend's sign), via unsigned ops on magnitudes.
            signedDivMod :: Bool -> T.Type -> A.Exp -> A.Exp -> Either Text A.Exp
            signedDivMod isDiv n a b = do
                  nv <- tyNat' (if isDiv then "/$" else "%$") n
                  let w     = fromIntegral nv
                      z     = A.Lit noAnn $ zeros $ fromIntegral w
                      neg x = A.Prim noAnn w A.Sub [z, x]
                      sgn x = A.Prim noAnn 1 A.SLt [x, z]
                      mag x = A.If noAnn w (sgn x) (neg x) x
                      q     = A.Prim noAnn w (if isDiv then A.UDiv else A.UMod) [mag a, mag b]
                  pure $ if isDiv
                        then A.If noAnn w (A.Prim noAnn 1 A.XOr [sgn a, sgn b]) (neg q) q
                        else A.If noAnn w (sgn a) (neg q) q

            -- Polynomial (carry-less) long division by a constant
            -- divisor: quotient and remainder bits as XOR combinations of
            -- the dividend's bits (both MSB-first).
            pdivmod :: A.Exp -> A.Exp -> Either Text ([A.Exp], [A.Exp])
            pdivmod a b = do
                  bv <- maybe (Left "cryptol: polynomial division by a non-constant divisor is not supported (pdiv/pmod need a constant polynomial).") pure $ litVal b
                  unless (bv /= 0) $ Left "cryptol: polynomial division by zero."
                  let d    = genericLength (takeWhile (> 1) $ iterate (`div` 2) bv) :: Integer
                      wa   = fromIntegral (A.sizeOf a) :: Integer
                      abit k = A.Slice noAnn (fromIntegral $ wa - 1 - k) 1 a
                      xor1 x y = A.Prim noAnn 1 A.XOr [x, y]
                      step (qs, r) k =
                            let (top, rest) = case r <> [abit k] of -- coefficients x^d .. x^0
                                    t : r' -> (t, r')
                                    []     -> (abit k, [])          -- unreachable: the list is nonempty
                                r2  = [ if testBit bv (fromIntegral $ d - 1 - j) then xor1 rj top else rj
                                      | (j, rj) <- zip [0 :: Integer ..] rest ]
                            in (qs <> [top], r2)
                      (q, r) = foldl step ([], replicate (fromIntegral d) (A.Lit noAnn $ zeros 1)) [0 .. wa - 1]
                  pure (q, r)

            -- Sequence shifts move whole elements (bit shifts when the
            -- elements are bits): the amount scales by the element width.
            shift :: Text -> T.Type -> T.Type -> A.Op -> A.Exp -> A.Exp -> Either Text A.Exp
            shift _nm _n el op a b = do
                  ew <- tyWidth el
                  let w = A.sizeOf a
                  if ew == 1 then pure $ A.Prim noAnn w op [a, b] else case litVal b of
                        Just k  -> pure $ A.Prim noAnn w op [a, A.Lit noAnn $ bitVec (fromIntegral $ max 1 $ nbits $ fromIntegral $ k * ew + 1) $ k * ew]
                        Nothing -> do
                              -- Wide enough that the element-to-bit scaling can't wrap.
                              let wamt = A.sizeOf b + fromIntegral (max 1 $ nbits $ fromIntegral $ ew + 1)
                                  b'   = A.Prim noAnn wamt (A.ZExt wamt) [b]
                              pure $ A.Prim noAnn w op [a, A.Prim noAnn wamt A.Mul [b', A.Lit noAnn $ bitVec (fromIntegral wamt) ew]]

            -- Element selection from the front (@) or back (!): a static
            -- slice for a constant index; for a variable index, a shift
            -- by index-times-element-width (toward the MSB end for @,
            -- since element 0 is most significant) and a fixed slice.
            index :: Text -> Bool -> T.Type -> T.Type -> A.Exp -> A.Exp -> Either Text A.Exp
            index nm fromFront n el a i = do
                  nv <- tyNat' nm n
                  ew <- tyWidth el
                  case i of
                        A.Lit _ bv -> do
                              let iv = nat bv
                              unless (iv >= 0 && iv < nv) $ Left $ "cryptol: (" <> nm <> ") index out of bounds."
                              pure $ if fromFront then elemSlice nv ew iv a
                                                  else A.Slice noAnn (fromIntegral $ iv * ew) (fromIntegral ew) a
                        _ -> do
                              let wAmt = max 1 $ fromIntegral $ nbits $ fromIntegral $ (nv - 1) * ew + 1
                                  w    = max wAmt $ A.sizeOf i
                                  i'   | A.sizeOf i == w = i
                                       | otherwise       = A.Prim noAnn w (A.ZExt w) [i]
                                  amt  = A.Prim noAnn w A.Mul [i', A.Lit noAnn $ bitVec (fromIntegral w) ew]
                                  aw   = A.sizeOf a
                              pure $ if fromFront
                                    then A.Slice noAnn (aw - fromIntegral ew) (fromIntegral ew) $ A.Prim noAnn aw A.Shl [a, amt]
                                    else A.Slice noAnn 0 (fromIntegral ew) $ A.Prim noAnn aw A.LShr [a, amt]

            -- Elementwise reduction of a sequence to one element.
            reduce :: Text -> A.Op -> T.Type -> T.Type -> A.Exp -> Either Text A.Exp
            reduce nm op n el a = do
                  nv <- tyNat' nm n
                  ew <- likeWord nm el
                  let z = A.Lit noAnn $ zeros $ fromIntegral ew
                      zsum | op == A.Mul = A.Lit noAnn $ bitVec (fromIntegral ew) (1 :: Integer)
                           | otherwise   = z
                  pure $ foldl (\ acc i -> A.Prim noAnn ew op [acc, elemSlice nv (fromIntegral ew) i a]) zsum [0 .. nv - 1]

            -- A word (or Bit) instance width.
            likeWord :: Text -> T.Type -> Either Text A.Size
            likeWord nm t = case wordWidth t of
                  Just w  -> pure $ fromIntegral w
                  Nothing -> Left $ "cryptol: (" <> nm <> ") at an unsupported instance type: " <> tshow t

            tyNat' :: Text -> T.Type -> Either Text Integer
            tyNat' nm t = maybe (Left $ "cryptol: (" <> nm <> "): expected a numeric type, got: " <> tshow t) pure $ tyNat t

            -- Modular reduction of a binary Ring op on Z n: widen the
            -- operands, apply the op at the wider width @cap@ (a bound on
            -- the unreduced result), reduce modulo n, and narrow back.
            zMod :: Integer -> Integer -> A.Op -> A.Exp -> A.Exp -> Either Text A.Exp
            zMod n cap op a b = do
                  let w  = fromIntegral $ nbits $ fromIntegral n
                      ww = fromIntegral $ max (fromIntegral w) $ nbits $ fromIntegral cap
                      up x = A.Prim noAnn ww (A.ZExt ww) [x]
                      r  = A.Prim noAnn ww A.UMod [A.Prim noAnn ww op [up a, up b], A.Lit noAnn $ bitVec (fromIntegral ww) n]
                  pure $ A.Prim noAnn w (A.Trunc w) [r]

            -- Negation in Z n: n - a for a /= 0, else 0 (n - a mod n).
            zNeg :: Integer -> A.Exp -> A.Exp
            zNeg n a = let w = fromIntegral $ nbits $ fromIntegral n
                       in A.If noAnn w (A.Prim noAnn 1 A.Eq [a, A.Lit noAnn $ zeros $ fromIntegral w]) a
                              $ A.Prim noAnn w A.Sub [A.Lit noAnn $ bitVec (fromIntegral w) n, a]

            -- The multiplicative inverse in Z p (p prime), a^(p-2) mod p
            -- by square-and-multiply over the constant exponent's bits.
            -- The successive squares are let-bound (each is reused by the
            -- next square and, when the bit is set, by the product);
            -- the product chain threads as an expression (single-use).
            zRecip :: Integer -> A.Exp -> Either Text A.Exp
            zRecip n a = do
                  let w    = fromIntegral $ nbits $ fromIntegral n
                      e    = n - 2                            -- p >= 3, so e >= 1
                      top  = fromIntegral $ nbits (fromIntegral n) - 1 :: Integer -- >= highest set bit of p-2
                      one  = A.Lit noAnn $ bitVec (fromIntegral w) (1 :: Integer)
                      bn i = "zr$b$" <> showt (i :: Integer)
                      bvar i = A.Var noAnn w $ bn i
                  -- base_0 = a, base_{i+1} = base_i^2 (mod p).
                  sqs   <- mapM (\ i -> (bn (i + 1), ) <$> zMod n (n * n) A.Mul (bvar i) (bvar i)) [0 .. top - 1]
                  let lets = (bn 0, a) : sqs
                  -- product of base_i for each set bit i of the exponent.
                  result <- foldM (\ acc i -> if testBit e (fromIntegral i)
                                                    then zMod n (n * n) A.Mul acc (bvar i)
                                                    else pure acc)
                                  one [0 .. top]
                  pure $ foldr (\ (x, rhs) b -> A.Let noAnn (A.sizeOf b) x rhs b) result lets


---
--- Cryptol types to widths.
---

-- | The width of a representable Cryptol value type: Bit, words,
--   sequences, tuples, records, and nominal types (newtypes are their
--   field record; enums are nbits(#constructors) of tag plus the widest
--   constructor payload).
tyWidth :: T.Type -> Either Text Integer
tyWidth t = case T.tNoUser t of
      T.TCon (T.TC T.TCBit) []      -> pure 1
      T.TCon (T.TC T.TCSeq) [n, el] -> (*) <$> maybe (Left $ "cryptol: sequence length is not a literal: " <> tshow n) pure (tyNat n)
                                           <*> tyWidth el
      T.TCon (T.TC (T.TCTuple _)) es -> sum <$> mapM tyWidth es
      T.TCon (T.TC T.TCIntMod) [n] -> maybe (Left $ "cryptol: Z at a non-literal modulus: " <> tshow n)
                                            (pure . fromIntegral . nbits . fromIntegral) (tyNat n)
      T.TRec fs                     -> sum <$> mapM tyWidth (recordElements fs)
      T.TNominal nt tys             -> do
            su <- paramSubst nt tys
            case T.ntDef nt of
                  T.Struct sc -> sum <$> mapM (tyWidth . TS.apSubst su) (recordElements $ T.ntFields sc)
                  T.Enum ecs  -> do
                        ws <- mapM (fmap sum . mapM (tyWidth . TS.apSubst su) . T.ecFields) ecs
                        pure $ toInteger (nbits $ fromIntegral $ length ecs) + maximum (0 : ws)
                  T.Abstract  -> Left $ "cryptol: abstract type: " <> tshow t
      _                             -> Left $ "cryptol: unrepresentable type: " <> tshow t

-- | Record fields and widths, in canonical (label-sorted) order -- the
--   layout order, first field most significant. Newtypes are their
--   underlying record.
recFields :: T.Type -> Either Text [(I.Ident, Integer)]
recFields t = case T.tNoUser t of
      T.TRec fs -> mapM (\ (f, ft) -> (f, ) <$> tyWidth ft) $ canonicalFields fs
      T.TNominal nt tys | T.Struct sc <- T.ntDef nt -> do
            su <- paramSubst nt tys
            mapM (\ (f, ft) -> (f, ) <$> tyWidth (TS.apSubst su ft)) $ canonicalFields $ T.ntFields sc
      _         -> Left $ "cryptol: expected a record type, got: " <> tshow t

-- | The substitution instantiating a nominal type's parameters.
paramSubst :: T.NominalType -> [T.Type] -> Either Text TS.Subst
paramSubst nt tys = do
      unless (length (T.ntParams nt) == length tys)
            $ Left $ "cryptol: under-applied nominal type: " <> I.identText (N.nameIdent $ T.ntName nt)
      pure $ TS.listParamSubst $ zip (T.ntParams nt) tys

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

-- | The modulus of a @Z n@ instance type.
zN :: T.Type -> Maybe Integer
zN t = case T.tNoUser t of
      T.TCon (T.TC T.TCIntMod) [n] -> tyNat n
      _                            -> Nothing

-- | The type of a subexpression, reconstructed from the schemas in scope.
exprTy :: TEnv -> T.Expr -> Either Text T.Type
exprTy env e = pure $ T.fastTypeOf (teTypes env) e

tshow :: T.Type -> Text
tshow = T.pack . show . pp

-- | An application spine, stripping locations and proofs and collecting
--   type applications (which survive specialization only on
--   constructors, whose instantiation they carry).
spine :: T.Expr -> (T.Expr, [T.Type], [T.Expr])
spine = go [] []
      where go :: [T.Type] -> [T.Expr] -> T.Expr -> (T.Expr, [T.Type], [T.Expr])
            go tacc acc = \ case
                  T.ELocated _ e -> go tacc acc e
                  T.EProofApp e  -> go tacc acc e
                  T.EApp f a     -> go tacc (a : acc) f
                  T.ETApp e t    -> go (t : tacc) acc e
                  e              -> (e, tacc, acc)

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
