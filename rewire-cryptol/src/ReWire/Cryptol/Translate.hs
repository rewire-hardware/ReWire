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
--   Supported fragment: first-order functions over Bit, words, vectors,
--   tuples, records, newtypes, and enums (case expressions); if-then-else;
--   local value bindings; comprehensions and folds (unrolled); the scalar
--   and slicing primitives. Records and enums are interior-only: the
--   entry point's type must still be words/vectors/tuples (the rwc side
--   has no Cryptol-record counterpart). Enum values are laid out
--   tag#pad#args with the tag at the most-significant end and the tag
--   width nbits(#constructors) -- the same convention the Eidos fold
--   uses for ReWire ADTs. Local function bindings and recursion are
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
import Data.Char (isAlphaNum)
import Data.Graph (stronglyConnComp, SCC (..))
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
                        liftEither $ transClosure entry body dgs nmap $ ME.loadedNominalTypes env2

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
      }

-- | A nominal-type constructor: a struct/newtype's (transparent), or an
--   enum's (tagged).
type ConDef = Either T.StructCon T.EnumCon

transClosure :: Text -> T.Expr -> [T.DeclGroup] -> Map.Map N.Name (TM.TypesMap N.Name) -> Map.Map N.Name T.NominalType -> Either Text [A.Defn]
transClosure entry body dgs nmap noms = do
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
                     }
      mapM (transDecl env . snd) fits
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
                  T.Recursive ds   -> mapM unSCC $ stronglyConnComp
                        [ (d, N.nameUnique $ T.dName d, filter (`HM.member` us) $ map N.nameUnique $ declRefs d)
                        | d <- ds ]
                        where us = HM.fromList [ (N.nameUnique $ T.dName d, ()) | d <- ds ]

            unSCC :: SCC T.Decl -> Either Text T.Decl
            unSCC = \ case
                  AcyclicSCC d -> pure d
                  CyclicSCC ds -> Left $ "cryptol: recursive definitions are not supported: "
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

-- | Translate an expression; @etas@ are pending (already-translated)
--   arguments applied to it: the eta-expansion of a point-free
--   definition, or arguments awaiting a function-valued subexpression
--   (they thread through lambdas, ifs, wheres, and cases).
transExp :: TEnv -> [A.Exp] -> T.Expr -> Either Text A.Exp
transExp env etas e0 = case spine e0 of
      (T.EVar x, tys, args)
            | Just (nt, con) <- HM.lookup (N.nameUnique x) (teCons env) -> do
                  unless (null etas) $ Left "cryptol: partial application of a constructor cannot cross the Cryptol boundary."
                  args' <- mapM (transExp env []) args
                  transCon nt con tys args'
            | Just (envF, rhs) <- HM.lookup (N.nameUnique x) (teFuns env) ->
                  inline env envF rhs args etas
            | Just (pn, tys') <- HM.lookup (N.nameUnique x) (tePrims env), pn `elem` (["foldl", "foldr"] :: [Text]) -> do
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
--   close over and inlined at their use sites.
transWhere :: TEnv -> [A.Exp] -> T.Expr -> [T.DeclGroup] -> Either Text A.Exp
transWhere env etas body dgs = do
      decls <- orderDecls dgs
      go env decls
      where go :: TEnv -> [T.Decl] -> Either Text A.Exp
            go env' []       = transExp env' etas body
            go env' (d : ds) = case T.dDefinition d of
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

-- | A comprehension, fully unrolled: the lengths are concrete after
--   specialization, so each element of the result is the body translated
--   with the generator variables bound to slices of their (translated)
--   sources. Arms zip; generators within an arm nest (the last one
--   fastest), matching Cryptol's semantics.
transComp :: TEnv -> T.Type -> T.Expr -> [[T.Match]] -> Either Text A.Exp
transComp env _ety body mss = do
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

-- | Extend the environment with translated bindings.
bindAll :: TEnv -> [(N.Name, T.Type, A.Exp)] -> TEnv
bindAll env binds = env
      { teScope = HM.fromList [ (N.nameUnique x, a) | (x, _, a) <- binds ] <> teScope env
      , teTypes = foldr (\ (x, t, _) -> Map.insert x (T.tMono t)) (teTypes env) binds
      }

-- | Unrolled folds; the function argument is taken unstranslated (it may
--   be a reference or a lambda -- there are no function values in Hyle).
transFold :: TEnv -> Text -> [T.Type] -> [T.Expr] -> Either Text A.Exp
transFold env pn tys args = case (pn, tys, args) of
      ("foldl", [n, _b, a], [f, z, xs]) -> do
            (nv, we, xsA, zA) <- setup n a xs z
            foldM (\ acc i -> applyFn env f [acc, elemSlice nv we i xsA]) zA [0 .. nv - 1]
      ("foldr", [n, a, _b], [f, z, xs]) -> do
            (nv, we, xsA, zA) <- setup n a xs z
            foldM (\ acc i -> applyFn env f [elemSlice nv we i xsA, acc]) zA [nv - 1, nv - 2 .. 0]
      _ -> Left $ "cryptol: unsupported use of " <> pn <> " (expected a fully applied fold)."
      where setup :: T.Type -> T.Type -> T.Expr -> T.Expr -> Either Text (Integer, Integer, A.Exp, A.Exp)
            setup n a xs z = do
                  nv  <- maybe (Left $ "cryptol: (" <> pn <> "): non-literal length.") pure $ tyNat n
                  we  <- tyWidth a
                  xsA <- transExp env [] xs
                  zA  <- transExp env [] z
                  pure (nv, we, xsA, zA)

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

-- | Element @i@ (from the front -- index 0 is the most significant) of a
--   sequence of @n@ elements of width @we@.
elemSlice :: Integer -> Integer -> Integer -> A.Exp -> A.Exp
elemSlice n we i = A.Slice noAnn (fromIntegral $ (n - 1 - i) * we) (fromIntegral we)

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
      ("fromTo", [first, lst, bits], []) -> do
            (fv, lv, bv) <- (,,) <$> tyNat' "fromTo" first <*> tyNat' "fromTo" lst <*> tyNat' "fromTo" bits
            pure $ A.cat [ A.Lit noAnn $ bitVec (fromIntegral bv) v | v <- [fv .. lv] ]
      ("fromThenTo", [first, next, lst, bits, _len], []) -> do
            (fv, nv, lv, bv) <- (,,,) <$> tyNat' "fromThenTo" first <*> tyNat' "fromThenTo" next
                                      <*> tyNat' "fromThenTo" lst <*> tyNat' "fromThenTo" bits
            pure $ A.cat [ A.Lit noAnn $ bitVec (fromIntegral bv) v | v <- [fv, nv .. lv] ]
      ("@", [n, el, _ix], [a, i])    -> index "@" True  n el a i
      ("!", [n, el, _ix], [a, i])    -> index "!" False n el a i
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
