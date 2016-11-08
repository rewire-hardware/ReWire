{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE Safe, LambdaCase #-}
module ReWire.FrontEnd.ToCoq (toCoq) where

import System.IO
import ReWire.Pretty
import ReWire.Annotation
import ReWire.FrontEnd.Unbound
      ( Fresh (..), name2String, string2Name, runFreshMT
      )
import ReWire.Error
import qualified ReWire.FrontEnd.Syntax as M
import ReWire.FrontEnd.Records (freshVar, freshVars, replaceAtIndex, poly2Ty)
import Control.Monad.IO.Class

import ReWire.FrontEnd.Purify

import Debug.Trace

-- import ReWire.FrontEnd.CoqSyntax

toCoq :: (Fresh m, MonadError AstError m, MonadIO m) => M.FreeProgram -> m M.FreeProgram
toCoq (ddecls,fdecls) = do
      handle <- liftIO $ openFile "some.coq" WriteMode
--      let p = M.Program $ M.trec (ddecls,fdecls)
      dcons <- mapM foo (filter isMainDefn ddecls)
      liftIO $ hPutStrLn handle $ foldr (\ x xs -> x ++ "\n" ++ xs) "" $ concat dcons
  --    hClose handle
      return (ddecls,fdecls)

foo :: Fresh m => M.DataDefn -> m [String]
foo d = mapM prettyDataCon (M.dataCons d)
       
prettyDataCon :: Fresh m => M.DataCon -> m String
prettyDataCon (M.DataCon _ name (M.Embed phi)) = do
  ty <- poly2Ty phi
  return $ n ++ " :: " ++ show (delMainTy ty)
     where n = delMain $ name2String name

isMainDefn :: M.DataDefn -> Bool
isMainDefn d = case isMain (name2String (M.dataName d)) of
  Just _  -> True
  Nothing -> False

delMain :: String -> String
delMain n = case isMain n of
  Just cs -> cs
  Nothing -> n

isMain :: String -> Maybe String
isMain = \ case
  ('M':'a':'i':'n':'.':cs) -> Just cs
  _                        -> Nothing

delMainTy :: M.Ty -> M.Ty
delMainTy (M.TyApp an t1 t2)   = M.TyApp an (delMainTy t1) (delMainTy t2)
delMainTy (M.TyCon an n)       = M.TyCon an n'
  where
    n' = string2Name $ delMain $ name2String n
delMainTy (M.TyVar an k n)     = M.TyVar an k n'
  where
    n' = string2Name $ delMain $ name2String n
delMainTy (M.TyComp an t1 t2)  = M.TyComp an (delMainTy t1) (delMainTy t2)
delMainTy (M.TyBlank an)       = M.TyBlank an

{-
data DataDefn = DataDefn
      { dataAnnote :: Annote
      , dataName   :: Name TyConId
      , dataKind   :: Kind
      , dataCons   :: [DataCon]
      } deriving (Generic, Show, Typeable, Data)

data DataConId = DataConId String
      deriving (Generic, Typeable, Data)
data TyConId = TyConId String
      deriving (Generic, Typeable, Data)
data FieldId  = FieldId String
      deriving (Generic, Typeable, Data)

data DataCon = DataCon Annote (Name DataConId) (Embed Poly)
             | RecCon Annote (Name DataConId) (Embed Poly) [([Name FieldId],Embed Poly)]
      deriving (Generic, Eq, Show, Typeable, Data)


Inductive Eff : Type :=
  | EffNone : Eff
  | EffR    : Eff
  | EffW    : Eff
  | EffRW   : Eff.

Inductive SMo : Type :=
  | MIdentity : SMo  (* zero-layer state monad *)
  | MStateT   : Ty -> Eff -> SMo -> SMo

with Mo : Type :=
  | MReactT   : Ty -> Ty -> SMo -> Mo
  | MNonReact : SMo -> Mo

with Ty : Type :=
  | TArrow   : Ty -> Ty -> Ty
  | TProd    : Ty -> Ty -> Ty
  | TSum     : Ty -> Ty -> Ty
  | TNil     : Ty
  | TMonadic : Mo -> Ty -> Ty.
-}

{-
data Ty = TyApp Annote Ty Ty
        | TyCon Annote (Name TyConId)
        | TyVar Annote Kind (Name Ty)
        | TyComp Annote Ty Ty -- application of a monad
        | TyBlank Annote
           deriving (Eq, Generic, Typeable, Data)
-}

data Eff = EffNone | EffR | EffW | EffRW
data SMo = MIdentity | MStateT Ty Eff SMo
data Mo  = MReactT Ty Ty SMo | MNonReact SMo
data Ty  = TArrow Ty Ty | TProd Ty Ty | TSum Ty Ty | TNil | TMonadic Mo Ty

transTy :: M.Ty -> Maybe Ty
transTy t = do
  t' <- classifyTy t
  case t' of

   Arrow _ t1 t2    -> do
     t1' <- transTy t1
     t2' <- transTy t2
     return $ TArrow t1' t2'

   ReTApp _ i o m a -> do
     i'                          <- transTy i
     o'                          <- transTy o
     TMonadic (MNonReact smo) a' <- transTy (M.TyApp NoAnnote m a)
     return $ TMonadic (MReactT i' o' smo) a'
     
   StTApp _ s m a   -> do
     s'                          <- transTy s
     TMonadic (MNonReact smo) a' <- transTy (M.TyApp NoAnnote m a)
     return $ TMonadic (MNonReact (MStateT s' EffRW smo)) a'
     
   IdApp _ a        -> do
     a' <- transTy a
     return $ TMonadic (MNonReact MIdentity) a'
     
   PairApp _ t1 t2  -> do
     t1' <- transTy t1
     t2' <- transTy t2
     return $ TProd t1' t2'
     
   Pure _ t         -> case t of
     M.TyCon _ n | name2String n == "()" -> return TNil
     _                                   -> Nothing
        -- ^^^ last case there isn't anything corresponding in Coq semantics.
        --     that'll have to be extended.

type Id = String -- obviously, something's going to have to be done about this.

data Tm = Tvar Id          
        | Tapp Tm Tm       
        | Tabs Id Ty Tm
        | Tcon Id Ty --- new, not in the Coq code.
        | Tunit            
        | Tpair Tm Tm      
        | Tproj Tm Tm
        | Tinl Ty Tm
        | Tinr Ty Tm
        | Tcase Tm Tm Tm
        | Treturn Mo Tm
        | Tbind Ty Tm Tm
        | Tlift Mo Tm
        | Televate SMo Tm
        | Tget SMo
        | Tput SMo Tm
        | Trunst Tm Tm
        | Trunid Tm
        | TPause Mo Ty Tm
        | Tunfold Mo Ty Ty Tm Tm
        | Trunre Ty Tm

{-
data Exp = App        Annote Exp Exp
         | Lam        Annote Ty (Bind (Name Exp) Exp)
         | Var        Annote Ty (Name Exp)
         | Con        Annote Ty (Name DataConId)
         | RecConApp  Annote Ty (Name DataConId) [(Name FieldId,Exp)] 
         | RecUp      Annote Ty Exp  [(Name FieldId,Exp)] 
         | Case       Annote Ty Exp (Bind Pat Exp) (Maybe Exp)
         | Match      Annote Ty Exp MatchPat Exp [Exp] (Maybe Exp)
         | NativeVHDL Annote String Exp
         | Error      Annote Ty String
         deriving (Generic, {- Show,-} Typeable, Data)
-}

transTm :: (Fresh m,MonadError AstError m) => M.Exp -> m Tm
transTm = \ case
  (M.App _ e1 e2)          -> do
     t1 <- transTm e1
     t2 <- transTm e2
     return $ Tapp t1 t2
  (M.Lam _ t b)            -> do
    (arg,e) <- M.unbind b
    t'      <- liftMaybe "Ty didn't translate" $ transTy t
    let x = name2String arg
    e'      <- transTm e
    return $ Tabs x t' e'
  (M.Var _ _ n)            -> do -- N.b., we're forgetting the type.
    let x = name2String n
    return $ Tvar x
  (M.Con _ _ n)            -> do -- N.b., treating constructors as variables. Have to think this through.
    let x = name2String n
    return $ Tvar x
  (M.RecConApp _ _ _ _)    -> failAt NoAnnote "One thing at a time"
  (M.RecUp _ _ _ _)        -> failAt NoAnnote "One thing at a time"
  (M.Case _ t e b _)       -> error "not sure about this one yet"
  (M.Match _ _ _ _ _ _ _)  -> error "not sure about this one yet"
  (M.NativeVHDL _ _ _)     -> error "not sure about this one yet"
  (M.Error _ _ m)          -> failAt NoAnnote m

{-

Inductive tm : Type :=
  (* Lambda calculus *)
  | tvar     : id -> tm
  | tapp     : tm -> tm -> tm
  | tabs     : id -> Ty -> tm -> tm  (* ty is type of binder *)
  (* Data *)
  | tunit    : tm
  | tpair    : tm -> tm -> tm
  | tproj    : tm -> tm -> tm
  | tinl     : Ty -> tm -> tm
  | tinr     : Ty -> tm -> tm
  | tcase    : tm -> tm -> tm -> tm
  (* Monad operations (generic) *)
  | treturn  : Mo -> tm -> tm
  | tbind    : Ty -> tm -> tm -> tm
  | tlift    : Mo -> tm -> tm
  (* State monad operations *)
  | televate : SMo -> tm -> tm
  | tget     : SMo -> tm
  | tput     : SMo -> tm -> tm
  | trunst   : tm -> tm -> tm
  | trunid   : tm -> tm
  (* Reactive monad operations *)
  | tpause   : Mo -> Ty -> tm -> tm
  | tunfold  : Mo -> Ty -> Ty -> tm -> tm -> tm
  | trunre   : Ty -> tm -> tm.
-}
