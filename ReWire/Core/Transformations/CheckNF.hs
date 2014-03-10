module ReWire.Core.Transformations.CheckNF where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.Monad
import Unbound.LocallyNameless hiding (empty)
import Data.Set
import Control.Monad.State
import Control.Monad.Error

type NFM = ErrorT NFMError (StateT NFMState RW)
data NFMState = NFM { seen :: Set (Name RWCExp) }
type NFMError = String

getSeen = get >>= return . seen
modifySeen f = modify (\ s -> s { seen = f (seen s) })

checkArg :: RWCExp -> NFM ()
checkArg e@(RWCApp t _ _)   = do checkArgTy t
                                 let (ef:es) = flattenApp e
                                 mapM_ checkArg es
                                 case ef of
                                   -- RWCApp can't happen
                                   RWCLam _ b    -> throwError $ "checkArg: Encountered lambda expression (this needs to be lifted to the top level): " ++ show ef
                                   RWCVar _ n    -> checkIsNonRecursiveDefn n
                                   RWCCon _ i    -> return ()
                                   -- RWCLiteral can't happen
                                   RWCCase _ _ _ -> throwError $ "checkArg: Encountered case expression in function position: " ++ show ef
checkArg e@(RWCLam {})      = throwError $ "checkArg: Encountered lambda expression in argument position: " ++ show e
checkArg (RWCVar t n)       = checkArgTy t >> checkIsNonRecursiveDefn n
checkArg (RWCCon t _)       = checkArgTy t
checkArg (RWCLiteral t _)   = checkArgTy t
checkArg (RWCCase t e alts) = checkArgTy t >> checkArg e >> mapM_ checkArgAlt alts
  where checkArgAlt (RWCAlt b) = lunbind b (\(_,e) -> checkArg e)

checkIsNonRecursiveDefn :: Name RWCExp -> NFM ()
checkIsNonRecursiveDefn _ = return () -- FIXME

checkContTy :: RWCTy -> NFM ()
checkContTy (RWCTyApp (RWCTyApp (RWCTyCon "(->)") t1) t2) = checkMachineTy t2 -- FIXME: check that t1 is proper input type
checkContTy t                                             = throwError $ "checkContTy: Not an arrow type"

checkContCall :: RWCExp -> NFM ()
checkContCall e = do checkContTy (typeOf e)
                     let (ef:es) = flattenApp e
                     mapM_ checkArg es
                     case ef of
                       RWCVar _ n -> do md <- askDefn n
                                        case md of
                                          Just d  -> checkMachineDefn d
                                          Nothing -> throwError $ "checkContCall: Continuation name " ++ show n ++ "  does not exist or is bound locally"
                       _          -> throwError $ "checkContCall: Continuation head is not a named function: " ++ show ef

checkPauseApp :: RWCExp -> NFM ()
checkPauseApp e = do checkPauseTy (typeOf e)
                     case flattenApp e of
                       [RWCCon _ "P",esig,econt] -> checkArg esig >> checkContCall econt
                       _                         -> throwError $ "checkPauseApp: Malformed pause: " ++ show e

checkPause :: RWCExp -> NFM ()
checkPause (RWCCase _ e alts) = checkArg e >> mapM_ checkPauseAlt alts
  where checkPauseAlt (RWCAlt b) = lunbind b (\(_,e) -> checkPause e)
checkPause e                  = checkPauseApp e

checkPauseTy :: RWCTy -> NFM ()
checkPauseTy t = case flattenTyApp t of
                   (RWCTyCon "React":ts) -> mapM_ checkArgTy ts
                   _                     -> throwError $ "checkMachineResultTy: Result type is not in `React': " ++ show t

checkArgTy :: RWCTy -> NFM ()
checkArgTy t = case flattenTyApp t of
                 (RWCTyCon "(->)":_) -> throwError $ "checkArgTy: Argument has arrow type: " ++ show t
                 (RWCTyCon i:ts)     -> mapM_ checkArgTy ts -- FIXME: check if i is a recursive type
                 (RWCTyVar _:_)      -> throwError $ "checkArgTy: Argument has polymorphic type: " ++ show t

checkMachineTy :: RWCTy -> NFM ()
checkMachineTy t = mapM_ checkArgTy targs >> checkPauseTy tres -- FIXME: check that the last arg type is the input signal type
  where (targs,tres) = flattenArrow t

checkMachineDefn :: RWCDefn -> NFM ()
checkMachineDefn (RWCDefn n (Embed b)) = lunbind b $ \(tvs,(t,e_)) ->
                                          do s <- getSeen
                                             if n `member` s
                                              then return ()
                                              else do modifySeen (insert n)
                                                      checkMachineTy t
                                                      let (targs,tres) =  flattenArrow t
                                                      flattenLambda e_ $ \(xts,e) ->
                                                       if length targs /= length xts
                                                        then throwError $ "checkMachineDefn: Number of argument types does not match number of lambda binders"
                                                        else checkPause e

checkMainDefn :: RWCDefn -> NFM ()
checkMainDefn (RWCDefn _ (Embed b)) = lunbind b $ \(tvs,(t,e)) ->
                                      do modifySeen (insert (s2n "main"))
                                         checkMachineTy t
                                         checkPause e
                                         

checkProg :: NFM ()
checkProg = do md <- askDefn (s2n "main")
               case md of
                 Just d  -> checkMainDefn d -- FIXME: main is special
                 Nothing -> throwError "main does not exist"
               return ()

runNFM :: RWCProg -> NFM a -> Either NFMError a
runNFM p phi = fst $ runRW p (runStateT (runErrorT phi) (NFM { seen = empty }))

cmdCheckNF :: TransCommand
cmdCheckNF _ p = (Nothing,Just s)
  where s = show (runNFM p checkProg)