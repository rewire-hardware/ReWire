module ReWire.Core.Transformations.CheckNF where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.Monad
import Unbound.LocallyNameless
import Data.Set
import Control.Monad.State
import Control.Monad.Error

type NFM = ErrorT NFMError (StateT NFMState RW)
data NFMState = NFM { seen :: Set (Name RWCExp) }
type NFMError = String

getSeen = get >>= return . seen
modifySeen f = modify (\ s -> s { seen = f (seen s) })

checkMachineBody :: RWCExp -> NFM ()
checkMachineBody _ = return ()

checkMachineResultTy :: RWCTy -> NFM ()
checkMachineResultTy _ = return () -- FIXME

checkArgTy :: RWCTy -> NFM ()
checkArgTy _ = return () -- FIXME

checkMachineTy :: RWCTy -> NFM ()
checkMachineTy t = mapM_ checkArgTy targs >> checkMachineResultTy tres
  where (targs,tres) = flattenArrow t

checkMachineDefn :: RWCDefn -> NFM ()
checkMachineDefn (RWCDefn n (Embed b)) = do ip <- getSeen
                                            if n `member` ip then return ()
                                              else do modifySeen (insert n)
                                                      lunbind b (\(tvs,(t,e)) ->
                                                       do checkMachineTy t
                                                          checkMachineBody e)

checkProg :: NFM ()
checkProg = do md <- askDefn (s2n "main")
               case md of
                 Just d  -> checkMachineDefn d
                 Nothing -> throwError "main does not exist"
               return ()