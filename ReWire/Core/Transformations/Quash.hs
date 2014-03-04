module ReWire.Core.Transformations.Quash where

import ReWire.Core.Syntax
import ReWire.Core.Transformations.Types
import ReWire.Core.Transformations.CBN
import Control.Monad.Reader
import Unbound.LocallyNameless

quashdefn :: [Name RWCExp] -> RWCDefn -> M RWCDefn
quashdefn ns (RWCDefn n (Embed b)) | not $ n `elem` ns = lunbind b (\(tvs,(t,e)) ->
                                                          do e' <- quashexpr ns e
                                                             return (RWCDefn n (Embed (setbind tvs (t,e')))))
                                   | otherwise         = return (RWCDefn n (Embed b))

quashexpr :: [Name RWCExp] -> RWCExp -> M RWCExp
quashexpr ns e_@(RWCApp t e1_ e2_) = let (e:_) = flattenApp e_
                                     in case e of
                                          RWCVar _ n | n `elem` ns -> evalexpr e_
                                          _                        -> do e1 <- quashexpr ns e1_
                                                                         e2 <- quashexpr ns e2_
                                                                         return (RWCApp t e1 e2)
quashexpr ns (RWCLam t b)          = lunbind b (\(n,e) ->
                                      do e' <- quashexpr ns e
                                         return (RWCLam t (bind n e')))
quashexpr ns e@(RWCVar t n) | n `elem` ns = evalexpr e
                            | otherwise   = return e
quashexpr ns e@(RWCCon {})         = return e
quashexpr ns e@(RWCLiteral {})     = return e
quashexpr ns (RWCCase t e_ alts_)  = do e    <- quashexpr ns e_
                                        alts <- mapM (quashalt ns) alts_
                                        return (RWCCase t e alts)

quashalt :: [Name RWCExp] -> RWCAlt -> M RWCAlt
quashalt ns (RWCAlt b) = lunbind b (\(p,eb) ->
                          do eb' <- quashexpr ns eb
                             return (RWCAlt (bind p eb')))

quash :: [Name RWCExp] -> RWCProg -> M RWCProg
quash ns p = do ds  <- luntrec (defns p)
                ds' <- avoid (map defnName ds) (local (const ds) (mapM (quashdefn ns) ds))
                return (p { defns = trec ds' })
   where defnName (RWCDefn n _) = AnyName n

cmdQuash :: TransCommand
cmdQuash s p = let ns = map s2n (words s)
                   p' = runM (quash ns p)
               in (Just p',Nothing)