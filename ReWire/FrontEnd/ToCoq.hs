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
import ReWire.FrontEnd.Syntax
import ReWire.FrontEnd.Records (freshVar, freshVars, replaceAtIndex, poly2Ty)
import Control.Monad.IO.Class
import Debug.Trace

toCoq :: (MonadError AstError m, MonadIO m) => FreeProgram -> m FreeProgram
toCoq fp = do
      handle <- liftIO $ openFile "some.coq" WriteMode
      let p = Program $ trec fp
      liftIO $ hPutStrLn handle $ prettyPrint p
  --    hClose handle
      return fp

data CoqAst = CoqAst deriving Show

--trans2coq :: FreeProgram -> CoqAst
--trans2coq _ = CoqAst
