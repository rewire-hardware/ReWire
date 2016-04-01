{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, LambdaCase #-}

module ReWire.FrontEnd.Kinds (Kind (..)) where

import ReWire.Pretty
import ReWire.Scoping

import Control.DeepSeq (NFData (..), deepseq)
import Data.ByteString.Char8 (pack)
import Data.Data (Typeable, Data)
import Text.PrettyPrint (text, parens, (<+>))

data Kind = Kvar (Id Kind) | Kstar | Kfun Kind Kind | Kmonad
      deriving (Ord, Eq, Show, Typeable, Data)

infixr `Kfun`

instance IdSort Kind where
      idSort _ = pack "K"

instance Alpha Kind where
      aeq' (Kvar i) (Kvar j)           = return $ i == j
      aeq' Kstar Kstar                 = return True
      aeq' (Kfun k1 k2) (Kfun k1' k2') = (&&) <$> aeq' k1 k1' <*> aeq' k2 k2'
      aeq' Kmonad Kmonad               = return True
      aeq' _ _                         = return False

instance Subst Kind Kind where
      fv = \ case
            Kvar i     -> [i]
            Kstar      -> []
            Kfun k1 k2 -> fv k1 ++ fv k2
            Kmonad     -> []
      bv _            = []
      subst' = \ case
            Kvar i     -> query i >>= return . \ case
                  Just (Left j)   -> Kvar j
                  Just (Right k') -> k'
                  Nothing         -> Kvar i
            Kstar      -> return Kstar
            Kfun k1 k2 -> Kfun <$> subst' k1 <*> subst' k2
            Kmonad     -> return Kmonad

instance NFData Kind where
      rnf = \ case
            Kvar i     -> i `deepseq` ()
            Kstar      -> ()
            Kfun k1 k2 -> k1 `deepseq` k2 `deepseq` ()
            Kmonad     -> ()

instance Pretty Kind where
      pretty = \ case
            Kvar x   -> pretty x
            Kstar    -> text "*"
            Kfun a b -> parens $ pretty a <+> text "->" <+> pretty b
            Kmonad   -> text "'nad"
