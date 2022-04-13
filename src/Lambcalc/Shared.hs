{-# LANGUAGE FlexibleInstances #-}
module Lambcalc.Shared where

import Data.Char (toLower)
import Control.Monad.Trans.State.Strict (State, get, modify')
import Text.Printf (PrintfArg (formatArg), formatString)

type Var = String

class Pretty p where
  pretty :: p -> String

instance Pretty [Char] where
  pretty = id

data Bop = Add | Sub | Mul | Shl | Lshr | Ashr | And | Or | Xor
  deriving Show

instance PrintfArg Bop where
  formatArg = formatString . pretty

instance Pretty Bop where
  pretty bop = case show bop of { (c:cs) -> toLower c:cs; s -> s }

fresh :: String -> State Int String
fresh s = modify' (+ 1) >> (s ++) . show <$> get