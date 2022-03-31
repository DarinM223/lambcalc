{-# LANGUAGE FlexibleInstances #-}
module Lambcalc.Shared where

import Control.Monad.Trans.State.Strict (State, get, modify')

type Var = String

class Pretty p where
  pretty :: p -> String

instance Pretty [Char] where
  pretty = id

data Bop

fresh :: String -> State Int String
fresh s = modify' (+ 1) >> (s ++) . show <$> get