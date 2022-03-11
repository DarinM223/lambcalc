module Lambcalc.Shared where

import Control.Monad.Trans.State.Strict (State, get, modify')

type Var = String

data Bop

fresh :: String -> State Int String
fresh s = modify' (+ 1) >> (s ++) . show <$> get