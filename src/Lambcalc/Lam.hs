module Lambcalc.Lam where

import Lambcalc.Shared (Bop, Var)

data Exp
  = Int Int
  | Var Var
  | Lam Var Exp
  | App Exp Exp
  | Bop Bop Exp Exp
  | If Exp Exp Exp