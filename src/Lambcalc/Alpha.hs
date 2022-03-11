module Lambcalc.Alpha where

import Control.Monad.Trans.State.Strict (State)
import Lambcalc.Lam (Exp (..))
import Lambcalc.Shared (fresh)
import qualified Data.HashMap.Strict as HM

rename :: Exp -> State Int Exp
rename = go HM.empty
 where
  go env (Var x) = case HM.lookup x env of
    Just x' -> pure $ Var x'
    Nothing -> error $ x ++ " not in scope"
  go env (Lam x t) = do
    x' <- fresh x
    Lam x' <$> go (HM.insert x x' env) t
  go env (App f x) = App <$> go env f <*> go env x
  go env (Bop op l r) = Bop op <$> go env l <*> go env r
  go env (If e t f) = If <$> go env e <*> go env t <*> go env f
  go _ (Int i) = pure $ Int i