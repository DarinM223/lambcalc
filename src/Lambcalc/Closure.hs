module Lambcalc.Closure where

import Control.Monad.Trans.State.Strict (State)
import Data.Foldable (foldl')
import Lambcalc.Anf (Exp (..), Value (..))
import Lambcalc.Shared (fresh)
import qualified Data.HashSet as HS

freeVars :: Exp -> HS.HashSet String
freeVars = go HS.empty
 where
  free env (Var v) | not (HS.member v env) = HS.singleton v
  free _ _ = HS.empty

  go env (Halt v) = free env v
  go env (Fun f xs e e') = HS.union
    (go (HS.union env (HS.fromList xs)) e)
    (go (HS.insert f env) e')
  go env (Join _ p e e') = HS.union (go (foldr HS.insert env p) e) (go env e')
  go env (Jump _ v) = maybe HS.empty (free env) v
  go env (App r f xs e') = HS.unions
    (go (HS.insert r env) e':fmap (free env) (Var f:xs))
  go env (Bop r _ x y e') = HS.unions
    [free env x, free env y, go (HS.insert r env) e']
  go env (If v t f) = HS.unions [free env v, go env t, go env f]
  go env (Tuple r xs e') = HS.unions
    (go (HS.insert r env) e':fmap (free env) xs)
  go env (Proj r x _ e') = HS.union (free env (Var x)) (go (HS.insert r env) e')

convert :: Exp -> State Int Exp
convert (Fun f xs e rest) = do
  env <- fresh "env"
  let fvs = HS.toList $ HS.difference (freeVars e) (HS.fromList xs)
      proj (x, i) = Proj x env i
  e' <- foldl' (flip proj) <$> convert e <*> pure (zip fvs [1..])
  rest' <- Tuple f (Glob f:fmap Var fvs) <$> convert rest
  return $ Fun f (env:xs) e' rest'
convert (App r f vs e) = do
  ptr <- fresh f
  Proj ptr f 0 . App r ptr (Var f:vs) <$> convert e
convert (Halt v) = pure $ Halt v
convert (Join j p e e') = Join j p <$> convert e <*> convert e'
convert (Jump j p) = pure $ Jump j p
convert (Bop r bop x y e') = Bop r bop x y <$> convert e'
convert (If v t f) = If v <$> convert t <*> convert f
convert (Tuple r vs e') = Tuple r vs <$> convert e'
convert (Proj r x i e') = Proj r x i <$> convert e'