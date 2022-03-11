module Lambcalc.Anf where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT (ContT, runContT))
import Control.Monad.Trans.State.Strict (State)
import Lambcalc.Shared (Bop, Var, fresh)
import qualified Lambcalc.Lam as L

data Value
  = Int Int
  | Var Var
  | Glob Var

data Exp
  = Halt Value
  | Fun Var [Var] Exp Exp
  | Join Var (Maybe Var) Exp Exp
  | Jump Var (Maybe Value)
  | App Var Var [Value] Exp
  | Bop Var Bop Value Value Exp
  | If Value Exp Exp
  | Tuple Var [Value] Exp
  | Proj Var Var Int Exp

convert :: L.Exp -> State Int Exp
convert e0 = runContT (go e0) (pure . Halt)
 where
  go (L.Int i) = pure $ Int i
  go (L.Var x) = pure $ Var x
  go (L.Lam x t) = ContT $ \k -> do
    f <- fresh "f"
    t' <- runContT (go t) (pure . Halt)
    Fun f [x] t' <$> k (Var f)
  go (L.App f x) = do
    f' <- go f
    x' <- go x
    case f' of
      Var f'' -> ContT $ \k -> do
        r <- fresh "r"
        App r f'' [x'] <$> k (Var r)
      _ -> error "Must apply named value"
  go (L.Bop op x y) = do
    x' <- go x
    y' <- go y
    r <- lift $ fresh "r"
    ContT $ \k -> Bop r op x' y' <$> k (Var r)
  go (L.If e t f) = do
    e' <- go e
    (j, p) <- lift $ (,) <$> fresh "j" <*> fresh "p"
    let join = pure . Jump j . Just
    ContT $ \k -> Join j (Just p)
      <$> k (Var p)
      <*> (If e' <$> runContT (go t) join <*> runContT (go f) join)