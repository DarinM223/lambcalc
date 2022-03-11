module Lambcalc.Hoist where

import Control.Monad.Trans.State.Strict (State)
import Data.Monoid (Endo (Endo, appEndo))
import Lambcalc.Anf (Exp (..), Value (Int))
import Lambcalc.Shared (Var, fresh)

type DList a = Endo [a]
type Join = (Var, Maybe Var, Exp)
type Func = (Var, [Var], Join, [Join])

hoist :: Exp -> State Int [Func]
hoist e0 = do
  (fs, _, _) <- go (Fun "main" [] e0 (Halt (Int 0)))
  return $ appEndo fs []
 where
  go :: Exp -> State Int (DList Func, DList Join, Exp)
  go (Fun f xs e rest) = do
    (fs, js, e') <- go e
    (fs', js', rest') <- go rest
    entry <- fresh "entry"
    let fn = (f, xs, (entry, Nothing, e'), appEndo js [])
    return (fs <> fs' <> Endo (fn :), js', rest')
  go (Join j p e rest) = do
    (fs, js, e') <- go e
    (fs', js', rest') <- go rest
    let jn = (j, p, e')
    return (fs <> fs', Endo (jn :) <> js <> js', rest')
  go (If e t f) = do
    (fs, js, t') <- go t
    (fs', js', f') <- go f
    (th, el) <- (,) <$> fresh "then" <*> fresh "else"
    let (bt, bf) = ((th, Nothing, t'), (el, Nothing, f'))
        e' = If e (Jump th Nothing) (Jump el Nothing)
    return (fs <> fs', Endo (bt :) <> Endo (bf :) <> js <> js', e')
  go (Halt v) = pure (mempty, mempty, Halt v)
  go (Jump j p) = pure (mempty, mempty, Jump j p)
  go (App r f xs e) = fmap (App r f xs) <$> go e
  go (Bop r bop x y e) = fmap (Bop r bop x y) <$> go e
  go (Tuple r xs e) = fmap (Tuple r xs) <$> go e
  go (Proj r x i e) = fmap (Proj r x i) <$> go e