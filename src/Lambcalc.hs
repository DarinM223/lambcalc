{-# LANGUAGE OverloadedStrings #-}
module Lambcalc (compile, parse) where

import Control.Monad.Trans.State.Strict (evalState)
import Lambcalc.Lam (Exp)
import Lambcalc.Llvm (Prog (Prog), Ty (Fun, I64, Ptr))
import Lambcalc.Parser (parseLam)
import Lambcalc.Shared (Pretty (pretty))
import qualified Lambcalc.Alpha as Alpha
import qualified Lambcalc.Anf as Anf
import qualified Lambcalc.Closure as Closure
import qualified Lambcalc.Hoist as Hoist
import qualified Lambcalc.Lower as Lower

compile :: Exp -> String
compile e = flip evalState (-1) $ do
  e' <- Alpha.rename e
  anf <- Anf.convert e'
  anf' <- Closure.convert anf
  fns <- Hoist.hoist anf'
  loweredFns <- traverse Lower.lowerFunc fns
  let prog = Prog [] [] loweredFns [("malloc", Fun ([I64], Ptr I64))]
  return $ pretty prog

parse :: String -> Exp
parse = parseLam