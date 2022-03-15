{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
module Lambcalc.Lower where

import Control.Monad.Trans.State.Strict (State)
import Data.Foldable (foldlM)
import GHC.Generics (Generic)
import Lambcalc.Hoist
import Lambcalc.LL (Insn)
import Lambcalc.Shared (fresh, Var)
import Optics
import qualified Data.HashMap.Strict as HM
import Lambcalc.Anf (Exp (Halt))

data LowerState = LowerState
  { last    :: !Insn
  , counter :: {-# UNPACK #-} !Int
  } deriving Generic

preprocess :: [Join] -> State Int (HM.HashMap String String)
preprocess = foldlM go HM.empty
 where
  go spills (j, Just p, _) = HM.insert j <$> fresh p <*> pure spills
  go spills _ = pure spills

-- lowerBlock :: HM.HashMap String String -> Join -> State Int (Var, Insn)
-- lowerBlock spills = undefined
--  where
--   go :: Exp -> State LowerState [(Var, Insn)]
--   go (Halt v) = #last .= Ret