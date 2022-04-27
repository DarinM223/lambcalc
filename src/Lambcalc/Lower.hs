{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module Lambcalc.Lower where

import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.Foldable (foldlM)
import Data.Functor (($>))
import GHC.Generics (Generic)
import Lambcalc.Hoist (Join, Func)
import Lambcalc.Llvm
import Lambcalc.Shared (fresh)
import Optics (Zoom (zoom))
import Optics.State.Operators ((.=))
import qualified Data.HashMap.Strict as HM
import qualified Lambcalc.Anf as Anf
import qualified Lambcalc.Shared as Anf

data LowerState = LowerState
  { last    :: !Terminator
  , counter :: {-# UNPACK #-} !Int
  } deriving Generic

fty :: Fty
fty = ([I64, I64], I64)

lowerFunc :: Func -> State Int (Gid, Fdecl)
lowerFunc (f, xs, entry, joins) = do
  spills <- preprocess joins
  let allocas = (, Alloca I64) <$> HM.elems spills
  (_, entryBlock) <- lowerBlock spills entry
  let entryBlock' = entryBlock { insns = allocas ++ insns entryBlock }
  labeledBlocks <- traverse (lowerBlock spills) joins
  return (MkGid f, Fdecl fty (Uid <$> xs) (entryBlock', labeledBlocks))

preprocess :: [Join] -> State Int (HM.HashMap String Uid)
preprocess = foldlM go HM.empty
 where
  go spills (j, Just p, _) = HM.insert j <$> (Uid <$> fresh p) <*> pure spills
  go spills _ = pure spills

lowerValue :: Anf.Value -> Operand
lowerValue v = case v of
  Anf.Int n  -> Const n
  Anf.Var s  -> Id $ Uid s
  Anf.Glob s -> Gid $ MkGid s

lowerBop :: Anf.Bop -> Anf.Bop
lowerBop = id

lowerBlock :: HM.HashMap String Uid -> Join -> State Int (Lbl, Block)
lowerBlock spills (j0, p0, e0) = state $ \c ->
  let (instrs, LowerState last' c') =
        flip runState (LowerState (Br "undefined") c) $ case p0 of
          Just p -> let slot = spills HM.! j0
                    in ((Uid p, Load (Ptr I64) (Id slot)) :) <$> go e0
          Nothing -> go e0
  in ((j0, Block instrs ("", last')), c')
 where
  fresh' = fmap Uid . zoom #counter . fresh

  go :: Anf.Exp -> State LowerState [(Uid, Insn)]
  go (Anf.Halt v) = (#last .= Ret I64 (Just (lowerValue v))) $> []
  go (Anf.Jump j Nothing) = (#last .= Br j) $> []
  go (Anf.Jump j (Just v)) = case HM.lookup j spills of
    Just slot -> (#last .= Br j) $> [("", Store I64 (lowerValue v) (Id slot))]
    Nothing   -> error "Join points must have spill slots"
  go (Anf.Bop r op x y e) = ((Uid r, Binop op' I64 x' y') :) <$> go e
   where
    op' = lowerBop op
    (x', y') = (lowerValue x, lowerValue y)
  go (Anf.If v (Anf.Jump t Nothing) (Anf.Jump f Nothing)) = do
    cmp <- fresh' "c"
    #last .= Cbr (Id cmp) t f
    return [(cmp, Icmp Sgt I64 (lowerValue v) (Const 0))]
  go (Anf.App r ptr vs e) = do
    let arg op@(Const _) = (I64, op)
        arg op@(Id _)    = (I64, op)
        arg _            = error "Invalid argument"
    let vs' = fmap (arg . lowerValue) vs
    casted <- fresh' "c"
    let cast = (casted, IntToPtr I64 (Id (Uid ptr)) (Ptr (Fun fty)))
        call = (Uid r, Call I64 (Id casted) vs')
    (cast :) . (call :) <$> go e
  go (Anf.Tuple r vs e) = do
    ptr <- fresh' "ptr"
    let size  = length vs * 8
        alloc = (ptr, Call (Ptr I64) (Gid "malloc") [(I64, Const size)])
    let populate i op = do
          off <- fresh' "off"
          let gep     = (off, Gep (Ptr I64) (Id ptr) [Const i])
              store x = ("", Store I64 (Id x) (Id off))
          case op of
            Gid g -> do
              casted <- fresh' "casted"
              let cast = (casted, PtrToInt (Ptr (Fun fty)) (Gid g) I64)
              return [gep, cast, store casted]
            Id x -> return [gep, store x]
            _ -> error "Constants or null cannot reach here"
    stores <- traverse (uncurry populate . fmap lowerValue) $ zip [0..] vs
    let cast = (Uid r, PtrToInt (Ptr I64) (Id ptr) I64)
    (alloc :) . (cast :) . (mconcat stores ++) <$> go e
  go (Anf.Proj r x i e) = do
    (addr, tuple) <- (,) <$> fresh' "a" <*> fresh' "t"
    let is = [ (tuple, IntToPtr I64 (Id (Uid x)) (Ptr I64))
             , (addr, Gep (Ptr I64) (Id tuple) [Const i])
             , (Uid r, Load (Ptr I64) (Id addr))
             ]
    (is ++) <$> go e
  go Anf.Fun{}  = error "Code must be straightline"
  go Anf.Join{} = error "Code must be straightline"
  go Anf.If{}   = error "Ifs must immediately branch"