module Lambcalc.LL where

type Uid = String
type Gid = String
type Tid = String
type Lbl = String

data Ty
  = Void
  | I1
  | I8
  | I64
  | Ptr Ty
  | Struct [Ty]
  | Array Int Ty
  | Fun Fty
  | Namedt Tid

type Fty = ([Ty], Ty)

data Operand
  = Null
  | Const Int
  | Gid Gid
  | Id Uid

data Bop = Add | Sub | Mul | Shl | Lshr | Ashr | And | Or | Xor

data Cnd = Eq | Ne | Slt | Sle | Sgt | Sge

data Insn
  = Binop Bop Ty Operand Operand
  | Alloca Ty
  | Load Ty Operand
  | Store Ty Operand Operand
  | Icmp Cnd Ty Operand Operand
  | Call Ty Operand [(Ty, Operand)]
  | Bitcast Ty Operand Ty
  | Gep Ty Operand [Operand]

data Terminator
  = Ret Ty (Maybe Operand)
  | Br Lbl
  | Cbr Operand Lbl Lbl

data Block = Block
  { insns      :: [(Uid, Insn)]
  , terminator :: (Uid, Terminator)
  }

class Pretty p where
  pretty :: p -> String