{-# LANGUAGE FlexibleInstances #-}
module Lambcalc.Llvm where

import Data.Char (toLower)
import Data.List (intersperse)
import Lambcalc.Shared (Pretty (pretty))
import Text.Printf

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

instance PrintfArg Ty where
  formatArg = formatString . pretty

instance Pretty Ty where
  pretty Void = "void"
  pretty I1 = "i1"
  pretty I8 = "i8"
  pretty I64 = "i64"
  pretty (Ptr ty) = pretty ty ++ "*"
  pretty (Struct ts) = printf "{ %s }" (mconcat (pretty <$> ts))
  pretty (Array n t) = printf "[%d x %s]" n t
  pretty (Fun (ts, t)) =
    printf "%s (%s)" t (mconcat (intersperse ", " (pretty <$> ts)))
  pretty (Namedt s) = "%%" ++ s

type Fty = ([Ty], Ty)

data Operand
  = Null
  | Const Int
  | Gid Gid
  | Id Uid

instance PrintfArg Operand where
  formatArg = formatString . pretty

instance Pretty Operand where
  pretty Null = "null"
  pretty (Const i) = show i
  pretty (Gid g) = "@" ++ g
  pretty (Id u) = "%" ++ u

instance Pretty (Ty, Operand) where
  pretty (t, o) = printf "%s %s" t o

data Bop = Add | Sub | Mul | Shl | Lshr | Ashr | And | Or | Xor
  deriving Show

instance PrintfArg Bop where
  formatArg = formatString . pretty

instance Pretty Bop where
  pretty bop = case show bop of { (c:cs) -> toLower c:cs; s -> s }

data Cnd = Eq | Ne | Slt | Sle | Sgt | Sge
  deriving Show

instance PrintfArg Cnd where
  formatArg = formatString . pretty

instance Pretty Cnd where
  pretty cnd = case show cnd of { (c:cs) -> toLower c:cs; s -> s }

data Insn
  = Binop Bop Ty Operand Operand
  | Alloca Ty
  | Load Ty Operand
  | Store Ty Operand Operand
  | Icmp Cnd Ty Operand Operand
  | Call Ty Operand [(Ty, Operand)]
  | Bitcast Ty Operand Ty
  | Gep Ty Operand [Operand] -- getElementPtr
  | PtrToInt Ty Operand Ty
  | IntToPtr Ty Operand Ty

instance PrintfArg Insn where
  formatArg = formatString . pretty

instance Pretty Insn where
  pretty (Binop bop t x y) = printf "%s %s %s, %s" bop t x y
  pretty (Alloca t) = printf "alloca %s" t
  pretty (Load pt@(Ptr t) o) = printf "load %s, %s %s" t pt o
  pretty (Load _ _) = error "Load: expected pointer type"
  pretty (Store t src des) = printf "store %s %s, %s %s" t src (Ptr t) des
  pretty (Icmp c t x y) = printf "icmp %s %s %s, %s" c t x y
  pretty (Call t f args) = printf "call %s %s(%s)" t f prettyArgs
   where prettyArgs = mconcat $ intersperse ", " $ fmap pretty args
  pretty (Bitcast t1 o t2) = printf "bitcast %s %s to %s" t1 o t2
  pretty (Gep pt@(Ptr t) o is) = printf "getelementptr %s, %s, %s" pt t o pis
   where pis = mconcat $ intersperse ", " $ fmap prettyGepIndex is
  pretty Gep{} = error "Gep: expected pointer type"
  pretty (PtrToInt t1 o t2) = printf "ptrtoint %s %s to %s" t1 o t2
  pretty (IntToPtr t1 o t2) = printf "inttoptr %s %s to %s" t1 o t2

prettyGepIndex :: Operand -> String
prettyGepIndex (Const i) = "i32 " ++ show i
prettyGepIndex o         = "i64 " ++ pretty o

instance Pretty (Uid, Insn) where
  pretty (u, i) = case i of
    Store{}       -> pretty i
    Call Void _ _ -> pretty i
    _             -> printf "%%%s = %s" u i

data Terminator
  = Ret Ty (Maybe Operand)
  | Br Lbl
  | Cbr Operand Lbl Lbl

instance Pretty Terminator where
  pretty (Ret _ Nothing) = "ret void"
  pretty (Ret t (Just o)) = printf "ret %s %s" t o
  pretty (Br l) = printf "br label %%%s" l
  pretty (Cbr o l1 l2) = printf "br i1 %s, label %%%s, label %%%s" o l1 l2

data Block = Block
  { insns      :: [(Uid, Insn)]
  , terminator :: (Uid, Terminator)
  }

instance Pretty Block where
  pretty (Block is (_, term)) = pis ++ newline ++ pad ++ pretty term
   where
    pad = "  "
    pis = mconcat $ intersperse "\n" $ fmap ((pad ++) . pretty) is
    newline = if null is then "" else "\n"

instance Pretty (Lbl, Block) where
  pretty (l, b) = l ++ ":\n" ++ pretty b

type Cfg = (Block, [(Lbl, Block)])

instance Pretty Cfg where
  pretty (e, bs) =
    pretty e ++ "\n" ++ mconcat (intersperse "\n" (pretty <$> bs))

data Fdecl = Fdecl
  { fdeclFty   :: Fty
  , fdeclParam :: [Uid]
  , fdeclCfg   :: Cfg
  }

instance Pretty (Gid, Fdecl) where
  pretty (g, Fdecl (ts, t) param cfg) =
    printf "define %s @%s(%s) {\n%s\n}\n" t g pargs (pretty cfg)
   where
    prettyArg (t', u) = printf "%s %%%s" t' u
    pargs = mconcat $ intersperse ", " $ prettyArg <$> zip ts param