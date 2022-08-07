module Lambcalc.Llvm where

import Data.Char (toLower)
import Data.List (intersperse)
import Data.String (IsString)
import Lambcalc.Shared (Bop, Pretty (pretty))
import Text.Printf (PrintfArg (formatArg), formatString, printf)

newtype Uid = Uid String deriving (IsString, Pretty, PrintfArg)
newtype Gid = MkGid String deriving (IsString, Pretty, PrintfArg)
newtype Tid = MkTid String deriving (IsString, Pretty, PrintfArg)
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
  pretty (Struct ts) = printf "{ %s }" (mapcat ", " pretty ts)
  pretty (Array n t) = printf "[%d x %s]" n t
  pretty (Fun (ts, t)) = printf "%s (%s)" t (mapcat ", " pretty ts)
  pretty (Namedt s) = "%%" ++ pretty s

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
  pretty (Gid g) = "@" ++ pretty g
  pretty (Id u) = "%" ++ pretty u

instance Pretty (Ty, Operand) where
  pretty (t, o) = printf "%s %s" t o

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
  pretty (Call t f args) = printf "call %s %s(%s)" t f (mapcat ", " pretty args)
  pretty (Bitcast t1 o t2) = printf "bitcast %s %s to %s" t1 o t2
  pretty (Gep pt@(Ptr t) o is) = printf "getelementptr %s, %s %s, %s" t pt o pis
   where pis = mapcat ", " prettyGepIndex is
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
  pretty (Block is (_, term)) =
    (mapcat "\n" ((pad ++) . pretty) is ++. "\n") ++ pad ++ pretty term
   where pad = "  "

instance Pretty (Lbl, Block) where
  pretty (l, b) = l ++ ":\n" ++ pretty b

type Cfg = (Block, [(Lbl, Block)])

instance Pretty Cfg where
  pretty (e, bs) = pretty e ++ "\n" ++ mapcat "\n" pretty bs

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
    pargs = mapcat ", " prettyArg $ zip ts param

data Ginit
  = GNull
  | GGid Gid
  | GInt Int
  | GString String
  | GArray [Gdecl]
  | GStruct [Gdecl]

instance Pretty Ginit where
  pretty GNull = "null"
  pretty (GGid g) = "@" ++ pretty g
  pretty (GInt i) = show i
  pretty (GString s) = printf "c\"%s\\00\"" s
  pretty (GArray gis) = printf "[ %s ]" (mapcat ", " pretty gis)
  pretty (GStruct gis) = printf "{ %s }" (mapcat ", " pretty gis)

type Gdecl = (Ty, Ginit)

instance Pretty Gdecl where
  pretty (t, gi) = printf "%s %s" t (pretty gi)

instance Pretty (Gid, Gdecl) where
  pretty (g, gd) = printf "@%s = global %s" g (pretty gd)

instance Pretty (Tid, Ty) where
  pretty (n, t) = printf "%%%s = type %s" n t

instance Pretty (Gid, Ty) where
  pretty (g, Fun (ts, rt)) = printf "declare %s @%s(%s)" rt g (mapcat ", " pretty ts)
  pretty (g, t) = printf "@%s = external global %s" g t

data Prog = Prog
  { progTdecls :: [(Tid, Ty)]    -- named types
  , progGdecls :: [(Gid, Gdecl)] -- global data
  , progFdecls :: [(Gid, Fdecl)] -- code
  , progEdecls :: [(Gid, Ty)]    -- external declarations
  }

instance Pretty Prog where
  pretty (Prog tdecls gdecls fdecls edecls)
    =  (mapcat "\n" pretty tdecls ++. "\n\n")
    ++ (mapcat "\n" pretty gdecls ++. "\n\n")
    ++ (mapcat "\n" pretty fdecls ++. "\n\n")
    ++  mapcat "\n" pretty edecls

mapcat :: Monoid c => c -> (a -> c) -> [a] -> c
mapcat c f = mconcat . intersperse c . fmap f

(++.) :: [a] -> [a] -> [a]
(++.) a b = if null a then a else a ++ b