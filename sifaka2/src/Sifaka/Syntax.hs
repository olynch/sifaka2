module Sifaka.Syntax (Id(..), BinOp(..), Tm(..), Ty(..), Literal(..), MetaSub(..), TopDecl(..), Def(..)) where

import Sifaka.Common

data Id a = Id BwdIdx Name

data BinOp = Add | Sub | Mul | Div

data Ty
  = TMeta MetaVar MetaSub
  | Fin Tm
  | Int
  | Double
  | Record (Row Ty)
  | Arr Tm Ty

data Literal
  = LitInt Int
  | LitFin Int
  | LitDouble Double

data MetaSub = MSId

data Tm
  = LocalVar (Id Tm)
  | Meta MetaVar MetaSub
  | Lit Literal
  | BinOp BinOp Tm Tm
  | Block [(Name, Tm)] Tm
  | RecordCon (Row Tm)
  | Proj Tm Name
  | ArrCon [Tm]
  | ArrLam Name Tm
  | Index [Tm] Tm

data Def = Def [(Name, Ty)] Ty Tm

data TopDecl = TDef Def
