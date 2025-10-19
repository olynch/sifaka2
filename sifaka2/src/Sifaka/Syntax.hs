module Sifaka.Syntax (Id(..), BinOp(..), Tm(..), Ty(..), Literal(..)) where

import Sifaka.Common

data Id a = Id BwdIdx Name

data BinOp = Add | Sub | Mul | Div

data Ty
  = Fin Tm
  | Int
  | Double
  | Record (Row Ty)
  | Arr Tm Ty

data Literal
  = LitInt Int
  | LitFin Int
  | LitDouble Double

data Tm
  = LocalVar (Id Tm)
  | Lit Literal
  | BinOp BinOp Tm Tm
  | RecordCon (Row Tm)
  | Proj Tm Name
  | ArrCon [Tm]
  | ArrLam Name Tm
  | Index [Tm] Tm
