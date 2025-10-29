module Sifaka.Syntax (Id (..), BinOp (..), Tm (..), Ty (..), Literal (..), MetaSub (..), TopDecl (..), Func (..), Eval (..), Module (..), emptyModule, addFunc, addEval) where

import Sifaka.Common

data Id a = Id BwdIdx Name

data BinOp = Add | Sub | Mul | Div

data Ty
  = TMeta MetaVar MetaSub
  | Fin Tm
  | Nat
  | Double
  | Record (Row Ty)
  | Arr Tm Ty

data Literal
  = LitNat Word
  | LitFin Word
  | LitDouble Double

data MetaSub = MSId

data Tm
  = LocalVar (Id Tm)
  | TopApp (Id Func) [Tm]
  | Meta MetaVar MetaSub
  | Lit Literal
  | BinOp BinOp Tm Tm
  | Block [(Name, Tm)] Tm
  | RecordCon (Row Tm)
  | Proj Tm Name
  | ArrCon [Tm]
  | ArrLam Name Tm
  | Index [Tm] Tm

data Func = Func
  { funcName :: Name,
    funcArgs :: [(Name, Ty)],
    funcRetTy :: Ty,
    funcBody :: Tm
  }

data Eval = Eval Tm Ty

data TopDecl = TFunc Func | TEval Eval

data Module = Module
  { moduleFuncs :: Bwd Func,
    moduleEvals :: Bwd Eval
  }

emptyModule :: Module
emptyModule = Module BwdNil BwdNil

addFunc :: Module -> Func -> Module
addFunc m f = m {moduleFuncs = Snoc (moduleFuncs m) f}

addEval :: Module -> Eval -> Module
addEval m f = m {moduleEvals = Snoc (moduleEvals m) f}
