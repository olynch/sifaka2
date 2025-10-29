module Sifaka.Syntax (Id (..), BinOp (..), Tm (..), Ty (..), Literal (..), BD (..), TopDecl (..), Func (..), Eval (..), Module (..), emptyModule, addFunc, addEval) where

import Sifaka.Common

data Id a = Id BwdIdx Name

data BinOp = Add | Sub | Mul | Div

data Ty
  = TMetaApp MetaVar (Bwd Tm)
  | TInsertedMeta MetaVar (Bwd BD)
  | Fin Tm
  | Nat
  | Double
  | Record (Row Ty)
  | Arr Tm Ty

data Literal
  = LitNat Word
  | LitFin Word
  | LitDouble Double

data BD = Bound | Defined

data Tm
  = LocalVar (Id Tm)
  | TopApp (Id Func) [Tm]
  | InsertedMeta MetaVar (Bwd BD)
  | MetaApp MetaVar (Bwd Tm)
  | Lit Literal
  | BinOp BinOp Tm Tm
  | Block [(Name, Tm)] Tm
  | RecordCon (Row Tm)
  | Proj Tm Name
  | ArrCon [Tm]
  | ArrLam Name Tm
  | Index Tm Tm
  | Opaque

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
