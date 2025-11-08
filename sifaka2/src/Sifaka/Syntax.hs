module Sifaka.Syntax where

import Data.Map qualified as Map
import Sifaka.Common

data Id a = Id BwdIdx Name

data GlobalId a = GlobalId Name

data BinOp = Add | Sub | Mul | Div

data Ty
  = TTopApp (GlobalId TypeDef) (Bwd Tm)
  | TMetaApp MetaVar (Bwd Tm)
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
  | TopApp (GlobalId Func) (Bwd Tm)
  | InsertedMeta MetaVar (Bwd BD)
  | MetaApp MetaVar (Bwd Tm)
  | Lit Literal
  | IToF Tm
  | BinOp BinOp Tm Tm
  | Block [(Name, Tm)] Tm
  | RecordCon (Row Tm)
  | Proj Tm Name
  | ArrCon [Tm]
  | ArrLam Name {- inferred length -} Tm {- return type-} Ty {- body -} Tm
  | Index Tm Ty Tm
  | Opaque

data Func = Func
  { funcName :: Name,
    funcArgs :: [(Name, Ty)],
    funcRetTy :: Ty,
    funcBody :: Tm
  }

data Eval = Eval Tm Ty

data TypeDef = TypeDef
  { typeDefName :: Name,
    typeDefArgs :: [(Name, Ty)],
    typeDefBody :: Ty
  }

data Locals
  = LNil
  | LDef Locals Name Tm Ty
  | LBind Locals Name Ty

data TopDecl = TFunc Func | TEval Eval | TTypedDef TypeDef

data Module = Module
  { moduleFuncs :: Map Name Func,
    moduleFuncsInOrder :: Bwd Func,
    moduleEvals :: Bwd Eval
  }

emptyModule :: Module
emptyModule = Module (Map.empty) BwdNil BwdNil

addFunc :: Module -> Func -> Module
addFunc m f =
  m
    { moduleFuncs = Map.insert (funcName f) f (moduleFuncs m),
      moduleFuncsInOrder = moduleFuncsInOrder m :> f
    }

addEval :: Module -> Eval -> Module
addEval m f = m {moduleEvals = moduleEvals m :> f}
