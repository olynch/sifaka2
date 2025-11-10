module Sifaka.Syntax where

import Sifaka.Common
import Sifaka.Value qualified as V

data BinOp = Add | Sub | Mul | Div

data TyClo = TyClo
  { tyCloArg :: Name,
    tyCloDom :: Ty,
    tyCloBody :: Ty
  }

data Ty
  = OfSig Sig
  | TFlex MetaVar
  | Fin Tm
  | Nat
  | Double
  | Record (Row Ty)
  | Pi TyClo
  | Arr Tm Ty

data Literal
  = LitNat Word
  | LitFin Word
  | LitDouble Double

data BD = Bound | Defined

data For = For
  { forVar :: Name,
    forDom :: Tm,
    forCod :: Ty,
    forBody :: Tm
  }

data Clo = Clo
  { cloVar :: Name,
    cloDom :: Ty,
    cloCod :: Ty,
    cloBody :: Tm
  }

data Tm
  = -- Variables
    LocalVar BwdIdx
  | OfDef Def
  | -- Builtins
    Cast Tm Ty
  | Lit Literal
  | BinOp BinOp Tm Tm
  | -- Pi
    Lam Clo
  | App Tm Tm
  | -- Let blocks
    Block [(Name, Tm)] Tm
  | -- Record
    RecordLit (Row Tm)
  | Proj Tm Name
  | -- Arrays
    ArrLit [Tm]
  | ArrLam For
  | Index Tm Ty Tm
  | -- Readback of opaque values
    Opaque

data Def = Def
  { defName :: TopName,
    defTy :: Ty,
    defTyVal :: V.Ty,
    defBody :: Tm,
    defBodyVal :: V.Tm
  }

data Sig = Sig
  { sigName :: TopName,
    sigBody :: Ty,
    sigBodyVal :: V.Ty
  }

data Locals
  = LNil
  | LDef Locals Name Tm Ty
  | LBind Locals Name Ty

data TopDecl = TDef Def | TSig Sig | TTask Task

data Task = TEval Tm Ty

data Namespace = Namespace
  { namespaceSigs :: Bwd Sig,
    namespaceDefs :: Bwd Def,
    namespaceTasks :: Bwd Task
  }

emptyNamespace :: Namespace
emptyNamespace = Namespace BwdNil BwdNil BwdNil

addDef :: Namespace -> Def -> Namespace
addDef m f =
  m {namespaceDefs = namespaceDefs m :> f}

addTask :: Namespace -> Task -> Namespace
addTask m f = m {namespaceTasks = namespaceTasks m :> f}
