module Syntax where

import Common

data Tm
  = LocalVar BwdIdx
  | U
  | Pi Name Ty Ty
  | Lam Name Tm
  | App Tm Tm
  | Block [(Name, Tm)] Tm
  | Meta MetaVar

type Ty = Tm

data Locals
  = LNil
  | LDef Locals Name Tm
  | LBind Locals Name

instance Show Locals where
  show _ = "<locals>"

type LocalsArg = (?locals :: Locals)

data Def = Def Name Ty Tm
