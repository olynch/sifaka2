module Sifaka.Value where

import Sifaka.Common

data Spine
  = SId
  | SApp Spine Tm

newtype Literal = LitNat Word
  deriving (Eq)

data Neutral
  = NVar FwdIdx

data Tm
  = Neu Neutral
  | Flex MetaVar Spine
  | Lit Literal
  | Opaque

data Ty
  = TFlex MetaVar Spine
  | Fin Tm
  | Nat
  | Double
  | Record (Row Ty)
  | Arr Tm Ty

data Env
  = ENil
  | EDef Env Tm
