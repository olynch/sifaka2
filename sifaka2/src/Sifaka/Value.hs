module Sifaka.Value (Spine (..), Literal (..), Tm (..), Ty (..)) where

import Sifaka.Common

data Spine
  = SId
  | SApp Spine Tm

newtype Literal = LitNat Word

data Tm
  = Var FwdIdx Name
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
