module Sifaka.Value (Spine(..), Literal(..), Tm(..), Ty(..)) where

import Sifaka.Common

data Spine = SId
           | SApp Spine Tm

newtype Literal = LitFin Int

data Tm
  = Rigid FwdIdx Spine
  | Flex MetaVar Spine
  | Lit Literal
  | Opaque

data Ty
  = Fin Tm
  | TFlex MetaVar Spine
  | Int
  | Double
  | Record (Row Ty)
  | Arr Tm Ty
