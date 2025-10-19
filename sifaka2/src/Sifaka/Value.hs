module Sifaka.Value (Spine(..), Literal(..), Tm(..), Ty(..)) where

import Sifaka.Common

data Spine = SId

newtype Literal = LitFin Int

data Tm
  = Neu FwdIdx Spine
  | Lit Literal
  | Opaque

data Ty
  = Fin Tm
  | Int
  | Double
  | Record (Row Ty)
  | Arr Tm Ty
