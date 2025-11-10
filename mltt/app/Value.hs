module Value where

import Common

data Spine
  = SId
  | SApp Spine Tm

data Tm
  = Flex MetaVar Spine
  | Rigid FwdIdx Spine
  | U
  | Pi Name Ty (Tm -> Ty)
  | Lam Name (Tm -> Tm)

instance Show Tm where
  show _ = "<tm>"

pattern Var :: FwdIdx -> Tm
pattern Var i = Rigid i SId

pattern Meta :: MetaVar -> Tm
pattern Meta m = Flex m SId

type Ty = Tm

type Env = Bwd Tm
