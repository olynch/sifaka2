module Sifaka.Value where

import Data.Foldable (foldr')
import Data.Map.Ordered.Strict (OMap, (>|))
import Data.Map.Ordered.Strict qualified as OMap
import Sifaka.Common

newtype Literal = LitNat Word
  deriving (Eq)

data TyClo = TyClo
  { tyCloArg :: Name,
    tyCloDom :: Ty,
    tyCloBody :: Tm -> Ty
  }

data Clo = Clo
  { cloArg :: Name,
    cloBody :: Tm -> Tm
  }

data Spine
  = SId
  | SApp Spine Tm
  | SProj Spine Name

data Tm
  = Rigid FwdIdx Spine
  | Flex MetaVar Spine
  | Lit Literal
  | Lam Clo
  | RecordLit (OMap Name Tm)
  | Opaque

data Tele = Tele (OMap Name (OMap Name Tm -> Ty))

teleFromList :: [(Name, OMap Name Tm -> Ty)] -> Tele
teleFromList = Tele . foldr' (\(x, f) m -> m >| (x, f)) OMap.empty

data Ty
  = TFlex MetaVar Spine
  | Fin Tm
  | Nat
  | Double
  | Pi TyClo
  | Record Tele
  | Arr Tm Ty

type Env = Bwd Tm
