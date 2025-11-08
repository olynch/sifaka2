module Sifaka.Evaluation where

import Sifaka.Common
import Sifaka.Syntax qualified as S
import Sifaka.Value qualified as V

type EnvArg = (?env :: V.Env)

class Eval a b | a -> b where
  eval :: (EnvArg) => a -> b

instance Eval S.Ty V.Ty where
  eval (S.TTopApp _ _) = impossible
  eval (S.TMetaApp _ _) = impossible
  eval (S.TInsertedMeta _ _) = impossible
  eval (S.Fin tm) = V.Fin (eval tm)
  eval S.Nat = V.Nat
  eval S.Double = V.Double
  eval (S.Record _) = impossible
  eval (S.Arr dom cod) = V.Arr (eval dom) (eval cod)

instance Eval S.Tm V.Tm where
  eval _ = impossible

-- quoteTy :: Elab (V.Ty -> S.Ty)
-- quoteTy (V.TFlex _ _) = impossible
-- quoteTy (V.Fin tm) = S.Fin $ quote tm
-- quoteTy V.Nat = S.Nat
-- quoteTy V.Double = S.Double
-- quoteTy (V.Record _) = impossible
-- quoteTy (V.Arr dom cod) = S.Arr (quote dom) (quoteTy cod)

-- quoteSp :: Elab (V.Spine -> Bwd S.Tm)
-- quoteSp V.SId = BwdNil
-- quoteSp (V.SApp sp t) = quoteSp sp :> quote t

-- quoteLit :: Elab (V.Literal -> S.Literal)
-- quoteLit (V.LitNat i) = S.LitNat i

-- quote :: Elab (V.Tm -> S.Tm)
-- quote (V.Var i name) = S.LocalVar (S.Id (fwdToBwd i) name)
-- quote (V.Flex mv sp) = S.MetaApp mv (quoteSp sp)
-- quote (V.Lit lit) = S.Lit (quoteLit lit)
-- quote V.Opaque = S.Opaque
