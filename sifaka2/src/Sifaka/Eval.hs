module Sifaka.Eval where

import Sifaka.Common
import Sifaka.Value qualified as V
import Sifaka.Syntax qualified as S

type Env = Bwd V.Tm

type EnvArg = (?env :: Env)

-- evalTy :: EnvArg => S.Ty -> V.Ty
-- evalTy (S.Fin tm) = V.Fin (eval tm)
-- evalTy S.Nat = V.Nat
-- evalTy S.Double = V.Double
-- evalTy (S.Record fields) = V.Record (evalTy <$> fields)
-- evalTy (S.Arr dom cod) = V.Arr (eval dom) (evalTy cod)

-- eval :: EnvArg => S.Tm -> V.Tm
-- eval (S.LocalVar (S.Id i _)) = elemAt ?env i
-- eval (S.Lit (S.LitFin i)) = V.Lit (V.LitFin i)
-- eval _ = V.Opaque
