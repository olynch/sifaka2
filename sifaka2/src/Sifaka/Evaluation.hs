module Sifaka.Evaluation where

import Sifaka.Common
import Sifaka.Syntax qualified as S
import Sifaka.Value qualified as V
import Data.Map.Ordered.Strict qualified as OMap
import Data.Maybe (fromJust)

type EnvArg = (?env :: V.Env)

type LenArg = (?len :: FwdIdx)

class Eval a b | a -> b where
  eval :: EnvArg => a -> b

evalIn :: (Eval a b) => V.Env -> a -> b
evalIn e = let ?env = e in eval

instance Eval (Row S.Ty) V.Tele where
  eval (Row fields) = V.teleFromList $ go fields []
    where
      extract e _ [] = e
      extract e m (v:vs) = extract e m vs :> fromJust (OMap.lookup v m)
      go [] _ = []
      go ((name, tyS):rest) vars =
        (name, \m -> evalIn (extract ?env m vars) tyS) : go rest (name:vars)

instance Eval S.TyClo V.TyClo where
  eval (S.TyClo arg dom body) =
    V.TyClo arg (eval dom) (\v -> evalIn (?env :> v) body)

instance Eval S.Ty V.Ty where
  eval (S.OfSig s) = S.sigBodyVal s
  eval (S.Fin tm) = V.Fin (eval tm)
  eval S.Nat = V.Nat
  eval S.Double = V.Double
  eval (S.Record fields) = V.Record $ eval fields
  eval (S.Pi clo) = V.Pi $ eval clo
  eval (S.Arr dom cod) = V.Arr (eval dom) (eval cod)

instance Eval S.Literal V.Tm where
  eval (S.LitFin _) = V.Opaque
  eval (S.LitNat n) = V.Lit (V.LitNat n)
  eval (S.LitDouble _) = V.Opaque

instance Eval S.Clo V.Clo where
  eval (S.Clo arg _ _ body) =
    V.Clo arg (\v -> evalIn (?env :> v) body)

app :: V.Tm -> V.Tm -> V.Tm
app V.Opaque _ = V.Opaque
app (V.Flex i sp) v = V.Flex i (V.SApp sp v)
app (V.Lam clo) v = (V.cloBody clo) v
app _ _ = impossible

proj :: V.Tm -> Name -> V.Tm
proj V.Opaque _ = V.Opaque
proj (V.Flex i sp) f = V.Flex i (V.SProj sp f)
proj (V.RecordLit fields) f = fromJust $ OMap.lookup f fields
proj _ _ = impossible

instance Eval S.Tm V.Tm where
  eval (S.LocalVar i) = elemAt ?env i
  eval (S.OfDef d) = S.defBodyVal d
  eval (S.Cast _ _) = V.Opaque
  eval (S.Lit l) = eval l
  eval (S.BinOp _ _ _) = V.Opaque
  eval (S.Lam c) = V.Lam $ eval c
  eval (S.App f x) = app (eval f) (eval x)
  eval (S.Block bindings ret) = go bindings
    where
      go [] = eval ret
      go ((_, tm):rest) = let ?env = ?env :> eval tm in go rest
  eval (S.RecordLit fields) = V.RecordLit $ OMap.fromList $ unRow $ eval <$> fields
  eval (S.Proj r f) = proj (eval r) f
  eval (S.ArrLit _) = V.Opaque
  eval (S.ArrLam _) = V.Opaque
  eval (S.Index _ _ _) = V.Opaque
  eval S.Opaque = V.Opaque

class Readback a b | a -> b where
  readb :: (LenArg) => a -> b

instance Readback FwdIdx BwdIdx where
  readb (FwdIdx i) =
    let (FwdIdx l) = ?len in BwdIdx (l - i - 1)

uf :: a
uf = undefined

instance Readback V.Ty S.Ty where
  readb (V.TFlex mv sp) = go (S.T)
  readb _ = uf

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
