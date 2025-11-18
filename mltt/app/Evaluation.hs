module Evaluation where

import Common
import Syntax qualified as S
import Unification
import Value qualified as V

type EnvArg = (?env :: V.Env)

class Eval a b | a -> b where
  eval :: (EnvArg) => (MetaCtxArg) => a -> b

evalIn :: (MetaCtxArg) => (Eval a b) => V.Env -> a -> b
evalIn e = let ?env = e in eval

app :: V.Tm -> V.Tm -> V.Tm
app (V.Lam _ f) ~x = f x
app (V.Flex mv sp) ~x = V.Flex mv (V.SApp sp x)
app (V.Rigid mv sp) ~x = V.Rigid mv (V.SApp sp x)
app _ _ = impossible

appSp :: V.Tm -> V.Spine -> V.Tm
appSp f V.SId = f
appSp f (V.SApp sp x) = appSp f sp `app` x

instance Eval MetaVar V.Tm where
  eval m = case lookupMeta m of
    Solved v -> v
    Unsolved -> V.Flex m V.SId

instance Eval S.Tm V.Tm where
  eval (S.LocalVar i) = elemAt ?env i
  eval S.U = V.U
  eval (S.Pi arg dom cod) = V.Pi arg (eval dom) (\v -> evalIn (?env :> v) cod)
  eval (S.Lam arg body) = V.Lam arg (\v -> evalIn (?env :> v) body)
  eval (S.App f x) = app (eval f) (eval x)
  eval (S.Block bindings body) = go bindings
    where
      go [] = eval body
      go ((_, tm) : rest) = let ?env = (?env :> eval tm) in go rest
  eval (S.Meta mv) = eval mv

class Readback a b | a -> b where
  readb :: MetaCtxArg => LenArg => a -> b

instance Readback FwdIdx BwdIdx where
  readb (FwdIdx i) = let (FwdIdx l) = ?len in BwdIdx (l - i - 1)

readbWith :: (Readback a b, MetaCtxArg) => FwdIdx -> a -> b
readbWith l = let ?len = l in readb

instance Readback V.Spine (S.Tm -> S.Tm) where
  readb V.SId t = t
  readb (V.SApp sp x) t = S.App (readb sp t) (readb x)

defunctionalize :: MetaCtxArg => LenArg => (V.Tm -> V.Tm) -> S.Tm
defunctionalize f =
  let i = ?len in let ?len = ?len + 1 in readb (f (V.Rigid i V.SId))

force :: (MetaCtxArg) => V.Tm -> V.Tm
force = \case
  V.Flex mv sp | Solved t <- lookupMeta mv -> force (appSp t sp)
  t -> t

instance Readback V.Tm S.Tm where
  readb v = case force v of
    V.Flex mv sp -> readb sp (S.Meta mv)
    V.Rigid i sp -> readb sp (S.LocalVar (readb i))
    V.U -> S.U
    V.Pi arg dom cod -> S.Pi arg (readb dom) (defunctionalize cod)
    V.Lam arg f -> S.Lam arg (defunctionalize f)
