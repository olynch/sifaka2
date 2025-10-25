module Sifaka.CodeGen where

import Control.Monad.State

import Sifaka.Common
import Sifaka.Qbe
import Sifaka.Syntax qualified as S

data BS = BS
    { bsFresh :: Int
    , bsInstructions :: Bwd Inst
    }

newtype BlockM a = BlockM {runGenM :: State BS a}
    deriving (Functor, Applicative, Monad, MonadState BS)

emit0 :: Inst0 -> BlockM ()
emit0 i = modify (\bs -> bs{bsInstructions = Snoc (bsInstructions bs) (Inst0 i)})

emit1 :: Inst1 -> BaseTy -> BlockM Val
emit1 i ty = state doEmit1
  where
    doEmit1 (BS fresh is) = (VIdent name, BS (fresh + 1) (Snoc is (Inst1 name ty i)))
      where name = LocalName (FreshName fresh)

type Env = Bwd Val

compileLit :: S.Literal -> Const
compileLit (S.LitInt i) = CNum i
compileLit (S.LitFin i) = CNum i
compileLit (S.LitDouble x) = CFloat64 x

opToInst :: S.BinOp -> Val -> Val -> Inst1
opToInst S.Add = Add
opToInst S.Sub = Sub
opToInst S.Mul = Mul
opToInst S.Div = Div

compile :: (?env :: Env) => S.Tm -> BlockM Val
compile tm = case tm of
  S.LocalVar (S.Id i _) -> pure $ elemAt ?env i
  S.Lit l -> pure $ VConst $ compileLit l
  S.BinOp op tm1 tm2 -> do
    v1 <- compile tm1
    v2 <- compile tm2
    emit1 (opToInst op v1 v2) Float64
  S.Block ((_, tm1):bs) ret -> do
    v <- compile tm1
    let ?env = Snoc ?env v in compile (S.Block bs ret)
  S.Block [] ret -> compile ret
  _ -> impossible
