module Sifaka.CodeGen where

import Control.Monad.State

import Sifaka.Common
import Sifaka.Qbe
import Sifaka.Syntax qualified as S

data BS = BS
    { bsFresh :: Int
    , bsInstructions :: Bwd Inst
    }

newtype BlockM a = BlockM {unBlockM :: State BS a}
    deriving (Functor, Applicative, Monad, MonadState BS)

runBlockM :: BlockM a -> (a, [Inst])
runBlockM action =
  let (x, BS _ is) = runState (unBlockM action) (BS 0 BwdNil) in (x, bwdToList is)

emit0 :: Inst0 -> BlockM ()
emit0 i = modify (\bs -> bs{bsInstructions = Snoc (bsInstructions bs) (Inst0 i)})

emit1 :: Inst1 -> BaseTy -> BlockM Val
emit1 i ty = do
  name <- freshLocal
  state $ \(BS fresh is) -> (VIdent name, BS fresh (Snoc is (Inst1 name ty i)))

type Env = Bwd Val

freshLocal :: BlockM LocalName
freshLocal = state $
  \(BS fresh is) -> (LocalName (FreshName fresh), (BS (fresh + 1) is))

compileLit :: S.Literal -> Const
compileLit (S.LitInt i) = CNum i
compileLit (S.LitFin i) = CNum i
compileLit (S.LitDouble x) = CFloat64 x

opToInst :: S.BinOp -> Val -> Val -> Inst1
opToInst S.Add = Add
opToInst S.Sub = Sub
opToInst S.Mul = Mul
opToInst S.Div = Div

doTm :: (?env :: Env) => S.Tm -> BlockM Val
doTm tm = case tm of
  S.LocalVar (S.Id i _) -> pure $ elemAt ?env i
  S.Lit l -> pure $ VConst $ compileLit l
  S.BinOp op tm1 tm2 -> do
    v1 <- doTm tm1
    v2 <- doTm tm2
    emit1 (opToInst op v1 v2) Float64
  S.Block ((_, tm1):bs) ret -> do
    v <- doTm tm1
    let ?env = Snoc ?env v in doTm (S.Block bs ret)
  S.Block [] ret -> doTm ret
  _ -> impossible

doTy :: S.Ty -> BaseTy
doTy S.Double = Float64
doTy _ = impossible

doArgs :: (?env :: Env) => [(Name, S.Ty)] -> BlockM (Env, [Param])
doArgs args = go args []
  where
    go :: (?env :: Env) => [(Name, S.Ty)] -> [Param] -> BlockM (Env, [Param])
    go [] params = pure (?env, reverse params)
    go ((_, ty):rest) params = do
      name <- freshLocal
      let ?env = Snoc ?env (VIdent name) in
        go rest (Param (ABase (doTy ty)) name : params)

doDef :: Name -> S.Def -> FuncDef
doDef (Name name) (S.Def args retTy body) =
  FuncDef
    [Export]
    (Just (ABase (doTy retTy)))
    (GlobalName (StaticName name))
    params
    [startBlock]
  where
    ((params, ret), instructions) = runBlockM $ do
      (env, ps) <- let ?env = BwdNil in doArgs args
      let ?env = env in do
        r <- doTm body
        pure (ps, r)
    startBlock =
      Block (LabelName (StaticName "start")) [] instructions (Ret $ Just ret)

doDecls :: [(Name, S.TopDecl)] -> Module
doDecls decls = go (Module [] [] []) decls
  where
    go (Module tys datas funcs) [] =
      Module (reverse tys) (reverse datas) (reverse funcs)
    go m ((name, S.TDef d) : rest) = go m' rest
      where m' = m { moduleFuncs = doDef name d : (moduleFuncs m) }

genTop :: [(Name, S.TopDecl)] -> FilePath -> IO ()
genTop decls out = assemble mod out
  where mod = doDecls decls
