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

emit :: Inst -> BlockM ()
emit i = modify $ \(BS fresh is) -> BS fresh (Snoc is i)

emit0 :: Inst0 -> BlockM ()
emit0 i = emit (Inst0 i)

emit1 :: Inst1 -> BaseTy -> BlockM Val
emit1 i ty = do
  name <- freshLocal
  emit $ Inst1 name ty i
  pure $ VIdent name

call1 :: GlobalName -> AbiTy -> [(AbiTy, Val)] -> BlockM Val
call1 fname retTy args = do
  name <- freshLocal
  emit $ Call (Just (name, retTy)) (VConst (CGlobal fname)) args
  pure $ VIdent name

call0 :: GlobalName -> [(AbiTy, Val)] -> BlockM ()
call0 fname args = emit $ Call Nothing (VConst (CGlobal fname)) args

type Env = Bwd Val

freshLocal :: BlockM LocalName
freshLocal = state $
  \(BS fresh is) -> (LocalName (FreshName fresh), (BS (fresh + 1) is))

compileLit :: S.Literal -> Const
compileLit (S.LitNat i) = CNum $ fromIntegral i
compileLit (S.LitFin i) = CNum $ fromIntegral i
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
  S.TopApp (S.Id _ (Name f)) argStxs -> do
    argVals <- mapM doTm argStxs
    call1
      (GlobalName (StaticName f))
      (ABase Float64)
      (zip (repeat $ ABase Float64) argVals)
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

doFunc :: S.Func -> FuncDef
doFunc (S.Func (Name name) args retTy body) =
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

doEvals :: [S.Eval] -> BlockM ()
doEvals [] = pure ()
doEvals (S.Eval tm _ : rest) = do
  ret <- let ?env = BwdNil in doTm tm
  call0 "printd" [(ABase Float64, ret)]
  doEvals rest

doMain :: Bwd S.Eval -> FuncDef
doMain evals =
  FuncDef
    [Export]
    (Just (ABase Word32))
    (GlobalName (StaticName "main"))
    []
    [startBlock]
  where
    (_, instructions) = runBlockM $ doEvals $ bwdToList evals
    startBlock = Block
      (LabelName (StaticName "start"))
      []
      instructions
      (Ret $ Just (VConst (CNum 0)))

start :: FuncDef
start = FuncDef [Export] Nothing "_start" [] [startBlock]
  where
    (_, instructions) = runBlockM $ do
      ret <- call1 "main" (ABase Word32) []
      call0 "exit" [(ABase Word32, ret)]
    startBlock = Block "start" [] instructions Hlt

genModule :: S.Module -> Module
genModule m = Module
  []
  []
  (bwdToList $ Snoc (fmap doFunc (S.moduleFuncs m)) (doMain (S.moduleEvals m)))
