module Sifaka.CodeGen where

import Control.Monad.State
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Sifaka.Common
import Sifaka.Qbe
import Sifaka.Syntax qualified as S

-- Block Builder
data BB = BB
  { bbNextLocal :: Int,
    bbNextBlock :: Int,
    bbBlocks :: Bwd Block,
    bbCurrentLabel :: LabelName,
    bbPhis :: Bwd Phi,
    bbInstructions :: Bwd Inst
  }

newtype BlockM a = BlockM {unBlockM :: State BB a}
  deriving (Functor, Applicative, Monad, MonadState BB)

runBlockM :: BlockM a -> (a, [Block])
runBlockM action = (x, bwdToList $ bbBlocks bb')
  where
    (x, bb') = runState (unBlockM action) bb
    bb = BB 0 0 BwdNil "start" BwdNil BwdNil

phi :: Phi -> BlockM ()
phi p = modify $ \bb -> bb {bbPhis = bbPhis bb :> p}

emit :: Inst -> BlockM ()
emit i = modify $ \bb -> bb {bbInstructions = bbInstructions bb :> i}

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

freshLocal :: BlockM LocalName
freshLocal = state $
  \bb ->
    ( LocalName (FreshName (bbNextLocal bb)),
      bb {bbNextLocal = bbNextLocal bb + 1}
    )

currentLabel :: BlockM LabelName
currentLabel = get <&> bbCurrentLabel

finishBlock :: (LabelName -> Jump) -> BlockM ()
finishBlock j = modify finishBlock'
  where
    finishBlock' bb =
      bb
        { bbNextBlock = i + 1,
          bbInstructions = BwdNil,
          bbPhis = BwdNil,
          bbCurrentLabel = l,
          bbBlocks = bbBlocks bb :> b
        }
      where
        i = bbNextBlock bb
        l = LabelName (FreshName i)
        b =
          Block
            (bbCurrentLabel bb)
            (bwdToList $ bbPhis bb)
            (bwdToList $ bbInstructions bb)
            (j l)

nextBlock :: BlockM ()
nextBlock = finishBlock $ \l -> Jmp l

type Env = Bwd Val

type CodeGen a = (?module :: S.Module) => (?env :: Env) => a

compileLit :: S.Literal -> Const
compileLit (S.LitNat i) = CNum $ fromIntegral i
compileLit (S.LitFin i) = CNum $ fromIntegral i
compileLit (S.LitDouble x) = CFloat64 x

opToInst :: S.BinOp -> Val -> Val -> Inst1
opToInst S.Add = Add
opToInst S.Sub = Sub
opToInst S.Mul = Mul
opToInst S.Div = Div

-- TODO: proper mangling
mangle :: Name -> QbeName
mangle (Name n) = StaticName n

doTm :: CodeGen (S.Tm -> BlockM Val)
doTm tm = case tm of
  S.LocalVar (S.Id i _) -> pure $ elemAt ?env i
  S.Lit l -> pure $ VConst $ compileLit l
  S.BinOp op tm1 tm2 -> do
    v1 <- doTm tm1
    v2 <- doTm tm2
    emit1 (opToInst op v1 v2) Float64
  S.Block ((_, tm1) : bs) ret -> do
    v <- doTm tm1
    let ?env = ?env :> v in doTm (S.Block bs ret)
  S.Block [] ret -> doTm ret
  S.TopApp (S.GlobalId fName) argStxs -> do
    let f = (S.moduleFuncs ?module) Map.! fName
    argVals <- mapM doTm argStxs
    call1
      (GlobalName $ mangle fName)
      (ABase $ doTy $ S.funcRetTy f)
      (zip (ABase . doTy . snd <$> S.funcArgs f) argVals)
  S.ArrLam _ dom cod body -> do
    n <- doTm dom
    size <- emit1 (Mul (sizeOf cod) n) Word64
    dest <- call1 "malloc" (ABase Word64) [(ABase Word64, size)]
    l0 <- currentLabel
    nextBlock
    l1 <- currentLabel
    iName <- freshLocal
    let i = VIdent iName
    x <- let ?env = ?env :> i in doTm body
    i' <- emit1 (Add i (VConst (CNum 1))) Word64
    phi $ Phi iName Word64 [(l0, VConst (CNum 0)), (l1, i')]
    offset <- emit1 (Mul i (sizeOf cod)) Word64
    curDest <- emit1 (Add dest offset) Word64
    emit0 (Store (Base $ doTy cod) x curDest)
    done <- emit1 (Cult Word64 i' n) Word64
    finishBlock $ \next -> Jnz done l1 next
    pure dest
  S.Index aTm ty iTm -> do
    a <- doTm aTm
    i <- doTm iTm
    offset <- emit1 (Mul i (sizeOf ty)) Word64
    ptr <- emit1 (Add a offset) Word64
    emit1 (Load (Base (doTy ty)) ptr) (doTy ty)
  S.IToF tm -> do
    v <- doTm tm
    emit1 (UWToF v) Float64
  _ -> unimplemented

doTy :: S.Ty -> BaseTy
doTy S.Double = Float64
doTy S.Nat = Word64
doTy (S.Arr _ _) = Word64 -- pointer type
doTy (S.Fin _) = Word64
doTy _ = impossible

-- Right now, all values are 8 bytes
sizeOf :: S.Ty -> Val
sizeOf _ = VConst $ CNum 8

doArgs :: (?env :: Env) => [(Name, S.Ty)] -> BlockM (Env, [Param])
doArgs args = go args []
  where
    go :: (?env :: Env) => [(Name, S.Ty)] -> [Param] -> BlockM (Env, [Param])
    go [] params = pure (?env, reverse params)
    go ((_, ty) : rest) params = do
      name <- freshLocal
      let ?env = ?env :> VIdent name
       in go rest (Param (ABase (doTy ty)) name : params)

doFunc :: (?module :: S.Module) => S.Func -> FuncDef
doFunc (S.Func (Name name) args retTy body) =
  FuncDef
    [Export]
    (Just (ABase (doTy retTy)))
    (GlobalName (StaticName name))
    params
    blocks
  where
    (params, blocks) = runBlockM $ do
      (env, ps) <- let ?env = BwdNil in doArgs args
      let ?env = env
       in do
            r <- doTm body
            finishBlock $ const $ Ret $ Just r
            pure ps

doEvals :: (?module :: S.Module) => [S.Eval] -> BlockM ()
doEvals [] = pure ()
doEvals (S.Eval tm _ : rest) = do
  ret <- let ?env = BwdNil in doTm tm
  call0 "printd" [(ABase Float64, ret)]
  doEvals rest

doMain :: (?module :: S.Module) => Bwd S.Eval -> FuncDef
doMain evals =
  FuncDef
    [Export]
    (Just (ABase Word32))
    (GlobalName (StaticName "main"))
    []
    blocks
  where
    (_, blocks) = runBlockM $ do
      doEvals $ bwdToList evals
      finishBlock $ const $ Ret $ Just $ VConst $ CNum 0

start :: FuncDef
start = FuncDef [Export] Nothing "_start" [] blocks
  where
    (_, blocks) = runBlockM $ do
      ret <- call1 "main" (ABase Word32) []
      call0 "exit" [(ABase Word32, ret)]
      finishBlock $ const $ Hlt

genModule :: S.Module -> Module
genModule m =
  let ?module = m
   in Module
        []
        []
        (bwdToList $ fmap doFunc (S.moduleFuncsInOrder m) :> doMain (S.moduleEvals m))
