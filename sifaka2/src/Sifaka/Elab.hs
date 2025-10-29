{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Sifaka.Elab (elabModule) where

import Control.Applicative (empty)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.IORef
import Data.Map qualified as Map
import FNotation.Diagnostic (Annot (..), Diagnostic (..), FileId (..), Loc (..), Marker (..), Reporter (..), Severity (..))
import FNotation.FNtn as FNtn
import FNotation.Prelude (ADoc)
import FNotation.Span
import Prettyprinter
import Sifaka.Common
import Sifaka.Syntax qualified as S
import Sifaka.Value qualified as V
import Prelude hiding (error, lookup)

data Locals
  = LNil
  | LDef Locals Name V.Tm V.Ty
  | LBind Locals Name V.Ty

data MetaEnv = MetaEnv Int

data Ctx = Ctx
  { ctxLen :: Word,
    ctxLocals :: Locals,
    ctxBDs :: Bwd S.BD
  }

bind :: Ctx -> Name -> V.Ty -> Ctx
bind (Ctx n l bds) name ty = Ctx (n + 1) (LBind l name ty) (Snoc bds S.Bound)

define :: Ctx -> Name -> V.Tm -> V.Ty -> Ctx
define (Ctx n l bds) name v ty = Ctx (n + 1) (LDef l name v ty) (Snoc bds S.Defined)

type CtxArg = (?ctx :: Ctx)

type ModuleArg = (?module :: S.Module)

type ReporterArg = (?reporter :: Reporter)

type FileIdArg = (?fileId :: FileId)

type MetaEnvArg = (?metaEnv :: IORef MetaEnv)

type Elab a = (FileIdArg) => (ReporterArg) => (MetaEnvArg) => (ModuleArg) => (CtxArg) => a

-- | Types and terms used during elaboration have a strict syntactic component
-- | and a lazy semantic component
data Ty = Ty
  { tyStx :: S.Ty,
    tyVal :: ~V.Ty
  }

data Tm = Tm
  { tmStx :: S.Tm,
    tmVal :: ~V.Tm
  }

binop :: S.BinOp -> Tm -> Tm -> Tm
binop op (Tm s1 _) (Tm s2 _) = (Tm (S.BinOp op s1 s2) V.Opaque)

report :: (ReporterArg) => Diagnostic -> IO ()
report d = do
  let (Reporter r) = ?reporter
  r d

error :: (FileIdArg) => (ReporterArg) => Span -> ADoc -> IO ()
error s msg = do
  let l = Loc ?fileId s
  let m = Marker l Here
  let d = Diagnostic ErrorS msg "" [m]
  report d

bwdToFwd :: (CtxArg) => BwdIdx -> FwdIdx
bwdToFwd (BwdIdx i) = FwdIdx ((ctxLen ?ctx) - i - 1)

fwdToBwd :: (CtxArg) => FwdIdx -> BwdIdx
fwdToBwd (FwdIdx i) = BwdIdx ((ctxLen ?ctx) - i - 1)

lookup :: Elab (Name -> Maybe (BwdIdx, V.Tm, V.Ty))
lookup name = go (ctxLocals ?ctx) 0
  where
    go LNil _ = Nothing
    go (LDef locals name' tm ty) i
      | name == name' = Just (i, tm, ty)
      | otherwise = go locals (i + 1)
    go (LBind locals name' ty) i
      | name == name' = Just (i, (V.Var (bwdToFwd i) name), ty)
      | otherwise = go locals (i + 1)

quoteTy :: Elab (V.Ty -> S.Ty)
quoteTy (V.TFlex _ _) = impossible
quoteTy (V.Fin tm) = S.Fin $ quote tm
quoteTy V.Nat = S.Nat
quoteTy V.Double = S.Double
quoteTy (V.Record _) = impossible
quoteTy (V.Arr dom cod) = S.Arr (quote dom) (quoteTy cod)

evalTy :: Elab (S.Ty -> V.Ty)
evalTy (S.TMetaApp _ _) = impossible
evalTy (S.TInsertedMeta _ _) = impossible
evalTy (S.Fin tm) = V.Fin (eval tm)
evalTy S.Nat = V.Nat
evalTy S.Double = V.Double
evalTy (S.Record _) = impossible
evalTy (S.Arr dom cod) = V.Arr (eval dom) (evalTy cod)

quoteSp :: Elab (V.Spine -> Bwd S.Tm)
quoteSp V.SId = BwdNil
quoteSp (V.SApp sp t) = Snoc (quoteSp sp) (quote t)

quoteLit :: Elab (V.Literal -> S.Literal)
quoteLit (V.LitNat i) = S.LitNat i

quote :: Elab (V.Tm -> S.Tm)
quote (V.Var i name) = S.LocalVar (S.Id (fwdToBwd i) name)
quote (V.Flex mv sp) = S.MetaApp mv (quoteSp sp)
quote (V.Lit lit) = S.Lit (quoteLit lit)
quote V.Opaque = S.Opaque

getVar :: Locals -> FwdIdx -> BwdIdx -> V.Tm
getVar LNil _ _ = impossible
getVar (LDef l _ tm _) i j
  | j == 0 = tm
  | otherwise = getVar l (i-1) (j-1)
getVar (LBind l name _) i j
  | j == 0 = V.Var i name
  | otherwise = getVar l (i-1) (j-1)

evalLit :: S.Literal -> V.Tm
evalLit (S.LitFin _) = V.Opaque
evalLit (S.LitNat n) = V.Lit (V.LitNat n)
evalLit (S.LitDouble _) = V.Opaque

eval :: Elab (S.Tm -> V.Tm)
eval (S.LocalVar (S.Id i _)) = getVar (ctxLocals ?ctx) (FwdIdx (ctxLen ?ctx)) i
eval (S.TopApp _f _args) = impossible
eval (S.InsertedMeta _mv _bds) = impossible
eval (S.MetaApp _mv _args) = impossible
eval (S.Lit lit) = evalLit lit
eval (S.BinOp _ _ _) = V.Opaque
eval (S.Block _bindings _ret) = impossible
eval (S.RecordCon _) = impossible
eval (S.Proj _ _) = impossible
eval (S.ArrCon _) = impossible
eval (S.ArrLam _ _) = impossible
eval (S.Index _ _) = impossible
eval S.Opaque = V.Opaque

freshMeta :: Elab (IO MetaVar)
freshMeta = atomicModifyIORef ?metaEnv (\(MetaEnv i) -> (MetaEnv (i + 1), MetaVar i))

-- Create a new type in an inferring context
checkTyMeta :: Elab (IO Ty)
checkTyMeta = do
  mv <- freshMeta
  -- NOTE: this is incorrect; we should actually pull out the spine from the
  -- curent environment
  pure $ Ty (S.TInsertedMeta mv (ctxBDs ?ctx)) (V.TFlex mv V.SId)

-- Create a new meta in a checking context
checkMeta :: Elab (IO Tm)
checkMeta = do
  mv <- freshMeta
  -- NOTE: this is incorrect; we should actually pull out the spine from the
  -- curent environment
  pure $ Tm (S.InsertedMeta mv (ctxBDs ?ctx)) (V.Flex mv V.SId)

-- Create a new meta in an inferring context
inferMeta :: Elab (IO (Tm, V.Ty))
inferMeta = do
  tmM <- freshMeta
  tyM <- freshMeta
  pure $ (Tm (S.InsertedMeta tmM (ctxBDs ?ctx)) (V.Flex tmM V.SId), V.TFlex tyM V.SId)

checkTy :: Elab (FNtn -> IO Ty)
checkTy (L _ (Keyword "Double")) = pure $ Ty S.Double V.Double
checkTy (L _ (Keyword "Nat")) = pure $ Ty S.Nat V.Nat
checkTy (L _ (App1 (L _ (Keyword "Fin")) nN)) = do
  (Tm nS nV) <- check V.Nat nN
  pure $ Ty (S.Fin nS) (V.Fin nV)
checkTy (L _ (App2 (L _ (Keyword "=>")) domN codN)) = do
  (Tm domS domV) <- check V.Nat domN
  (Ty codS codV) <- checkTy codN
  pure $ Ty (S.Arr domS codS) (V.Arr domV codV)
checkTy (L s _) = do
  error s "unexpected notation for type"
  checkTyMeta

unifyTy :: Elab (V.Ty -> V.Ty -> IO Bool)
unifyTy V.Double V.Double = pure True
unifyTy V.Nat V.Nat = pure True
unifyTy (V.Arr dom1 cod1) (V.Arr dom2 cod2) =
  (&&) <$> unify dom1 dom2 <*> unifyTy cod1 cod2
unifyTy (V.Fin n1) (V.Fin n2) = unify n1 n2
unifyTy _ _ = impossible

unify :: Elab (V.Tm -> V.Tm -> IO Bool)
unify (V.Lit l1) (V.Lit l2) = pure $ l1 == l2
unify (V.Var i1 _) (V.Var i2 _) = pure $ i1 == i2
unify V.Opaque V.Opaque = pure True
unify _ _ = impossible

check :: Elab (V.Ty -> FNtn -> IO Tm)
check ty (L s (Int i)) = case ty of
  V.Double -> pure $ Tm (S.Lit (S.LitDouble (fromIntegral i))) V.Opaque
  V.Nat -> let n = fromIntegral i in
    pure $ Tm (S.Lit (S.LitNat n)) (V.Lit (V.LitNat n))
  _ -> do
    error s "can only check integer against Double or Nat"
    checkMeta
check ty (L s (App2 (L _ (Keyword "↦")) argNameN bodyN)) = case ty of
  V.Arr dom cod -> runMaybeT (asName argNameN) >>= \case
      Just name ->
        let ?ctx = bind ?ctx name (V.Fin dom) in do
          (Tm bodyS _) <- check cod bodyN
          pure $ Tm (S.ArrLam name bodyS) V.Opaque
      Nothing -> do
        error (FNtn.span argNameN) "expected an ident"
        checkMeta
  _ -> do
    error s "can only check lambda expression against array type"
    checkMeta
check ty n@(L s _) = do
  (tm, synthed) <- infer n
  unifies <- unifyTy ty synthed
  if unifies
    then pure tm
    else do
      error s "inferred type could not be unified with type being checked against"
      checkMeta

ops :: Map Text S.BinOp
ops =
  Map.fromList
    [ ("+", S.Add),
      ("-", S.Sub),
      ("*", S.Mul),
      ("/", S.Div)
    ]

gatherApp1s :: FNtn -> [FNtn] -> (FNtn, [FNtn])
gatherApp1s (L _ (App1 f x)) args = gatherApp1s f (x : args)
gatherApp1s n args = (n, args)

checkArgs :: Elab ([FNtn] -> [(Name, S.Ty)] -> [S.Tm] -> IO (Ctx, [S.Tm]))
checkArgs [] [] args = pure $ (?ctx, reverse args)
checkArgs (argN : argNs) ((name, argTy) : argTys) args = do
  let ty = evalTy argTy
  arg <- check ty argN
  let ?ctx = define ?ctx name (tmVal arg) ty
   in checkArgs argNs argTys (tmStx arg : args)
checkArgs _ _ _ = impossible

lookupFunc :: Elab (Span -> Name -> MaybeT IO (S.Id S.Func, S.Func))
lookupFunc s name = go 0 (S.moduleFuncs ?module)
  where
    go :: BwdIdx -> Bwd S.Func -> MaybeT IO (S.Id S.Func, S.Func)
    go _ BwdNil = do
      liftIO $ error s $ "no such function" <+> pretty name
      empty
    go i (Snoc funcs f@(S.Func name' _ _ _))
      | name == name' = pure (S.Id i name, f)
      | otherwise = go (i + 1) funcs

orInferMeta :: Elab (MaybeT IO (Tm, V.Ty) -> IO (Tm, V.Ty))
orInferMeta action =
  runMaybeT action >>= \case
    Just res -> pure res
    Nothing -> inferMeta

infer :: Elab (FNtn -> IO (Tm, V.Ty))
infer (L s (Ident n)) = do
  let name = Name n
  case lookup name of
    Just (i, tm, ty) -> pure $ (Tm (S.LocalVar (S.Id i name)) tm, ty)
    Nothing -> do
      error s ("no such variable" <+> pretty name)
      inferMeta
infer n@(L _ (App1 _ _)) = do
  let (fN, argNs) = gatherApp1s n []
  orInferMeta $ do
    name <- asName fN
    (fid, fdef) <- lookupFunc (FNtn.span fN) name
    (ctx, args) <- liftIO $ checkArgs argNs (S.funcArgs fdef) []
    let ?ctx = ctx
     in let ty = evalTy (S.funcRetTy fdef)
            val = eval (S.funcBody fdef)
         in pure (Tm (S.TopApp fid args) val, ty)
infer (L s (App2 (L _ (Keyword opName)) n1 n2))
  | opName == "!" = do
    (Tm aS _, aTy) <- infer n1
    case aTy of
      V.Arr dom cod -> do
        Tm iS _ <- check (V.Fin dom) n2
        pure (Tm (S.Index aS iS) V.Opaque, cod)
      _ -> do
        error (FNtn.span n1) "expected variable of array type"
        inferMeta
  | otherwise = do
    t1 <- check V.Double n1
    t2 <- check V.Double n2
    case ops Map.!? opName of
      Just op -> pure (binop op t1 t2, V.Double)
      Nothing -> do
        error s $ "unrecognized operator" <+> pretty opName
        inferMeta
infer (L s (Int _)) = do
  error s "integer literal must occur in checking position"
  inferMeta
infer (L s _) = do
  error s "unexpected notation for term"
  inferMeta

asDefinition :: Elab (FNtn -> MaybeT IO (FNtn, FNtn))
asDefinition (L _ (App2 (L _ (Keyword "=")) lhs rhs)) = pure (lhs, rhs)
asDefinition (L s _) = do
  liftIO $ error s "expected definition <pattern> = <term>"
  empty

asBinding :: Elab (FNtn -> MaybeT IO (FNtn, FNtn))
asBinding (L _ (App2 (L _ (Keyword ":")) lhs rhs)) = pure (lhs, rhs)
asBinding (L s _) = do
  liftIO $ error s "expected binding <term> : <type>"
  empty

asApplication :: Elab (FNtn -> MaybeT IO (Name, [FNtn]))
asApplication n = go n []
  where
    go (L _ (Ident name)) args = pure (Name name, args)
    go (L _ (App1 f x)) args = go f (x : args)
    go (L s _) _ = do
      liftIO $ error s "expected an application of a name to arguments"
      empty

asName :: Elab (FNtn -> MaybeT IO Name)
asName (L _ (Ident name)) = pure $ Name name
asName (L s _) = do
  liftIO $ error s "expected a name"
  empty

elabArgs :: Elab ([FNtn] -> [(Name, S.Ty)] -> MaybeT IO ([(Name, S.Ty)], Ctx))
elabArgs [] args = pure (reverse args, ?ctx)
elabArgs (argN : rest) args = do
  (nameN, tyN) <- asBinding argN
  name <- asName nameN
  (Ty tyS tyV) <- liftIO $ checkTy tyN
  let ?ctx = bind ?ctx name tyV
   in elabArgs rest ((name, tyS) : args)

topDecl :: Elab (FNtnTop -> IO (Maybe S.TopDecl))
topDecl (FNtnTop "def" _ n) = runMaybeT $ do
  (pat, bodyN) <- asDefinition n
  (app, retTyN) <- asBinding pat
  (name, argNs) <- asApplication app
  (args, ctx) <- elabArgs argNs []
  let ?ctx = ctx
   in do
        (Ty retTyS retTyV) <- liftIO $ checkTy retTyN
        (Tm bodyS _) <- liftIO $ check retTyV bodyN
        pure $ S.TFunc (S.Func name args retTyS bodyS)
topDecl (FNtnTop "eval" _ n) = do
  (tm, ty) <- infer n
  pure $ Just $ S.TEval $ S.Eval (tmStx tm) (quoteTy ty)
topDecl (FNtnTop name s _) = do
  error s $ "unexpected toplevel \"" <> pretty name <> "\""
  pure Nothing

doModule :: Elab ([FNtnTop] -> IO S.Module)
doModule [] = pure ?module
doModule (tn : rest) =
  topDecl tn >>= \case
    Just (S.TFunc f) ->
      let ?module = S.addFunc ?module f in doModule rest
    Just (S.TEval tm) ->
      let ?module = S.addEval ?module tm in doModule rest
    Nothing -> doModule rest

elabModule :: Reporter -> FileId -> [FNtnTop] -> IO S.Module
elabModule reporter fileId tns = do
  metaIORef <- newIORef (MetaEnv 0)
  let ?reporter = reporter
      ?fileId = fileId
      ?metaEnv = metaIORef
      ?module = S.emptyModule
      ?ctx = Ctx 0 LNil BwdNil
  doModule tns
