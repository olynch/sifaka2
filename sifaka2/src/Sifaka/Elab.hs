module Sifaka.Elab (elabTop) where

import Control.Applicative (empty)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (error, lookup)
import Prettyprinter
import Data.IORef
import Data.Map qualified as Map

import Sifaka.Value qualified as V
import Sifaka.Syntax qualified as S
import Sifaka.Common

import FNotation.FNtn
import FNotation.Span
import FNotation.Prelude (ADoc)
import FNotation.Diagnostic (Reporter(..), Diagnostic(..), FileId(..), Loc(..), Marker(..), Severity(..), Annot(..))

data Locals
  = LNil
  | LDef Locals Name V.Tm V.Ty
  | LBind Locals Name V.Ty

data MetaEnv = MetaEnv Int

data Ctx = Ctx {
  ctxLen :: Word,
  ctxLocals :: Locals
  }

newtype TopCtx = TopCtx {
  topCtxDecls :: Bwd (Name, S.TopDecl)
}

intro :: Ctx -> Name -> V.Ty -> Ctx
intro (Ctx n l) name ty = Ctx (n + 1) (LBind l name ty)

type CtxArg = (?ctx :: Ctx)
type TopCtxArg = (?topCtx :: TopCtx)
type ReporterArg = (?reporter :: Reporter)
type FileIdArg = (?fileId :: FileId)
type MetaEnvArg = (?metaEnv :: IORef MetaEnv)

type Elab a = FileIdArg => ReporterArg => MetaEnvArg => TopCtxArg => CtxArg => a

data SVTy = SVTy S.Ty ~V.Ty

data SVTm = SVTm S.Tm ~V.Tm

binop :: S.BinOp -> SVTm -> SVTm -> SVTm
binop op (SVTm s1 _) (SVTm s2 _) = (SVTm (S.BinOp op s1 s2) V.Opaque)

-- Options for handling failure:
--
-- 1. Exceptions
-- 2. MaybeT IO
-- 3. No failures, just make more metavariables
--
-- 3 is the option suggested by Andras and Jon.

report :: ReporterArg => Diagnostic -> IO ()
report d = do
  let (Reporter r) = ?reporter
  r d

error :: FileIdArg => ReporterArg => Span -> ADoc -> IO ()
error s msg = do
  let l = Loc ?fileId s
  let m = Marker l Here
  let d = Diagnostic ErrorS msg "" [m]
  report d

bwdToFwd :: CtxArg => BwdIdx -> FwdIdx
bwdToFwd (BwdIdx i) = FwdIdx ((ctxLen ?ctx) - i - 1)

lookup :: Elab (Name -> IO (Maybe (BwdIdx, V.Tm, V.Ty)))
lookup name = pure $ go (ctxLocals ?ctx) 0
  where
    go LNil _ = Nothing
    go (LDef locals name' tm ty) i
      | name == name' = Just (i, tm, ty)
      | otherwise     = go locals (i + 1)
    go (LBind locals name' ty) i
      | name == name' = Just (i, (V.Rigid (bwdToFwd i) V.SId), ty)
      | otherwise     = go locals (i + 1)


freshMeta :: Elab (IO MetaVar)
freshMeta = atomicModifyIORef ?metaEnv (\(MetaEnv i) -> (MetaEnv (i+1), MetaVar i))

checkTyMeta :: Elab (IO SVTy)
checkTyMeta = do
  mv <- freshMeta
  -- NOTE: this is incorrect; we should actually pull out the spine from the
  -- curent environment
  pure $ SVTy (S.TMeta mv S.MSId) (V.TFlex mv V.SId)

checkMeta :: Elab (IO SVTm)
checkMeta = do
  mv <- freshMeta
  -- NOTE: this is incorrect; we should actually pull out the spine from the
  -- curent environment
  pure $ SVTm (S.Meta mv S.MSId) (V.Flex mv V.SId)

inferMeta :: Elab (IO (SVTm, V.Ty))
inferMeta = do
  tmM <- freshMeta
  tyM <- freshMeta
  pure $ (SVTm (S.Meta tmM S.MSId) (V.Flex tmM V.SId), V.TFlex tyM V.SId)

checkTy :: Elab (FNtn -> IO SVTy)
checkTy (L _ (Keyword "Double")) = pure $ SVTy S.Double V.Double
checkTy (L s _) = do
  error s "unexpected notation for type"
  checkTyMeta

unifyTy :: Elab (V.Ty -> V.Ty -> IO Bool)
unifyTy V.Double V.Double = pure True
unifyTy _ _ = pure False

check :: Elab (V.Ty -> FNtn -> IO SVTm)
check ty (L s (Int i)) = case ty of
  V.Double -> pure (SVTm (S.Lit (S.LitDouble (fromIntegral i))) V.Opaque)
  _ -> do
    error s "can only check integer against double for now"
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
ops = Map.fromList [
  ("+", S.Add),
  ("-", S.Sub),
  ("*", S.Mul),
  ("/", S.Div)
  ]

infer :: Elab (FNtn -> IO (SVTm, V.Ty))
infer (L s (Ident n)) = do
  let name = Name n
  lookup name >>= \case
    Just (i, tm, ty) -> pure $ (SVTm (S.LocalVar (S.Id i name)) tm, ty)
    Nothing -> do
      error s ("no such variable" <+> pretty name)
      inferMeta
infer (L s (App2 (L _ (Keyword opN)) n1 n2)) = do
  t1 <- check V.Double n1
  t2 <- check V.Double n2
  case ops Map.!? opN of
    Just op -> pure (binop op t1 t2, V.Double)
    Nothing -> do
      error s $ "unrecognized operator" <+> pretty opN
      inferMeta

infer (L s (Int _)) = do
  error s "integer literal only allowed in checking position"
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
    go (L _ (App1 f x)) args = go f (x:args)
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
  (SVTy tyS tyV) <- liftIO $ checkTy tyN
  let ?ctx = intro ?ctx name tyV in
    elabArgs rest ((name, tyS):args)


topDecl :: Elab (FNtnTop -> IO (Maybe (Name, S.TopDecl)))
topDecl (FNtnTop "def" _ n) = runMaybeT $ do
  (pat, bodyN) <- asDefinition n
  (app, retTyN) <- asBinding pat
  (name, argNs) <- asApplication app
  (args, ctx) <- elabArgs argNs []
  let ?ctx = ctx in do
    (SVTy retTyS retTyV) <- liftIO $ checkTy retTyN
    (SVTm bodyS _) <- liftIO $ check retTyV bodyN
    pure (name, S.TDef (S.Def args retTyS bodyS))
topDecl (FNtnTop name s _) = do
  error s $ "unexpected toplevel \"" <> pretty name <> "\""
  pure Nothing

topDecls :: Elab ([FNtnTop] -> IO TopCtx)
topDecls [] = pure $ ?topCtx
topDecls (tn:rest) = topDecl tn >>= \case
  Just d -> let ?topCtx = TopCtx (Snoc (topCtxDecls ?topCtx) d) in topDecls rest
  Nothing -> pure ?topCtx


elabTop :: Reporter -> FileId -> [FNtnTop] -> IO [(Name, S.TopDecl)]
elabTop reporter fileId tns = do
  metaIORef <- newIORef (MetaEnv 0)
  let ?reporter = reporter
      ?fileId = fileId
      ?metaEnv = metaIORef
      ?topCtx = TopCtx BwdNil
      ?ctx = Ctx 0 LNil
  (TopCtx decls) <- topDecls tns
  pure $ bwdToList decls
