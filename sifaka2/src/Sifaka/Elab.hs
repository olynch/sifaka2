module Sifaka.Elab where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (MonadPlus)
import Prelude hiding (error, lookup)
import Prettyprinter
import Data.IORef

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

type CtxLenArg = (?ctxLen :: Int)
type LocalsArg = (?locals :: Locals)
type ReporterArg = (?reporter :: Reporter)
type FileIdArg = (?fileId :: FileId)
type MetaEnvArg = (?metaEnv :: IORef MetaEnv)

type Elab a = FileIdArg => ReporterArg => MetaEnvArg => CtxLenArg => LocalsArg => a

data SVTy = SVTy S.Ty ~V.Ty

data SVTm = SVTm S.Tm ~V.Tm

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

bwdToFwd :: CtxLenArg => BwdIdx -> FwdIdx
bwdToFwd (BwdIdx i) = FwdIdx (?ctxLen - i - 1)

lookup :: Elab (Name -> IO (Maybe (BwdIdx, V.Tm, V.Ty)))
lookup name = pure $ go ?locals 0
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

check :: Elab (FNtn -> V.Ty -> IO SVTm)
check n@(L s _) ty = do
  (tm, synthed) <- infer n
  unifies <- unifyTy ty synthed
  if unifies
    then pure tm
    else do
      error s "inferred type could not be unified with type being checked against"
      checkMeta


infer :: Elab (FNtn -> IO (SVTm, V.Ty))
infer (L s (Ident n)) = do
  let name = Name n
  lookup name >>= \case
    Just (i, tm, ty) -> pure $ (SVTm (S.LocalVar (S.Id i name)) tm, ty)
    Nothing -> do
      error s ("no such variable" <+> pretty name)
      inferMeta

infer (L s (Int i)) = do
  error s "integer literal only allowed in checking position"
  inferMeta

infer (L s _) = do
  error s "unexpected notation for term"
  inferMeta
