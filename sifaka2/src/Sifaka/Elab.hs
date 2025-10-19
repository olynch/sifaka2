module Sifaka.Elab where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (MonadPlus)
import Prelude hiding (error, lookup)
import Prettyprinter

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

type LocalsArg = (?locals :: Locals)
type ReporterArg = (?reporter :: Reporter)
type FileIdArg = (?fileId :: FileId)

type Elab a = FileIdArg => ReporterArg => LocalsArg => a

-- Options for handling failure:
--
-- 1. Exceptions
-- 2. MaybeT IO
--
-- The annoying thing about MaybeT IO is that we force everything to be
-- in MaybeT IO... unless we sprinkle around a bunch of (LiftIO m) => m a
-- definitions everywhere.
--
-- The nice thing about exceptions is that we can start by propagating
-- the exception all the way up, and then progressively enhance to still
-- check more things with strategic `try`s.
--
-- The nasty thing about exceptions is that they involve type equality
-- checks, and I don't want to use the smalltt fancy schmancy stuff.
--
-- Let's just use MaybeT IO for now, and we'll see how annoying it
-- gets.

newtype MIO a = MIO { unMIO :: MaybeT IO a }
  deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

runMIO :: MIO a -> IO (Maybe a)
runMIO = runMaybeT . unMIO

makeMIO :: IO (Maybe a) -> MIO a
makeMIO = MIO . MaybeT

instance MonadError () MIO where
  throwError _ = empty
  {-# INLINE throwError #-}

  catchError act k = makeMIO $ runMIO act >>= \case
    Just x -> pure (Just x)
    Nothing -> runMIO $ k ()
  {-# INLINE catchError #-}

try :: MIO a -> MIO (Maybe a)
try act = catchError (Just <$> act) (\_ -> pure Nothing)
{-# INLINE try #-}

unwrap :: Maybe a -> MIO a
unwrap = \case
  Just x -> pure x
  Nothing -> throwError ()
{-# INLINE unwrap #-}

-- | A version of <*> that doesn't short circuit
(<*?>) :: MIO (a -> b) -> MIO a -> MIO b
(<*?>) mf ma = do
  f <- try mf
  a <- try ma
  unwrap $ f <*> a

report :: ReporterArg => Diagnostic -> MIO ()
report d = do
  let (Reporter r) = ?reporter
  liftIO $ r d

error :: FileIdArg => ReporterArg => Span -> ADoc -> MIO a
error s msg = do
  let l = Loc ?fileId s
  let m = Marker l Here
  let d = Diagnostic ErrorS msg "" [m]
  report d
  unwrap Nothing

lookup :: Elab (Text -> MIO (BwdIdx, V.Ty))
lookup name = unwrap $ go ?locals 0
  where
    go LNil _ = Nothing
    go (LDef locals name' _ ty) i
      | name == name' = Just (i, ty)
      | otherwise     = go locals (i + 1)
    go (LBind locals name' ty) i
      | name == name' = Just (i, ty)
      | otherwise     = go locals (i + 1)

checkTy :: Elab (FNtn -> MIO S.Tm)
checkTy (L s n) = _

check :: Elab (FNtn -> V.Ty -> MIO S.Tm)
check (L s n) ty = _

infer :: Elab (FNtn -> MIO (S.Tm, V.Ty))
infer (L s n) = case n of
  Ident name -> do
    (ix, ty) <- lookup name <|> error s ("name not found:" <+> pretty name)
    pure (S.LocalVar (S.Id ix name), ty)
  Int i -> error s ("integer literal only allowed in checking position")

  _ -> unwrap Nothing
