{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-unused-imports #-}

module Sifaka.Elaboration where

import Control.Applicative (empty)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Vector.Hashtables
import Data.Vector.Hashtables qualified as HT
import Data.Vector.Mutable qualified as VM
import FNotation.Diagnostic (Annot (..), Diagnostic (..), FileId (..), Loc (..), Marker (..), Reporter (..), Severity (..))
import FNotation.FNtn as FNtn
import FNotation.Prelude (ADoc)
import FNotation.Span
import Prettyprinter
import Sifaka.Common
import Sifaka.Evaluation
import Sifaka.Syntax (Locals (..))
import Sifaka.Syntax qualified as S
import Sifaka.Value (Env (..))
import Sifaka.Value qualified as V
import Prelude hiding (error, lookup)

data MetaEnv = MetaEnv Int

data LocalInfo = LocalInfo
  { localInfoName :: Name,
    localInfoIdx :: FwdIdx,
    localInfoTy :: V.Ty
  }

data ScopeEntry
  = SENil
  | SELocal LocalInfo ScopeEntry

type Scope = Dictionary (PrimState IO) VM.MVector Name VM.MVector ScopeEntry

type LenArg = (?len :: FwdIdx)

type LocalsArg = (?locals :: S.Locals)

data State = State
  { stateReporter :: Reporter,
    stateFileId :: FileId,
    stateMetaEnv :: IORef MetaEnv,
    stateScope :: Scope
  }

type StateArg = (?state :: State)

type Elab a = StateArg => LenArg => EnvArg => LocalsArg => a

scopeDefineLocal :: (StateArg) => FwdIdx -> Name -> V.Ty -> IO a -> IO a
scopeDefineLocal i x ty act = do
  let l = LocalInfo x i ty
  let scope = stateScope ?state
  old <- fromMaybe SENil <$> HT.lookup scope x
  HT.insert scope x (SELocal l old)
  res <- act
  HT.insert scope x old
  pure res

define :: Name -> S.Ty -> V.Ty -> S.Tm -> V.Tm -> Elab (IO a) -> Elab (IO a)
define x tyS tyV tmS tmV act =
  let i = ?len
   in let ?len = ?len + 1
          ?env = EDef ?env tmV
          ?locals = LDef ?locals x tmS tyS
       in scopeDefineLocal i x tyV act

bind :: Name -> S.Ty -> V.Ty -> Elab (IO a) -> Elab (IO a)
bind x tyS tyV act =
  let i = ?len
      tmV = V.Neu (V.NVar i)
   in let ?len = ?len + 1
          ?env = EDef ?env tmV
          ?locals = LBind ?locals x tyS
       in scopeDefineLocal i x tyV act

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

report :: (StateArg) => Diagnostic -> IO ()
report d = do
  let (Reporter r) = stateReporter ?state
  r d

error :: (StateArg) => Span -> ADoc -> IO ()
error s msg = do
  let l = Loc (stateFileId ?state) s
  let m = Marker l Here
  let d = Diagnostic ErrorS msg "" [m]
  report d

lookup :: Elab (Name -> IO ScopeEntry)
lookup x = do
  let scope = stateScope ?state
  fromMaybe SENil <$> HT.lookup scope x

-- getVar :: S.Locals -> FwdIdx -> BwdIdx -> V.Tm
-- getVar S.LNil _ _ = impossible
-- getVar (S.LDef l _ tm _) i j
--   | j == 0 = tm
--   | otherwise = getVar l (i-1) (j-1)
-- getVar (S.LBind l name _) i j
--   | j == 0 = V.Var i name
--   | otherwise = getVar l (i-1) (j-1)

-- freshMeta :: Elab (IO MetaVar)
-- freshMeta = atomicModifyIORef ?metaEnv (\(MetaEnv i) -> (MetaEnv (i + 1), MetaVar i))

-- -- Create a new type in an inferring context
-- checkTyMeta :: Elab (IO Ty)
-- checkTyMeta = do
--   mv <- freshMeta
--   -- NOTE: this is incorrect; we should actually pull out the spine from the
--   -- curent environment
--   pure $ Ty (S.TInsertedMeta mv (ctxBDs ?ctx)) (V.TFlex mv V.SId)

-- -- Create a new meta in a checking context
-- checkMeta :: Elab (IO Tm)
-- checkMeta = do
--   mv <- freshMeta
--   -- NOTE: this is incorrect; we should actually pull out the spine from the
--   -- curent environment
--   pure $ Tm (S.InsertedMeta mv (ctxBDs ?ctx)) (V.Flex mv V.SId)

-- -- Create a new meta in an inferring context
-- inferMeta :: Elab (IO (Tm, V.Ty))
-- inferMeta = do
--   tmM <- freshMeta
--   tyM <- freshMeta
--   pure $ (Tm (S.InsertedMeta tmM (ctxBDs ?ctx)) (V.Flex tmM V.SId), V.TFlex tyM V.SId)

-- checkTy :: Elab (FNtn -> IO Ty)
-- checkTy (L _ (Keyword "Double")) = pure $ Ty S.Double V.Double
-- checkTy (L _ (Keyword "Nat")) = pure $ Ty S.Nat V.Nat
-- checkTy (L _ (App1 (L _ (Keyword "Fin")) nN)) = do
--   (Tm nS nV) <- check V.Nat nN
--   pure $ Ty (S.Fin nS) (V.Fin nV)
-- checkTy (L _ (App2 (L _ (Keyword "=>")) domN codN)) = do
--   (Tm domS domV) <- check V.Nat domN
--   (Ty codS codV) <- checkTy codN
--   pure $ Ty (S.Arr domS codS) (V.Arr domV codV)
-- checkTy (L s _) = do
--   error s "unexpected notation for type"
--   checkTyMeta

-- unifyTy :: Elab (V.Ty -> V.Ty -> IO Bool)
-- unifyTy V.Double V.Double = pure True
-- unifyTy V.Nat V.Nat = pure True
-- unifyTy (V.Arr dom1 cod1) (V.Arr dom2 cod2) =
--   (&&) <$> unify dom1 dom2 <*> unifyTy cod1 cod2
-- unifyTy (V.Fin n1) (V.Fin n2) = unify n1 n2
-- unifyTy _ _ = pure False

-- unify :: Elab (V.Tm -> V.Tm -> IO Bool)
-- unify (V.Lit l1) (V.Lit l2) = pure $ l1 == l2
-- unify (V.Var i1 _) (V.Var i2 _) = pure $ i1 == i2
-- unify V.Opaque V.Opaque = pure True
-- unify _ _ = impossible

-- check :: Elab (V.Ty -> FNtn -> IO Tm)
-- check ty (L s (Int i)) = case ty of
--   V.Double -> pure $ Tm (S.Lit (S.LitDouble (fromIntegral i))) V.Opaque
--   V.Nat -> let n = fromIntegral i in
--     pure $ Tm (S.Lit (S.LitNat n)) (V.Lit (V.LitNat n))
--   V.Fin n -> let j = fromIntegral i in
--     pure $ Tm (S.Lit (S.LitFin j)) V.Opaque
--   _ -> do
--     error s "can only check integer against Double or Nat"
--     checkMeta
-- check ty (L s (App2 (L _ (Keyword "↦")) argNameN bodyN)) = case ty of
--   V.Arr dom cod -> do
--     let domS = quote dom
--     let codS = quoteTy cod
--     runMaybeT (asName argNameN) >>= \case
--       Just name ->
--         let ?ctx = bind ?ctx name (V.Fin dom) in do
--           (Tm bodyS _) <- check cod bodyN
--           pure $ Tm (S.ArrLam name domS codS bodyS) V.Opaque
--       Nothing -> do
--         error (FNtn.span argNameN) "expected an ident"
--         checkMeta
--   _ -> do
--     error s "can only check lambda expression against array type"
--     checkMeta
-- check ty (L s (App1 (L _ (Prim "cast")) tmN)) = do
--   (Tm tmS tmV, inferred) <- infer tmN
--   unifyTy ty inferred >>= \unifies -> if unifies
--     then pure (Tm tmS tmV)
--     else case (ty, inferred) of
--       (V.Double, V.Fin _) -> pure $ Tm (S.IToF tmS) V.Opaque
--       (V.Double, V.Nat) -> pure $ Tm (S.IToF tmS) V.Opaque
--       _ -> do
--         error s "cannot cast"
--         checkMeta
-- check ty n@(L s _) = do
--   (tm, synthed) <- infer n
--   unifies <- unifyTy ty synthed
--   if unifies
--     then pure tm
--     else do
--       error s "inferred type could not be unified with type being checked against"
--       checkMeta

-- ops :: Map Text S.BinOp
-- ops =
--   Map.fromList
--     [ ("+", S.Add),
--       ("-", S.Sub),
--       ("*", S.Mul),
--       ("/", S.Div)
--     ]

-- gatherApp1s :: FNtn -> [FNtn] -> (FNtn, [FNtn])
-- gatherApp1s (L _ (App1 f x)) args = gatherApp1s f (x : args)
-- gatherApp1s n args = (n, args)

-- checkArgs :: Elab ([FNtn] -> [(Name, S.Ty)] -> [S.Tm] -> IO (Ctx, [S.Tm]))
-- checkArgs [] [] args = pure $ (?ctx, reverse args)
-- checkArgs (argN : argNs) ((name, argTy) : argTys) args = do
--   let ty = evalTy argTy
--   arg <- check ty argN
--   let ?ctx = define ?ctx name (tmVal arg) ty
--    in checkArgs argNs argTys (tmStx arg : args)
-- checkArgs _ _ _ = impossible

-- lookupFunc :: Elab (Span -> Name -> MaybeT IO (S.GlobalId S.Func, S.Func))
-- lookupFunc s name = case (S.moduleFuncs ?module) Map.!? name of
--   Just f -> pure (S.GlobalId name, f)
--   Nothing -> do
--     liftIO $ error s $ "no such function" <+> pretty name
--     empty

-- orInferMeta :: Elab (MaybeT IO (Tm, V.Ty) -> IO (Tm, V.Ty))
-- orInferMeta action =
--   runMaybeT action >>= \case
--     Just res -> pure res
--     Nothing -> inferMeta

-- infer :: Elab (FNtn -> IO (Tm, V.Ty))
-- infer (L s (Ident n)) = do
--   let name = Name n
--   case lookup name of
--     Just (i, tm, ty) -> pure $ (Tm (S.LocalVar (S.Id i name)) tm, ty)
--     Nothing -> do
--       error s ("no such variable" <+> pretty name)
--       inferMeta
-- infer n@(L _ (App1 _ _)) = do
--   let (fN, argNs) = gatherApp1s n []
--   orInferMeta $ do
--     name <- asName fN
--     (fid, fdef) <- lookupFunc (FNtn.span fN) name
--     (ctx, args) <- liftIO $ checkArgs argNs (S.funcArgs fdef) []
--     let ?ctx = ctx
--      in let ty = evalTy (S.funcRetTy fdef)
--             val = eval (S.funcBody fdef)
--          in pure (Tm (S.TopApp fid args) val, ty)
-- infer (L s (App2 (L _ (Keyword opName)) n1 n2))
--   | opName == "!" = do
--     (Tm aS _, aTy) <- infer n1
--     case aTy of
--       V.Arr dom cod -> do
--         Tm iS _ <- check (V.Fin dom) n2
--         let codS = quoteTy cod
--         pure (Tm (S.Index aS codS iS) V.Opaque, cod)
--       _ -> do
--         error (FNtn.span n1) "expected variable of array type"
--         inferMeta
--   | otherwise = do
--     t1 <- check V.Double n1
--     t2 <- check V.Double n2
--     case ops Map.!? opName of
--       Just op -> pure (binop op t1 t2, V.Double)
--       Nothing -> do
--         error s $ "unrecognized operator" <+> pretty opName
--         inferMeta
-- infer (L s (Int _)) = do
--   error s "integer literal must occur in checking position"
--   inferMeta
-- infer (L s _) = do
--   error s "unexpected notation for term"
--   inferMeta

-- asDefinition :: Elab (FNtn -> MaybeT IO (FNtn, FNtn))
-- asDefinition (L _ (App2 (L _ (Keyword "=")) lhs rhs)) = pure (lhs, rhs)
-- asDefinition (L s _) = do
--   liftIO $ error s "expected definition <pattern> = <term>"
--   empty

-- asBinding :: Elab (FNtn -> MaybeT IO (FNtn, FNtn))
-- asBinding (L _ (App2 (L _ (Keyword ":")) lhs rhs)) = pure (lhs, rhs)
-- asBinding (L s _) = do
--   liftIO $ error s "expected binding <term> : <type>"
--   empty

-- asApplication :: Elab (FNtn -> MaybeT IO (Name, [FNtn]))
-- asApplication n = go n []
--   where
--     go (L _ (Ident name)) args = pure (Name name, args)
--     go (L _ (App1 f x)) args = go f (x : args)
--     go (L s _) _ = do
--       liftIO $ error s "expected an application of a name to arguments"
--       empty

-- asName :: Elab (FNtn -> MaybeT IO Name)
-- asName (L _ (Ident name)) = pure $ Name name
-- asName (L s _) = do
--   liftIO $ error s "expected a name"
--   empty

-- elabArgs :: Elab ([FNtn] -> [(Name, S.Ty)] -> MaybeT IO ([(Name, S.Ty)], Ctx))
-- elabArgs [] args = pure (reverse args, ?ctx)
-- elabArgs (argN : rest) args = do
--   (nameN, tyN) <- asBinding argN
--   name <- asName nameN
--   (Ty tyS tyV) <- liftIO $ checkTy tyN
--   let ?ctx = bind ?ctx name tyV
--    in elabArgs rest ((name, tyS) : args)

-- topDecl :: Elab (FNtnTop -> IO (Maybe S.TopDecl))
-- topDecl (FNtnTop "def" _ n) = runMaybeT $ do
--   (pat, bodyN) <- asDefinition n
--   (app, retTyN) <- asBinding pat
--   (name, argNs) <- asApplication app
--   (args, ctx) <- elabArgs argNs []
--   let ?ctx = ctx
--    in do
--         (Ty retTyS retTyV) <- liftIO $ checkTy retTyN
--         (Tm bodyS _) <- liftIO $ check retTyV bodyN
--         pure $ S.TFunc (S.Func name args retTyS bodyS)
-- topDecl (FNtnTop "eval" _ n) = do
--   (tm, ty) <- infer n
--   pure $ Just $ S.TEval $ S.Eval (tmStx tm) (quoteTy ty)
-- topDecl (FNtnTop name s _) = do
--   error s $ "unexpected toplevel \"" <> pretty name <> "\""
--   pure Nothing

-- doModule :: Elab ([FNtnTop] -> IO S.Module)
-- doModule [] = pure ?module
-- doModule (tn : rest) =
--   topDecl tn >>= \case
--     Just (S.TFunc f) ->
--       let ?module = S.addFunc ?module f in doModule rest
--     Just (S.TEval tm) ->
--       let ?module = S.addEval ?module tm in doModule rest
--     Nothing -> doModule rest

-- elabModule :: Reporter -> FileId -> [FNtnTop] -> IO S.Module
-- elabModule reporter fileId tns = do
--   metaIORef <- newIORef (MetaEnv 0)
--   let ?reporter = reporter
--       ?fileId = fileId
--       ?metaEnv = metaIORef
--       ?module = S.emptyModule
--       ?ctx = Ctx 0 LNil BwdNil
--   doModule tns
