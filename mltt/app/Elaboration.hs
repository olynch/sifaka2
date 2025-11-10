{-# LANGUAGE DeriveAnyClass, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Elaboration where

import Common
import Control.Exception
import Data.IntMap qualified as IM
import Data.String (fromString)
import Data.Vector.Hashtables
import Data.Vector.Hashtables qualified as HT
import Data.Vector.Mutable qualified as VM

import FNotation.FNtn hiding (Error)

import Evaluation
import Syntax (Locals (..), LocalsArg)
import Syntax qualified as S
import Unification
import Value qualified as V

data PRen = PRen
  { pRenDom :: FwdIdx,
    pRenCod :: FwdIdx,
    pRenBody :: IM.IntMap FwdIdx
  }

lift :: PRen -> PRen
lift (PRen dom cod body) =
  PRen (dom + 1) (cod + 1) (IM.insert (unFwdIdx cod) dom body)

data UnifyError = UnifyError
  deriving (Show, Exception)

data ElabError = NameNotInScope Name | CantUnify V.Tm V.Tm | UnexpectedNotation
  deriving (Show, Exception)

data Error = Error Locals ElabError
  deriving (Show, Exception)

invert :: MetaCtxArg => LenArg => V.Spine -> IO PRen
invert sp = do
  let go :: V.Spine -> IO (FwdIdx, IM.IntMap FwdIdx)
      go V.SId = pure (0, mempty)
      go (V.SApp sp t) = do
        (dom, ren) <- go sp
        case force t of
          V.Rigid (FwdIdx x) V.SId
            | IM.notMember x ren ->
                pure (dom + 1, IM.insert x dom ren)
          _ -> throwIO UnifyError
  (dom, ren) <- go sp
  pure $ PRen dom ?len ren

rename :: MetaCtxArg => MetaVar -> PRen -> V.Tm -> IO S.Tm
rename m pren v = go pren v
  where
    goSp :: MetaCtxArg => PRen -> S.Tm -> V.Spine -> IO S.Tm
    goSp _ t V.SId = pure t
    goSp pren t (V.SApp sp u) = S.App <$> goSp pren t sp <*> go pren u

    go :: MetaCtxArg => PRen -> V.Tm -> IO S.Tm
    go pren t = case force t of
      V.Flex m' sp
        | m == m' -> throwIO UnifyError
        | otherwise -> goSp pren (S.Meta m') sp
      V.Rigid (FwdIdx x) sp -> case IM.lookup x (pRenBody pren) of
        Nothing -> throwIO UnifyError
        Just x' -> goSp pren (S.LocalVar $ readbWith (pRenDom pren) x') sp
      V.U -> pure S.U
      V.Lam x t -> S.Lam x <$> go (lift pren) (t (V.Var (pRenCod pren)))
      V.Pi x a b -> S.Pi x <$> go pren a <*> go (lift pren) (b (V.Var (pRenCod pren)))

lams :: FwdIdx -> S.Tm -> S.Tm
lams l = go 0
  where
    go x t | x == l = t
    go x t = S.Lam (fromString $ "x" ++ show (x + 1)) $ go (x + 1) t

solve :: MetaCtxArg => LenArg => MetaVar -> V.Spine -> V.Tm -> IO ()
solve m sp rhs = do
  pren <- invert sp
  rhs <- rename m pren rhs
  let solution = evalIn BwdNil $ lams (pRenDom pren) rhs
  writeMeta m (Solved solution)

unifyIntro :: MetaCtxArg => LenArg => (V.Tm -> V.Tm) -> (V.Tm -> V.Tm) -> IO ()
unifyIntro t t' =
  let l = ?len in let ?len = ?len + 1 in unify (t (V.Var l)) (t' (V.Var l))

unifySp :: MetaCtxArg => LenArg => V.Spine -> V.Spine -> IO ()
unifySp sp sp' = case (sp, sp') of
  (V.SId, V.SId) -> pure ()
  (V.SApp sp t, V.SApp sp' t') -> do
    unifySp sp sp'
    unify t t'
  _ -> throwIO UnifyError

unify :: MetaCtxArg => LenArg => V.Tm -> V.Tm -> IO ()
unify t t' = case (force t, force t') of
  (V.Lam _ t, V.Lam _ t') -> unifyIntro t t'
  (t, V.Lam _ t') -> unifyIntro (app t) t'
  (V.Lam _ t, t') -> unifyIntro t (app t')
  (V.U, V.U) -> pure ()
  (V.Pi _ a b, V.Pi _ a' b') -> do
    unify a a'
    unifyIntro b b'
  (V.Rigid x sp, V.Rigid x' sp') | x == x' -> unifySp sp sp'
  (V.Flex m sp, V.Flex m' sp') | m == m' -> unifySp sp sp'
  (V.Flex m sp, t') -> solve m sp t'
  (t, V.Flex m' sp') -> solve m' sp' t
  _ -> throwIO UnifyError

data ScopeEntry
  = SENil
  | SELocal ScopeEntry FwdIdx V.Ty

type Scope = Dictionary (PrimState IO) VM.MVector Name VM.MVector ScopeEntry

type ScopeArg = (?scope :: Scope)

type Elab a = MetaCtxArg => ScopeArg => LocalsArg => LenArg => EnvArg => a

unifyCatch :: Elab (V.Tm -> V.Tm -> IO ())
unifyCatch t t' = do
  unify t t'
  `catch` \UnifyError ->
    throwIO $ Error ?locals (CantUnify t t')

findInScope :: ScopeArg => Name -> IO ScopeEntry
findInScope x = HT.lookup ?scope x >>= \case
  Just se -> pure se
  Nothing -> pure SENil


scopeDefineLocal :: ScopeArg => FwdIdx -> Name -> V.Ty -> IO a -> IO a
scopeDefineLocal i x ty act = do
  old <- findInScope x
  HT.insert ?scope x (SELocal old i ty)
  res <- act
  HT.insert ?scope x old
  pure res

bind :: Name -> V.Ty -> Elab (V.Tm -> IO a) -> Elab (IO a)
bind x a act =
  let
    i = ?len
    v = V.Var i
  in let
    ?env = ?env :> v
    ?len = ?len + 1
    ?locals = LBind ?locals x
  in
    scopeDefineLocal i x a (act v)

def :: Name -> S.Tm -> V.Tm -> V.Ty -> Elab (IO a) -> Elab (IO a)
def x t v a act =
  let i = ?len in let
    ?env = ?env :> v
    ?len = ?len + 1
    ?locals = LDef ?locals x t
  in
    scopeDefineLocal i x a act

syn :: Elab (FNtn -> IO (S.Tm, V.Ty))
syn (L _ n0) = case n0 of
  Ident "_" -> do
    a <- eval <$> freshMeta
    t <- freshMeta
    pure (t, a)
  Ident rx ->
    let x = Name rx in
    findInScope x >>= \case
      SELocal _ i ty -> pure (S.LocalVar (readb i), ty)
      SENil -> throw $ Error ?locals (NameNotInScope x)
  Keyword "U" -> pure (S.U, V.U)
  App2
    (L _ (Keyword "->"))
    (L _ (App2 (L _ (Keyword ":")) (L _ (Ident rx)) an))
    bn -> do
    let x = Name rx
    a <- chk V.U an
    let va = eval a
    b <- bind x va $ \_ -> chk V.U bn
    pure (S.Pi x a b, V.U)
  App2 (L _ (Keyword "->")) an bn -> do
    let x = Name "_"
    a <- chk V.U an
    let va = eval a
    b <- bind x va $ \_ -> chk V.U bn
    pure (S.Pi x a b, V.U)
  App2 (L _ (Keyword "=>")) (L _ (Ident rx)) bodyN -> do
    let x = Name rx
    a <- eval <$> freshMeta
    (bodyS, bS) <- bind x a $ \_ -> do
      (bodyS, bV) <- syn bodyN
      pure (bodyS, readb bV)
    pure (S.Lam x bodyS, V.Pi x a $ \v -> let ?env = ?env :> v in eval bS)
  App1 fn xn -> do
    (f, fty) <- syn fn
    (dom, cod) <- case force fty of
      V.Pi _ dom cod -> pure (dom, cod)
      fty -> do
        dom <- eval <$> freshMeta
        cod <- do
          m <- eval <$> freshMeta
          pure $ \x -> app (force m) x
        unifyCatch fty (V.Pi "x" dom cod)
        pure (dom, cod)

    x <- chk dom xn
    pure (S.App f x, cod (eval x))
  Block bindings (Just retN) -> go bindings BwdNil where
    go [] bound = do
      (retS, retTy) <- syn retN
      pure (S.Block (bwdToList bound) retS, retTy)
    go (n:rest) bound = do
      let (x, tn) = case n of
            L _ (App2 (L _ (Keyword "=")) (L _ (Ident rx)) tn) -> (Name rx, tn)
            _ -> throw $ Error ?locals UnexpectedNotation
      (t, tty) <- syn tn
      def x t (eval t) tty $ go rest (bound :> (x, t))
  _ -> throw $ Error ?locals UnexpectedNotation

chk :: Elab (V.Ty -> FNtn -> IO S.Tm)
chk a n@(L _ n0) = case (n0, force a) of
  (App2 (L _ (Keyword "=>")) (L _ (Ident rx)) t, V.Pi _ a b) -> do
    let x = Name rx
    S.Lam x <$> (bind x a $ \v -> chk (b v) t)
  (Block bindings (Just retN), a) -> go bindings BwdNil where
    go [] bound = do
      retS <- chk a retN
      pure $ S.Block (bwdToList bound) retS
    go (n:rest) bound = do
      let (x, tn) = case n of
            L _ (App2 (L _ (Keyword "=")) (L _ (Ident rx)) tn) -> (Name rx, tn)
            _ -> throw $ Error ?locals UnexpectedNotation
      (t, tty) <- syn tn
      def x t (eval t) tty $ go rest (bound :> (x, t))
  _ -> do
    (t, b) <- syn n
    unifyCatch a b
    pure t

asDefinition :: Elab (FNtn -> IO (FNtn, FNtn))
asDefinition (L _ (App2 (L _ (Keyword "=")) lhs rhs)) = pure (lhs, rhs)
asDefinition (L _ _) = throwIO $ Error ?locals UnexpectedNotation

asBinding :: Elab (FNtn -> IO (FNtn, FNtn))
asBinding (L _ (App2 (L _ (Keyword ":")) lhs rhs)) = pure (lhs, rhs)
asBinding (L _ _) = throwIO $ Error ?locals UnexpectedNotation

asApplication :: Elab (FNtn -> IO (Name, [FNtn]))
asApplication n = go n []
  where
    go (L _ (Ident name)) args = pure (Name name, args)
    go (L _ (App1 f x)) args = go f (x : args)
    go (L _ _) _ = throwIO $ Error ?locals UnexpectedNotation

asName :: Elab (FNtn -> IO Name)
asName (L _ (Ident name)) = pure $ Name name
asName (L _ _) = throwIO $ Error ?locals UnexpectedNotation

elabWithArgs :: forall a. [FNtn] -> Elab ([(Name, S.Ty)] -> IO a) -> Elab (IO a)
elabWithArgs ns act = go ns [] where
  go :: Elab ([FNtn] -> [(Name, S.Ty)] -> IO a)
  go [] args = act (reverse args)
  go (argN : rest) args = do
    (nameN, tyN) <- asBinding argN
    name <- asName nameN
    tyS <- chk V.U tyN
    let tyV = eval tyS
    bind name tyV $ \_ -> go rest ((name, tyS) : args)

wrapLams :: S.Tm -> [(Name, S.Ty)] -> S.Tm
wrapLams t [] = t
wrapLams t ((name, _):xs) = S.Lam name (wrapLams t xs)

wrapPis :: S.Ty -> [(Name, S.Ty)] -> S.Ty
wrapPis b [] = b
wrapPis b ((name, a):xs) = S.Pi name a (wrapPis b xs)

topDecl :: Elab (FNtnTop -> IO (Name, S.Ty, S.Tm))
topDecl (FNtnTop "def" _ n) = do
  (pat, bodyN) <- asDefinition n
  (app, retTyN) <- asBinding pat
  (name, argNs) <- asApplication app
  elabWithArgs argNs $ \args -> do
   do
     retTyS <- chk V.U retTyN
     let retTyV = eval retTyS
     bodyS <- chk retTyV bodyN
     pure (name, wrapPis retTyS args, wrapLams bodyS args)
topDecl _ = throwIO $ Error ?locals UnexpectedNotation

topDecls :: Elab ([FNtnTop] -> IO ())
topDecls [] = pure ()
topDecls (tn:rest) = do
  (name, a, t) <- topDecl tn
  let v = eval t
  let av = eval a
  unsolvedMetas >>= \case
    True -> error $ "unsolved metas in " ++ show name
    False -> pure ()
  def name t v av $ topDecls rest

elabTop :: [FNtnTop] -> IO ()
elabTop tns = do
  mctx <- newMetaCtx
  scope <- HT.initialize 10
  let
    ?mctx = mctx
    ?scope = scope
    ?locals = LNil
    ?len = 0
    ?env = BwdNil
    in topDecls tns
