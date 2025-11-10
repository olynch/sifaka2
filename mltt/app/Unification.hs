{-# LANGUAGE DeriveAnyClass #-}
module Unification where

import Common
import Data.Array.Dynamic.L as ADL
import Value as V
import Syntax as S

data MetaEntry = Solved V.Tm | Unsolved

type MetaCtx = ADL.Array MetaEntry

newMetaCtx :: IO MetaCtx
newMetaCtx = ADL.empty

type MetaCtxArg = (?mctx :: MetaCtx)

lookupMeta :: (MetaCtxArg) => MetaVar -> MetaEntry
lookupMeta (MetaVar i) = runIO $ ADL.read ?mctx i

writeMeta :: (MetaCtxArg) => MetaVar -> MetaEntry -> IO ()
writeMeta (MetaVar i) e = ADL.write ?mctx i e

freshMeta :: MetaCtxArg => LocalsArg => IO S.Tm
freshMeta = do
  let go l t i = case l of
        LNil -> t
        LBind l' _  -> go l' (S.App t (S.LocalVar i)) (i + 1)
        LDef l' _ _ -> go l' t (i + 1)
  s <- ADL.size ?mctx
  ADL.push ?mctx Unsolved
  pure $ go ?locals (S.Meta (MetaVar s)) 0

unsolvedMetas :: MetaCtxArg => IO Bool
unsolvedMetas = ADL.foldl'
  (\hasUnsolved -> \case
      Solved _ -> hasUnsolved
      Unsolved -> True)
  False
  ?mctx
