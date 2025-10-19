module Sifaka.Common (Map, IntMap, Vector, Text, ElemAt(..), FwdIdx, Fwd(..), BwdIdx, Bwd(..), Name, Row(..)) where

import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.Vector (Vector)
import Data.Text (Text)
import Data.Bits

---------------------------------------------------------------------

#define DEBUG

#ifdef DEBUG
import GHC.Stack
#endif

#ifdef DEBUG

type Dbg = HasCallStack

#else

type Dbg = () :: Constraint

#endif

impossible :: Dbg => a
impossible = error "impossible"
{-# NOINLINE impossible #-}

---------------------------------------------------------------------

class ElemAt a i b | a -> i b where
  elemAt :: a -> i -> b

newtype FwdIdx = FwdIdx Word
  deriving (Eq, Ord, Show, Num, Enum, Bits, Integral, Real) via Word

data Fwd a = FwdNil | Cons a (Fwd a)

instance ElemAt (Fwd a) FwdIdx a where
  elemAt FwdNil _ = impossible
  elemAt (Cons x xs) i | i == 0    = x
                       | otherwise = elemAt xs (i + 1)

newtype BwdIdx = BwdIdx Word
  deriving (Eq, Ord, Show, Num, Enum, Bits, Integral, Real) via Word

data Bwd a = BwdNil | Snoc (Bwd a) a

instance ElemAt (Bwd a) BwdIdx a where
  elemAt BwdNil _ = impossible
  elemAt (Snoc xs x) i | i == 0    = x
                       | otherwise = elemAt xs (i + 1)


---------------------------------------------------------------------

type Name = Text

newtype Row a = Row [(Name, a)]

instance Functor Row where
  fmap f (Row entries) = Row (fmap (\(n,x) -> (n, f x)) entries)
