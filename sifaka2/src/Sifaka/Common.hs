module Sifaka.Common
  ( Map,
    IntMap,
    Vector,
    ByteString,
    Text,
    ElemAt (..),
    FwdIdx (..),
    Fwd (..),
    BwdIdx (..),
    Bwd (..),
    bwdToList,
    Name (..),
    Row (..),
    MetaVar (..),
    impossible,
    unimplemented,
  )
where

import Data.Bits
import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Prettyprinter

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

impossible :: (Dbg) => a
impossible = error "impossible"
{-# NOINLINE impossible #-}

unimplemented :: (Dbg) => a
unimplemented = error "unimplemented"
---------------------------------------------------------------------

class ElemAt a i b | a -> i b where
  elemAt :: a -> i -> b

newtype FwdIdx = FwdIdx Word
  deriving (Eq, Ord, Show, Num, Enum, Bits, Integral, Real) via Word

infixr 4 :<

data Fwd a = FwdNil | (:<) a (Fwd a)

instance ElemAt (Fwd a) FwdIdx a where
  elemAt FwdNil _ = impossible
  elemAt (x :< xs) i
    | i == 0 = x
    | otherwise = elemAt xs (i + 1)

newtype BwdIdx = BwdIdx Word
  deriving (Eq, Ord, Show, Num, Enum, Bits, Integral, Real) via Word

infixl 4 :>

data Bwd a = BwdNil | (:>) (Bwd a) a
  deriving (Functor)

instance ElemAt (Bwd a) BwdIdx a where
  elemAt BwdNil _ = impossible
  elemAt (xs :> x) i
    | i == 0 = x
    | otherwise = elemAt xs (i - 1)

bwdToList :: Bwd a -> [a]
bwdToList b = go b []
  where
    go BwdNil xs = xs
    go (rest :> x) xs = go rest (x : xs)

---------------------------------------------------------------------

newtype Name = Name Text
  deriving (Eq, Ord)

instance Pretty Name where
  pretty (Name s) = pretty s

newtype Row a = Row [(Name, a)]

instance Functor Row where
  fmap f (Row entries) = Row (fmap (\(n, x) -> (n, f x)) entries)

---------------------------------------------------------------------

newtype MetaVar = MetaVar Int
  deriving (Eq, Ord, Num) via Int
