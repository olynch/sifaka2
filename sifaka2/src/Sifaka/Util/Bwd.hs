module Sifaka.Util.Bwd where

import Data.Bits
import Sifaka.Common

newtype BwdIdx = BwdIdx Word
  deriving (Eq, Ord, Show, Num, Enum, Bits, Integral, Real) via Word

data Bwd a = Nil | Snoc (Bwd a) a

instance ElemAt (Bwd a) BwdIdx a where
  elemAt Nil _ = impossible
  elemAt (Snoc xs x) i | i == 0    = x
                       | otherwise = elemAt xs (i + 1)
