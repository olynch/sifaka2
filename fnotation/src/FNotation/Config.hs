module FNotation.Config (Assoc (..), Prec (..), FNotationConfig (..)) where

import FNotation.Prelude

data Assoc = LeftA | RightA | NonA
  deriving (Eq)

data Prec = Prec {tightness :: Int, assoc :: Assoc}
  deriving (Eq)

instance Ord Prec where
  (<=) p1 p2
    | tightness p1 < tightness p2 = True
    | tightness p1 == tightness p2 = case (assoc p1, assoc p2) of
        (LeftA, LeftA) -> False
        (RightA, RightA) -> True
        _ -> False
    | otherwise = False

data FNotationConfig = FNotationConfig
  { keywords :: Set ByteString,
    precedences :: Map Text Prec
  }
