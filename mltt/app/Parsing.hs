module Parsing where

import FNotation
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text

mlttPrecedences :: Map.Map Text Prec
mlttPrecedences = Map.fromList
  [ ("=", Prec 10 NonA),
    (":", Prec 20 NonA),
    ("=>", Prec 30 RightA),
    ("->", Prec 30 RightA),
    ("↦", Prec 40 RightA),
    ("+", Prec 50 LeftA),
    ("*", Prec 60 LeftA),
    ("!", Prec 100 RightA)
  ]

fnotationConfig :: FNotationConfig
fnotationConfig =
  FNotationConfig
    { keywords = Set.fromList ["+", "-", "*", "/", "=", ":", "U", "=>", "->", "↦", "!"],
      topdecls = Set.fromList ["def", "eval"],
      precedences = mlttPrecedences
    }
