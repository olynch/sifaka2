module FNotation.Prelude (Map, Vector, Set, IntMap, ADoc, Text, ByteString, module FNotation.Util) where

import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)
import FNotation.Util
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Data.Text (Text)
import Data.ByteString (ByteString)

type ADoc = Doc AnsiStyle
