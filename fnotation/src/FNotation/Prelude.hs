module FNotation.Prelude (Map, Vector, Set, IntMap, ADoc, Text, ByteString, module FNotation.Util) where

import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import FNotation.Util
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

type ADoc = Doc AnsiStyle
