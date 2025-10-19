module FNotation.Parser.ParseState (ParseState (..)) where

import Data.ByteString (ByteString)
import FNotation.Config (FNotationConfig)
import FNotation.Diagnostic (FileId, Reporter)
import FNotation.Token (Token)

data ParseState = ParseState
  { config :: FNotationConfig,
    tokens :: [Token],
    source :: ByteString,
    closePos :: Int,
    endPos :: Int,
    gas :: Int,
    fileId :: FileId,
    reporter :: Reporter
  }
