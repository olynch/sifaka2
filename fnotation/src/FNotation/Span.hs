module FNotation.Span (Span (..), slice) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

data Span = Span {start :: Int, end :: Int}
  deriving (Show)

slice :: Span -> ByteString -> ByteString
slice (Span s e) bs = BS.drop s $ BS.take e bs
