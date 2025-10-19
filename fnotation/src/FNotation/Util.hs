module FNotation.Util where

import Data.ByteString (ByteString)
import Data.Text.Encoding as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Prettyprinter

prettyBS :: ByteString -> Doc ann
prettyBS = pretty . TE.decodeUtf8

insertionPoint :: (Ord a) => Vector a -> a -> Int
insertionPoint xs x
  | V.null xs = 0
  | otherwise =
      let m = V.length xs `div` 2
          p = xs V.! m
       in if x <= p
            then insertionPoint (V.take m xs) x
            else m + 1 + insertionPoint (V.drop (m + 1) xs) x

charLen :: Char -> Int
charLen c = case fromEnum c of
  x
    | x <= 0x7F -> 1
    | x <= 0x7FF -> 2
    | x <= 0xFFFF -> 3
    | otherwise -> 4
