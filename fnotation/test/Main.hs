module Main (main) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as Map
import Data.Set qualified as Set
import FNotation.Diagnostic
    ( FileId,
      Reporter,
      pprDiagnostic,
      defaultStyle,
      ioRefReporter,
      insertFile,
      emptyFiles,
      newFile )
import FNotation.Lexer (lex)
import FNotation.Config (Assoc (..), Prec (Prec), FNotationConfig (FNotationConfig))
import FNotation.Parser (parse)
import FNotation.Token (tokensPretty)
import FNotation.Util (insertionPoint)
import FNotation.Prelude
import Prettyprinter
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.SmallCheck qualified as SC
import Prelude hiding (lex)
import Data.IORef
import Prettyprinter.Render.Text (renderStrict)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.List qualified as List

main :: IO ()
main = do
  gt <- goldenTests
  let allTests = testGroup "all tests" [gt, scTests]
  defaultMain allTests

keywords :: Set ByteString
keywords = Set.fromList ["+", "*"]

precs :: Map Text Prec
precs = Map.fromList [("+", Prec 50 LeftA), ("*", Prec 60 LeftA)]

config :: FNotationConfig
config = FNotationConfig keywords precs

runWithReporter :: FilePath -> (Reporter -> ByteString -> FileId -> IO ADoc) -> IO LBS.ByteString
runWithReporter p f = do
  contents <- BS.readFile p
  let file = newFile p contents
  let (files, fileId) = insertFile file emptyFiles
  ref <- newIORef []
  let reporter = ioRefReporter ref
  result <- f reporter contents fileId
  diagnostics <- readIORef ref
  let doc = vsep $ reverse $ (result:) $ pprDiagnostic defaultStyle files <$> diagnostics
  return $ LBS.fromStrict $ TE.encodeUtf8 $ renderStrict $ layoutPretty defaultLayoutOptions doc

runLex :: FilePath -> IO LBS.ByteString
runLex p = do
  runWithReporter p $ \r contents _ -> do
    tokens <- lex r config contents
    return $ tokensPretty tokens

runParse :: FilePath -> IO LBS.ByteString
runParse p = do
  runWithReporter p $ \r contents fileId -> do
    tokens <- lex r config contents
    n <- parse config r fileId tokens contents
    return $ pretty n

goldenTests :: IO TestTree
goldenTests = do
  fntnFiles <- findByExtension [".fntn"] "."
  return $
    testGroup
      "golden tests"
      [ testGroup
          "lexing tests"
          [goldenVsString (takeBaseName fntnFile) outputFile (runLex fntnFile) | fntnFile <- fntnFiles, let outputFile = replaceExtension fntnFile ".lexed"],
        testGroup
          "parsing tests"
          [goldenVsString (takeBaseName fntnFile) outputFile (runParse fntnFile) | fntnFile <- fntnFiles, let outputFile = replaceExtension fntnFile ".parsed"]
      ]

scTests :: TestTree
scTests =
  testGroup
    "smallcheck tests"
    [
      SC.testProperty "insertionPoint xs x <= length xs" $
        \xs' x ->
          let xs = V.fromList xs'
           in insertionPoint xs (x :: Int) <= length xs,
      SC.testProperty "0 < i < length xs --> xs!(i - 1) <= x <= xs!i" $
        \xs' x ->
          let xs = V.fromList (List.sort xs')
           in let i = insertionPoint xs (x :: Int)
               in (i >= V.length xs) || (i == 0) || (xs V.! (i - 1) <= x && x <= xs V.! i)
    ]
