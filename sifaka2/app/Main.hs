{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (hPutBuilder)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import FNotation
import FNotation.Diagnostic qualified as FD
import Options.Applicative
  ( argument,
    execParser,
    flag,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    short,
    str,
    (<**>),
  )
import Options.Applicative qualified as Opts
import Sifaka.CodeGen
import Sifaka.Elab
import Sifaka.Qbe qualified as Qbe
import System.IO (stdout)
import System.Process
import Prelude hiding (lex)

data Args = Args {
  argsSourceName :: FilePath,
  argsOut :: FilePath,
  argsCodeGen :: Bool,
  argsRun :: Bool
}

argsParser :: Opts.Parser Args
argsParser =
  Args
    <$> argument str (metavar "SOURCE")
    <*> argument str (metavar "OUT")
    <*> flag False True (long "compile" <> short 'c' <> help "compile the program")
    <*> flag False True (long "run" <> short 'r' <> help "run the compiled program")

opts :: Opts.ParserInfo Args
opts =
  info
    (argsParser <**> helper)
    ( fullDesc
        <> progDesc "compile a sifaka file"
        <> header "sifaka2 - a compiler for pain-free numerical computing"
    )

fnotationConfig :: FNotationConfig
fnotationConfig =
  FNotationConfig
    { keywords = Set.fromList ["+", "-", "*", "/", "=", ":", "Double", "Nat", "=>", "↦", "!"],
      topdecls = Set.fromList ["def", "eval"],
      precedences =
        Map.fromList
          [ ("=", Prec 10 NonA),
            (":", Prec 20 NonA),
            ("=>", Prec 30 RightA),
            ("↦", Prec 40 RightA),
            ("+", Prec 50 LeftA),
            ("*", Prec 60 LeftA),
            ("!", Prec 100 RightA)
          ]
    }

main :: IO ()
main = do
  args <- execParser opts
  let sourceName = argsSourceName args
  source <- BS.readFile sourceName
  let files = FD.emptyFiles
  let file = FD.newFile sourceName source
  let (files', fileId) = FD.insertFile file files
  let reporter = FD.stderrReporter files'
  tokens <- lex reporter fnotationConfig source
  tns <- parseTop reporter fnotationConfig fileId tokens source
  coreMod <- elabModule reporter fileId tns
  if (argsCodeGen args)
    then do
      let irMod = genModule coreMod
      hPutBuilder stdout (Qbe.txt irMod)
      Qbe.compile irMod (argsOut args)
      if (argsRun args)
        then callProcess (argsOut args) []
        else pure ()
    else pure ()
