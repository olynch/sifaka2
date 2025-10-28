{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Prelude hiding (lex)
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import FNotation
import FNotation.Diagnostic qualified as FD
import Options.Applicative (argument, execParser, fullDesc, header, helper, info, metavar, progDesc, str, (<**>))
import Options.Applicative qualified as Opts
import Sifaka.Elab
import Sifaka.CodeGen

data Args = Args {argsSourceName :: FilePath, argsOut :: FilePath}

argsParser :: Opts.Parser Args
argsParser = Args <$> argument str (metavar "SOURCE") <*> argument str (metavar "OUT")

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
    { keywords = Set.fromList ["+", "-", "*", "/", "=", ":", "Double"],
      topdecls = Set.fromList ["def"],
      precedences = Map.fromList [
        ("=", Prec 10 NonA),
        (":", Prec 20 NonA),
        ("+", Prec 50 LeftA),
        ("*", Prec 60 LeftA)
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
  decls <- elabTop reporter fileId tns
  genTop decls (argsOut args)
