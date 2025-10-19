module FNotation.Token where

import FNotation.Span (Span)
import FNotation.Span qualified as Span
import Prettyprinter
import FNotation.Prelude
import Prelude hiding (span)

data Tag
  = LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LCURLY
  | RCURLY
  | COMMA
  | SEMICOLON
  | INT
  | FLOAT
  | STRING
  | IDENT
  | KEYWORD
  | OP
  | KEYWORD_OP
  | FIELD
  | TAG
  | SPECIAL
  | PRIM
  | ERROR
  | EOF
  deriving (Show, Eq, Ord)

instance Pretty Tag where
  pretty = pretty . show

data Token = Token {tag :: Tag, span :: Span}

start :: Token -> Int
start = Span.start . span

end :: Token -> Int
end = Span.end . span

tokenPretty :: Token -> ADoc
tokenPretty t =
  pretty (show $ tag t)
    <> "["
    <> pretty (start t)
    <> ":"
    <> pretty (end t)
    <> "]"

tokensPretty :: [Token] -> ADoc
tokensPretty = hsep . fmap tokenPretty
