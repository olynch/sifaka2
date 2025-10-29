module FNotation.Lexer (lex) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState, get, modify, state)
import Control.Monad.Trans.State.Strict (StateT, execStateT)
import Data.ByteString.UTF8 as Utf8
import Data.Char qualified as Char
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Set qualified as Set
import Data.Vector qualified as V
import Data.Text.Encoding qualified as TE
import FNotation.Config (FNotationConfig)
import FNotation.Config qualified as Config
import FNotation.Diagnostic (HasReporter (getReporter), Reporter)
import FNotation.Prelude
import FNotation.Span (Span (Span))
import FNotation.Span qualified as Span
import FNotation.Token (Tag (..), Token (Token))
import Prelude hiding (lex)

data LexState = LexState
  { config :: FNotationConfig,
    orig :: ByteString,
    remaining :: ByteString,
    prev :: Int,
    cur :: Int,
    tokens :: [Token],
    reporter :: Reporter
  }

newtype Lex a = Lex {unLex :: StateT LexState IO a}
  deriving (Functor, Applicative, Monad, MonadState LexState, MonadIO)

instance HasReporter Lex where
  getReporter = get <&> \s -> reporter s

next :: Lex (Maybe Char)
next = state next'
  where
    next' :: LexState -> (Maybe Char, LexState)
    next' s = case Utf8.uncons $ remaining s of
      Just (c, bs) -> (Just c, s {remaining = bs, cur = cur s + charLen c})
      Nothing -> (Nothing, s)

slice :: Lex Text
slice = do
  s <- get <&> \s -> Span.slice (Span (prev s) (cur s)) (orig s)
  pure $ TE.decodeUtf8 s

peek :: Lex (Maybe Char)
peek = get <&> \s -> fst <$> Utf8.uncons (remaining s)

advance :: Lex ()
advance = modify $ \s -> case Utf8.uncons $ remaining s of
  Just (c, bs) -> s {cur = cur s + charLen c, remaining = bs}
  Nothing -> s

emit :: Tag -> Lex ()
emit t = modify $ \s ->
  s
    { prev = cur s,
      tokens = Token t (Span (prev s) (cur s)) : tokens s
    }

skip :: Lex ()
skip = modify (\s -> s {prev = cur s})

lex :: Reporter -> FNotationConfig -> ByteString -> IO [Token]
lex r config bs = do
  let s = LexState config bs bs 0 0 [] r
  s' <- execStateT (unLex run) s
  pure $ reverse $ tokens s'

opChars :: Vector Char
opChars = V.fromList ['+', '*', ':', '=', '/', '>', '<', '↦', '!', '&']

run :: Lex ()
run = do
  mc <- next
  for_ mc (\c -> lex1 c >> run)

lex1 :: Char -> Lex ()
lex1 '(' = emit LPAREN
lex1 ')' = emit RPAREN
lex1 '[' = emit LBRACK
lex1 ']' = emit RBRACK
lex1 '{' = emit LCURLY
lex1 '}' = emit RCURLY
lex1 ',' = emit COMMA
lex1 ';' = emit SEMICOLON
lex1 '@' = word >> emit PRIM
lex1 '%' = word >> emit SPECIAL
lex1 '\'' = word >> emit TAG
lex1 '.' = word >> emit FIELD
lex1 '"' = many (/= '"') >> advance >> emit STRING
lex1 c
  | Char.isSpace c = skip
  | charIsIdentStart c = do
      word
      slice >>= classifyIdent >>= emit
      run
  | Char.isDigit c = do
      many Char.isDigit
      mc <- peek
      case mc of
        Just '.' -> do
          advance
          many Char.isDigit
          emit FLOAT
        _ -> emit INT
  | V.elem c opChars = do
      many (`V.elem` opChars)
      slice >>= classifyOp >>= emit
  | otherwise = emit ERROR

many :: (Char -> Bool) -> Lex ()
many f = do
  mc <- peek
  for_ mc (\c -> when (f c) (advance >> many f))

charIsIdentStart :: Char -> Bool
charIsIdentStart c = Char.isAlpha c || c == '_'

charIsIdent :: Char -> Bool
charIsIdent c = Char.isAlpha c || Char.isDigit c || c == '_'

classifyOp :: Text -> Lex Tag
classifyOp s = do
  keywords <- Config.keywords . config <$> get
  if Set.member s keywords
    then pure KEYWORD_OP
    else pure OP

classifyIdent :: Text -> Lex Tag
classifyIdent s = do
  keywords <- Config.keywords . config <$> get
  topdecls <- Config.topdecls . config <$> get
  if Set.member s keywords
    then pure KEYWORD
    else
      if Set.member s topdecls
        then pure TOPDECL
        else pure IDENT

word :: Lex ()
word = many charIsIdent
