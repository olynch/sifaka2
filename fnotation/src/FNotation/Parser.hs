module FNotation.Parser where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState, get, modify, state)
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as V
import FNotation.Config (Assoc (..), FNotationConfig, Prec (..))
import FNotation.Config qualified as Config
import FNotation.Diagnostic (Annot (Here), Diagnostic (Diagnostic), FileId, HasReporter (getReporter), Loc (Loc), Marker (Marker), Reporter, Severity (..), report)
import FNotation.FNtn (FNtn (..), FNtn0 (..), FNtnTop (..))
import FNotation.FNtn qualified as FNtn
import FNotation.Parser.ParseState (ParseState (ParseState))
import FNotation.Parser.ParseState qualified as PS
import FNotation.Span (Span (Span))
import FNotation.Span qualified as Span
import FNotation.Token (Tag (..), Token)
import FNotation.Token qualified as Token
import FNotation.Prelude
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prelude hiding (error)
import Prelude qualified

newtype Parser a = Parser {unParser :: StateT ParseState IO a}
  deriving (Functor, Applicative, Monad, MonadState ParseState, MonadIO)

instance HasReporter Parser where
  getReporter = get <&> PS.reporter

errorNode :: Int -> Parser FNtn
errorNode m = do
  s <- spanStartingAt m
  pure $ L s Error

error :: Int -> Doc AnsiStyle -> Parser FNtn
error m msg = do
  s <- spanStartingAt m
  fi <- get <&> PS.fileId
  let l = Loc fi s
  report (Diagnostic ErrorS msg "" [Marker l Here])
  pure $ L s Error

errorFromSpan_ :: Span -> ADoc -> Parser ()
errorFromSpan_ s msg = do
  fi <- get <&> PS.fileId
  let l = Loc fi s
  report (Diagnostic ErrorS msg "" [Marker l Here])

error_ :: Int -> Doc AnsiStyle -> Parser ()
error_ m msg = do
  s <- spanStartingAt m
  fi <- get <&> PS.fileId
  let l = Loc fi s
  report (Diagnostic ErrorS msg "" [Marker l Here])

errorHere :: Doc AnsiStyle -> Parser ()
errorHere msg = do
  tokens <- get <&> PS.tokens
  case tokens of
    (token:_) -> errorFromSpan_ (Token.span token) msg
    [] -> do
      endPos <- get <&> PS.endPos
      errorFromSpan_ (Span endPos endPos) msg

panic :: String -> a
panic = Prelude.error

unwrap :: Maybe a -> a
unwrap (Just x) = x
unwrap Nothing = panic "expected a Just"

cur :: Parser Tag
cur = state cur'
  where
    cur' :: ParseState -> (Tag, ParseState)
    cur' s
      | PS.gas s == 0 = panic "out of gas"
      | otherwise =
          let s' = s {PS.gas = PS.gas s - 1}
           in case PS.tokens s of
                (t : _) -> (Token.tag t, s')
                [] -> (EOF, s')

tailSafe :: [a] -> [a]
tailSafe (_ : xs) = xs
tailSafe [] = []

advance :: Parser ()
advance =
  modify
    ( \s -> case PS.tokens s of
        (t : ts) -> s {PS.gas = 256, PS.tokens = ts, PS.closePos = Token.end t}
        [] -> s
    )

open :: Parser Int
open =
  get <&> \s -> case PS.tokens s of
    (t : _) -> Token.start t
    [] -> PS.endPos s

spanStartingAt :: Int -> Parser Span
spanStartingAt m = Span m <$> (get <&> PS.closePos)

close :: Int -> FNtn0 -> Parser FNtn
close m n = L <$> spanStartingAt m <*> pure n

eat :: Tag -> Parser Bool
eat t = do
  t' <- cur
  if t' == t
    then advance >> pure True
    else pure False

expect :: Tag -> Parser ()
expect t =
  eat t >>= \case
    True -> pure ()
    False -> do
      tag <- cur
      errorHere $ "expected" <+> pretty t <+> "got" <+> pretty tag

slice :: Int -> Parser ByteString
slice m = do
  st <- get
  s <- spanStartingAt m
  pure $ Span.slice s (PS.source st)

data StackElt
  = Complete FNtn
  | HalfApp FNtn FNtn Prec
  | ErrorElt FNtn

type TermStack = NonEmpty StackElt

combineSpan :: FNtn -> FNtn -> Span
combineSpan n n' = Span (Span.start $ FNtn.span n) (Span.end $ FNtn.span n')

app1 :: FNtn -> FNtn -> FNtn
app1 n n' = L (combineSpan n n') (App1 n n')

app2 :: FNtn -> FNtn -> FNtn -> FNtn
app2 op l r = L (combineSpan l r) (App2 op l r)

pushTerm :: TermStack -> FNtn -> TermStack
pushTerm ((Complete n) :| rest) n' = Complete (app1 n n') :| rest
pushTerm (t :| rest) n = Complete n :| t : rest

collectFor :: Prec -> [StackElt] -> FNtn -> ([StackElt], FNtn)
collectFor _ ((Complete _) : _) _ = panic ""
collectFor p (e@(HalfApp l op p') : rest) r
  | p < p' = collectFor p rest (app2 op l r)
  | otherwise = (e : rest, r)
collectFor _ (e@(ErrorElt _) : rest) n' = (e : rest, n')
collectFor _ [] n = ([], n)

pushOp :: Prec -> FNtn -> TermStack -> Parser TermStack
pushOp p op ((Complete n) :| rest) = pure $ HalfApp n' op p :| rest'
  where
    (rest', n') = collectFor p rest n
pushOp _ op ((HalfApp l _ _) :| rest) = do
  error_ (Span.start $ FNtn.span l) "expected term after binary op"
  pure $ ErrorElt (L (combineSpan l op) Error) :| rest
pushOp _ _ ((ErrorElt n) :| rest) = pure $ ErrorElt n :| rest

finishStack :: TermStack -> Parser FNtn
finishStack ((Complete n) :| []) = pure n
finishStack ((HalfApp l _ _) :| _) = do
  let m = Span.start $ FNtn.span l
  error m "uncompleted binary operator"
finishStack ((Complete n) :| elts) = do
  let (rest, n') = collectFor (Prec 0 NonA) elts n
  case rest of
    [] -> pure n'
    _ -> error 0 "failed to finish term stack"
finishStack ((ErrorElt n) :| _) = pure n

parenthesized :: Parser a -> Parser a
parenthesized p = do
  expect LPAREN
  x <- p
  expect RPAREN
  pure x

termStarts :: Vector Tag
termStarts = V.fromList [LPAREN, INT, STRING, IDENT, KEYWORD, PRIM, SPECIAL, FIELD, TAG]

wrapSlice :: (ByteString -> FNtn0) -> Parser FNtn
wrapSlice constructor = do
  m <- open
  advance
  s <- slice m
  close m (constructor s)

term :: Parser FNtn
term =
  cur >>= \case
    LPAREN -> parenthesized expr
    LBRACK -> tuple
    IDENT -> wrapSlice (Ident . decodeUtf8)
    KEYWORD -> wrapSlice (Keyword . decodeUtf8)
    PRIM -> wrapSlice (Prim . decodeUtf8 . BS.drop 1)
    SPECIAL -> wrapSlice (Special . decodeUtf8 . BS.drop 1)
    FIELD -> wrapSlice (Field . decodeUtf8 . BS.drop 1)
    TAG -> wrapSlice (Tag . decodeUtf8 . BS.drop 1)
    INT -> wrapSlice (Int . fst . unwrap . C8.readInt)
    STRING -> wrapSlice (String . decodeUtf8 . BS.drop 1 . BS.take 1)
    _ -> panic "term should only be called when the current token can start a term"

tuple :: Parser FNtn
tuple = do
  m <- open
  expect LBRACK
  elts <- cur >>= go []
  expect RBRACK
  close m (Tuple $ reverse elts)
  where
    go :: [FNtn] -> Tag -> Parser [FNtn]
    go ns t
      | V.elem t termStarts = do
          n <- expr
          let ns' = n : ns
          cur >>= \case
            COMMA -> do
              advance
              cur >>= \t' -> case t' of
                RBRACK -> pure ns'
                _ -> go ns' t'
            _ -> pure ns'
      | otherwise = pure ns

lookupPrec :: Text -> Parser (Maybe Prec)
lookupPrec op = do
  precs <- Config.precedences . PS.config <$> get
  pure $ Map.lookup op precs

expr :: Parser FNtn
expr = do
  n <- term
  go (Complete n :| [])
  where
    go :: TermStack -> Parser FNtn
    go stack = cur >>= go1 stack
    go1 stack t
      | V.elem t termStarts = do
          n <- term
          go (pushTerm stack n)
      | t == OP || t == KEYWORD_OP = do
          m' <- open
          advance
          s <- decodeUtf8 <$> slice m'
          op <- if t == OP then close m' (Ident s) else close m' (Keyword s)
          lookupPrec s >>= \case
            Just prec -> pushOp prec op stack >>= go
            Nothing -> error m' $ "could not find precedence for" <+> pretty (show s)
      | otherwise = finishStack stack

tops :: Parser [FNtnTop]
tops = go []
  where
    go acc = nextTopDecl False >>= \case
      Just (name, m) -> do
        n <- expr
        s <- spanStartingAt m
        go (FNtnTop name s n : acc)
      Nothing -> pure $ reverse acc
    nextTopDecl :: Bool -> Parser (Maybe (Text, Int))
    nextTopDecl skip = cur >>= \case
      TOPDECL -> do
        m <- open
        advance
        name <- decodeUtf8 <$> slice m
        pure $ Just (name, m)
      EOF -> pure Nothing
      _ -> if skip then advance >> nextTopDecl True else do
        m <- open
        advance
        error_ m $ "expected a toplevel declaration"
        nextTopDecl True

runParser :: Parser a -> Reporter -> FNotationConfig -> FileId -> [Token] -> ByteString -> IO a
runParser action r config fi ts bs = evalStateT (unParser action) s
  where
    s = ParseState config ts bs 0 (BS.length bs - 1) 256 fi r

parse :: Reporter -> FNotationConfig -> FileId -> [Token] -> ByteString -> IO FNtn
parse = runParser expr

parseTop :: Reporter -> FNotationConfig -> FileId -> [Token] -> ByteString -> IO [FNtnTop]
parseTop = runParser tops
