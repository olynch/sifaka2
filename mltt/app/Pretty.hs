module Pretty (Pretty, pretty) where

import Prelude hiding (show)
import Syntax qualified as S
import Parsing
import Common
import Data.Text (show)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import FNotation hiding (error)
import Data.Map qualified as Map

type Names = Bwd Name

type NamesArg = (?names :: Names)
type PrecArg = (?prec :: Int)
type IndentArg = (?indent :: Int)

type DoPretty a = NamesArg => PrecArg => IndentArg => a

class Pretty a where
  prt :: NamesArg => PrecArg => IndentArg => a -> Builder

pretty :: (Pretty a) => Names -> a -> Builder
pretty names x = let
  ?names = names
  ?prec = 0
  ?indent = 0
  in prt x

numShadowing :: Name -> Names -> Int
numShadowing n = go 0 where
  go i = \case
    BwdNil -> i
    (ns :> n') -> if n == n' then go (i+1) ns else go i ns

instance Pretty Name where
  prt (Name name) = fromText name

instance Pretty BwdIdx where
  prt = go ?names BwdNil where
    go (_ :> name) s 0 = case numShadowing name s of
      0 -> prt name
      i -> prt name <> "~" <> fromText (show i)
    go (names :> name) s i = go names (s :> name) (i-1)
    go BwdNil _ _ = error "out of scope variable"

par :: PrecArg => Int -> Builder -> Builder
par p s | p <= ?prec = "(" <> s <> ")"
        | True = s

bind :: Name -> DoPretty a -> DoPretty a
bind x act = let ?names = ?names :> x in act

(<+>) :: Builder -> Builder -> Builder
x <+> y = x <> " " <> y

appPrec :: Int
appPrec = 110

getPrec :: Text -> Int
getPrec op = case mlttPrecedences Map.! op of
  Prec i _ -> i

binop :: Text -> DoPretty Builder -> DoPretty Builder -> DoPretty Builder
binop op x y =
  let p = getPrec op in
    par p (let ?prec = p in x <+> fromText op <+> y)

indent :: DoPretty a -> DoPretty a
indent act = let ?indent = ?indent + 2 in act

line :: DoPretty Builder -> DoPretty Builder
line b = "\n" <> (mconcat [singleton ' ' | _ <- [0 .. ?indent]]) <> b

instance Pretty S.Tm where
  prt = \case
    S.LocalVar i -> prt i
    S.U -> "U"
    S.Pi x a b -> case x of
      (Name "_") -> binop "->" (prt a) (bind x (prt b))
      _ -> binop "->" (binop ":" (prt x) (prt a)) (bind x (prt b))
    S.Lam x t -> binop "=>" (prt x) (bind x (prt t))
    S.App t s -> par appPrec (prt t <+> prt s)
    S.Block bindings body -> "{" <>
      indent (
        mconcat [line (binop "=" (prt x) (prt t)) <> ";" | (x,t) <- bindings] <>
        line (prt body)
      ) <>
      line "}"
    S.Meta (MetaVar i) -> "?" <> decimal i

instance Pretty S.Def where
  prt (S.Def name ty body) = binop
    "="
    (binop ":" (prt name) (prt ty))
    (indent (line (prt body)))
