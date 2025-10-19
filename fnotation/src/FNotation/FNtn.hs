module FNotation.FNtn (FNtn (..), FNtn0 (..)) where

import Data.Text (Text)
import FNotation.Span
import Prettyprinter
import Prelude hiding (span)

data FNtn = L {span :: Span, fntn0 :: FNtn0}
  deriving (Show)

data FNtn0
  = Ident Text
  | Keyword Text
  | Field Text
  | Prim Text
  | Special Text
  | Tag Text
  | App1 FNtn FNtn
  | App2 FNtn FNtn FNtn
  | Int Int
  | Float Double
  | String Text
  | Tuple [FNtn]
  | Block [FNtn] (Maybe FNtn)
  | Error
  deriving (Show)

instance Pretty FNtn0 where
  pretty (Ident s) = pretty s
  pretty (Keyword s) = pretty s
  pretty (Field s) = "." <> pretty s
  pretty (Prim s) = "@" <> pretty s
  pretty (Special s) = "%" <> pretty s
  pretty (Tag s) = "'" <> pretty s
  pretty (App1 f x) = parens (pretty f <+> pretty x)
  pretty (App2 op l r) = parens (pretty l <+> pretty op <+> pretty r)
  pretty (Int i) = pretty i
  pretty (Float f) = pretty f
  pretty (String s) = "\"" <> pretty s <> "\""
  pretty (Tuple ns) = list (pretty <$> ns)
  pretty (Block bs mn) = surround "{" "}" $ vsep ((<> ";") . pretty <$> bs) <> end
    where
      end = maybe mempty pretty mn
  pretty Error = "<error>"

instance Pretty FNtn where
  pretty (L _ n0) = pretty n0
