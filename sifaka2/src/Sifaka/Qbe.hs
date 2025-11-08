module Sifaka.Qbe where

import Data.ByteString.Builder
import Data.String (IsString (..))
import Data.Text.Encoding (encodeUtf8Builder)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TE
import Debug.Trace (trace)
import Sifaka.Common
import System.IO
import System.IO.Temp
import System.Process

class ToText a where
  txt :: a -> Builder

traceQbe :: (ToText a) => a -> b -> b
traceQbe x y = trace s y
  where
    s = T.unpack $ TE.decodeUtf8 $ toLazyByteString $ txt x

data QbeName
  = StaticName Text
  | FreshName Int

instance ToText QbeName where
  txt (StaticName s) = encodeUtf8Builder s
  txt (FreshName i) = "_" <> intDec i

instance IsString QbeName where
  fromString s = StaticName (fromString s)

newtype TypeName = TypeName QbeName

instance ToText TypeName where
  txt (TypeName n) = ":" <> txt n

newtype GlobalName = GlobalName QbeName

instance ToText GlobalName where
  txt (GlobalName n) = "$" <> txt n

instance IsString GlobalName where
  fromString s = GlobalName (fromString s)

newtype LocalName = LocalName QbeName

instance ToText LocalName where
  txt (LocalName n) = "%" <> txt n

newtype LabelName = LabelName QbeName

instance IsString LabelName where
  fromString s = LabelName (fromString s)

instance ToText LabelName where
  txt (LabelName n) = "@" <> txt n

data BaseTy = Word32 | Word64 | Float32 | Float64

instance ToText BaseTy where
  txt Word32 = "w"
  txt Word64 = "l"
  txt Float32 = "s"
  txt Float64 = "d"

data ExtTy
  = Base BaseTy
  | Word8
  | Word16

instance ToText ExtTy where
  txt (Base ty) = txt ty
  txt Word8 = "b"
  txt Word16 = "h"

data SubTy = SExt ExtTy | SIdent TypeName

instance ToText SubTy where
  txt (SExt ty) = txt ty
  txt (SIdent n) = txt n

data Const = CNum Int | CFloat32 Double | CFloat64 Double | CGlobal GlobalName

instance ToText Const where
  txt (CNum i) = intDec i
  txt (CFloat32 d) = "s_" <> doubleDec d
  txt (CFloat64 d) = "d_" <> doubleDec d
  txt (CGlobal n) = txt n

data Linkage = Export | Thread | Section ByteString

instance ToText Linkage where
  txt Export = "export"
  txt Thread = "thread"
  txt (Section name) = "section " <> byteString name

newtype Align = Align Int

instance ToText Align where
  txt (Align i) = "align " <> intDec i

newtype Repeat = Repeat Int

instance ToText Repeat where
  txt (Repeat i) = intDec i

newtype Size = Size Int

instance ToText Size where
  txt (Size i) = intDec i

data TypeDefBody
  = Struct (Maybe Align) [(SubTy, Maybe Repeat)]
  | Union (Maybe Align) [(SubTy, Maybe Repeat)]
  | Opaque Align Size

(<+>) :: Builder -> Builder -> Builder
(<+>) b b' = b <> " " <> b'

(<+?>) :: Builder -> Maybe Builder -> Builder
(<+?>) b (Just b') = b <> " " <> b'
(<+?>) b Nothing = b

(<?+>) :: Maybe Builder -> Builder -> Builder
(<?+>) (Just b) b' = b <> " " <> b'
(<?+>) Nothing b' = b'

instance ToText TypeDefBody where
  txt (Struct a fields) =
    (txt <$> a)
      <?+> "{"
      <+> mconcat [txt f <+?> (txt <$> n) <> "," | (f, n) <- fields]
      <+> "}"
  txt (Union a variants) =
    (txt <$> a)
      <?+> "{"
      <+> mconcat ["{" <+> txt f <+?> (txt <$> n) <+> "}" | (f, n) <- variants]
      <+> "}"
  txt (Opaque a n) = txt a <+> "{" <+> txt n <+> "}"

data TypeDef = TypeDef TypeName TypeDefBody

instance ToText TypeDef where
  txt (TypeDef name body) = "type" <+> txt name <+> "=" <+> txt body

data DataDef = DataDef GlobalName (Maybe Align) [(ExtTy, [DataItem])]

instance ToText DataDef where
  txt (DataDef name align fields) =
    "data"
      <+> txt name
      <+> "="
      <+?> fmap txt align
      <+> "{"
      <+> mconcat
        [txt ty <> mconcat [" " <> txt d | d <- ds] | (ty, ds) <- fields]
      <+> "}"

data DataItem = DISym GlobalName (Maybe Int) | DIString ByteString | DIConst Const

instance ToText DataItem where
  txt (DISym name (Just i)) = txt name <+> "+" <+> intDec i
  txt (DISym name Nothing) = txt name
  txt (DIString s) = "\"" <> byteString s <> "\""
  txt (DIConst c) = txt c

data SubwordTy = Int8 | UInt8 | Int16 | UInt16

instance ToText SubwordTy where
  txt Int8 = "sb"
  txt UInt8 = "ub"
  txt Int16 = "sh"
  txt UInt16 = "uh"

data AbiTy = ABase BaseTy | ASub SubwordTy | AIdent TypeName

instance ToText AbiTy where
  txt (ABase ty) = txt ty
  txt (ASub ty) = txt ty
  txt (AIdent n) = txt n

data FuncDef = FuncDef [Linkage] (Maybe AbiTy) GlobalName [Param] [Block]

instance ToText FuncDef where
  txt (FuncDef linkage ret name params blocks) =
    mconcat [txt l <> " " | l <- linkage]
      <> "function" <+?> (txt <$> ret) <+> txt name <+> "("
      <> mconcat [txt p <> ", " | p <- params]
      <> ")" <+> "{\n"
      <> mconcat [txt b | b <- blocks]
      <> "}"

data Param = Param AbiTy LocalName

instance ToText Param where
  txt (Param ty name) = txt ty <+> txt name

data Jump = Jmp LabelName | Jnz Val LabelName LabelName | Ret (Maybe Val) | Hlt

commaSep :: [Builder] -> Builder
commaSep [] = ""
commaSep (x : []) = x
commaSep (x : xs) = x <> ", " <> commaSep xs

instance ToText Jump where
  txt (Jmp l) = "jmp" <+> txt l
  txt (Jnz v l1 l2) = "jnz" <+> commaSep [txt v, txt l1, txt l2]
  txt (Ret v) = "ret" <+?> (txt <$> v)
  txt Hlt = "hlt"

data Val = VConst Const | VIdent LocalName

instance ToText Val where
  txt (VConst c) = txt c
  txt (VIdent n) = txt n

data Inst
  = Inst1 LocalName BaseTy Inst1
  | Inst0 Inst0
  | Call (Maybe (LocalName, AbiTy)) Val [(AbiTy, Val)]

instance ToText Inst where
  txt (Inst1 name ty i) = txt name <+> "=" <> txt ty <+> txt i
  txt (Inst0 i) = txt i
  txt (Call ret f args) = doRet ret <> "call" <+> txt f <+> "(" <> commaSep (doArg <$> args) <> ")"
    where
      doRet (Just (name, ty)) = txt name <+> "=" <> txt ty <> " "
      doRet Nothing = ""
      doArg (ty, v) = txt ty <+> txt v

data Inst1
  = Add Val Val
  | Sub Val Val
  | Mul Val Val
  | Div Val Val
  | Neg Val
  | Udiv Val Val
  | Rem Val Val
  | URem Val Val
  | Or Val Val
  | Xor Val Val
  | And Val Val
  | Sar Val Val
  | Shr Val Val
  | Shl Val Val
  | Load ExtTy Val
  | Alloc Align Val
  | Cule BaseTy Val Val
  | Cult BaseTy Val Val
  | UWToF Val

inst :: ByteString -> [Val] -> Builder
inst name args = byteString name <+> commaSep (txt <$> args)

instance ToText Inst1 where
  txt (Add v1 v2) = inst "add" [v1, v2]
  txt (Sub v1 v2) = inst "sub" [v1, v2]
  txt (Mul v1 v2) = inst "mul" [v1, v2]
  txt (Div v1 v2) = inst "div" [v1, v2]
  txt (Neg v1) = inst "neg" [v1]
  txt (Udiv v1 v2) = inst "udiv" [v1, v2]
  txt (Rem v1 v2) = inst "rem" [v1, v2]
  txt (URem v1 v2) = inst "urem" [v1, v2]
  txt (Or v1 v2) = inst "or" [v1, v2]
  txt (Xor v1 v2) = inst "xor" [v1, v2]
  txt (And v1 v2) = inst "and" [v1, v2]
  txt (Sar v1 v2) = inst "sar" [v1, v2]
  txt (Shr v1 v2) = inst "shr" [v1, v2]
  txt (Shl v1 v2) = inst "shl" [v1, v2]
  txt (Cule ty v1 v2) = "cule" <> txt ty <+> commaSep [txt v1, txt v2]
  txt (Cult ty v1 v2) = "cult" <> txt ty <+> commaSep [txt v1, txt v2]
  txt (UWToF v) = inst "uwtof" [v]
  txt (Load ty v1) = "load" <> txt ty <+> commaSep [txt v1]
  txt (Alloc (Align i) v) = "alloc" <> intDec i <+> txt v

data Inst0
  = Store ExtTy Val Val
  | Blit Val Val Val

instance ToText Inst0 where
  txt (Store ty v1 v2) = "store" <> txt ty <+> commaSep [txt v1, txt v2]
  txt (Blit v1 v2 v3) = inst "blit" [v1, v2, v3]

data Phi = Phi LocalName BaseTy [(LabelName, Val)]

instance ToText Phi where
  txt (Phi name ty vs) =
    txt name
      <+> "="
      <> txt ty
        <+> "phi"
        <+> commaSep [txt b <+> txt v | (b, v) <- vs]

data Block = Block LabelName [Phi] [Inst] Jump

indentedNL :: Builder -> Builder
indentedNL b = "\t" <> b <> "\n"

instance ToText Block where
  txt (Block label phis instructions jump) =
    txt label
      <> "\n"
      <> mconcat (indentedNL . txt <$> phis)
      <> mconcat (indentedNL . txt <$> instructions)
      <> indentedNL (txt jump)

data Module = Module
  { moduleTypes :: [TypeDef],
    moduleDatas :: [DataDef],
    moduleFuncs :: [FuncDef]
  }

instance ToText Module where
  txt (Module ts ds fs) =
    mconcat [txt t <> "\n\n" | t <- ts]
      <> mconcat [txt d <> "\n\n" | d <- ds]
      <> mconcat [txt f <> "\n\n" | f <- fs]

emitIR :: Module -> FilePath -> IO ()
emitIR m irPath = do
  withFile irPath WriteMode $ \h -> hPutBuilder h (txt m)

compile :: Module -> FilePath -> IO ()
compile m out = do
  withSystemTempFile "out.s" $ \asmPath _ -> do
    let qbeProc = (proc "qbe" ["-o", asmPath]) {std_in = CreatePipe}
    withCreateProcess
      qbeProc
      ( \maybeQbeIn _ _ qbeHandle -> do
          (Just qbeIn) <- pure maybeQbeIn
          hPutBuilder qbeIn (txt m)
          hClose qbeIn
          _ <- waitForProcess qbeHandle
          pure ()
      )
    callProcess "cc" [asmPath, "runtime/runtime.c", "-o", out]

assemble :: Module -> FilePath -> IO ()
assemble m objectPath = do
  let qbeProc = (proc "qbe" []) {std_in = CreatePipe, std_out = CreatePipe}
  withCreateProcess
    qbeProc
    ( \maybeQbeIn maybeQbeOut _ qbeHandle -> do
        (Just qbeIn) <- pure maybeQbeIn
        (Just qbeOut) <- pure maybeQbeOut
        let asProc = (proc "as" ["-o", objectPath]) {std_in = UseHandle qbeOut}
        withCreateProcess
          asProc
          ( \_ _ _ asHandle -> do
              hPutBuilder qbeIn (txt m)
              hClose qbeIn
              _ <- waitForProcess qbeHandle
              _ <- waitForProcess asHandle
              pure ()
          )
    )

link :: [String] -> [FilePath] -> FilePath -> IO ()
link options objectPaths out = do
  let ldProc = (proc "ld" (options ++ objectPaths ++ ["-o", out]))
  withCreateProcess
    ldProc
    ( \_ _ _ ldHandle -> do
        _ <- waitForProcess ldHandle
        pure ()
    )
