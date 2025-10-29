module FNotation.Diagnostic where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.UTF8 qualified as Utf8
import Data.IORef (IORef, modifyIORef)
import Data.IntMap qualified as IntMap
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as V
import FNotation.Prelude
import FNotation.Span (Span (Span))
import FNotation.Span qualified as Span
import Prettyprinter hiding (line)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..))
import Prettyprinter.Render.Terminal qualified as Terminal
import Prettyprinter.Render.Text (hPutDoc)
import System.IO (Handle, stderr)
import Prelude hiding (error, lines, span)

data Severity = ErrorS | WarningS | InfoS | DebugS

type BytePos = Int

type LineNumber = Int

newtype FileId = FileId Int
  deriving (Eq, Ord)

data File = File
  { fileContent :: ByteString,
    fileNewlines :: Vector BytePos,
    fileName :: FilePath
  }

getNewlines :: BytePos -> [BytePos] -> ByteString -> Vector BytePos
getNewlines p ps bs = case Utf8.uncons bs of
  Just (c, bs') ->
    let p' = p + charLen c
     in if c == '\n'
          then getNewlines p' (p' : ps) bs'
          else getNewlines p' ps bs'
  Nothing -> V.fromList $ reverse (p : ps)

newFile :: FilePath -> ByteString -> File
newFile name content = File content newlines name
  where
    newlines = getNewlines 0 [0] content

data Line = Line
  { lineSpan :: Span,
    lineNumber :: Int
  }
  deriving (Show)

lineOf :: File -> BytePos -> Line
lineOf f p = Line lineSpan (lineIdx + 1)
  where
    newlines = fileNewlines f
    lineIdx = max 0 (insertionPoint newlines p - 1)
    nextLineIdx = min (V.length newlines - 1) (lineIdx + 1)
    lineSpan = Span (newlines V.! lineIdx) (newlines V.! nextLineIdx - 1)

linesBetween :: File -> BytePos -> BytePos -> [Line]
linesBetween f p1 p2
  | p1 >= p2 = []
  | otherwise = l : linesBetween f (Span.end (lineSpan l)) p2
  where
    l = lineOf f p1

linesOf :: File -> Span -> [Line]
linesOf f s = l1 : linesBetween f (Span.end (lineSpan l1)) (Span.end s)
  where
    l1 = lineOf f (Span.start s)

data Files = Files
  { filesById :: IntMap File,
    filesByName :: Map FilePath FileId,
    filesNextId :: Int
  }

emptyFiles :: Files
emptyFiles =
  Files
    { filesById = IntMap.empty,
      filesByName = Map.empty,
      filesNextId = 0
    }

lookupFile :: FileId -> Files -> Maybe File
lookupFile (FileId i) files = IntMap.lookup i (filesById files)

insertFile :: File -> Files -> (Files, FileId)
insertFile file files = (files', fileId)
  where
    fileId = FileId (filesNextId files)
    files' =
      Files
        { filesById = IntMap.insert (filesNextId files) file (filesById files),
          filesByName = Map.insert (fileName file) fileId (filesByName files),
          filesNextId = filesNextId files + 1
        }

data Loc = Loc {fileId :: FileId, span :: Span}

data Annot = Here

data Marker = Marker {loc :: Loc, annot :: Annot}

data Diagnostic = Diagnostic
  { diagnosticSeverity :: Severity,
    diagnosticDescription :: ADoc,
    diagnosticExplanation :: ADoc,
    diagnosticMarkers :: [Marker]
  }

newtype Reporter = Reporter (Diagnostic -> IO ())

class (MonadIO m) => HasReporter m where
  getReporter :: m Reporter

report :: (HasReporter m) => Diagnostic -> m ()
report d = do
  (Reporter r) <- getReporter
  liftIO $ r d

dummyReporter :: Reporter
dummyReporter = Reporter $ \_ -> pure ()

data Style = Style
  { styleError :: AnsiStyle,
    styleWarning :: AnsiStyle,
    styleInfo :: AnsiStyle,
    styleDebug :: AnsiStyle
  }

defaultStyle :: Style
defaultStyle =
  Style
    { styleError = Terminal.color Red,
      styleWarning = Terminal.color Yellow,
      styleInfo = Terminal.color Green,
      styleDebug = Terminal.color Blue
    }

prettySeverity :: Style -> Severity -> ADoc
prettySeverity (Style {styleError}) ErrorS = annotate styleError "[error]"
prettySeverity (Style {styleWarning}) WarningS = annotate styleWarning "[warning]"
prettySeverity (Style {styleInfo}) InfoS = annotate styleInfo "[info]"
prettySeverity (Style {styleDebug}) DebugS = annotate styleDebug "[debug]"

spanOnLine :: File -> Span -> Line -> ADoc
spanOnLine f s l =
  vsep
    [ ln <+> "|" <+> annotatedLine,
      ln_ <+> "|" <+> pointers
    ]
  where
    ln = fill 3 $ pretty (lineNumber l)
    ln_ = fill 3 ""
    ls = lineSpan l
    content = fileContent f
    errorStart = Span.start ls `max` Span.start s
    errorEnd = Span.end ls `min` Span.end s
    start = decodeUtf8 $ Span.slice (Span (Span.start ls) errorStart) content
    error = decodeUtf8 $ Span.slice (Span errorStart errorEnd) content
    end = decodeUtf8 $ Span.slice (Span errorEnd (Span.end ls)) content
    annotatedLine =
      pretty start
        <> annotate (Terminal.color Red) (pretty error)
        <> pretty end
    pointers =
      pretty (T.replicate (T.length start) " ")
        <> annotate (Terminal.color Red) (pretty (T.replicate (T.length error) "^"))

spanOnSource :: File -> Span -> ADoc
spanOnSource f s = vsep $ spanOnLine f s <$> linesOf f s

markerOnSource :: Files -> Marker -> ADoc
markerOnSource files m = case lookupFile (fileId $ loc m) files of
  Just f ->
    vsep
      [ "-->" <> pretty (fileName f),
        spanOnSource f (span $ loc m)
      ]
  Nothing -> "<source not found>"

pprDiagnostic :: Style -> Files -> Diagnostic -> ADoc
pprDiagnostic style files (Diagnostic {..}) = vsep [overview, sourceCode, diagnosticExplanation]
  where
    overview = prettySeverity style diagnosticSeverity <+> diagnosticDescription
    sourceCode = vsep $ markerOnSource files <$> diagnosticMarkers

data UseAnsiColors = UseAnsiColors | DontUseAnsiColors

handleReporter :: Handle -> UseAnsiColors -> Files -> Reporter
handleReporter h UseAnsiColors files =
  Reporter $ hPutDoc h . pprDiagnostic defaultStyle files
handleReporter h DontUseAnsiColors files =
  Reporter $ hPutDoc h . unAnnotate . pprDiagnostic defaultStyle files

stderrReporter :: Files -> Reporter
stderrReporter = handleReporter stderr UseAnsiColors

ioRefReporter :: IORef [Diagnostic] -> Reporter
ioRefReporter ref = Reporter $ \d -> do
  modifyIORef ref (d :)
