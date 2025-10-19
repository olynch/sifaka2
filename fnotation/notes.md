# Notes

## Strings

The only reasonable string type in Haskell is `ByteString`, because it's the
only one that supports efficient slicing. However, `Prettyprinter`'s `Doc`
requires `Text`.

Thus, the following compromise:

1. We read in source files as `ByteString`, and lex the bytestring.
2. `FNtn` uses `Text`, and the conversion from `ByteString` to `Text` happens at parse time.

## Diagnostics

Diagnose forces use to convert our source files to `String`, so it is ruled out.
So we write our diagnostics from scratch.

The message for the diagnostic is a `Doc`, but we use `ByteString`s to display
the source position.

## Naming conventions

Field accessors should always be used qualified (e.g. `Span.start`).

Explicit export lists.
