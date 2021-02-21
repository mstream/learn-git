module Core.Fs
  ( File(..)
  , FileContent
  , FileName
  , FileType(..)
  , Path
  , PathSpec
  , binaryFileContent
  , textualFileContentParser
  , fromFileName
  , pathParser
  , pathSpecParser
  ) where

import Prelude
import Core.StringCodec (class StringCodec, decodeUsingParser, encodeToString)
import Data.Array (fromFoldable)
import Data.Array.NonEmpty (fromFoldable1)
import Data.Either.Nested (type (\/))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map, isEmpty)
import Data.String (codePointFromChar, joinWith, singleton)
import Data.String.CodeUnits (fromCharArray)
import Data.String.NonEmpty (NonEmptyString, toString)
import Data.String.NonEmpty.CodeUnits (fromNonEmptyCharArray)
import Motsunabe (Doc(..), pretty)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (alphaNum, anyChar, char)
import Text.Parsing.StringParser.CodeUnits (noneOf)
import Text.Parsing.StringParser.Combinators (choice, many, many1, sepBy)

data File
  = DirectoryOf (Map FileName File)
  | RegularFileContaining FileContent

instance showFile :: Show File where
  show = fileToDoc >>> pretty 80

fileToDoc :: File -> Doc
fileToDoc = go 0
  where
  go :: Int -> File -> Doc
  go level parentFile =
    DNest level
      $ case parentFile of
          RegularFileContaining content -> DText $ ">>|" <> encodeToString content <> "|<<"
          DirectoryOf entries ->
            if isEmpty entries then
              DText "[]"
            else
              dirEntriesToDoc level entries

  dirEntriesToDoc :: Int -> Map FileName File -> Doc
  dirEntriesToDoc level =
    foldlWithIndex
      ( \fileName acc file ->
          acc
            <> DLine
            <> DText "|- "
            <> (DText $ encodeToString fileName)
            <> DText " "
            <> (go (level + 1) file)
      )
      DNil

newtype FileNameSeparator
  = FileNameSeparator Char

fileNameSeparator :: FileNameSeparator
fileNameSeparator = FileNameSeparator '/'

instance stringCodecFileNameSeparator :: StringCodec FileNameSeparator where
  decodeFromString :: String -> String \/ FileNameSeparator
  decodeFromString = decodeUsingParser fileNameSeparatorParser
  encodeToString :: FileNameSeparator -> String
  encodeToString (FileNameSeparator c) = (codePointFromChar >>> singleton) c

newtype Path
  = Path (Array FileName)

derive newtype instance semigroupPath :: Semigroup Path

derive newtype instance monoidPath :: Monoid Path

instance stringCodecPath :: StringCodec Path where
  decodeFromString :: String -> String \/ Path
  decodeFromString = decodeUsingParser pathParser
  encodeToString :: Path -> String
  encodeToString (Path segments) =
    let
      sepStr :: String
      sepStr = encodeToString fileNameSeparator
    in
      sepStr <> (joinWith sepStr $ encodeToString <$> segments)

newtype PathSpec
  = PathSpec NonEmptyString

instance stringCodecPathSpec :: StringCodec PathSpec where
  decodeFromString :: String -> String \/ PathSpec
  decodeFromString = decodeUsingParser pathSpecParser
  encodeToString :: PathSpec -> String
  encodeToString (PathSpec s) = toString s

newtype FileName
  = FileName NonEmptyString

derive newtype instance eqFileName :: Eq FileName

derive newtype instance ordFileName :: Ord FileName

data FileType
  = RegularFile
  | Directory

instance stringCodecFileName :: StringCodec FileName where
  decodeFromString :: String -> String \/ FileName
  decodeFromString = decodeUsingParser fileNameParser
  encodeToString :: FileName -> String
  encodeToString (FileName s) = toString s

data FileContent
  = Binary
  | Textual String

instance stringCodecFileContent :: StringCodec FileContent where
  decodeFromString :: String -> String \/ FileContent
  decodeFromString = decodeUsingParser textualFileContentParser
  encodeToString :: FileContent -> String
  encodeToString = case _ of
    Binary -> "010101..."
    Textual s -> s

binaryFileContent :: FileContent
binaryFileContent = Binary

pathParser :: Parser Path
pathParser = segmentsParser <#> (fromFoldable >>> Path)
  where
  segmentsParser :: Parser (List FileName)
  segmentsParser = fileNameSeparatorParser *> sepBy fileNameParser fileNameSeparatorParser

pathSpecParser :: Parser PathSpec
pathSpecParser = charsParser <#> (fromFoldable1 >>> fromNonEmptyCharArray >>> PathSpec)
  where
  charsParser :: Parser (NonEmptyList Char)
  charsParser = many1 anyChar

fileNameSeparatorParser :: Parser FileNameSeparator
fileNameSeparatorParser = char '/' <#> FileNameSeparator

textualFileContentParser :: Parser FileContent
textualFileContentParser = charsParser <#> (fromFoldable >>> fromCharArray >>> Textual)
  where
  charsParser :: Parser (List Char)
  charsParser = many $ noneOf [ '"', '\'' ]

fileNameParser :: Parser FileName
fileNameParser = charsParser <#> (fromFoldable1 >>> fromNonEmptyCharArray >>> FileName)
  where
  charsParser :: Parser (NonEmptyList Char)
  charsParser = many1 $ choice [ alphaNum, char '.', char '_', char '-' ]

fromFileName :: FileName -> Path
fromFileName name = Path [ name ]
