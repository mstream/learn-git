module Core.FileSystem
  ( FileContent
  , FileName
  , FileType(..)
  , Path
  , fileContentParser
  , pathParser
  ) where

import Prelude
import Core.StringCodec (class StringCodec, decodeUsingParser, encodeToString)
import Data.Array (fromFoldable)
import Data.Array.NonEmpty (fromFoldable1)
import Data.Either.Nested (type (\/))
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)
import Data.String (codePointFromChar, joinWith, singleton)
import Data.String.CodeUnits (fromCharArray)
import Data.String.NonEmpty (NonEmptyString, toString)
import Data.String.NonEmpty.CodeUnits (fromNonEmptyCharArray)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (alphaNum, anyChar, char)
import Text.Parsing.StringParser.CodeUnits (noneOf)
import Text.Parsing.StringParser.Combinators (choice, many, many1, sepBy)

data File
  = DirectoryOf (Map FileName File)
  | RegularFileOf FileContent

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

newtype FileName
  = FileName NonEmptyString

derive newtype instance eqFileName :: Eq FileName

data FileType
  = RegularFile
  | Directory

instance stringCodecFileName :: StringCodec FileName where
  decodeFromString :: String -> String \/ FileName
  decodeFromString = decodeUsingParser fileNameParser
  encodeToString :: FileName -> String
  encodeToString (FileName s) = toString s

newtype FileContent
  = FileContent String

instance stringCodecFileContent :: StringCodec FileContent where
  decodeFromString :: String -> String \/ FileContent
  decodeFromString = decodeUsingParser fileContentParser
  encodeToString :: FileContent -> String
  encodeToString (FileContent s) = s

pathParser :: Parser Path
pathParser = segmentsParser <#> (fromFoldable >>> Path)
  where
  segmentsParser :: Parser (List FileName)
  segmentsParser = fileNameSeparatorParser *> sepBy fileNameParser fileNameSeparatorParser

fileNameSeparatorParser :: Parser FileNameSeparator
fileNameSeparatorParser = char '/' <#> FileNameSeparator

fileContentParser :: Parser FileContent
fileContentParser = charsParser <#> (fromFoldable >>> fromCharArray >>> FileContent)
  where
  charsParser :: Parser (List Char)
  charsParser = many $ noneOf [ '"', '\'' ]

fileNameParser :: Parser FileName
fileNameParser = charsParser <#> (fromFoldable1 >>> fromNonEmptyCharArray >>> FileName)
  where
  charsParser :: Parser (NonEmptyList Char)
  charsParser = many1 $ choice [ alphaNum, char '.', char '_', char '-' ]
