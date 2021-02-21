module Core.Git
  ( CommitMessage
  , PathSpec
  , commitMessageParser
  , pathSpecParser
  ) where

import Prelude
import Core.StringCodec (class StringCodec, decodeUsingParser)
import Data.Array.NonEmpty (fromFoldable1)
import Data.Either.Nested (type (\/))
import Data.List.NonEmpty (NonEmptyList)
import Data.String.NonEmpty (NonEmptyString, toString)
import Data.String.NonEmpty.CodeUnits (fromNonEmptyCharArray)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (anyChar, noneOf)
import Text.Parsing.StringParser.Combinators (many1)

newtype PathSpec
  = PathSpec NonEmptyString

instance stringCodecPathSpec :: StringCodec PathSpec where
  decodeFromString :: String -> String \/ PathSpec
  decodeFromString = decodeUsingParser pathSpecParser
  encodeToString :: PathSpec -> String
  encodeToString (PathSpec s) = toString s

newtype CommitMessage
  = CommitMessage NonEmptyString

instance stringCodecCommitMessage :: StringCodec CommitMessage where
  decodeFromString :: String -> String \/ CommitMessage
  decodeFromString = decodeUsingParser commitMessageParser
  encodeToString :: CommitMessage -> String
  encodeToString (CommitMessage s) = toString s

pathSpecParser :: Parser PathSpec
pathSpecParser = charsParser <#> (fromFoldable1 >>> fromNonEmptyCharArray >>> PathSpec)
  where
  charsParser :: Parser (NonEmptyList Char)
  charsParser = many1 anyChar

commitMessageParser :: Parser CommitMessage
commitMessageParser = charsParser <#> (fromFoldable1 >>> fromNonEmptyCharArray >>> CommitMessage)
  where
  charsParser :: Parser (NonEmptyList Char)
  charsParser = many1 $ noneOf [ '"', '\'' ]
