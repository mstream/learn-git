module Core.Cli (Cmd(..), commandArgs, commandName) where

import Prelude
import Control.Alt ((<|>))
import Core.Fs (FileContent, Path, fileContentParser, pathParser)
import Core.StringCodec (class StringCodec, decodeUsingParser, encodeToString)
import Data.Either.Nested (type (\/))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, skipSpaces, string)
import Text.Parsing.StringParser.Combinators (between, choice)

data Cmd
  = EditFile Path FileContent
  | MakeDirectory Path

instance stringCodecCmd :: StringCodec Cmd where
  decodeFromString :: String -> String \/ Cmd
  decodeFromString = decodeUsingParser cmdParser
  encodeToString :: Cmd -> String
  encodeToString = case _ of
    EditFile path content -> "edit " <> encodeToString path <> " \"" <> encodeToString content <> "\""
    MakeDirectory path -> "mkdir " <> encodeToString path

cmdParser :: Parser Cmd
cmdParser = do
  skipSpaces
  cmd <- choice cmdParsers
  skipSpaces
  pure $ cmd
  where
  cmdParsers :: Array (Parser Cmd)
  cmdParsers =
    [ do
        void $ string "edit"
        skipSpaces
        path <- pathParser
        skipSpaces
        content <- between (char '"') (char '"') fileContentParser
        pure $ EditFile path content
    , do
        void $ string "mkdir"
        skipSpaces
        path <- pathParser
        pure $ MakeDirectory path
    ]

quoteParser :: Parser Char
quoteParser = char '"' <|> char '\''

commandName :: Cmd -> String
commandName = case _ of
  EditFile _ _ -> "edit"
  MakeDirectory _ -> "mkdir"

commandArgs :: Cmd -> Array String
commandArgs = case _ of
  EditFile path content -> [ encodeToString path, encodeToString content ]
  MakeDirectory path -> [ encodeToString path ]
