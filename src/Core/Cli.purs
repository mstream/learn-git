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
  | GitInit Path
  | MakeDir Path

instance stringCodecCmd :: StringCodec Cmd where
  decodeFromString :: String -> String \/ Cmd
  decodeFromString = decodeUsingParser cmdParser
  encodeToString :: Cmd -> String
  encodeToString = case _ of
    EditFile path content -> "edit " <> encodeToString path <> " \"" <> encodeToString content <> "\""
    GitInit path -> "git init " <> encodeToString path
    MakeDir path -> "mkdir " <> encodeToString path

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
        void $ string "git"
        skipSpaces
        void $ string "init"
        skipSpaces
        path <- pathParser
        pure $ GitInit path
    , do
        void $ string "mkdir"
        skipSpaces
        path <- pathParser
        pure $ MakeDir path
    ]

quoteParser :: Parser Char
quoteParser = char '"' <|> char '\''

commandName :: Cmd -> String
commandName = case _ of
  EditFile _ _ -> "edit"
  GitInit _ -> "git init"
  MakeDir _ -> "mkdir"

commandArgs :: Cmd -> Array String
commandArgs = case _ of
  EditFile path content -> [ encodeToString path, encodeToString content ]
  GitInit path -> [ encodeToString path ]
  MakeDir path -> [ encodeToString path ]
