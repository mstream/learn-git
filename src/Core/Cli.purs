module Core.Cli (Cmd(..), commandArgs, commandName) where

import Prelude
import Control.Alt ((<|>))
import Core.FileSystem (FileContent, Path, fileContentParser, pathParser)
import Core.StringCodec (class StringCodec, decodeUsingParser, encodeToString)
import Data.Either.Nested (type (\/))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, skipSpaces, string)
import Text.Parsing.StringParser.Combinators (between)

data Cmd
  = EditFile Path FileContent

instance stringCodecCmd :: StringCodec Cmd where
  decodeFromString :: String -> String \/ Cmd
  decodeFromString = decodeUsingParser cmdParser
  encodeToString :: Cmd -> String
  encodeToString = case _ of
    (EditFile path content) -> "edit " <> encodeToString path <> " \"" <> encodeToString content <> "\""

cmdParser :: Parser Cmd
cmdParser = do
  skipSpaces
  void $ string "edit"
  skipSpaces
  path <- pathParser
  skipSpaces
  content <- between (char '"') (char '"') fileContentParser
  skipSpaces
  pure $ EditFile path content

quoteParser :: Parser Char
quoteParser = char '"' <|> char '\''

commandName :: Cmd -> String
commandName = case _ of
  EditFile _ _ -> "edit"

commandArgs :: Cmd -> Array String
commandArgs = case _ of
  EditFile path content -> [ encodeToString path, encodeToString content ]
