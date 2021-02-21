module Core.Cli (Cmd(..), GitCmd(..), commandArgs, commandName) where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Core.Fs (FileContent, Path, pathParser, textualFileContentParser)
import Core.Git (CommitMessage, PathSpec, commitMessageParser, pathSpecParser)
import Core.StringCodec (class StringCodec, decodeUsingParser, encodeToString)
import Data.Either.Nested (type (\/))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (char, skipSpaces, string)
import Text.Parsing.StringParser.Combinators (between, choice)

data GitCmd
  = GitAdd Path PathSpec
  | GitCommit Path CommitMessage
  | GitInit Path

data Cmd
  = EditFile Path FileContent
  | Git GitCmd
  | MakeDir Path

instance stringCodecCmd :: StringCodec Cmd where
  decodeFromString :: String -> String \/ Cmd
  decodeFromString = decodeUsingParser cmdParser
  encodeToString :: Cmd -> String
  encodeToString = case _ of
    EditFile path content -> "edit " <> encodeToString path <> " \"" <> encodeToString content <> "\""
    Git (GitAdd repoDirPath pathSpec) -> "git add " <> encodeToString repoDirPath <> " " <> encodeToString pathSpec
    Git (GitCommit repoDirPath msg) -> "git commit " <> encodeToString repoDirPath <> " \"" <> encodeToString msg <> "\""
    Git (GitInit repoDirPath) -> "git init " <> encodeToString repoDirPath
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
        content <- between (char '"') (char '"') textualFileContentParser
        pure $ EditFile path content
    , do
        void $ string "git"
        skipSpaces
        gitSubCmd <- choice gitSubCmdParsers
        pure $ Git gitSubCmd
    , do
        void $ string "mkdir"
        skipSpaces
        path <- pathParser
        pure $ MakeDir path
    ]

  gitSubCmdParsers :: Array (Parser GitCmd)
  gitSubCmdParsers =
    [ do
        void $ string "add"
        skipSpaces
        repoDirPath <- pathParser
        skipSpaces
        pathSpec <- pathSpecParser
        pure $ GitAdd repoDirPath pathSpec
    , do
        void $ string "commit"
        skipSpaces
        repoDirPath <- pathParser
        skipSpaces
        msg <- between (char '"') (char '"') commitMessageParser
        pure $ GitCommit repoDirPath msg
    , do
        void $ string "init"
        skipSpaces
        repoDirPath <- pathParser
        pure $ GitInit repoDirPath
    ]

quoteParser :: Parser Char
quoteParser = char '"' <|> char '\''

commandName :: Cmd -> String
commandName = case _ of
  EditFile _ _ -> "edit"
  Git (GitAdd _ _) -> "git add"
  Git (GitCommit _ _) -> "git commit"
  Git (GitInit _) -> "git init"
  MakeDir _ -> "mkdir"

commandArgs :: Cmd -> Array String
commandArgs = case _ of
  EditFile path content -> [ encodeToString path, encodeToString content ]
  Git (GitAdd repoDirPath pathSpec) -> [ encodeToString repoDirPath, encodeToString pathSpec ]
  Git (GitCommit repoDirPath msg) -> [ encodeToString repoDirPath, encodeToString msg ]
  Git (GitInit repoDirPath) -> [ encodeToString repoDirPath ]
  MakeDir path -> [ encodeToString path ]
