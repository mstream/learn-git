module Main (main) where

import Prelude
import Api (AppM, runAppM)
import Api.Fs (isDir, readDir, readFile, writeFile)
import Core.Event (Event(..))
import Core.FileSystem (FileContent, FileName, FileType(..), Path)
import Core.Logger (LogEntry, LogLevel(..), logLevel)
import Core.State (State, CliLogs)
import Core.StringCodec (decodeFromString, encodeToString)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List(..))
import Data.Map (fromFoldable)
import Data.Monoid (mempty)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Domain.App (handleEvent)
import Effect (Effect)
import Effect.Aff (Aff, attempt, message)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (error, info, warn)
import Infrastructure.Ui.Element as E
import Infrastructure.Ui.Terminal (terminal)

main :: Effect Unit
main = E.runInDom "main" $ app initialState

initialState :: State
initialState =
  { cliHistory: Nil
  , cliInput: ""
  , cliLogs: mempty
  }

app :: State -> E.Element State
app state = do
  event <- E.div [ E.className "container mx-auto px-4 h-screen py-4" ] [ page state ]
  newState <- liftAff $ runApi $ handleEvent state event
  app newState

page :: forall r. { cliHistory :: List String, cliLogs :: CliLogs | r } -> E.Element Event
page state =
  E.div
    [ E.className "grid grid-cols-12" ]
    [ E.div
        [ E.className "col-span-3" ]
        [ CmdExecRequested <$> terminal { history: state.cliHistory, logs: state.cliLogs } ]
    , E.div
        [ E.className "col-span-9" ]
        [ E.text "perspective" ]
    ]

runApi :: AppM ~> Aff
runApi =
  runAppM
    { getFileContent: getFileContent
    , getFileNames: getFileNames
    , getFileType: getFileType
    , log: log
    , saveFileContent: saveFileContent
    }

getFileContent :: Path -> Aff (String \/ FileContent)
getFileContent path = do
  result <- attempt $ readFile $ encodeToString path
  pure case result of
    Left error -> Left $ message error
    Right s -> decodeFromString s

getFileNames :: Path -> Aff (String \/ Array FileName)
getFileNames path = do
  result <- attempt $ readDir $ encodeToString path
  pure case result of
    Left error -> Left $ message error
    Right ss -> traverse decodeFromString ss

getFileType :: Path -> Aff (String \/ FileType)
getFileType path = do
  result <- attempt $ isDir $ encodeToString path
  pure case result of
    Left error -> Left $ message error
    Right b -> if b then Right Directory else Right RegularFile

log :: LogEntry -> Aff Unit
log entry = liftEffect $ logToConsole $ encodeToString entry
  where
  logToConsole :: String -> Effect Unit
  logToConsole = case logLevel entry of
    Error -> error
    Info -> info
    Warn -> warn

saveFileContent :: Path -> FileContent -> Aff (String \/ Unit)
saveFileContent path content = do
  result <- attempt $ writeFile (encodeToString path) (encodeToString content)
  pure case result of
    Left error -> Left $ message error
    Right _ -> Right unit
