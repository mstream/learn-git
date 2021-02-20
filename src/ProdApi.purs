module ProdApi (runApi) where

import Prelude
import Api (AppM, runAppM)
import Core.Fs (FileContent, FileName, FileType(..), Path)
import Core.Logger (LogEntry, LogLevel(..), logLevel)
import Core.StringCodec (decodeFromString, encodeToString)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, attempt, message)
import Effect.Class (liftEffect)
import Effect.Console (error, info, warn)
import Infra.Fs (gitInit, isDir, mkDir, readDir, readFile, writeFile)

runApi :: AppM ~> Aff
runApi =
  runAppM
    { createDirectory: createDirectory
    , getFileContent: getFileContent
    , getFileNames: getFileNames
    , getFileType: getFileType
    , initGitRepo: initGitRepo
    , log: log
    , saveFileContent: saveFileContent
    }

createDirectory :: Path -> Aff (String \/ Unit)
createDirectory path = do
  result <- attempt $ mkDir $ encodeToString path
  pure case result of
    Left error -> Left $ message error
    Right _ -> pure unit

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

initGitRepo :: Path -> Aff (String \/ Unit)
initGitRepo path = do
  result <- attempt $ gitInit $ encodeToString path
  pure case result of
    Left error -> Left $ message error
    Right _ -> Right unit

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
