module ProdApi (runApi) where

import Prelude
import Api (AppM, runAppM)
import Core.Fs (FileContent, FileName, FileType(..), Path, binaryFileContent)
import Core.Git (CommitMessage, PathSpec)
import Core.Logger (LogEntry, LogLevel(..), logLevel)
import Core.StringCodec (decodeFromString, encodeToString)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, attempt, message)
import Effect.Class (liftEffect)
import Effect.Console (error, info, warn)
import Infra.Fs (gitAdd, gitCommit, gitInit, isBinary, isDir, mkDir, readDir, readFile, writeFile)
import Node.Buffer.Immutable (ImmutableBuffer, fromArrayBuffer, toArrayBuffer, toString)
import Node.Encoding (Encoding(..))

runApi :: AppM ~> Aff
runApi =
  runAppM
    { commitChanges: commitChanges
    , createDirectory: createDirectory
    , getFileContent: getFileContent
    , getFileNames: getFileNames
    , getFileType: getFileType
    , initGitRepo: initGitRepo
    , log: log
    , saveFileContent: saveFileContent
    , stageFiles: stageFiles
    }

commitChanges :: Path -> CommitMessage -> Aff (String \/ Unit)
commitChanges repoDirPath msg = do
  result <- attempt $ gitCommit (encodeToString repoDirPath) (encodeToString msg)
  pure case result of
    Left error -> Left $ message error
    Right _ -> pure unit

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
    Right b ->
      let
        buffer :: ImmutableBuffer
        buffer = fromArrayBuffer b
      in
        if isBinary $ toArrayBuffer buffer then
          Right binaryFileContent
        else
          decodeFromString $ toString UTF8 buffer

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

stageFiles :: Path -> PathSpec -> Aff (String \/ Unit)
stageFiles repoDirPath pathSpec = do
  result <- attempt $ gitAdd (encodeToString repoDirPath) (encodeToString pathSpec)
  pure case result of
    Left error -> Left $ message error
    Right _ -> Right unit
