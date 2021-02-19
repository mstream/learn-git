module Domain.App (handleEvent) where

import Prelude
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Parallel (class Parallel, parSequence)
import Core.Cli (Cmd(..))
import Core.Event (Event(..))
import Core.Fs (File(..), FileType(..), Path, fromFileName)
import Core.Logger (LogEntry, LogLevel(..), logEntry)
import Core.State (State)
import Core.StringCodec (decodeFromString)
import Data.Array (zip)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List ((:))
import Data.Map (fromFoldable)
import Data.Set (Set, insert)
import Data.Symbol (SProxy(..))
import Domain.Caps (class CreateDirectory, class GetFileContent, class GetFileNames, class GetFileType, class Log, class SaveFileContent, createDirectory, getFileContent, getFileNames, getFileType, log, saveFileContent)

handleEvent :: forall f m. Parallel f m => CreateDirectory m => GetFileContent m => GetFileNames m => GetFileType m => Log m => SaveFileContent m => State -> Event -> m State
handleEvent state event = case event of
  CmdExecRequested input -> handleCmdExecRequested state input

handleCmdExecRequested :: forall f m. Parallel f m => CreateDirectory m => GetFileContent m => GetFileNames m => GetFileType m => Log m => SaveFileContent m => State -> String -> m State
handleCmdExecRequested state input = case decodeFromString input of
  Right editFileCmd@(EditFile path content) -> do
    result <- saveFileContent path content
    case result of
      Right _ -> do
        newCliLogs <- cliLog mempty $ logEntry Info (SProxy :: SProxy "Command executed") input
        newFileTreeRes <- getFileTree
        case newFileTreeRes of
          Left errMsg -> do
            newCliLogs' <- cliLog newCliLogs $ logEntry Error (SProxy :: SProxy "File tree retrieval failed") errMsg
            pure $ state { cliLogs = newCliLogs' }
          Right newFileTree -> pure $ state { cliHistory = editFileCmd : state.cliHistory, cliInput = "", cliLogs = newCliLogs, fileTree = newFileTree }
      Left errMsg -> do
        newCliLogs <- cliLog mempty $ logEntry Error (SProxy :: SProxy "Command execution failed") input
        pure $ state { cliLogs = newCliLogs }
  Right makeDirectoryCmd@(MakeDirectory path) -> do
    result <- createDirectory path
    case result of
      Right _ -> do
        newCliLogs <- cliLog mempty $ logEntry Info (SProxy :: SProxy "Command executed") input
        newFileTreeRes <- getFileTree
        case newFileTreeRes of
          Left errMsg -> do
            newCliLogs' <- cliLog newCliLogs $ logEntry Error (SProxy :: SProxy "File tree retrieval failed") errMsg
            pure $ state { cliLogs = newCliLogs' }
          Right newFileTree -> pure $ state { cliHistory = makeDirectoryCmd : state.cliHistory, cliInput = "", cliLogs = newCliLogs, fileTree = newFileTree }
      Left errMsg -> do
        newCliLogs <- cliLog mempty $ logEntry Error (SProxy :: SProxy "Command execution failed") input
        pure $ state { cliLogs = newCliLogs }
  Left errMsg -> do
    newCliLogs <- cliLog mempty $ logEntry Error (SProxy :: SProxy "Command validation failed") (input <> " " <> errMsg)
    pure $ state { cliLogs = newCliLogs }

getFileTree :: forall f m. Parallel f m => GetFileContent m => GetFileNames m => GetFileType m => m (String \/ File)
getFileTree = runExceptT $ go $ mempty
  where
  go :: Path -> ExceptT String m File
  go path = do
    fileType <- ExceptT $ getFileType path
    case fileType of
      Directory -> do
        fileNames <- ExceptT $ getFileNames path
        let
          filePaths :: Array Path
          filePaths = (append path <<< fromFileName) <$> fileNames
        files <- parSequence $ go <$> filePaths
        pure $ DirectoryOf $ fromFoldable $ zip fileNames files
      RegularFile -> do
        fileContent <- ExceptT $ getFileContent path
        pure $ RegularFileContaining fileContent

cliLog :: forall m. Log m => Set LogEntry -> LogEntry -> m (Set LogEntry)
cliLog logs entry = do
  log entry
  pure $ insert entry logs
