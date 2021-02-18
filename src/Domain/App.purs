module Domain.App (handleEvent) where

import Prelude
import Core.Event (Event(..))
import Core.Logger (LogEntry, LogLevel(..), logEntry)
import Core.State (State, CliLogs)
import Core.StringCodec (decodeFromString)
import Data.Either (Either(..))
import Data.List ((:))
import Data.Set (insert)
import Data.Symbol (SProxy(..))
import Domain.Capabilities (class GetFileContent, class GetFileNames, class GetFileType, class Log, class SaveFileContent, log, saveFileContent)

handleEvent :: forall m. GetFileContent m => GetFileNames m => GetFileType m => Log m => SaveFileContent m => State -> Event -> m State
handleEvent state event = case event of
  CmdExecRequested input -> handleCmdExecRequested state input

handleCmdExecRequested :: forall m. Log m => SaveFileContent m => State -> String -> m State
handleCmdExecRequested state input = case decodeFromString input, decodeFromString "conent1" of
  Right path, Right content -> do
    result <- saveFileContent path content
    case result of
      Right _ -> do
        newCliLogs <- cliLog mempty $ logEntry Info (SProxy :: SProxy "Command executed") input
        pure $ state { cliHistory = input : state.cliHistory, cliInput = "", cliLogs = newCliLogs }
      Left errMsg -> do
        newCliLogs <- cliLog mempty $ logEntry Error (SProxy :: SProxy "Command execution failed") input
        pure $ state { cliLogs = newCliLogs }
  _, _ -> do
    newCliLogs <- cliLog mempty $ logEntry Error (SProxy :: SProxy "Command validation failed") input
    pure $ state { cliLogs = newCliLogs }

cliLog :: forall m. Log m => CliLogs -> LogEntry -> m CliLogs
cliLog logs entry = do
  log entry
  pure $ insert entry logs
