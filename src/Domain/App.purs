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
import Domain.Caps (class CreateDirectory, class GetFileContent, class GetFileNames, class GetFileType, class InitGitRepo, class Log, class SaveFileContent, createDirectory, getFileContent, getFileNames, getFileType, initGitRepo, log, saveFileContent)

handleEvent :: forall f m. Parallel f m => CreateDirectory m => GetFileContent m => GetFileNames m => GetFileType m => InitGitRepo m => Log m => SaveFileContent m => State -> Event -> m State
handleEvent state event = case event of
  CmdExecRequested input -> do
    cmdExecRes <- handleCmdExecRequested state input
    newFileTreeRes <- getFileTree
    case cmdExecRes, newFileTreeRes of
      Left cmdExecErrMsg, Left newFileTreeErrMsg -> do
        newCliLogs <- cliLog mempty $ logEntry Error (SProxy :: SProxy "Command execution failed failed") cmdExecErrMsg
        newCliLogs' <- cliLog newCliLogs $ logEntry Error (SProxy :: SProxy "File tree retrieval failed") newFileTreeErrMsg
        pure $ state { cliLogs = newCliLogs }
      Left cmdExecErrMsg, Right newFileTree -> do
        newCliLogs <- cliLog mempty $ logEntry Error (SProxy :: SProxy "Command execution failed failed") cmdExecErrMsg
        pure $ state { cliLogs = newCliLogs, fileTree = newFileTree }
      Right cmd, Left newFileTreeErrMsg -> do
        newCliLogs <- cliLog mempty $ logEntry Error (SProxy :: SProxy "File tree retrieval failed") newFileTreeErrMsg
        pure $ state { cliHistory = cmd : state.cliHistory, cliLogs = newCliLogs }
      Right cmd, Right newFileTree -> do
        pure $ state { cliHistory = cmd : state.cliHistory, fileTree = newFileTree }

handleCmdExecRequested :: forall f m. Parallel f m => CreateDirectory m => GetFileContent m => GetFileNames m => GetFileType m => InitGitRepo m => Log m => SaveFileContent m => State -> String -> m (String \/ Cmd)
handleCmdExecRequested state input = case decodeFromString input of
  Right editFileCmd@(EditFile path content) -> do
    res <- saveFileContent path content
    pure case res of
      Left errMsg -> Left $ "could not edit file because of " <> errMsg
      Right _ -> Right editFileCmd
  Right gitInitCmd@(GitInit path) -> do
    res <- initGitRepo path
    pure case res of
      Left errMsg -> Left $ "could not initialize git repository because of " <> errMsg
      Right _ -> Right gitInitCmd
  Right makeDirCmd@(MakeDir path) -> do
    res <- createDirectory path
    pure case res of
      Left errMsg -> Left $ "could not create directory because of " <> errMsg
      Right _ -> Right makeDirCmd
  Left decodingErrMsg -> pure $ Left $ "could not parse the input '" <> input <> "'"

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
