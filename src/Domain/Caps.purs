module Domain.Caps
  ( class CreateDirectory
  , class GetFileContent
  , class GetFileNames
  , class GetFileType
  , class InitGitRepo
  , class Log
  , class SaveFileContent
  , class StageFiles
  , createDirectory
  , getFileContent
  , getFileNames
  , getFileType
  , initGitRepo
  , log
  , saveFileContent
  , stageFiles
  ) where

import Prelude
import Core.Fs (FileContent, FileName, FileType, Path, PathSpec)
import Core.Logger (LogEntry)
import Data.Either.Nested (type (\/))

class
  (Monad m) <= CreateDirectory m where
  createDirectory :: Path -> m (String \/ Unit)

class
  (Monad m) <= GetFileContent m where
  getFileContent :: Path -> m (String \/ FileContent)

class
  (Monad m) <= GetFileNames m where
  getFileNames :: Path -> m (String \/ Array FileName)

class
  (Monad m) <= GetFileType m where
  getFileType :: Path -> m (String \/ FileType)

class
  (Monad m) <= StageFiles m where
  stageFiles :: Path -> PathSpec -> m (String \/ Unit)

class
  (Monad m) <= InitGitRepo m where
  initGitRepo :: Path -> m (String \/ Unit)

class
  (Monad m) <= Log m where
  log :: LogEntry -> m Unit

class
  (Monad m) <= SaveFileContent m where
  saveFileContent :: Path -> FileContent -> m (String \/ Unit)
