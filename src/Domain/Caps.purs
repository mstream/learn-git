module Domain.Caps
  ( class CreateDirectory
  , class GetFileContent
  , class GetFileNames
  , class GetFileType
  , class Log
  , class SaveFileContent
  , createDirectory
  , getFileContent
  , getFileNames
  , getFileType
  , log
  , saveFileContent
  ) where

import Prelude
import Core.Event (Event)
import Core.Fs (FileContent, FileName, FileType, Path)
import Core.Logger (LogEntry(..))
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
  (Monad m) <= Log m where
  log :: LogEntry -> m Unit

class
  (Monad m) <= SaveFileContent m where
  saveFileContent :: Path -> FileContent -> m (String \/ Unit)
