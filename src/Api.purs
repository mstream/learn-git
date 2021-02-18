module Api (AppM, Environment, runAppM) where

import Prelude
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Core.FileSystem (FileContent, FileName, FileType, Path)
import Core.Logger (LogEntry)
import Data.Either.Nested (type (\/))
import Domain.Capabilities (class GetFileContent, class GetFileNames, class GetFileType, class Log, class SaveFileContent)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Type.Prelude (class TypeEquals, from)

type Environment
  = { getFileContent :: Path -> Aff (String \/ FileContent)
    , getFileNames :: Path -> Aff (String \/ Array FileName)
    , getFileType :: Path -> Aff (String \/ FileType)
    , log :: LogEntry -> Aff Unit
    , saveFileContent :: Path -> FileContent -> Aff (String \/ Unit)
    }

newtype AppM a
  = AppM (ReaderT Environment Aff a)

runAppM :: Environment -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Environment => MonadAsk e AppM where
  ask = AppM $ asks from

instance getFileContentAppM :: GetFileContent AppM where
  getFileContent :: Path -> AppM (String \/ FileContent)
  getFileContent path = do
    env <- ask
    liftAff $ env.getFileContent path

instance getFileNamesAppM :: GetFileNames AppM where
  getFileNames :: Path -> AppM (String \/ Array FileName)
  getFileNames path = do
    env <- ask
    liftAff $ env.getFileNames path

instance getFileTypeAppM :: GetFileType AppM where
  getFileType :: Path -> AppM (String \/ FileType)
  getFileType path = do
    env <- ask
    liftAff $ env.getFileType path

instance logAppM :: Log AppM where
  log :: LogEntry -> AppM Unit
  log entry = do
    env <- ask
    liftAff $ env.log entry

instance saveFileContentAppM :: SaveFileContent AppM where
  saveFileContent :: Path -> FileContent -> AppM (String \/ Unit)
  saveFileContent path content = do
    env <- ask
    liftAff $ env.saveFileContent path content
