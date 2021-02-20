module Api (AppM, Env, ParAppM, runAppM) where

import Prelude
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Control.Parallel (class Parallel, parallel, sequential)
import Core.Fs (FileContent, FileName, FileType, Path)
import Core.Logger (LogEntry)
import Data.Either.Nested (type (\/))
import Domain.Caps (class CreateDirectory, class GetFileContent, class GetFileNames, class GetFileType, class InitGitRepo, class Log, class SaveFileContent)
import Effect.Aff (Aff, ParAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Type.Prelude (class TypeEquals, from)

type Env
  = { createDirectory :: Path -> Aff (String \/ Unit)
    , getFileContent :: Path -> Aff (String \/ FileContent)
    , getFileNames :: Path -> Aff (String \/ Array FileName)
    , getFileType :: Path -> Aff (String \/ FileType)
    , initGitRepo :: Path -> Aff (String \/ Unit)
    , log :: LogEntry -> Aff Unit
    , saveFileContent :: Path -> FileContent -> Aff (String \/ Unit)
    }

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

newtype ParAppM a
  = ParAppM (ReaderT Env ParAff a)

derive newtype instance functorParAppM :: Functor ParAppM

derive newtype instance applyParAppM :: Apply ParAppM

derive newtype instance applicativeParAppM :: Applicative ParAppM

instance parallelAppM :: Parallel ParAppM AppM where
  parallel (AppM readerT) = ParAppM (parallel readerT)
  sequential (ParAppM readerT) = AppM (sequential readerT)

instance createDirectoryAppM :: CreateDirectory AppM where
  createDirectory :: Path -> AppM (String \/ Unit)
  createDirectory path = do
    env <- ask
    liftAff $ env.createDirectory path

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

instance initGitRepoAppM :: InitGitRepo AppM where
  initGitRepo :: Path -> AppM (String \/ Unit)
  initGitRepo path = do
    env <- ask
    liftAff $ env.initGitRepo path

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
