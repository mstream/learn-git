module Infra.Fs
  ( exists
  , gitAdd
  , gitCommit
  , gitInit
  , isBinary
  , isDir
  , joinPaths
  , mkDir
  , readDir
  , readFile
  , writeFile
  ) where

import Prelude
import Control.Promise (Promise, toAffE)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import isBinary :: ArrayBuffer -> Boolean

foreign import joinPaths :: String -> String -> String

foreign import mkExistsPromise :: String -> Effect (Promise Boolean)

foreign import mkGitAddPromise :: String -> String -> Effect (Promise Unit)

foreign import mkGitCommitPromise :: String -> String -> Effect (Promise Unit)

foreign import mkGitInitPromise :: String -> Effect (Promise Unit)

foreign import mkReadFilePromise :: String -> Effect (Promise ArrayBuffer)

foreign import mkReadDirPromise :: String -> Effect (Promise (Array String))

foreign import mkIsDirPromise :: String -> Effect (Promise Boolean)

foreign import mkMkDirPromise :: String -> Effect (Promise Unit)

foreign import mkWriteFilePromise :: String -> String -> Effect (Promise Unit)

exists :: String -> Aff Boolean
exists = toAffE <<< mkExistsPromise

gitAdd :: String -> String -> Aff Unit
gitAdd repoDirPath pathSpec = toAffE $ mkGitAddPromise repoDirPath pathSpec

gitCommit :: String -> String -> Aff Unit
gitCommit repoDirPath pathSpec = toAffE $ mkGitCommitPromise repoDirPath pathSpec

gitInit :: String -> Aff Unit
gitInit = toAffE <<< mkGitInitPromise

readFile :: String -> Aff ArrayBuffer
readFile = toAffE <<< mkReadFilePromise

readDir :: String -> Aff (Array String)
readDir = toAffE <<< mkReadDirPromise

isDir :: String -> Aff Boolean
isDir = toAffE <<< mkIsDirPromise

mkDir :: String -> Aff Unit
mkDir path = toAffE $ mkMkDirPromise path

writeFile :: String -> String -> Aff Unit
writeFile path content = toAffE $ mkWriteFilePromise path content
