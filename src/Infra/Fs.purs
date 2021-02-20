module Infra.Fs
  ( exists
  , gitInit
  , isDir
  , joinPaths
  , mkDir
  , readDir
  , readFile
  , writeFile
  ) where

import Prelude
import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import joinPaths :: String -> String -> String

foreign import mkExistsPromise :: String -> Effect (Promise Boolean)

foreign import mkGitInitPromise :: String -> Effect (Promise Unit)

foreign import mkReadFilePromise :: String -> Effect (Promise String)

foreign import mkReadDirPromise :: String -> Effect (Promise (Array String))

foreign import mkIsDirPromise :: String -> Effect (Promise Boolean)

foreign import mkMkDirPromise :: String -> Effect (Promise Unit)

foreign import mkWriteFilePromise :: String -> String -> Effect (Promise Unit)

exists :: String -> Aff Boolean
exists = toAffE <<< mkExistsPromise

gitInit :: String -> Aff Unit
gitInit = toAffE <<< mkGitInitPromise

readFile :: String -> Aff String
readFile = toAffE <<< mkReadFilePromise

readDir :: String -> Aff (Array String)
readDir = toAffE <<< mkReadDirPromise

isDir :: String -> Aff Boolean
isDir = toAffE <<< mkIsDirPromise

mkDir :: String -> Aff Unit
mkDir path = toAffE $ mkMkDirPromise path

writeFile :: String -> String -> Aff Unit
writeFile path content = toAffE $ mkWriteFilePromise path content
