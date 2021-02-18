module Ffi.Fs (exists, isDir, joinPaths, readDir, readFile, writeFile) where

import Prelude
import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFn1)
import Foreign (Foreign)

foreign import joinPaths :: String -> String -> String

foreign import mkExistsPromise :: String -> Effect (Promise Boolean)

foreign import mkReadFilePromise :: String -> Effect (Promise String)

foreign import mkReadDirPromise :: String -> Effect (Promise (Array String))

foreign import mkIsDirPromise :: String -> Effect (Promise Boolean)

foreign import mkWriteFilePromise :: String -> String -> Effect (Promise Unit)

exists :: String -> Aff Boolean
exists = toAffE <<< mkExistsPromise

readFile :: String -> Aff String
readFile = toAffE <<< mkReadFilePromise

readDir :: String -> Aff (Array String)
readDir = toAffE <<< mkReadDirPromise

isDir :: String -> Aff Boolean
isDir = toAffE <<< mkIsDirPromise

writeFile :: String -> String -> Aff Unit
writeFile path content = toAffE $ mkWriteFilePromise path content
