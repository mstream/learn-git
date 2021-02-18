module Api.Log (logError, logInfo, logWarn) where

import Prelude
import Effect (Effect)

foreign import logError :: String -> Effect Unit

foreign import logInfo :: String -> Effect Unit

foreign import logWarn :: String -> Effect Unit
