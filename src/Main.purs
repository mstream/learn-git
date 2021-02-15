module Main (main) where

import Prelude
import Concur.React.DOM (text)
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)

main :: Effect Unit
main = runWidgetInDom "main" $ text "hello!"
