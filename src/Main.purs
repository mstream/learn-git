module Main (main) where

import Prelude
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, div', input, text)
import Concur.React.Props (_type, className, onChange, onKeyEnter, unsafeTargetValue, value)
import Concur.React.Run (runWidgetInDom)
import Data.Array (fromFoldable)
import Data.List (List(..), (:))
import Effect (Effect)

main :: Effect Unit
main = runWidgetInDom "main" $ app { cliInput: "", cliHistory: Nil }

data Action
  = CliInputUpdated String
  | CliInputSubmitted

type State
  = { cliHistory :: List String
    , cliInput :: String
    }

app :: forall a. State -> Widget HTML a
app state = do
  action <- div [ className "container mx-auto px-4" ] [ cli state ]
  case action of
    CliInputUpdated val -> app $ state { cliInput = val }
    CliInputSubmitted -> app $ state { cliInput = "", cliHistory = state.cliInput : state.cliHistory }

cli :: forall r. { cliInput :: String, cliHistory :: List String | r } -> Widget HTML Action
cli state = div [ className "flex flex-col" ] [ cliHistory state, cliInput state ]

cliHistory :: forall a r. { cliHistory :: List String | r } -> Widget HTML a
cliHistory state = div [ className "flex flex-col-reverse" ] $ fromFoldable $ state.cliHistory <#> cliHistoryEntry

cliHistoryEntry :: forall a. String -> Widget HTML a
cliHistoryEntry val = div' [ text val ]

cliInput :: forall r. { cliInput :: String | r } -> Widget HTML Action
cliInput state =
  input
    [ _type "text"
    , value state.cliInput
    , (CliInputUpdated <<< unsafeTargetValue) <$> onChange
    , CliInputSubmitted <$ onKeyEnter
    ]
