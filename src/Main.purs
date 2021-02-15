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
  action <- div [ className "container h-screen mx-auto px-4" ] [ layout state ]
  case action of
    CliInputUpdated val -> app $ state { cliInput = val }
    CliInputSubmitted -> app $ state { cliInput = "", cliHistory = state.cliInput : state.cliHistory }

layout :: State -> Widget HTML Action
layout state =
  div
    [ className "grid grid-cols-12 justify-items-stretch w-full h-full" ]
    [ div
        [ className "col-span-3 row-span-5 justify-content-center" ]
        [ cliHistory state ]
    , div
        [ className "col-span-9 row-span 5" ]
        []
    , div
        [ className "col-span-12" ]
        [ cliInput state ]
    ]

cliHistory :: forall a r. { cliHistory :: List String | r } -> Widget HTML a
cliHistory state = div [ className "flex flex-col-reverse w-full h-full" ] $ fromFoldable $ state.cliHistory <#> cliHistoryEntry

cliHistoryEntry :: forall a. String -> Widget HTML a
cliHistoryEntry val = div' [ text val ]

cliInput :: forall r. { cliInput :: String | r } -> Widget HTML Action
cliInput state =
  input
    [ _type "text"
    , value state.cliInput
    , (CliInputUpdated <<< unsafeTargetValue) <$> onChange
    , CliInputSubmitted <$ onKeyEnter
    , className "w-full h-6"
    ]
