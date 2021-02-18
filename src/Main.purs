module Main (main) where

import Prelude
import Core.Cli (Cmd)
import Core.Event (Event(..))
import Core.State (State, CliLogs)
import Data.List (List(..))
import Domain.App (handleEvent)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Infrastructure.Ui.Element as E
import Infrastructure.Ui.Terminal (terminal)
import ProdApi (runApi)

main :: Effect Unit
main = E.runInDom "main" $ app initialState

initialState :: State
initialState =
  { cliHistory: Nil
  , cliInput: ""
  , cliLogs: mempty
  }

app :: State -> E.Element State
app state = do
  event <- E.div [ E.className "container mx-auto px-4 h-screen py-4" ] [ page state ]
  newState <- liftAff $ runApi $ handleEvent state event
  app newState

page :: forall r. { cliHistory :: List Cmd, cliLogs :: CliLogs | r } -> E.Element Event
page state =
  E.div
    [ E.className "grid grid-cols-12" ]
    [ E.div
        [ E.className "col-span-3" ]
        [ CmdExecRequested <$> terminal { history: state.cliHistory, logs: state.cliLogs } ]
    , E.div
        [ E.className "col-span-9" ]
        [ E.text "perspective" ]
    ]
