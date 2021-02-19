module Main (main) where

import Prelude
import Core.Cli (Cmd)
import Core.Event (Event(..))
import Core.Fs (File(..))
import Core.Logger (LogEntry)
import Core.State (State)
import Data.List (List(..))
import Data.Map (empty)
import Data.Set (Set)
import Domain.App (handleEvent)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Infra.Ui.Element as E
import Infra.Ui.Perspective (perspective)
import Infra.Ui.Term (terminal)
import ProdApi (runApi)

main :: Effect Unit
main = E.runInDom "main" $ app initialState

initialState :: State
initialState =
  { cliHistory: Nil
  , cliInput: ""
  , cliLogs: mempty
  , fileTree: DirectoryOf empty
  }

app :: State -> E.Element State
app state = do
  event <- E.div [ E.className "container mx-auto px-4 h-screen py-4" ] [ page state ]
  newState <- liftAff $ runApi $ handleEvent state event
  app newState

page :: forall r. { cliHistory :: List Cmd, cliLogs :: Set LogEntry, fileTree :: File | r } -> E.Element Event
page state =
  E.div
    [ E.className "grid grid-cols-12" ]
    [ E.div
        [ E.className "col-span-3" ]
        [ CmdExecRequested <$> terminal { history: state.cliHistory, logs: state.cliLogs } ]
    , E.div
        [ E.className "col-span-9" ]
        [ perspective state ]
    ]
