module Infrastructure.Ui.Terminal (terminal) where

import Prelude
import Core.Logger (LogEntry, LogLevel(..), logLevel)
import Core.State (CliLogs)
import Core.StringCodec (encodeToString)
import Data.Array (fromFoldable)
import Data.List (List)
import Infrastructure.Ui.Element as E

data Event
  = InputSubmitted
  | InputUpdated String

terminal :: forall r. { history :: List String, logs :: CliLogs | r } -> E.Element String
terminal state =
  E.div
    [ E.className "grid grid-rows-6" ]
    [ E.div [ E.className "row-span-5" ]
        [ history state ]
    , E.div
        [ E.className "grid grid-rows-6" ]
        [ E.div
            [ E.className "row-span-5" ]
            [ logsWindow state.logs ]
        , E.div' [ commandLine "" ]
        ]
    ]

commandLine :: String -> E.Element String
commandLine val = do
  event <-
    E.input
      [ E._type "text"
      , E.value val
      , (InputUpdated <<< E.unsafeTargetValue) <$> E.onChange
      , InputSubmitted <$ E.onKeyEnter
      , E.className "w-full h-6"
      ]
  case event of
    InputSubmitted -> pure val
    InputUpdated newVal -> commandLine newVal

history :: forall a r. { history :: List String | r } -> E.Element a
history state = E.div [ E.className "flex flex-col-reverse w-full h-full" ] entries
  where
  entries :: Array (E.Element a)
  entries = fromFoldable $ state.history <#> historyEntry

historyEntry :: forall a. String -> E.Element a
historyEntry val = E.div' [ E.text val ]

logsWindow :: forall a. CliLogs -> E.Element a
logsWindow logs = E.ul' $ toElement <$> fromFoldable logs
  where
  toElement :: LogEntry -> E.Element a
  toElement entry =
    E.li
      [ E.className $ colorFor $ logLevel entry ]
      [ E.text $ encodeToString entry ]

  colorFor :: LogLevel -> String
  colorFor = case _ of
    Error -> "text-red-500"
    Info -> "text-blue-500"
    Warn -> "text-yellow-500"
