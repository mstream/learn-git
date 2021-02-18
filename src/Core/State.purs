module Core.State (CliLogs, State) where

import Core.Cli (Cmd)
import Core.Logger (LogEntry)
import Data.List (List)
import Data.Set (Set)

type State
  = { cliHistory :: List Cmd
    , cliInput :: String
    , cliLogs :: CliLogs
    }

type CliLogs
  = Set LogEntry
