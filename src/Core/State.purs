module Core.State (State) where

import Core.Cli (Cmd)
import Core.Fs (File)
import Core.Logger (LogEntry)
import Data.List (List)
import Data.Set (Set)

type State
  = { cliHistory :: List Cmd
    , cliInput :: String
    , cliLogs :: Set LogEntry
    , fileTree :: File
    }
