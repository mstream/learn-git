module Core.State (CliLogs, State) where

import Prelude
import Core.Logger (LogEntry(..), LogLevel(..))
import Data.List (List(..))
import Data.Map (Map, fromFoldable, update)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, class Semigroup, mempty)
import Data.Set (Set)
import Data.Tuple.Nested ((/\))

type State
  = { cliHistory :: List String
    , cliInput :: String
    , cliLogs :: CliLogs
    }

type CliLogs
  = Set LogEntry
