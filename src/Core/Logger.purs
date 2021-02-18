module Core.Logger (class LogEntryCreator, LogEntry, LogLevel(..), logEntry, logLevel) where

import Prelude
import Core.StringCodec (class StringCodec, encodeToString)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.String.NonEmpty (NonEmptyString, appendString, nes, toString)
import Data.Symbol (class IsSymbol, SProxy(..))

data LogLevel
  = Error
  | Info
  | Warn

instance showLogLevel :: Show LogLevel where
  show :: LogLevel -> String
  show = encodeToString

derive instance eqLogLevel :: Eq LogLevel

instance ordLogLevel :: Ord LogLevel where
  compare :: LogLevel -> LogLevel -> Ordering
  compare l1 l2 =
    if l1 == l2 then
      EQ
    else case l1 of
      Error -> GT
      Info -> LT
      Warn -> if l2 == Error then LT else GT

instance stringCodecLogLevel :: StringCodec LogLevel where
  decodeFromString :: String -> String \/ LogLevel
  decodeFromString = case _ of
    "error" -> Right Error
    "info" -> Right Info
    "warn" -> Right Warn
    other -> Left $ "invalid log level: " <> other
  encodeToString :: LogLevel -> String
  encodeToString = case _ of
    Error -> "error"
    Info -> "info"
    Warn -> "warn"

newtype LogEntry
  = LogEntry { level :: LogLevel, message :: NonEmptyString }

instance showLogEntry :: Show LogEntry where
  show :: LogEntry -> String
  show = encodeToString

derive instance eqLogEntry :: Eq LogEntry

instance ordLogEntry :: Ord LogEntry where
  compare :: LogEntry -> LogEntry -> Ordering
  compare (LogEntry le1) (LogEntry le2) = compare le1.level le2.level

instance stringCodecLogEntry :: StringCodec LogEntry where
  decodeFromString :: String -> String \/ LogEntry
  decodeFromString s = Left ""
  encodeToString :: LogEntry -> String
  encodeToString (LogEntry entry) = "[" <> encodeToString entry.level <> "] " <> toString entry.message

class LogEntryCreator (s :: Symbol) where
  logEntry :: LogLevel -> SProxy s -> String -> LogEntry

instance logEntryCreator :: IsSymbol s => LogEntryCreator s where
  logEntry lvl titleSym desc =
    LogEntry
      { level: lvl
      , message: appendString (nes titleSym <> nes (SProxy :: SProxy ": ")) desc
      }

logLevel :: LogEntry -> LogLevel
logLevel (LogEntry entry) = entry.level
