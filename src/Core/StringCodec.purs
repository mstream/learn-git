module Core.StringCodec
  ( class StringCodec
  , decodeFromString
  , decodeUsingParser
  , encodeToString
  ) where

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Text.Parsing.StringParser (ParseError(..), Parser(..), runParser)

class StringCodec a where
  decodeFromString :: String -> String \/ a
  encodeToString :: a -> String

decodeUsingParser :: forall a. Parser a -> String -> String \/ a
decodeUsingParser parser s = case runParser parser s of
  Left (ParseError errMsg) -> Left errMsg
  Right parsedVal -> Right parsedVal
