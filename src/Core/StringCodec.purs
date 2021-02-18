module Core.StringCodec
  ( class StringCodec
  , decodeFromString
  , encodeToString
  ) where

import Data.Either.Nested (type (\/))

class StringCodec a where
  decodeFromString :: String -> String \/ a
  encodeToString :: a -> String
