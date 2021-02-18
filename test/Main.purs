module Test.Main (main) where

import Prelude
import Core.Cli (Cmd)
import Core.FileSystem (FileContent, FileName, Path)
import Core.StringCodec (decodeFromString, encodeToString)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "FileSystem.FileName" do
          it "codes file name" $ recodeFileName "abc"
          it "codes path" $ recodePath "/abc/def"
          it "codes file content" $ recodeFileContent "content"
          it "codes command" $ recodeCmd "edit /abc \"content\""

recodeCmd :: String -> Aff Unit
recodeCmd input =
  let
    res :: String \/ Cmd
    res = decodeFromString input
  in
    case encodeToString <$> res of
      Left errMsg -> fail $ "could not parse cmd: " <> errMsg
      Right output -> output `shouldEqual` input

recodeFileContent :: String -> Aff Unit
recodeFileContent input =
  let
    res :: String \/ FileContent
    res = decodeFromString input
  in
    case encodeToString <$> res of
      Left errMsg -> fail $ "could not parse file content: " <> errMsg
      Right output -> output `shouldEqual` input

recodeFileName :: String -> Aff Unit
recodeFileName input =
  let
    res :: String \/ FileName
    res = decodeFromString input
  in
    case encodeToString <$> res of
      Left errMsg -> fail $ "could not parse file name: " <> errMsg
      Right output -> output `shouldEqual` input

recodePath :: String -> Aff Unit
recodePath input =
  let
    res :: String \/ Path
    res = decodeFromString input
  in
    case encodeToString <$> res of
      Left errMsg -> fail $ "could not parse path: " <> errMsg
      Right output -> output `shouldEqual` input
