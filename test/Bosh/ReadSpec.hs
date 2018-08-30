{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Bosh.ReadSpec where

import Test.Hspec

import Bosh.Operation
import Bosh.Operation.Parsers
import Bosh.Operation.Types
import Bosh.Read
import Data.Attoparsec.Text
import Data.Text as T
import Data.Yaml

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

readQuery :: Text -> [PathSegment]
readQuery t = case parseOnly (pathParser <* endOfInput) t of
                Right (OperationPath ps) -> ps
                Left e -> error ("Parser failed to parse: " ++ T.unpack t ++ " Error: " ++ e)

spec :: Spec
spec = do
  describe "Read YAML" $ do
    it "should read YAML from a map path" $ do
      let yaml = object ["foo" .= "bar"]
          query = readQuery "/foo"
      readYaml query yaml `shouldBe` Just "bar"

    it "should read YAML from a nested map path" $ do
      let yaml = object ["foo" .= object ["bar" .= "baz"]]
          query = readQuery "/foo/bar"
      readYaml query yaml `shouldBe` Just "baz"

    it "should read YAML from an array index" $ do
      let yaml = array ["foo"]
          query = readQuery "/0"
      readYaml query yaml `shouldBe` Just "foo"

    it "should read YAML from a nested array index" $ do
      let yaml = array [array ["foo"]]
          query = readQuery "/0/0"
      readYaml query yaml `shouldBe` Just "foo"

    it "should read YAML from a nested path of arrays and maps" $ do
      let yaml = array [array [object ["foo" .= object ["bar" .= array [object ["baz" .= "qux" ]]]]]]
          query = readQuery "/0/0/foo/bar/0/baz"
      readYaml query yaml `shouldBe` Just "qux"

    it "should read YAML for a map matcher query" $ do
      let yaml = array [ object ["foo" .= "bar1", "baz" .= "qux1"]
                       , object ["foo" .= "bar2", "baz" .= "qux2"]]
          query = readQuery "/foo=bar2"
      readYaml query yaml `shouldBe` Just (object ["foo" .= "bar2", "baz" .= "qux2"])

    it "should read YAML for a nested map matcher query" $ do
      let yaml = array [ object ["foo" .= "bar1", "baz" .= "qux1"]
                       , object ["foo" .= "bar2", "baz" .= "qux2"]]
          query = readQuery "/foo=bar2/baz"
      readYaml query yaml `shouldBe` Just "qux2"
