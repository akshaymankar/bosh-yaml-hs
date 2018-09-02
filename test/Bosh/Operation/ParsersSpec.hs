{-# LANGUAGE OverloadedStrings #-}
module Bosh.Operation.ParsersSpec where

import Test.Hspec
import Test.Hspec.Attoparsec

import Bosh.Operation.Parsers
import Bosh.Operation.Types
import Data.Either
import Data.Text
import Data.Yaml

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

spec :: Spec
spec = do
  describe "OperationPath" $ do
    context "segmentParser" $ do
      it "should parse a mandatory map segment" $ do
        ("/key" :: Text) ~> segmentParser `shouldParse` mandatorySegment "key"

      it "should parse a map segment with - in the key name" $ do
        ("/key-name" :: Text) ~> segmentParser `shouldParse` mandatorySegment "key-name"

      it "should parse an optional map segment" $ do
        ("/key?" :: Text) ~> segmentParser `shouldParse` optionalSegment "key"

      it "should parse last index array segment" $ do
        ("/-" :: Text) ~> segmentParser `shouldParse` ArraySegment LastIndex

      it "should parse numerical index array segment" $ do
        ("/1" :: Text) ~> segmentParser `shouldParse` ArraySegment (NumIndex 1)

      it "should parse numerical index with :prev in an array segment" $ do
        ("/2:prev" :: Text) ~> segmentParser `shouldParse` ArraySegment (NumIndex 1)

      it "should parse numerical index with :next in an array segment" $ do
        ("/2:next" :: Text) ~> segmentParser `shouldParse` ArraySegment (NumIndex 3)

      it "should parse numerical index with :before in an array segment" $ do
        ("/2:before" :: Text) ~> segmentParser `shouldParse` ArraySegment (BeforeIndex 2)
        ("/0:before" :: Text) ~> segmentParser `shouldParse` ArraySegment (BeforeIndex 0)

      it "should parse numerical index with :after in an array segment" $ do
        ("/2:after" :: Text) ~> segmentParser `shouldParse` ArraySegment (BeforeIndex 3)

      it "should parse some weird cases as map segment" $ do
        -- This is not how bosh-cli works
        ("/2:prevlol" :: Text) ~> segmentParser `shouldParse` mandatorySegment "2:prevlol"
        -- This is how bosh-cli works
        ("/2prevlol" :: Text) ~> segmentParser `shouldParse` mandatorySegment "2prevlol"
        ("/-lol" :: Text) ~> segmentParser `shouldParse` mandatorySegment "-lol"
        -- This case is not handled because it sounds tiring
        -- ("/2prev?lol" :: Text) ~> segmentParser `shouldParse` mandatorySegment "2prev?lol"

      it "should parse MapMatcher in an array segment" $ do
        ("/name=foo" :: Text) ~> segmentParser `shouldParse` ArraySegment (MapMatcher "name" "foo" False)

      it "should parse optional MapMatcher in an array segment" $ do
        ("/name=foo?" :: Text) ~> segmentParser `shouldParse` ArraySegment (MapMatcher "name" "foo" True)

    context "pathParser" $ do
      it "should parse a path with 1 segment" $ do
        ("/key" :: Text) ~> pathParser `shouldParse` OperationPath [mandatorySegment "key"]

      it "should parse a path with many segments" $ do
        ("/key/key2" :: Text) ~> pathParser `shouldParse` OperationPath [mandatorySegment "key", mandatorySegment "key2"]
        ("/0/0/key" :: Text) ~> pathParser `shouldParse` OperationPath [ArraySegment $ NumIndex 0, ArraySegment $ NumIndex 0, mandatorySegment "key"]

      it "should apply optionality to all segments after first optional segment" $ do
        ("/key/key2?/key3/key4" :: Text) ~> pathParser `shouldParse` OperationPath [ mandatorySegment "key"
                                                                                   , optionalSegment "key2"
                                                                                   , optionalSegment "key3"
                                                                                   , optionalSegment "key4"
                                                                                   ]
      it "should apply optionality to all segments except array segments after first optional segment" $ do
        ("/key/key2?/-/key4" :: Text) ~> pathParser `shouldParse` OperationPath [ mandatorySegment "key"
                                                                                , optionalSegment "key2"
                                                                                , ArraySegment LastIndex
                                                                                , optionalSegment "key4"
                                                                                ]

    context "FromJSON" $ do
      it "should be able to parse string" $ do
        case decodeEither' "/key/key2" of
          (Right o) -> o `shouldBe` OperationPath [mandatorySegment "key", mandatorySegment "key2"]
          (Left e) -> expectationFailure $ "Decoding failed with error: " ++ show e
      it "should be able to parse list of operations" $ do
        case decodeEither' "- type: remove\n  path: /foo" of
          (Right o) -> o `shouldBe` [Operation Remove $ OperationPath [mandatorySegment "foo"]]
          (Left e) -> expectationFailure $ "Decoding failed with error: " ++ show e
      it "should be able to parse empty file into empty list of operations" $ do
        case decodeEither' "" of
          (Right o) -> o `shouldBe` ([] :: [Operation])
          (Left e) -> expectationFailure $ "Decoding failed with error: " ++ show e
