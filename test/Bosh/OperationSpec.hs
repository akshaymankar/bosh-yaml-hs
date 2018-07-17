{-# LANGUAGE OverloadedStrings #-}
module Bosh.OperationSpec where

import Test.Hspec
import Test.Hspec.Attoparsec
import Data.Either
import Data.Text
import Bosh.Operation
import Data.Yaml
import Data.HashMap.Strict as H
import Data.Vector as V hiding ((++))

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

mandatorySegment :: Text -> PathSegment
mandatorySegment = flip MapSegment False

optionalSegment :: Text -> PathSegment
optionalSegment = flip MapSegment True

spec :: Spec
spec = do
  describe "OperationPath" $ do
    context "segmentParser" $ do
      it "should parse a mandatory map segment" $ do
        ("/key" :: Text) ~> segmentParser `shouldParse` mandatorySegment "key"

      it "should parse an optional map segment" $ do
        ("/key?" :: Text) ~> segmentParser `shouldParse` optionalSegment "key"

      it "should parse last index array segment" $ do
        ("/-" :: Text) ~> segmentParser `shouldParse` ArraySegment LastIndex

      it "should parse numerical index array segment" $ do
        ("/1" :: Text) ~> segmentParser `shouldParse` ArraySegment (NumIndex 1)

    context "pathParser" $ do
      it "should parse a path with 1 segment" $ do
        ("/key" :: Text) ~> pathParser `shouldParse` OperationPath [mandatorySegment "key"]

      it "should parse a path with many segments" $ do
        ("/key/key2" :: Text) ~> pathParser `shouldParse` OperationPath [mandatorySegment "key", mandatorySegment "key2"]

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
      it "should be able to parse JSON string" $ do
        case decodeEither' "/key/key2" of
          (Right o) ->  o `shouldBe` OperationPath [mandatorySegment "key", mandatorySegment "key2"]
          (Left e) -> expectationFailure $ "Decoding failed with error: " ++ show e

  describe "applyOp" $ do
    it "should replace given key's value" $ do
      let op = Operation (Replace "new-val") (OperationPath [mandatorySegment "key"])
          doc = Object $ H.singleton "key" "old-val"
      applyOp doc op `shouldBe` Right (Object $ H.singleton "key" "new-val")

    it "should delete a given key" $ do
      let op = Operation Remove (OperationPath [mandatorySegment "key1"])
          doc = Object $ H.fromList [("key1", "val"), ("key2", "val2")]
      applyOp doc op `shouldBe` Right (Object $ H.fromList [("key2", "val2")])

    it "should not replace a mandatory non existent key" $ do
      let op = Operation (Replace "val2") (OperationPath [mandatorySegment "key2"])
          doc = Object H.empty
      applyOp doc op `shouldBe` Left OperationErr

    it "should not replace when types don't match" $ do
      let op = Operation (Replace "val2") (OperationPath [mandatorySegment "key1", mandatorySegment "key1-1"])
          doc = Object $ H.singleton "key1" "val1"
      applyOp doc op `shouldBe` Left OperationErr

    it "should replace an optional non existent key" $ do
      let op = Operation (Replace "val2") (OperationPath [optionalSegment "key2"])
          doc = Object H.empty
      applyOp doc op `shouldBe` Right (Object $ H.singleton "key2" "val2")

    it "should replace an optional non existent nested key" $ do
      let op = Operation (Replace "val2") (OperationPath [optionalSegment "key2", optionalSegment "key3"])
          doc = Object H.empty
      applyOp doc op `shouldBe` Right (Object $ H.singleton "key2" (Object $ H.singleton "key3" "val2"))

    it "should remove an optional non existent key" $ do
      let op = Operation Remove (OperationPath [optionalSegment "key2"])
          doc = Object $ H.fromList [("key1", String "val1")]
      applyOp doc op `shouldBe` Right (Object $ H.fromList [("key1", String "val1")])

    it "should add an element at the end of an array" $ do
      let op = Operation (Replace "elem2") (OperationPath [ArraySegment LastIndex])
          doc = Array $ V.fromList ["elem1"]
      applyOp doc op `shouldBe` Right (Array $ V.fromList ["elem1", "elem2"])

    it "should replace an element at the given index in an array" $ do
      let op = Operation (Replace "new-elem2") (OperationPath [ArraySegment $ NumIndex 1])
          doc = Array $ V.fromList ["elem1", "elem2"]
      applyOp doc op `shouldBe` Right (Array $ V.fromList ["elem1", "new-elem2"])

    it "should remove an element at the given index in an array" $ do
      let op = Operation Remove (OperationPath [ArraySegment $ NumIndex 1])
          doc = Array $ V.fromList ["elem1", "elem2"]
      applyOp doc op `shouldBe` Right (Array $ V.fromList ["elem1"])
