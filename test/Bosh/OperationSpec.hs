{-# LANGUAGE OverloadedStrings #-}
module Bosh.OperationSpec where

import Test.Hspec
import Test.Hspec.Attoparsec
import Data.Either
import Data.Text
import Bosh.Operation
import Data.Yaml
import Data.HashMap.Strict as H

spec :: Spec
spec = do
  describe "OperationPath" $ do
    context "segmentParser" $ do
      it "should parse a pathSegment" $ do
        ("/key" :: Text) ~> segmentParser `shouldParse` "key"
    context "pathParser" $ do
      it "should parse a path with 1 segment" $ do
        ("/key" :: Text) ~> pathParser `shouldParse` OperationPath ["key"]
      it "should parse a path with many segments" $ do
        ("/key/key2" :: Text) ~> pathParser `shouldParse` OperationPath ["key", "key2"]
    context "FromJSON" $ do
      it "should be able to parse JSON string" $ do
        case decodeEither' "/key/key2" of
          (Right o) ->  o `shouldBe` OperationPath ["key", "key2"]
          (Left e) -> expectationFailure $ "Decoding failed with error: " ++ show e

  describe "applyOp" $ do
    it "should replace given key's value" $ do
      let op = Operation (Replace "new-val") (OperationPath ["key"])
          doc = Object $ H.singleton "key" "old-val"
      applyOp doc op `shouldBe` Right (Object $ H.singleton "key" "new-val")

    it "should delete a given key" $ do
      let op = Operation Remove (OperationPath ["key1"])
          doc = Object $ fromList [("key1", "val"), ("key2", "val2")]
      applyOp doc op `shouldBe` Right (Object $ fromList [("key2", "val2")])

    it "should not replace a non existent key" $ do
      let op = Operation (Replace "val2") (OperationPath ["key2"])
          doc = Object H.empty
      applyOp doc op `shouldBe` Left OperationErr

    it "should not replace when types don't match" $ do
      let op = Operation (Replace "val2") (OperationPath ["key1", "key1-1"])
          doc = Object $ H.singleton "key1" "val1"
      applyOp doc op `shouldBe` Left OperationErr
