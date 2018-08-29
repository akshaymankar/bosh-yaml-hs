{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Bosh.OperationSpec where

import Test.Hspec

import Bosh.Operation
import Bosh.Operation.Types
import Data.Either
import Data.HashMap.Strict as H
import Data.Text
import Data.Vector         as V hiding ((++))
import Data.Yaml

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

spec :: Spec
spec = do
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
      applyOp doc op `shouldBe` Left (MandatoryKeyNotFound "key2" doc)

    it "should not replace when types don't match" $ do
      let op = Operation (Replace "val2") (OperationPath [mandatorySegment "key1", mandatorySegment "key1-1"])
          doc = Object $ H.singleton "key1" "val1"
      applyOp doc op `shouldBe` Left (TypeMismatch (mandatorySegment "key1-1") "val1")

    it "should not replace when types don't match" $ do
      let op = Operation (Replace "val2") (OperationPath [ArraySegment $ NumIndex 0])
          doc = Object $ H.singleton "key1" "val1"
      applyOp doc op `shouldBe` Left (TypeMismatch (ArraySegment $ NumIndex 0) doc)

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

    it "should fail to replace an element from an array if the index is out of bounds" $ do
      let op = Operation (Replace "new-elem3") (OperationPath [ArraySegment $ NumIndex 3])
          doc = Array $ V.fromList ["elem1", "elem2"]
      applyOp doc op `shouldBe` Left (IndexOutOfBounds 3 doc)

    it "should remove an element at the given index in an array" $ do
      let op = Operation Remove (OperationPath [ArraySegment $ NumIndex 1])
          doc = Array $ V.fromList ["elem1", "elem2"]
      applyOp doc op `shouldBe` Right (Array $ V.fromList ["elem1"])

    it "should fail to remove an element from an array if the index is out of bounds" $ do
      let op = Operation Remove (OperationPath [ArraySegment $ NumIndex 3])
          doc = Array $ V.fromList ["elem1", "elem2"]
      applyOp doc op `shouldBe` Left (IndexOutOfBounds 3 doc)

    it "should insert an element before a given index in an array" $ do
      let op = Operation (Replace "elem1.5") (OperationPath [ArraySegment $ BeforeIndex 1])
          doc = Array $ V.fromList ["elem1", "elem2"]
      applyOp doc op `shouldBe` Right (Array $ V.fromList ["elem1", "elem1.5", "elem2"])

    it "should insert an element before index 0 in an array" $ do
      let op = Operation (Replace "elem0.5") (OperationPath [ArraySegment $ BeforeIndex 0])
          doc = Array $ V.fromList ["elem1", "elem2"]
      applyOp doc op `shouldBe` Right (Array $ V.fromList ["elem0.5", "elem1", "elem2"])

    it "should insert an element before the last index+1 in an array" $ do
      let op = Operation (Replace "elem2.5") (OperationPath [ArraySegment $ BeforeIndex 3])
          doc = Array $ V.fromList ["elem1", "elem2"]
      applyOp doc op `shouldBe` Right (Array $ V.fromList ["elem1", "elem2", "elem2.5"])

    it "should fail if a path segment follows insertion only path segment" $ do
      let extraSeg = ArraySegment $ NumIndex 1
          opF seg = Operation (Replace "elem1.5") (OperationPath [seg, extraSeg])
          doc = Array $ V.fromList ["elem1", "elem2"]
      applyOp doc (opF $ ArraySegment $ BeforeIndex 2) `shouldBe` Left (ExtraSegments (BeforeIndex 2) [extraSeg])
      applyOp doc (opF $ ArraySegment LastIndex)       `shouldBe` Left (ExtraSegments LastIndex [extraSeg])

    it "should replace an element of a map inside an array" $ do
      let opF index =  Operation (Replace "new-value") (OperationPath [ArraySegment index, mandatorySegment "key"])
          doc = array [object ["key" .= "value"], object["key" .= "value1"]]
      applyOp doc (opF $ NumIndex 0)               `shouldBe` Right (array [object ["key" .= "new-value"], object["key" .= "value1"]])
      applyOp doc (opF $ MapMatcher "key" "value" False) `shouldBe` Right (array [object ["key" .= "new-value"], object["key" .= "value1"]])

    it "should remove an element of a map insdie an array" $ do
      let opF index =  Operation Remove (OperationPath [ArraySegment index, mandatorySegment "key"])
          doc = array [object ["key" .= "value", "foo" .= "bar"], object["key" .= "value1", "foo" .= "bar1"]]
      applyOp doc (opF $ NumIndex 0)               `shouldBe` Right (array [object ["foo" .= "bar"], object["key" .= "value1", "foo" .= "bar1"]])
      applyOp doc (opF $ MapMatcher "key" "value" False) `shouldBe` Right (array [object ["foo" .= "bar"], object["key" .= "value1", "foo" .= "bar1"]])

    -- This case is not handled
    -- elem 3.5 just appears after elem2
    xit "should not insert an element before the last index+1+n in an array" $ do
      let op = Operation (Replace "elem3.5") (OperationPath [ArraySegment $ BeforeIndex 4])
          doc = Array $ V.fromList ["elem1", "elem2"]
      applyOp doc op `shouldBe` Left (IndexOutOfBounds 4 doc)

    it "should replace an element map by property matcher" $ do
      let op = Operation (Replace (object ["name" .= "new-elem2"])) (OperationPath [ArraySegment $ MapMatcher "name" "elem2" False])
          doc = array [object ["name" .= "elem1"], object ["name" .= "elem2"], "foo"]
      applyOp doc op `shouldBe` Right (array [object ["name" .= "elem1"], object ["name" .= "new-elem2"], "foo"])

    it "should add an element map by property matcher" $ do
      let op = Operation (Replace (object ["name" .= "new-elem2"])) (OperationPath [ArraySegment $ MapMatcher "name" "elem2" True])
          doc = array [object ["name" .= "elem1"], "foo"]
      applyOp doc op `shouldBe` Right (array [object ["name" .= "elem1"], "foo", object ["name" .= "new-elem2"]])

    it "should fail to add an element map by property matcher if there are extra segments" $ do
      let index = MapMatcher "name" "elem2" True
          extraSeg = mandatorySegment "foo"
          op = Operation (Replace (object ["name" .= "new-elem2"])) (OperationPath [ ArraySegment index, extraSeg])
          doc = array [object ["name" .= "elem1"], "foo"]
      applyOp doc op `shouldBe` Left (ExtraSegments index [extraSeg])


    it "should delete an element map by property matcher" $ do
      let op = Operation Remove (OperationPath [ArraySegment $ MapMatcher "name" "elem2" False])
          doc = array [object ["name" .= "elem1"], object ["name" .= "elem2"], "foo"]
      applyOp doc op `shouldBe` Right (array [object ["name" .= "elem1"], "foo"])

    it "should fail to replace if property matcher matches more than one property " $ do
      let op = Operation (Replace (object ["name" .= "new-elem"])) (OperationPath [ArraySegment $ MapMatcher "name" "elem" False])
          doc = array [object ["name" .= "elem"], object ["name" .= "elem"], "foo"]
      applyOp doc op `shouldBe` Left (MapMatcherError "name" "elem" doc)

    it "should fail to replace if property matcher matches no maps" $ do
      let op = Operation (Replace (object ["name" .= "new-elem"])) (OperationPath [ArraySegment $ MapMatcher "name" "not-elem" False])
          doc = array [object ["name" .= "elem"], object ["name" .= "elem"], "foo"]
      applyOp doc op `shouldBe` Left (MapMatcherError "name" "not-elem" doc)

    it "should fail to delete if property matcher matches more than one property " $ do
      let op = Operation Remove (OperationPath [ArraySegment $ MapMatcher "name" "elem" False])
          doc = array [object ["name" .= "elem"], object ["name" .= "elem"], "foo"]
      applyOp doc op `shouldBe` Left (MapMatcherError "name" "elem" doc)

    it "should fail to delete if property matcher matches no maps" $ do
      let op = Operation Remove (OperationPath [ArraySegment $ MapMatcher "name" "not-elem" False])
          doc = array [object ["name" .= "elem"], object ["name" .= "elem"], "foo"]
      applyOp doc op `shouldBe` Left (MapMatcherError "name" "not-elem" doc)

    it "should be able to add new keys in an object" $ do
      let op = Operation (Replace "value2") (OperationPath [optionalSegment "key2"])
          doc = object ["key1" .= "value1"]
      applyOp doc op `shouldBe` Right (object ["key1" .= "value1", "key2" .= "value2"])

    it "should be able to add array in a newly created object" $ do
      let op = Operation (Replace "value2") (OperationPath [optionalSegment "key2", ArraySegment LastIndex])
          doc = object ["key1" .= "value1"]
      applyOp doc op `shouldBe` Right (object ["key1" .= "value1", "key2" .= array ["value2"]])
    it "should not be able to add array using index in a newly created object" $ do
      let op = Operation (Replace "value2") (OperationPath [optionalSegment "key2", ArraySegment $ NumIndex 0])
          doc = object ["key1" .= "value1"]
      applyOp doc op `shouldBe` Left (UnexpectedSegment $ ArraySegment $ NumIndex 0)

