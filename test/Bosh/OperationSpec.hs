{-# LANGUAGE OverloadedStrings #-}
module Bosh.OperationSpec where

import Test.Hspec
import Test.Hspec.Attoparsec
import Data.Either
import Data.Text
import Bosh.Operation
import Data.Yaml

spec :: Spec
spec = do
  describe "OperationPath" $ do
    context "segmentParser" $ do
      it "should parse a pathSegment" $ do
        ("/key" :: Text) ~> segmentParser `shouldParse` "key"
    context "pathParser" $ do
      it "should parse a path with 1 segment" $ do
        ("/key" :: Text) ~> pathParser `shouldParse` OperationPath "key" EndOfPath
      it "should parse a path with many segments" $ do
        ("/key/key2" :: Text) ~> pathParser `shouldParse` OperationPath "key" (OperationPath "key2" EndOfPath)
    context "FromJSON" $ do
      it "should be able to parse JSON string" $ do
        case decodeEither' "/key/key2" of
          (Right o) ->  o `shouldBe` OperationPath "key" (OperationPath "key2" EndOfPath)
          (Left e) -> expectationFailure $ "Decoding failed with error: " ++ (show e)
