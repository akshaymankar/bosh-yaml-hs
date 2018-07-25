{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Bosh.InterpolationSpec where

import Test.Hspec
import Test.Hspec.Attoparsec

import Data.Yaml
import Data.HashMap.Strict
import Bosh.Operation.Types
import Bosh.Interpolation
import Bosh.Interpolation.Types
import Bosh.Interpolation.Parsers
import Data.Text
import Data.Scientific
import qualified Data.Vector as V

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

path :: Text -> OperationPath
path p = OperationPath $ path' $ split (== '/') p

path' :: [Text] -> [PathSegment]
path' [] = []
path' ("0":ps) = ArraySegment (NumIndex 0) : path' ps
path' ("1":ps) = ArraySegment (NumIndex 1) : path' ps
path' (  x:ps) = MapSegment x True : path' ps

pureStringValue :: Text -> LeafValue
pureStringValue x = StringValue $ IEText x End

spec :: Spec
spec = do
  describe "Interpolation" $ do
    describe "leaves" $ do
      it "should traverse to simple string object value" $ do
        let foo = object ["foo" .= "bar"]
        leaves foo `shouldBe` [(path "foo", pureStringValue "bar")]

      it "should traverse to string with 1 var in object value" $ do
        let foo = object ["foo" .= "((bar))"]
        leaves foo `shouldBe` [(path "foo", StringValue $ IEVar "bar" End)]

      it "should traverse to number object value" $ do
        let foo = object ["foo" .= 100]
        leaves foo `shouldBe` [(path "foo", NumValue 100)]

      it "should traverse to bool object value" $ do
        let foo = object ["foo" .= True]
        leaves foo `shouldBe` [(path "foo", BoolValue True)]

      it "should traverse to null object value" $ do
        let foo = object ["foo" .= Null]
        leaves foo `shouldBe` [(path "foo", NullValue)]

      it "should traverse through arrays as object value" $ do
        let foo = object [("foo", array [String "bar"])]
        leaves foo `shouldBe` [(path "foo/0", pureStringValue "bar")]

      it "should traverse through objects as object value" $ do
        let foo = object ["foo" .= object ["baz" .= "bar"]]
        leaves foo `shouldBe` [(path "foo/baz", pureStringValue "bar")]

      it "should traverse through simple string value" $ do
        let foo = array [String "bar"]
        leaves foo `shouldBe` [(path "0", pureStringValue "bar")]

      it "should traverse through simple object value" $ do
        let foo = array [object ["foo" .= String "bar"]]
        leaves foo `shouldBe` [(path "0/foo", pureStringValue "bar")]
