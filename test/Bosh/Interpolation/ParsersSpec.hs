{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Bosh.Interpolation.ParsersSpec where

import Test.Hspec
import Test.Hspec.Attoparsec

import Bosh.Interpolation.Parsers
import Bosh.Interpolation.Types
import Data.Text

spec :: Spec
spec = do
  describe "Interpolation Parsers" $ do
    describe "interpolExprParser" $ do
      it "should parse simple string" $ do
        ("string" :: Text) ~> interpolExprParser `shouldParse` IEText "string" End

      it "should parse simple var" $ do
        ("((var))" :: Text) ~> interpolExprParser `shouldParse` IEVar "var" End

      it "should parse two simple vars next to each other" $ do
        ("((var1))((var2))" :: Text) ~> interpolExprParser `shouldParse` IEVar "var1" (IEVar "var2" End)

      it "should parse var followed by string" $ do
        ("((var))string" :: Text) ~> interpolExprParser `shouldParse` IEVar "var" (IEText "string" End)

      it "should parse string followed by var" $ do
        ("string((var))" :: Text) ~> interpolExprParser `shouldParse` IEText "string" (IEVar "var" End)

      it "should parse var between two strings" $ do
        ("string1((var))string2" :: Text) ~> interpolExprParser `shouldParse` IEText "string1" (IEVar "var" (IEText "string2" End))

      it "should parse string between two vars" $ do
        ("((var1))string((var2))" :: Text) ~> interpolExprParser `shouldParse` IEVar "var1" (IEText "string" (IEVar "var2" End))

      it "should parse var inside parenthesis" $ do
        ("(((var)))" :: Text) ~> interpolExprParser `shouldParse` IEText "(" (IEVar "var" (IEText ")" End))

      it "should parse double parenthesis as string" $ do
        ("((" :: Text) ~> interpolExprParser `shouldParse` IEText "((" End

      it "should parse weird string cases" $ do
        ("((foo(lala))" :: Text) ~> interpolExprParser `shouldParse` IEText "((foo(lala))" End

      it "should parse weird var cases" $ do
        ("((foo((lala))" :: Text) ~> interpolExprParser `shouldParse` IEText "((foo" (IEVar "lala" End)


