{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bosh.Interpolation
import Bosh.Interpolation.Types
import Bosh.Operation
import Bosh.Operation.Parsers
import Bosh.Operation.Types
import Data.HashMap.Strict as H
import Bosh.Yaml
import Data.Yaml

import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  let vars = H.fromList [("this", "it"), ("also", "yes"), ("rk", "rm")]
  eitherOps <- decodeFileEither "ops.yml" :: IO (Either ParseException [Operation])
  case eitherOps of
    (Right ops) -> do
      eitherVal <- operate "foo.yml" ops vars
      case eitherVal of
        (Right v) -> B.putStrLn $ encode v
        _ -> putStrLn "Error!!"
    (Left s) -> do
      _ <- print "Error"
      print s
