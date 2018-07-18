{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bosh.Operation
import Bosh.Types
import Bosh.Parsers
import Bosh.Yaml
import Data.Yaml

import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  eitherOps <- decodeFileEither "ops.yml" :: IO (Either ParseException [Operation])
  case eitherOps of
    (Right ops) -> do
      eitherVal <- int "foo.yml" ops
      case eitherVal of
        (Right v) -> B.putStrLn $ encode v
        _ -> putStrLn "Error!!"
    (Left s) -> do
      _ <- print "Error"
      print s
