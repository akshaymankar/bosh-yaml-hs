{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bosh.Yaml
import Data.Yaml
import Bosh.Operation

import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  eitherVal <- int "foo.yml" (Operation Remove (OperationPath "key1" EndOfPath))
  case eitherVal of
    (Right v) -> B.putStrLn $ encode v
    _ -> putStrLn "Error!!"
