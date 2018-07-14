{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bosh.Yaml
import Data.Yaml
import Bosh.Operation

import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  let removeKey1 = Operation Remove (OperationPath "key1" EndOfPath)
      replaceMapKey3 = Operation (Replace $ Number 666) (OperationPath "key2" (OperationPath "mapKey3" EndOfPath))

  eitherVal <- int "foo.yml" [removeKey1, replaceMapKey3]
  case eitherVal of
    (Right v) -> B.putStrLn $ encode v
    _ -> putStrLn "Error!!"
