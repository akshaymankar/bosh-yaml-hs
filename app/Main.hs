module Main where

import Lib
import Bosh.Yaml
import Data.Yaml
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  eitherVal <- int "foo.yml"
  case eitherVal of
    (Right v) -> B.putStrLn $ encode v
    _ -> putStrLn "Error!!"
