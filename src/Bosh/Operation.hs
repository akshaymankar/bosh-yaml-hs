{-# LANGUAGE OverloadedStrings #-}
module Bosh.Operation where

import Data.Text
import Data.Yaml
import Data.HashMap.Strict
import Data.Aeson.Types
import qualified Data.Attoparsec.Text as AT

data Operation = Operation { opType :: OperationType, opPath :: OperationPath}
  deriving (Show, Eq)

data OperationType = Remove
                   | Replace { replacement :: Value }
  deriving (Show, Eq)

data OperationPath = OperationPath [Text]
  deriving (Show, Eq)

instance FromJSON OperationType where
  parseJSON (Object v) = do
    typeStr <- v .: "type" :: Parser Text
    if typeStr == "remove"
       then return Remove
       else Replace <$> v .: "value"
  parseJSON invalid = typeMismatch "OperationType" invalid

instance FromJSON Operation where
  parseJSON (Object v) = Operation
                       <$> parseJSON (Object v)
                       <*> v .: "path"
  parseJSON invalid = typeMismatch "Operation" invalid

instance FromJSON OperationPath where
  parseJSON (String s) = case AT.parseOnly pathParser s of
                           (Right r) -> return r
                           (Left err) -> fail $ "Parser failed: " ++ err

  parseJSON invalid = typeMismatch "OperationPath" invalid

segmentParser :: AT.Parser Text
segmentParser = do
    _ <- AT.char '/'
    AT.takeWhile (/= '/')

pathParser :: AT.Parser OperationPath
pathParser = OperationPath <$> AT.many1 segmentParser

applyOp :: Value -> Operation -> Value
applyOp v (Operation t path) =
  case t of
    Remove -> removeOp v path
    (Replace r) -> replaceOp v path r

applyOps :: [Operation] -> Value -> Value
applyOps os v = Prelude.foldl applyOp v os

removeOp :: Value -> OperationPath -> Value
removeOp v path = replaceOp v path Null

replaceOp :: Value -> OperationPath -> Value -> Value
replaceOp doc (OperationPath path) = replaceOp' doc path

replaceOp' :: Value -> [Text] -> Value -> Value
replaceOp' doc (key:rem) r =
      case doc of
        (Object x) -> case replaceOp' (x ! key) rem r of
                        Null   -> Object $ delete key x
                        newVal -> Object $ insert key newVal x
        _ -> error "foo"
replaceOp' v [] r = r


