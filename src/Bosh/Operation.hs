{-# LANGUAGE OverloadedStrings #-}
module Bosh.Operation where

import Control.Monad
import Data.Text
import Data.Yaml
import Data.HashMap.Strict as Map
import Data.Aeson.Types
import qualified Data.Attoparsec.Text as AT

data Operation = Operation { opType :: OperationType, opPath :: OperationPath}
  deriving (Show, Eq)

data OperationType = Remove
                   | Replace { replacement :: Value }
  deriving (Show, Eq)

newtype OperationPath = OperationPath [Text]
  deriving (Show, Eq)

data OperationErr = OperationErr
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

applyOp :: Value -> Operation -> Either OperationErr Value
applyOp v (Operation t path) =
  case t of
    Remove -> removeOp v path
    (Replace r) -> replaceOp v path r

applyOps :: [Operation] -> Value -> Either OperationErr Value
applyOps os v = foldM applyOp v os

removeOp :: Value -> OperationPath -> Either OperationErr Value
removeOp v path = replaceOp v path Null

replaceOp :: Value -> OperationPath -> Value -> Either OperationErr Value
replaceOp doc (OperationPath path) r = replaceOp' doc r path

replaceOp' :: Value -> Value -> [Text] -> Either OperationErr Value
replaceOp' doc r (key:rem) =
      case doc of
        (Object x) ->
            case Map.lookup key x of
              Just tree -> do
                newTree <- replaceOp' (x ! key) r rem
                case  newTree of
                  Null   -> return $ Object $ delete key x
                  newVal -> return $ Object $ insert key newVal x
              Nothing -> Left OperationErr
        _ -> Left OperationErr
replaceOp' v r [] = return r
