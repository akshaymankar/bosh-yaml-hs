{-# LANGUAGE OverloadedStrings #-}
module Bosh.Operation where

import Control.Monad
import Data.Functor
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

newtype OperationPath = OperationPath [PathSegment]
  deriving (Show, Eq)

data PathSegment = PathSegment { segment :: Text, isOptional :: Bool }
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

segmentParser :: AT.Parser PathSegment
segmentParser = do
    _ <- AT.char '/'
    PathSegment
      <$> AT.takeWhile (AT.notInClass "/?")
      <*> AT.choice [AT.char '?' $> True, pure False]

pathParser :: AT.Parser OperationPath
pathParser = do
  segs <- AT.many1 segmentParser
  return $ OperationPath $ propagateOptionality segs False

propagateOptionality :: [PathSegment] -> Bool -> [PathSegment]
propagateOptionality [] _ = []
propagateOptionality (p:ps) True  = p{isOptional=True}:propagateOptionality ps True
propagateOptionality (p:ps) False = if isOptional p
                                       then p:propagateOptionality ps True
                                       else p:propagateOptionality ps False

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

replaceOp' :: Value -> Value -> [PathSegment] -> Either OperationErr Value
replaceOp' doc r path@(seg:rem) = do
      let key = segment seg
      case doc of
        (Object o) ->
            if member key o
               then replaceOpKeyPresent o r key rem
               else replaceOpKeyAbsent o r path
        _ -> Left OperationErr
replaceOp' v r [] = return r

replaceOpKeyPresent o r key rem = do
  newTree <- replaceOp' (o ! key) r rem
  case  newTree of
    Null   -> return $ Object $ delete key o
    newVal -> return $ Object $ insert key newVal o

replaceOpKeyAbsent :: HashMap Text Value -> Value -> [PathSegment] -> Either OperationErr Value
replaceOpKeyAbsent o r path@(seg:rem) =
  if isOptional seg
    then
      if r == Null
         then return (Object o)
         else return $ createObject r path
    else Left OperationErr

createObject :: Value -> [PathSegment] -> Value
createObject v (p:ps) = Object $ Map.singleton (segment p) $ createObject v ps
createObject v [] = v
