{-# LANGUAGE OverloadedStrings #-}
module Bosh.Operation where

import Control.Monad
import Data.Functor
import Data.Text
import Data.Yaml
import Data.HashMap.Strict as Map
import Data.Aeson.Types
import qualified Data.Attoparsec.Text as AT
import qualified Data.Vector as V

data Operation = Operation { opType :: OperationType, opPath :: OperationPath}
  deriving (Show, Eq)

data OperationType = Remove
                   | Replace { replacement :: Value }
  deriving (Show, Eq)

newtype OperationPath = OperationPath [PathSegment]
  deriving (Show, Eq)

data ArrayIndex = NumIndex Int
                | LastIndex
  deriving (Show, Eq)

data PathSegment = MapSegment { segment :: Text, isOptional :: Bool }
                 | ArraySegment { index :: ArrayIndex}
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

arraySegmentParser :: AT.Parser PathSegment
arraySegmentParser =
  AT.choice [ ArraySegment . NumIndex <$> AT.decimal
            , AT.char '-' $> ArraySegment LastIndex]

mapSegmentParser :: AT.Parser PathSegment
mapSegmentParser = MapSegment
                    <$> AT.takeWhile1 (AT.notInClass "/?-")
                    <*> AT.choice [AT.char '?' $> True, pure False]

segmentParser :: AT.Parser PathSegment
segmentParser = do
    _ <- AT.char '/'
    AT.choice [arraySegmentParser, mapSegmentParser]


pathParser :: AT.Parser OperationPath
pathParser = do
  segs <- AT.many1 segmentParser
  return $ OperationPath $ propagateOptionality segs False

propagateOptionality :: [PathSegment] -> Bool -> [PathSegment]
propagateOptionality [] _                            = []
propagateOptionality (p@(ArraySegment _):ps)   b     = p:propagateOptionality ps b
propagateOptionality (p@(MapSegment _ _):ps)   True  = p{isOptional=True}:propagateOptionality ps True
propagateOptionality (p@(MapSegment _ opt):ps) False = if opt
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
replaceOp' _ r [] = return r
replaceOp' (Object o) r path@(seg@(MapSegment key _):rem) =
  if member key o
     then replaceOpKeyPresent o r key rem
     else replaceOpKeyAbsent o r path
replaceOp' (Array a) r    path@(seg@(ArraySegment LastIndex):rem)    = return $ Array $ V.snoc a r
replaceOp' (Array a) Null path@(seg@(ArraySegment (NumIndex i)):rem) = return $ Array $ deleteNth i a
replaceOp' (Array a) r    path@(seg@(ArraySegment (NumIndex i)):rem) = return $ Array $ V.update a (V.fromList [(i, r)])
replaceOp' _ _ (seg@(MapSegment _ _):_) = Left OperationErr

deleteNth :: Int -> V.Vector a -> V.Vector a
deleteNth n v = let (ys,zs) = V.splitAt n v in V.concat [ys, V.tail zs]

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
