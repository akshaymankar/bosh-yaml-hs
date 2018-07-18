{-# LANGUAGE OverloadedStrings #-}
module Bosh.Parsers where

import Bosh.Types
import Data.Aeson.Types
import Data.Functor
import Data.Text
import Data.Yaml
import qualified Data.Attoparsec.Text as AT

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

