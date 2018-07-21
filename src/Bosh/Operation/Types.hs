module Bosh.Operation.Types where

import Data.Text
import Data.Yaml

data Operation = Operation { opType :: OperationType, opPath :: OperationPath}
  deriving (Show, Eq)

data OperationType = Remove
                   | Replace { replacement :: Value }
  deriving (Show, Eq)

newtype OperationPath = OperationPath [PathSegment]
  deriving (Show, Eq)

data ArrayIndex = NumIndex Int
                | LastIndex
                | BeforeIndex Int
  deriving (Show, Eq)

data PathSegment = MapSegment { segment :: Text, isOptional :: Bool }
                 | ArraySegment { index :: ArrayIndex}
  deriving (Show, Eq)

data OperationErr = OperationErr
  deriving (Show, Eq)

mandatorySegment :: Text -> PathSegment
mandatorySegment = flip MapSegment False

optionalSegment :: Text -> PathSegment
optionalSegment = flip MapSegment True
