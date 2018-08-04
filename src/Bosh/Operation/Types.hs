{-# LANGUAGE DeriveGeneric #-}
module Bosh.Operation.Types where

import Data.Text
import Data.Yaml
import Data.Hashable
import GHC.Generics (Generic)

data Operation = Operation { opType :: OperationType, opPath :: OperationPath}
  deriving (Show, Eq)

data OperationType = Remove
                   | Replace { replacement :: Value }
  deriving (Show, Eq)

newtype OperationPath = OperationPath [PathSegment]
  deriving (Show, Eq, Generic)

data ArrayIndex = NumIndex Int
                | LastIndex
                | BeforeIndex Int
                | MapMatcher Text Text
  deriving (Show, Eq, Generic)

data PathSegment = MapSegment { segment :: Text, isOptional :: Bool }
                 | ArraySegment { index :: ArrayIndex}
  deriving (Show, Eq, Generic)

data OperationErr = OperationErr
  deriving (Show, Eq)

instance Hashable ArrayIndex
instance Hashable PathSegment
instance Hashable OperationPath

mandatorySegment :: Text -> PathSegment
mandatorySegment = flip MapSegment False

optionalSegment :: Text -> PathSegment
optionalSegment = flip MapSegment True
