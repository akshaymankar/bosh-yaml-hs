{-# LANGUAGE DeriveGeneric #-}
module Bosh.Operation.Types where

import Data.Text
import Data.Yaml
import Data.Hashable
import GHC.Generics (Generic)

import qualified Data.ByteString.Char8 as B

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
                | MapMatcher Text Text Bool
  deriving (Show, Eq, Generic)

data PathSegment = MapSegment { segment :: Text, isOptional :: Bool }
                 | ArraySegment { index :: ArrayIndex}
  deriving (Show, Eq, Generic)

data OperationErr = MandatoryKeyNotFound Text Value -- expected `key` to be present in `doc`
                  | TypeMismatch PathSegment Value -- expected to find `key` in `doc` due to type mismatch
                  | IndexOutOfBounds Int Value -- expected `index` to be in `doc`
                  | ExtraSegments ArrayIndex [PathSegment] -- expected `index` to be followed by nothing, but found `segments`
                  | MapMatcherError Text Text Value -- expected to find exactly one `key`=`value` in `doc`
                  | UnexpectedSegment PathSegment -- unexpected segment due to reason
  deriving Eq

instance Show OperationErr where
  show = showOperationErr

showOperationErr :: OperationErr -> String
showOperationErr (MandatoryKeyNotFound key doc) = "MandatoryKeyNotFound key = '" ++ unpack key ++ "' doc =\n" ++ toString doc
showOperationErr (TypeMismatch seg doc) = "TypeMismatch segment = " ++ show seg ++ " doc =\n" ++ toString doc ++ "\nbut couldn't due to type mismatch"
showOperationErr (IndexOutOfBounds i doc) = "IndexOutOfBounds index = '" ++ show i ++ ", doc =\n" ++ toString doc
showOperationErr (ExtraSegments i segs) = "ExtraSegments index = " ++ show i ++ ", extra segments = " ++ show segs
showOperationErr (MapMatcherError key value doc) = "MapMatcherError key = '" ++ unpack key ++ "', value = '" ++ unpack value ++ "', doc =\n" ++ toString doc
showOperationErr (UnexpectedSegment seg) = "UnexpectedSegment '" ++ show seg ++ "'"

toString :: Value -> String
toString val = B.unpack $ encode val

instance Hashable ArrayIndex
instance Hashable PathSegment
instance Hashable OperationPath

mandatorySegment :: Text -> PathSegment
mandatorySegment = flip MapSegment False

optionalSegment :: Text -> PathSegment
optionalSegment = flip MapSegment True
