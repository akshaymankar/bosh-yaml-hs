{-# LANGUAGE OverloadedStrings #-}
module Bosh.Interpolation where

import Bosh.Interpolation.Types
import Bosh.Interpolation.Parsers
import Bosh.Operation.Types
import Data.HashMap.Strict
import Data.Text
import Data.Yaml

import qualified Data.Attoparsec.Text as AT
import qualified Data.Vector as V

leaves :: Value -> [Leaf]
leaves (String x) = [(OperationPath [], parseLeafText x)]
leaves (Number n) = [(OperationPath [], NumValue n)]
leaves (Bool   b) = [(OperationPath [], BoolValue b)]
leaves  Null      = [(OperationPath [], NullValue)]
leaves (Object m) = foldlWithKey' traverseObjectElement [] m
leaves (Array  v) = V.ifoldl traverseArrayElement [] v

traverseObjectElement :: [Leaf] -> Text -> Value -> [Leaf]
traverseObjectElement acc key v = do
  let segment = MapSegment key True
  addSegToAllPaths segment (leaves v) ++ acc

traverseArrayElement :: [Leaf] -> Int -> Value -> [Leaf]
traverseArrayElement acc i v = do
  let segment = ArraySegment $ NumIndex i
  addSegToAllPaths segment (leaves v) ++ acc

addSegToAllPaths :: PathSegment -> [Leaf] -> [Leaf]
addSegToAllPaths _ [] = []
addSegToAllPaths s ((OperationPath segs, val):ls) = (OperationPath (s:segs), val):addSegToAllPaths s ls
