{-# LANGUAGE OverloadedStrings #-}
module Bosh.Interpolation where

import Bosh.Interpolation.Parsers
import Bosh.Interpolation.Types
import Bosh.Operation
import Bosh.Operation.Types
import Data.HashMap.Strict as H
import Data.Text
import Data.Yaml

import qualified Data.Attoparsec.Text as AT
import qualified Data.Vector as V

interpolate :: Value -> HashMap Text Text -> Either OperationErr Value
interpolate input vars =
  let (Interpol _ paths)  = toInterpol input
   in foldlWithKey' (foldfn vars) (Right input) paths

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

toInterpol :: Value -> Interpol
toInterpol v = Interpol v $ fromList $ onlyInterpol $ leaves v

onlyInterpol :: [Leaf] -> [InterpolLeaf]
onlyInterpol [] = []
onlyInterpol ((p, StringValue (IEText _ End)):ls) = onlyInterpol ls
onlyInterpol ((p, StringValue i):ls) = (p,i):onlyInterpol ls
onlyInterpol (l:ls) = onlyInterpol ls

-- TODO: Ideally the OperationErr should not happen because the path obviously exists
-- Maybe there is a way to statically prove that
-- TODO: Rename this function
foldfn :: HashMap Text Text            -- vars
          -> Either OperationErr Value -- accumulator value
          -> OperationPath             -- path to interpolate
          -> InterpolExpr              -- expression to interpolate
          -> Either OperationErr Value -- accumulated value
foldfn vars (Right v) p i = applyOp v $ Operation (Replace $ String $ resolve i vars) p
foldfn _ left _ _ = left

resolve :: InterpolExpr -> HashMap Text Text -> Text
resolve (IEText text rest) valueMap = text `append` resolve rest valueMap
resolve (IEVar var rest) valueMap = resolveVar var valueMap `append` resolve rest valueMap
resolve End _ = ""

resolveVar :: Text -> HashMap Text Text -> Text
resolveVar var = H.lookupDefault ("((" `append` var `append` "))") var
