module Bosh.Yaml where

import Bosh.Interpolation
import Bosh.Operation
import Bosh.Operation.Types
import Control.Monad ((>=>))
import Data.HashMap.Strict
import Data.Text
import Data.Yaml

data BoshErr = YamlErr ParseException
             | OpErr OperationErr

wrapParseException :: Either ParseException Value -> Either BoshErr Value
wrapParseException (Left l) = Left $ YamlErr l
wrapParseException (Right r) = Right r

wrapOpErr :: Either OperationErr Value -> Either BoshErr Value
wrapOpErr (Left l) = Left $ OpErr l
wrapOpErr (Right r) = Right r

operate :: FilePath -> [Operation] -> HashMap Text Text -> IO (Either BoshErr Value)
operate f ops vars = do
  doc <- wrapParseException <$> decodeFileEither f
  return $ (wrapOpErr . (flip interpolate vars >=> applyOps ops)) =<< doc
