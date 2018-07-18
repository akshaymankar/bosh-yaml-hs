module Bosh.Yaml where

import Data.Yaml
import Bosh.Operation
import Bosh.Types

wrapParseException :: Either ParseException Value -> Either BoshErr Value
wrapParseException (Left l) = Left $ YamlErr l
wrapParseException (Right r) = Right r

wrapOpErr :: Either OperationErr Value -> Either BoshErr Value
wrapOpErr (Left l) = Left $ OpErr l
wrapOpErr (Right r) = Right r

int :: FilePath -> [Operation] -> IO (Either BoshErr Value)
int f ops = do
  doc <- wrapParseException <$> decodeFileEither f
  return $ (wrapOpErr . applyOps ops) =<< doc
