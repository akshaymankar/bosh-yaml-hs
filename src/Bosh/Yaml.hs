module Bosh.Yaml where

import Data.Yaml
import Bosh.Operation

int :: FilePath -> [Operation] -> IO (Either ParseException Value)
int f os = do
  ev <- decodeFileEither f
  return $ fmap (applyOps os) ev
